import glm, future, algorithm, macros, strutils, tables

type
  Mesh[VertexType] = object

  Framebuffer[FragmentType] = object

  GlEnvironment = object
    Position: Vec4f

  DefaultFragmentType = object
    color: Vec4f

  GlslConstraint = enum
    gcFS
    gcVS
    gcCPU

  ConstraintRange = tuple[min,max: GlslConstraint]

proc newLit(arg: GlslConstraint): NimNode {.compileTime.} =
  case arg
  of gcCPU:
    return bindSym"gcCPU"
  of gcVS:
    return bindSym"gcVS"
  of gcFS:
    return bindSym"gcFS"

proc enumVal(arg: NimNode): GlslConstraint {.compileTime.} =
  if arg == bindSym("gcCPU"):
    return gcCPU
  if arg == bindSym("gcVS"):
    return gcVS
  if arg == bindSym("gcFS"):
    return gcFS
  error("cannot find the correct value in GlslConstraint", arg)


proc glslType(arg: NimNode): string {.compileTime.} =
  if arg.kind == nnkBracketExpr:
    if arg[0] == bindSym"Vec":
      if arg[2] == bindSym"float32":
        result = "vec"
      elif arg[2] == bindSym"float64":
        result = "dvec"
      elif arg[2] == bindSym"int32":
        result = "ivec"
      elif arg[2] == bindSym"bool":
        result = "bvec"

      arg[1].expectKind nnkIntLit
      let intVal = arg[1].intVal
      if 4 < intVal or intVal < 2:
        error "not compatible", arg
      result.add intVal

    elif arg[0] == bindSym"Mat":
      if arg[2] == bindSym"float32":
        result = "mat"
      elif arg[2] == bindSym"float64":
        result = "dmat"
      elif arg[2] == bindSym"int32":
        result = "imat"
      elif arg[2] == bindSym"bool":
        result = "bmat"

      arg[1].expectKind nnkIntLit
      arg[2].expectKind nnkIntLit

      let intVal1 = arg[1].intVal
      let intVal2 = arg[2].intVal

      if 4 < intVal1 or intVal1 < 2:
        error "not compatible", arg
      if 4 < intVal2 or intVal2 < 2:
        error "not compatible", arg

      result.add intVal1
      result.add "x"
      result.add intVal2
    else:
      return "<error type 1>"
  elif arg == bindSym"float32":
    return "float"
  elif arg == bindSym"Vec2f":
    return "vec2"
  elif arg == bindSym"Vec3f":
    return "vec3"
  elif arg == bindSym"Vec4f":
    return "vec4"
  else:
    echo arg.treeRepr
    return "<error type 2>"

proc newLetStmt(name, typ, value: NimNode): NimNode {.compiletime.} =
  ## Create a new let stmt
  return nnkLetSection.newTree(
    nnkIdentDefs.newTree(name, typ, value)
  )

proc transform_to_single_static_assignment(arg: NimNode): NimNode =
  ## Transforms the argument AST into a list of assignments. The
  ## assignments are either declaring a new identifier, like ``let x =
  ## foo(y,z)``, or they are assignments to already existing variables
  ## like ``gl.Position = foo(x,y)``
  arg.expectKind nnkStmtList

  var assignments = newStmtList()

  proc genSymForExpression(arg: NimNode): NimNode =
    let typ = arg.getTypeInst
    if arg.kind in nnkCallKinds or arg.kind == nnkDotExpr:
      result = genSym(nskLet, "tmp")
      let call = arg.kind.newTree(arg[0])
      for i in 1 ..< arg.len:
        call.add(genSymForExpression(arg[i]))
      assignments.add(newLetStmt(result, typ, call))
    else:
      result = arg

  for asgn in arg:
    asgn.expectKind nnkAsgn
    let sym = genSymForExpression(asgn[1])
    assert assignments[^1][0][0] == sym

    let rhs = assignments[^1][0][2]
    assignments[^1] = nnkAsgn.newTree(asgn[0], rhs)

  return assignments


static:
  # not really great, software design.
  # this table is creater in each call to the render macro and used used to store the constraints of symbols.
  var constraintsTable = initTable[NimNode,ConstraintRange]()
  constraintsTable.clear

import hashes
proc hash(arg: NimNode): Hash =
  result = result !& arg.kind.int
  case arg.kind
  of nnkCharLit..nnkUInt64Lit:
    result = result !& hash(arg.intVal)
  of nnkFloatLit..nnkFloat64Lit:
    result = result !& hash(arg.floatVal)
  of nnkStrLit..nnkTripleStrLit:
    result = result !& hash(arg.strVal)
  of nnkSym, nnkIdent:
    result = result !& hashIgnoreStyle(arg.repr)
  else:
    for child in arg.children:
      result = result !& hash(child)
    result = !$result


proc `constraint=`(arg:NimNode, constraint: ConstraintRange): void =
  constraintsTable[arg] = constraint

proc constraint(arg:NimNode): ConstraintRange =
  constraintsTable[arg]

#[
proc withConstraint(node:NimNode; min, max: GlslConstraint): NimNode =
  if node.kind == nnkPragmaExpr:
    # there is already a given constraint.
    # check if it is valid.
    node.expectLen(2)
    let pragma = node[1]
    pragma.expectLen(2)

    ## TODO do some math here
    let oldMin: GlslConstraint = pragma[0].enumVal
    let oldMax: GlslConstraint = pragma[1].enumVal

    return nnkPragmaExpr.newTree(
      node[0],
      nnkPragma.newTree(newLit( max(min, oldMin)), newLit(min(max, oldMax)))
    )
  else:
    return nnkPragmaExpr.newTree(
      node,
      nnkPragma.newTree(newLit(min), newLit(max))
    )
]#

proc withDefaultConstraint(asgn, glSym, resultSym: NimNode): NimNode {.compileTime.} =
  result = asgn
  if asgn.kind == nnkPragmaExpr:
    asgn[1].expectKind nnkPragma
    asgn[1].expectLen 2
  elif asgn.kind == nnkAsgn:
    let dotExpr = asgn[0]
    dotExpr.expectKind nnkDotExpr
    let lhsSym = if dotExpr[0].kind == nnkHiddenDeref: dotExpr[0][0] else: dotExpr[0]
    lhsSym.expectKind nnkSym
    if cmpIgnoreStyle($lhsSym, $glSym) == 0:
      #warning "no real symbol resolution of " & $glSym # , lhsSym
      lhsSym.constraint = (min: gcVS, max: gcVS)
    elif lhsSym == resultSym:
      lhsSym.constraint = (min: gcFS, max: gcFS)
    else:
      error("needs to be result or gl", lhsSym)
  elif asgn.kind == nnkLetSection:
    asgn.expectLen 1
    let identDefs = asgn[0]
    identDefs.expectLen(3)
    let lhsSym = identDefs[0]
    lhsSym.constraint = (min: gcFS, max: gcCPU)
  else:
    error("Bernhardt", asgn)

proc createGlslMain(arg: NimNode): string {.compileTime.} =
  result = "void main() {\n"
  for assignment in arg:
    var line = "    "
    if assignment.kind == nnkLetSection:
      line.add assignment[0][1].glslType
      line.add " "
      line.add assignment[0][0].repr
      line.add " = "
      line.add assignment[0][2].repr
      line.add "   // "
      line.add $assignment[0][0].constraint
    elif assignment.kind == nnkAsgn:
      assignment[0].expectKind nnkDotExpr
      let dotExpr = assignment[0]
      let lhsSym = if dotExpr[0].kind == nnkHiddenDeref: dotExpr[0][0] else: dotExpr[0]
      if cmpIgnoreStyle(lhsSym.repr, "result") == 0:
        line.add dotExpr[1].repr
      elif cmpIgnoreStyle(lhsSym.repr, "gl") == 0:
        line.add "gl_" & dotExpr[1].repr
      line.add " = "
      line.add assignment[1].repr
      line.add "   // "
      line.add $lhsSym.constraint
    else:
      echo assignment.treerepr
      error "Achim", assignment
    line.add ";\n"
    result.add line
  result.add "}\n"

macro render_even_more_inner(arg: typed): untyped =
  let stmtList = arg[6]
  let glslMain = createGlslMain(stmtList)
  echo glslMain

proc strip_pragma_expressions(arg: NimNode): NimNode {.compileTime.} =
  arg.expectKind nnkStmtList
  result = newStmtList()
  for pragmaExpr in arg:
    pragmaExpr.expectKind  nnkPragmaExpr
    result.add pragmaExpr[0]



#framebuffer.render(mesh) do (v, gl):
#  gl.Position = mvp * v.position
#  result.color = texture(myTexture, v.texCoord)

macro render_inner(mesh, arg: typed): untyped =
  let ssaList1 = transform_to_single_static_assignment(arg[6])

  let resultSym = arg.last
  let glSym = arg[3][2][0]

  # make each assingnment into an assignment that has constraint
  # attached to it
  ssaList1.expectKind nnkStmtList
  let ssaList2 = newStmtList()


  constraintsTable.clear
  for asgn in ssaList1:
    ssaList2.add withDefaultConstraint(asgn, glSym, resultSym)

  #let ssaList3 = resolve_constraints(ssaList2, glSym, resultSym)
  let newArg = arg
  newArg[6] = ssaList2
  #newArg[6] = strip_pragma_expressions(ssaList2)

  result = newCall(bindSym"render_even_more_inner", newArg)

  #echo result.repr

proc `or`(arg, alternative: NimNode): NimNode =
  if arg.kind != nnkEmpty:
    arg
  else:
    alternative

proc injectTypes(framebuffer, mesh, arg: NimNode): NimNode =
  ## Inject types to the ``rendor .. do: ...`` node.
  arg.expectKind(nnkDo)
  let formalParams = arg[3]
  formalParams.expectKind(nnkFormalParams)

  let vertexTypeExpr = quote do:
    `mesh`.type.VertexType

  let fragmentTypeExpr = quote do:
    `framebuffer`.type.FragmentType

  let resultType = formalParams[0] or fragmentTypeExpr

  var vertexArg, vertexType, envArg, envType: NimNode

  case formalParams.len
  of 2:
    let identDefs = formalParams[1]
    identDefs.expectKind nnkIdentDefs

    identDefs[2].expectKind nnkEmpty

    vertexArg = identDefs[0]
    vertexType = vertexTypeExpr
    envArg = identDefs[1]
    envType = nnkVarTy.newTree(bindSym"GlEnvironment")

  of 3:
    formalParams[1].expectKind nnkIdentDefs
    formalParams[2].expectKind nnkIdentDefs

    vertexArg = formalParams[1][0]
    vertexType = formalParams[1][1] or vertexTypeExpr

    envArg = formalParams[2][0]
    envType = formalParams[2][1] or nnkVarTy.newTree(bindSym"GlEnvironment")

  else:
    error("invalid ast", formalParams)

  let newParams =
    nnkFormalParams.newTree(
      resultType,
      nnkIdentDefs.newTree(
        vertexArg, vertexType, newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        envArg, envType, newEmptyNode()
      )
    )

  result = arg
  result[3] = newParams

macro render(framebuffer, mesh: typed; arg: untyped): untyped =
  result = newCall(bindSym"render_inner", mesh, injectTypes(framebuffer, mesh, arg))


## gl wrapper ##

type
  Texture2D            = object
    handle: uint32
  Texture3D            = object
    handle: uint32
  TextureCube          = object
    handle: uint32
  Texture2DShadow      = object
    handle: uint32
  TextureCubeShadow    = object
    handle: uint32
  Texture2DArray       = object
    handle: uint32
  Texture2DArrayShadow = object
    handle: uint32

proc texture*(sampler: Texture2D;            P: Vec2f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture3D;            P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: TextureCube;          P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture2DShadow;      P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: TextureCubeShadow;    P: Vec4f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture2DArray;       P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture2DArrayShadow; P: Vec4f): Vec4f =
  quit("only implemented in shader")

## user code ##

type
  MyFragmentType = object
    color: Vec4f

  MyVertexType = object
    position: Vec4f
    color: Vec4f
    normal: Vec4f
    texCoord: Vec2f

  MyMesh        = Mesh[MyVertexType]
  MyFramebuffer = Framebuffer[MyFragmentType]

var myTexture: Texture2D
var mesh: MyMesh
var framebuffer: MyFramebuffer
var mvp: Mat4f

framebuffer.render(mesh) do (v, gl):
  gl.Position = (mvp * v.position).normalize.normalize.normalize
  result.color = v.color

framebuffer.render(mesh) do (v, gl):
  gl.Position = mvp * v.position
  result.color = texture(myTexture, v.texCoord)
