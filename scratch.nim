import glm, future, algorithm, macros, strutils, tables, sequtils


## gl wrapper ##

type
  Texture1D            = object
    handle: uint32
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

# other types

type
  Mesh[VertexType] = object

  Framebuffer[FragmentType] = object

  GlEnvironment = object
    Position: Vec4f
    PointSize: float32
    ClipDistance: UncheckedArray[float32]

  DefaultFragmentType = object
    color: Vec4f

  GlslConstraint = enum
    gcFS
    gcVS
    gcCPU

  ConstraintRange = tuple[min,max: GlslConstraint]

iterator arguments*(n: NimNode): NimNode {.inline.} =
  ## Iterates over the arguments of a call ``n``.
  for i in 1 ..< n.len:
    yield n[i]

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
      let Tsym = arg[3]
      if Tsym == bindSym"float32":
        result = "mat"
      elif Tsym == bindSym"float64":
        result = "dmat"
      elif Tsym == bindSym"int32":
        result = "imat"
      elif Tsym == bindSym"bool":
        result = "bmat"
      else:
        echo arg.treeRepr
        result = "<error type 4>"

      arg[1].expectKind nnkIntLit
      arg[2].expectKind nnkIntLit

      let intVal1 = arg[1].intVal
      let intVal2 = arg[2].intVal

      if 4 < intVal1 or intVal1 < 2:
        error "not compatible", arg
      if 4 < intVal2 or intVal2 < 2:
        error "not compatible", arg


      result.add intVal1
      if intVal2 != intVal1:
        result.add "x"
        result.add intVal2
    else:
      echo arg.repr
      return "<error type 1>"
  elif arg.kind == nnkSym:
    if arg == bindSym"float32":
      return "float"
    elif arg == bindSym"float64":
      return "double"
    elif arg == bindSym"int32":
      return "int"
    elif arg == bindSym"bool":
      return "bool"

    elif arg == bindSym"Texture1D":
      return "sampler1D"
    elif arg == bindSym"Texture2D":
      return "sampler2D"
    elif arg == bindSym"Texture3D":
      return "sampler3D"
    elif arg == bindSym"TextureCube":
      return "samplerCube"
    elif arg == bindSym"Texture2DShadow":
      return "sampler2DShadow"
    elif arg == bindSym"TextureCubeShadow":
      return "samplerCubeShadow"
    elif arg == bindSym"Texture2DArray":
      return "sampler2DArray"
    elif arg == bindSym"Texture2DArrayShadow":
      return "sampler2DArrayShadow"

    else:
      let typ2 = arg.getTypeImpl
      if typ2.kind == nnkBracketExpr:
        return typ2.glslType
      else:
        echo arg.treeRepr
        return "<error type 3>"
  else:
    echo arg.treeRepr
    return "<error type 2>"

proc transform_to_single_static_assignment(stmtList: NimNode): NimNode {.compileTime.} =
  ## Transforms the argument AST into a list of assignments. The
  ## assignments are either declaring a new identifier, like ``let x =
  ## foo(y,z)``, or they are assignments to already existing variables
  ## like ``gl.Position = foo(x,y)``
  stmtList.expectKind nnkStmtList
  var assignments = newStmtList()

  proc genSymForExpression(expr: NimNode): NimNode {.compileTime.}=
    let typ = expr.getTypeInst
    if expr.kind in nnkCallKinds or expr.kind == nnkDotExpr:
      result = genSym(nskLet, "tmp")
      let call = expr.kind.newTree(expr[0])
      for i in 1 ..< expr.len:
        call.add(genSymForExpression(expr[i]))
      assignments.add(nnkLetSection.newTree(
        nnkIdentDefs.newTree(result, typ, call))
      )
    else:
      result = expr

  for asgn in stmtList:
    asgn.expectKind nnkAsgn
    let sym = genSymForExpression(asgn[1])
    assert assignments[^1][0][0] == sym

    let rhs = assignments[^1][0][2]
    assignments[^1] = nnkAsgn.newTree(asgn[0], rhs)

  return assignments


static:
  # not really great, software design.
  # this table is created in each call to the render macro and used used to store the constraints of symbols.
  var constraintsTable = initTable[NimNode,ConstraintRange]()
  constraintsTable.clear


iterator iterateSSAList(arg: NimNode): tuple[kind:NimNodeKind; lhs, typ, rhs: NimNode] {.inline.} =
  arg.expectKind nnkStmtList
  for asgn in arg:
    asgn.expectKind {nnkLetSection,nnkAsgn}
    var lhs,typ,rhs: NimNode
    if asgn.kind == nnkLetSection:
      lhs = asgn[0][0]
      typ = asgn[0][1]
      rhs = asgn[0][2]
    elif asgn.kind == nnkAsgn:
      lhs = asgn[0]
      rhs = asgn[1]
      typ = asgn[0].getTypeInst

    yield((asgn.kind, lhs, typ, rhs))

import hashes
proc hash(arg: NimNode): Hash {.compileTime.}=
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

proc firstSymbol(arg : NimNode; default:NimNode): NimNode {.compileTime.} =
  result = arg
  while result.kind != nnkSym:
    if result.len < 1:
      return default
    else:
      result = result[0]

proc firstSymbol(arg:NimNode): NimNode {.compileTime.} =
  result = firstSymbol(arg, nil)
  if result == nil:
    error("Cedrić " & arg.lispRepr)

proc `constraint=`(arg:NimNode, constraint: ConstraintRange): void  {.compileTime.} =
  constraintsTable[arg.firstSymbol] = constraint

proc constraint(arg:NimNode): ConstraintRange {.compileTime.} =
  constraintsTable[arg.firstSymbol]

proc hasConstraint(arg: NimNode): bool {.compileTime.} =
  let sym = arg.firstSymbol(nil)
  if sym != nil:
    constraintsTable.hasKey(sym)
  else:
    false

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




proc createGlslAttributesSection(arg: NimNode): string {.compileTime.} =
  result = ""
  let vertexName = arg[0].repr
  let objectTy = arg[1].getTypeImpl
  objectTy.expectKind nnkObjectTy
  let recList = objectTy[2]
  recList.expectKind nnkRecList
  var i = 0
  for identDefs in recList:
    let typ = identDefs[^2]
    for j in 0 ..< identDefs.len - 2:
      let sym = identDefs[j]
      result.add "layout(location = "
      result.add i
      result.add ") in "
      result.add typ.glslType
      result.add " "
      result.add vertexName
      result.add "_"
      result.add sym.repr
      result.add ";\n"
      i += 1

  discard


proc add[T](arg: var Table[T, seq[T]]; key, value: T): void =
  if arg.hasKey(key):
    let s: ptr seq[T] = arg[key].addr
    if value notin s[]:
      s[].add(value)
  else:
    arg[key] = @[value]

proc resolveConstraints(arg: NimNode): void {.compileTime.} =

  # build dependency graph
  # let a = foo(b,c,d)  --> a depends on {b,c,d}
  # and the inverse:    --> b,c,d all depend on a

  var dependencyGraph = initTable[NimNode, seq[NimNode]]()
  var inverseDependencyGraph = initTable[NimNode, seq[NimNode]]()

  for kind, lhs, typ, rhs in arg.iterateSSAList:
    let lhs = lhs.firstSymbol
    let lhsConstraint = lhs.constraint
    if rhs.kind in nnkCallKinds:
      var dependencies = newSeq[NimNode](0)
      for arg in rhs.arguments:
        if arg.hasConstraint:
          dependencyGraph.add(lhs,arg)
          inverseDependencyGraph.add(arg,lhs)

    elif rhs.kind == nnkDotExpr:
      discard
      # TODO maybe do something with this
    else:
      echo rhs.kind
      error("David")

  for key, dependencies in dependencyGraph:
    dependencyGraph[key] = deduplicate(dependencyGraph[key])

  ## done building dependency graph

  proc growMinConstraint(arg: NimNode; newMinConstraint: GlslConstraint): void
  proc updateDependentConstraints(arg: NimNode, newMinConstraint: GlslConstraint): void =
    if dependencyGraph.hasKey(arg):
      for dep in dependencyGraph[arg]:
        dep.growMinConstraint newMinConstraint

  proc shrinkMaxConstraint(arg: NimNode; newMaxConstraint: GlslConstraint): void
  proc updateDependentConstraints(arg: NimNode, newMaxConstraint: GlslConstraint): void =
    if inverseDependencyGraph.hasKey(arg):
      for dep in inverseDependencyGraph[arg]:
        dep.shrinkMaxConstraint newMaxConstraint

  proc growMinConstraint(arg: NimNode; newMinConstraint: GlslConstraint): void =
    if arg.hasConstraint:
      var argConstraint = arg.constraint
      if argConstraint.min < newMinConstraint:
        argConstraint.min = newMinConstraint
        arg.constraint = argConstraint
        arg.updateDependentConstraints(newMinConstraint = newMinConstraint)

  proc shrinkMaxConstraint(arg: NimNode; newMaxConstraint: GlslConstraint): void =
    if arg.hasConstraint:
      var argConstraint = arg.constraint
      if argConstraint.max > newMaxConstraint:
        argConstraint.max = newMaxConstraint
        arg.constraint = argConstraint
        arg.updateDependentConstraints(newMaxConstraint = newMaxConstraint)
        ## TODO update dependen Constraint maxima


  for _, lhs, _, rhs in arg.iterateSSAList:
    let lhsConstraint = lhs.constraint
    if rhs.kind in nnkCallKinds:
      for i in 1 ..< rhs.len:
        let arg = rhs[i]
        arg.growMinConstraint(lhsConstraint.min)
    elif rhs.kind == nnkDotExpr:
      echo "assuming ", rhs.lispRepr, " is an attribute, not tested"
      shrinkMaxConstraint(lhs, gcVS)



proc createGlslFragmentTypeSection(arg: NimNode): string {.compileTime.} =
  result = ""
  let objectTy = arg.getTypeImpl
  objectTy.expectKind nnkObjectTy
  let recList = objectTy[2]
  recList.expectKind nnkRecList

  var i = 0
  for identDefs in recList:
    let typ = identDefs[^2].glslType
    for j in 0 ..< identDefs.len - 2:
      let sym = identDefs[j]
      result.add "layout(location = "
      result.add i
      result.add ") out "
      result.add typ
      result.add " "
      result.add sym.repr
      result.add ";\n"
      i += 1

proc createGlslUniformsSection(stmtList: NimNode): string {.compileTime.} =
  var localSymbols = newSeq[NimNode](0)
  var externalSymbols = newSeq[NimNode](0)
  stmtList.expectKind nnkStmtList
  for letSection in stmtList:
    if letSection.kind == nnkLetSection:
      let identDefs = letSection[0]
      identDefs.expectKind nnkIdentDefs
      let lhsSym = identDefs[0]
      lhsSym.expectKind nnkSym

      let rhs = identDefs[2]
      if rhs.kind in nnkCallKinds:
        for i in 1 ..< rhs.len:
          let argSym = rhs[i]
          if argSym.kind == nnkSym and argSym notin localSymbols:
            externalSymbols.add argSym

      localSymbols.add lhsSym

  result = ""

  for sym in externalSymbols:
    result.add "uniform "
    result.add sym.getTypeInst.glslType
    result.add " "
    result.add sym.repr
    result.add ";\n"

proc flatDotExpr(arg: NimNode): string {.compileTime.} =
  if arg.kind == nnkDotExpr:
    result = arg[0].repr & "_" & arg[1].repr
  else:
    result = arg.repr

proc splitVertexFragmentShader(arg: NimNode): tuple[cpu,vs,fs: NimNode] {.compileTime.} =
  for asgn in arg:
    var lhs,rhs: NimNode
    if asgn.kind == nnkLetSection:
      lhs = asgn[0][0]
      rhs = asgn[0][2]
    elif asgn.kind == nnkAsgn:
      lhs = asgn[0]
      rhs = asgn[1]
      #let lhsSym = if dotExpr[0].kind == nnkHiddenDeref: dotExpr[0][0] else: dotExpr[0]

    echo lhs.repr, " ", lhs.constraint

  #[
  for assignment in arg:
    var line = "    "
    if assignment.kind == nnkLetSection:
      line.add assignment[0][1].glslType
      line.add " "
      line.add assignment[0][0].repr
      line.add " = "
      line.add assignment[0][2].flatDotExpr
      line.add "   // "
      line.add $assignment[0][0].constraint.max
    elif assignment.kind == nnkAsgn:
      assignment[0].expectKind nnkDotExpr
      let dotExpr = assignment[0]
      let lhsSym = if dotExpr[0].kind == nnkHiddenDeref: dotExpr[0][0] else: dotExpr[0]
      if cmpIgnoreStyle(lhsSym.repr, "result") == 0:
        line.add dotExpr[1].repr
      elif cmpIgnoreStyle(lhsSym.repr, "gl") == 0:
        line.add "gl_" & dotExpr[1].repr
      line.add " = "
      line.add assignment[1].flatDotExpr
      line.add "   // "
      line.add $lhsSym.constraint
    else:
      echo assignment.treerepr
      error "Achim", assignment
    line.add ";\n"
    result.add line
  ]#


proc createGlslMain(arg: NimNode): string {.compileTime.} =
  result = "void main() {\n"
  for assignment in arg:
    var line = "    "
    if assignment.kind == nnkLetSection:
      line.add assignment[0][1].glslType
      line.add " "
      line.add assignment[0][0].repr
      line.add " = "
      line.add assignment[0][2].flatDotExpr
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
      line.add assignment[1].flatDotExpr
      line.add "   // "
      line.add $lhsSym.constraint
    else:
      echo assignment.treerepr
      error "Achim", assignment
    line.add ";\n"
    result.add line
  result.add "}\n"

proc strip_pragma_expressions(arg: NimNode): NimNode {.compileTime.} =
  arg.expectKind nnkStmtList
  result = newStmtList()
  for pragmaExpr in arg:
    pragmaExpr.expectKind  nnkPragmaExpr
    result.add pragmaExpr[0]


macro render_inner(mesh, arg: typed): untyped =
  arg.expectKind nnkDo
  let ssaList1 = transform_to_single_static_assignment(arg[6])

  let resultSym = arg.last
  let glSym = arg[3][2][0]

  # make each assingnment into an assignment that has constraint
  # attached to it
  ssaList1.expectKind nnkStmtList
  let ssaList2 = newStmtList()


  let vSym = arg[3][1][0]
  let vSymUsage = arg[^2][1][1][0]
  echo vSym.lispRepr, vSymUsage.lispRepr, vSym == vSymUsage

  echo arg.treeRepr
  constraintsTable.clear
  for asgn in ssaList1:
    ssaList2.add withDefaultConstraint(asgn, glSym, resultSym)

  let formalParams = arg[3]
  formalParams.expectKind nnkFormalParams


  ssaList2.resolveConstraints

  let uniformsSection = ssaList2.createGlslUniformsSection
  let vertexTypeSection = formalParams[1].createGlslAttributesSection
  let fragmentTypeSection = formalParams[0].createGlslFragmentTypeSection
  let glslMain = createGlslMain(ssaList2)

  let (_,_,_) = ssaList2.splitVertexFragmentShader

  let shaderSource = uniformsSection & "\n" & vertexTypeSection & "\n" & fragmentTypeSection & "\n" & glslMain
  echo shaderSource

proc `or`(arg, alternative: NimNode): NimNode {.compileTime.} =
  if arg.kind != nnkEmpty:
    arg
  else:
    alternative

proc injectTypes(framebuffer, mesh, arg: NimNode): NimNode {.compileTime.} =
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
  result.color = texture(myTexture, v.texCoord) + vec4(sin(float32(Pi)))
