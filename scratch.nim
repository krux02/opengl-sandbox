import glm, future, algorithm, macros, strutils

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



proc transform_to_single_static_assignment(arg: NimNode): NimNode =
  ## Transforms the argument AST into a list of assignments. The
  ## assignments are either declaring a new identifier, like ``let x =
  ## foo(y,z)``, or they are assignments to already existing variables
  ## like ``gl.Position = foo(x,y)``
  arg.expectKind nnkStmtList

  var assignments = newStmtList()

  proc genSymForExpression(arg: NimNode): NimNode =
    if arg.kind in nnkCallKinds or arg.kind == nnkDotExpr:
      result = genSym(nskLet, "tmp")
      let call = arg.kind.newTree(arg[0])
      for i in 1 ..< arg.len:
        call.add(genSymForExpression(arg[i]))
      assignments.add(newLetStmt(result, call))
    else:
      result = arg

  for asgn in arg:
    asgn.expectKind nnkAsgn
    let sym = genSymForExpression(asgn[1])
    assert assignments[^1][0][0] == sym

    let rhs = assignments[^1][0][2]
    assignments[^1] = nnkAsgn.newTree(asgn[0], rhs)

  return assignments

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


proc inject_constraints(arg: NimNode): NimNode {.compileTime.} =
  ## expects an ast in single static assignment form and adds to each
  ## nodes to store information about the constraint

  arg.expectKind nnkStmtList

  result = newStmtList()
  for asgn in arg:
    result.add withConstraint(asgn, gcFS, gcCPU)


proc resolve_constraints(arg, glSym, resultSym: NimNode): NimNode {.compileTime.} =
  arg.expectKind(nnkStmtList)
  result = newStmtList()
  for pragmaExpr in arg:
    pragmaExpr.expectKind nnkPragmaExpr
    let asgn = pragmaExpr[0]
    asgn.expectKind({nnkAsgn, nnkLetSection})
    if asgn.kind == nnkAsgn:
      let dotExpr = asgn[0]
      dotExpr.expectKind nnkDotExpr
      let lhsSym = if dotExpr[0].kind == nnkHiddenDeref: dotExpr[0][0] else: dotExpr[0]
      lhsSym.expectKind nnkSym
      if cmpIgnoreStyle($lhsSym, $glSym) == 0:
        warning "no real symbol resolution of " & $glSym # , lhsSym
        result.add pragmaExpr.withConstraint(gcVS, gcVS)
      elif lhsSym == resultSym:
        result.add pragmaExpr.withConstraint(gcFS, gcFS)
      else:
        error("needs to be result or gl", lhsSym)
    else:
      result.add pragmaExpr

  ## TODO this is not resolving anything except setting basic constraints

proc createGlslMain(arg: NimNode): string {.compileTime.} =
  result = "void main() {\n"

  for assignment in arg:
    echo assignment.treeRepr

    echo assignment[0].getTypeInst.treeRepr
    echo assignment[0][0].getTypeInst.treeRepr
    echo assignment[0][2].getTypeInst.repr


proc strip_pragma_expressions(arg: NimNode): NimNode {.compileTime.} =
  arg.expectKind nnkStmtList
  result = newStmtList()
  for pragmaExpr in arg:
    pragmaExpr.expectKind  nnkPragmaExpr
    result.add pragmaExpr[0]



macro render_even_more_inner(arg: typed): untyped =
  let stmtList = arg[6]
  let glslMain = createGlslMain(stmtList)
  echo glslMain



macro render_inner(mesh, arg: typed): untyped =
  let ssaList1 = transform_to_single_static_assignment(arg[6])
  let ssaList2 = inject_constraints(ssaList1)
  let resultSym = arg.last
  let glSym = arg[3][2][0]
  let ssaList3 = resolve_constraints(ssaList2, glSym, resultSym)
  echo ssaList3.repr

  let newArg = arg
  newArg[6] = strip_pragma_expressions(ssaList3)

  result = newCall(bindSym"render_even_more_inner", newArg)

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
  Texture2D = object
    handle: uint32

  Texture3D = object
    handle: uint32

  TextureCube = object
    handle: uint32

  Texture2DShadow = object
    handle: uint32

  TextureCubeShadow = object
    handle: uint32

  Texture2DArray = object
    handle: uint32

  Texture2DArrayShadow = object
    handle: uint32

proc texture(sampler: Texture2D;            P: Vec2f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture(sampler: Texture3D;            P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture(sampler: TextureCube;          P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture(sampler: Texture2DShadow;      P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture(sampler: TextureCubeShadow;    P: Vec4f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture(sampler: Texture2DArray;       P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture(sampler: Texture2DArrayShadow; P: Vec4f): Vec4f =
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
