import glm, future, algorithm, macros

type
  Mesh[VertexType] = object

  Framebuffer[FragmentType] = object

  GlEnvironment = object
    Position: Vec4f


  DefaultFragmentType = object
    color: Vec4f

  GlslConstraint = enum
    gcCPU
    gcVS
    gcFS


proc newLit(arg: GlslConstraint): NimNode {.compileTime.} =
  case arg
  of gcCPU:
    return bindSym"gcCPU"
  of gcVS:
    return bindSym"gcVS"
  of gcFS:
    return bindSym"gcFS"

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
  if node.kind == nnkLetSection:
    node.expectLen(1)
    node[0].expectKind(nnkIdentDefs)
    result = node
    result[0][0] = node[0][0].withConstraint(min,max)
    return
  if node.kind == nnkAsgn:
    node.expectLen(2)
    result = node
    result[0] = result[0].withConstraint(min,max)
    return
  if node.kind in {nnkDotExpr, nnkSym, nnkIdent}:
    return nnkPragmaExpr.newTree(
      node,
      nnkPragma.newTree(newLit(min), newLit(max))
    )
  if node.kind == nnkPragmaExpr:
    # there is already a given constraint.
    # check if it is valid.
    node.expectLen(2)
    node[1].expectLen(2)
    return node

proc inject_constraints(arg: NimNode): NimNode {.compileTime.} =
  ## expects an ast in single static assignment form and adds to each
  ## nodes to store information about the constraint

  arg.expectKind nnkStmtList

  result = newStmtList()
  for asgn in arg:
    result.add withConstraint(asgn, gcFS, gcCPU)


proc resolve_constraints(arg: NimNode): NimNode {.compileTime.} =
  ## TODO resolve constraints on each nodes
  error("not implemented")

macro render_inner(mesh, arg: typed): untyped =
  let ssaList1 = transform_to_single_static_assignment(arg[6])
  let ssaList2 = inject_constraints(ssaList1)
  let ssaList3 = resolve_constraints(ssaList2)

  echo ssaList2.repr

  error("not implemented")

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
  quit("only implemented on gpu")

proc texture(sampler: Texture3D;            P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented on gpu")

proc texture(sampler: TextureCube;          P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented on gpu")

proc texture(sampler: Texture2DShadow;      P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented on gpu")

proc texture(sampler: TextureCubeShadow;    P: Vec4f; bias: float32 = 0): Vec4f =
  quit("only implemented on gpu")

proc texture(sampler: Texture2DArray;       P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented on gpu")

proc texture(sampler: Texture2DArrayShadow; P: Vec4f): Vec4f =
  quit("only implemented on gpu")

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
