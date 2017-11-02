import glm, future, algorithm, macros

type
  Mesh[VertexType] = object

  Framebuffer[FragmentType] = object

  GlEnvironment = object
    Position: Vec4f


  DefaultFragmentType = object
    color: Vec4f



dumpTree:
  let a = 23


proc transform_to_single_static_assignment(arg: NimNode): NimNode =
  ## TODO, this is not implemented
  arg.expectKind nnkStmtList

  var stmtList = newStmtList()

  proc genSymForExpression(arg: NimNode): NimNode =
    if arg.kind == nnkCall:
      let sym = genSym(nskLet, "tmp")
      stmtList.add quote do:
        let `sym` = `arg`
      result = sym
    else:
      result = arg

  echo "<ssa>"
  for asgn in arg:
    asgn.expectKind nnkAsgn
    stmtList.add nnkAsgn.newTree(asgn[0], genSymForExpression(asgn[1]))


  echo arg.treeRepr
  echo "</ssa>"
  echo stmtList.treeRepr

  result = stmtList


  error("not implemented")


macro render_inner(mesh, arg: typed): untyped =
  result = transform_to_single_static_assignment(arg[6])

proc `or`(arg, alternative: NimNode): NimNode =
  if arg.kind != nnkEmpty:
    arg
  else:
    alternative

proc injectTypes(framebuffer, mesh, arg: NimNode): NimNode =
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

  echo result.repr

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


var mesh: MyMesh

var framebuffer: MyFramebuffer

var mvp: Mat4f

framebuffer.render(mesh) do (v, gl):
  gl.Position = (mvp * v.position).normalize
  result.color = v.color
