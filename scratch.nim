import glm, future, algorithm, macros

var data = [5,4,6,3,7,2,8,1,9]

echo data

type
  Mesh = object

  GlEnvironment = object
    Position: Vec4f

  MyFragmentType = object
    color: Vec4f

  MyVertexType = object
    position: Vec4f
    color: Vec4f
    normal: Vec4f
    texCoord: Vec2f

var mesh: Mesh

macro render_inner(mesh: Mesh; arg: typed): untyped =
  echo arg.treeRepr

proc injectTypes(arg: NimNode): NimNode =
  let formalParams = arg[3]

  let resultType = bindSym"MyFragmentType"

  let vertexArg = formalParams[1][0]  ## todo this needs logic
  let vertexType = bindSym"MyVertexType"

  let envArg = formalParams[1][1]  ## todo this needs logic
  let envType = nnkVarTy.newTree(bindSym"GlEnvironment")

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

macro render(mesh: Mesh; arg: untyped): untyped =
  result = newCall(bindSym"render_inner", mesh, injectTypes(arg))

var mvp: Mat4f

#[
do (v: MyVertexType,gl:var GlEnvironment) -> MyFragmentType:
FormalParams
    Sym "MyFragmentType"
    IdentDefs
      Ident !"v"
      Sym "MyVertexType"
      Empty
    IdentDefs
      Ident !"gl"
      VarTy
        Sym "GlEnvironment"
      Empty
]#

render(mesh) do (v, gl):
  gl.Position = mvp * v.position
  result.color = v.color
