
import fancygl
import macros


type
  GlEnvironment = object
    Position: Vec4f
    VertexID: int32

proc `[]`[T](arg: ArrayBuffer[T]): T =
  discard

proc extractUniforms(arg: NimNode): NimNode =
  arg.expectKind nnkStmtList
  for stmt in arg:
    if stmt.kind == nnkLetSection:
      for identDefs in stmt:
        if identDefs[2].kind == nnkCall:
          let call = identDefs[2]
          echo call[0].lispRepr
          echo call[1].getTypeInst.lispRepr

  discard

macro shadingDsl2Inner(arg: typed): untyped =
  echo arg[6].treerepr
  discard extractUniforms(arg[6])

macro shadingDsl2(arg: untyped): untyped =
  arg[^1].expectKind nnkCall
  if $arg[^1][0] != "render":
    error("expect call to ``render``", arg[^1])
  result = arg[^1]
  result[0] = bindSym"shadingDsl2Inner"

type
  MyFragmentType = object
    color: Vec4f

  MyFramebuffer = object


template FragmentType(arg: typedesc[MyFramebuffer]): untyped = MyFragmentType

let (window, context) = defaultSetup()

let aspect = float32(window.size.x / window.size.y)
let projection_mat : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

var framebuffer: MyFramebuffer
var camera: WorldNode
var node: WorldNode

let vertices = arrayBuffer([
  vec4f(1,0,0,1),
  vec4f(0,1,0,1),
  vec4f(0,0,1,1),
])

let colors = arrayBuffer([
  vec4f(1,0,0,1),
  vec4f(0,1,0,1),
  vec4f(0,0,1,1),
])

let n = vec4f(normalize(vec3f(1)), 0)
let normals = arrayBuffer([n,n,n])

## lightweight MeshView object


shadingDsl2:
  primitiveMode = GL_TRIANGLES
  numVertices = mesh.numVertices
  vertexOffset = mesh.vertexOffset
  baseVertex = mesh.baseVertex
  indices = indices
  render do (gl: var GlEnvironment) -> framebuffer.type.FragmentType:
    let
      a_vertex = vertices[]
      a_normal = normals[]
    let a_color  = colors[]
    let proj = projection_mat
    let modelView = camera.viewMat * node.modelMat
    gl.Position = proj * modelView * a_vertex
    let v_vertex = a_vertex
    let v_normal = modelView * a_normal
    let v_color = a_color
    result.color = v_color * v_normal.z
