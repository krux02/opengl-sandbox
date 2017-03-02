
import ../fancygl

let (window, context) = defaultSetup()
let windowsize = window.size

let projection_mat : Mat4f = perspective(45'f32, windowsize.x / windowsize.y, 0.1, 100.0)

var camera = newWorldNode()
camera.moveAbsolute(vec3f(3,3,3))
camera.lookAt(vec3f(0))

var shape = newWorldNode()
shape.pos = vec4f(0,0,0,1)

var vertices: ArrayBuffer[Vec4f]
var colors: ArrayBuffer[Vec4f]

block init:
  let rawVertices = icosphereVertices()
  let rawIndices  = icosphereIndicesTriangles()
  var unrolledVertices = newSeq[Vec4f]()
  var unrolledColors = newSeq[Vec4f]()

  for i in countup(0, rawIndices.len-1, 3):
    for j in 0 ..< 3:
      let idx = rawIndices[i+j]
      unrolledVertices.add rawVertices[idx]

    let color = vec4f(rand_f32(), rand_f32(), rand_f32(), 1'f32)
    unrolledColors.add([color,color,color])

  vertices = arrayBuffer(unrolledVertices)
  colors   = arrayBuffer(unrolledColors)

# prevent opengl call per frame
let verticesLen = vertices.len

var evt: Event = defaultEvent
var runGame: bool = true

while runGame:
  shape.turnAbsoluteZ(0.01)
  shape.turnAbsoluteX(0.005)
  shape.turnAbsoluteY(0.0025)

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown and evt.key.keysym.scancode == SDL_SCANCODE_ESCAPE:
      runGame = false

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices   = verticesLen

    uniforms:
      proj = projection_mat
      modelView = camera.viewMat * shape.modelMat

    attributes:
      a_vertex = vertices
      a_color  = colors
    vertexMain:
      """
      gl_Position = proj * modelView * a_vertex;
      v_color = a_color;
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = v_color;
      """

  glSwapWindow(window)
