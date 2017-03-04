
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
  var unrolledVertices = newSeq[Vec4f]()
  var unrolledColors = newSeq[Vec4f]()

  for i in countup(0, icosphereIndicesTriangles.len-1, 3):
    for j in 0 ..< 3:
      let idx = icosphereIndicesTriangles[i+j]
      unrolledVertices.add icosphereVertices[idx]

    let color = vec4f(rand_f32(), rand_f32(), rand_f32(), 1'f32)
    unrolledColors.add([color,color,color])

  vertices = arrayBuffer(unrolledVertices)
  colors   = arrayBuffer(unrolledColors)

const numSegments = 32

let coneVertices = arrayBuffer(coneVertices(numSegments))
let coneNormals = arrayBuffer(coneNormals(numSegments))
let coneTexCoords = arrayBuffer(coneTexCoords(numSegments))
let coneIndices = elementArrayBuffer(coneIndices(numSegments))

#var planeVertices = arrayBuffer(infinitePlaneVertices(vec4f(1,2,3,0)))
#let planeMat      = infinitePlaneMat(vec4f(1,2,3,0))
var planeVertices = arrayBuffer([
  vec4f(0,0,0,1), vec4f( 10,   0,0,1), vec4f(   0, 10,0,1),
  vec4f(0,0,0,1), vec4f(   0, 10,0,1), vec4f(-10,   0,0,1),
  vec4f(0,0,0,1), vec4f(-10,   0,0,1), vec4f(   0,-10,0,1),
  vec4f(0,0,0,1), vec4f(   0,-10,0,1), vec4f( 10,   0,0,1)
])

# prevent opengl call per frame
let verticesLen = vertices.len

var evt: Event = defaultEvent
var runGame: bool = true

var frame = 0

while runGame:
  frame += 1
  shape.turnAbsoluteZ(0.01)
  shape.turnRelativeX(0.015)
  shape.turnRelativeY(0.0025)

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown and evt.key.keysym.scancode == SDL_SCANCODE_ESCAPE:
      runGame = false

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  let magic = int32(frame mod 2)

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices   = verticesLen

    uniforms:
      proj = projection_mat
      modelView = camera.viewMat * shape.modelMat
      magic

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
      if(int(gl_FragCoord.x + gl_FragCoord.y) % 2 == magic)
        discard;
      color = v_color;
      """

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = coneIndices.len

    indices = coneIndices

    uniforms:
      proj = projection_mat
      modelView = camera.viewMat * shape.modelMat
      magic

    attributes:
      a_vertex   = coneVertices
      a_normal   = coneNormals
      a_texCoord = coneTexCoords

    vertexMain:
      """
      gl_Position = proj * modelView * a_vertex;
      v_normal = modelView * a_normal;
      v_texCoord = a_texCoord;
      """
    vertexOut:
      "out vec4 v_normal"
      "out vec2 v_texCoord"

    fragmentMain:
      """
      if(int(gl_FragCoord.x + gl_FragCoord.y) % 2 != magic)
        discard;
      color.rg = v_texCoord;
      color.b = v_normal.z;
      """

  let invProj = inverse(projection_mat)# * inverse(diag(vec4f(vec2f(windowsize.xy), 1,1)))

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = planeVertices.len

    uniforms:
      proj = projection_mat
      modelView = camera.viewMat * shape.modelmat
      u_normal = vec4f(1,2,3,0)
      invProj    = inverse(projection_mat);
      windowSize = vec2f(window.size)

    attributes:
      a_vertex   = planeVertices
      a_normal   = coneNormals
      a_texCoord = coneTexCoords

    vertexMain:
      """
      gl_Position = proj * modelView * a_vertex;
      v_normal = modelView * a_normal;
      v_texCoord = a_texCoord;
      """
    vertexOut:
      "out vec4 v_normal"
      "out vec2 v_texCoord"

    fragmentMain:
      """
      color = vec4(1,0,1,0);
      vec4 tmp = gl_FragCoord;
      vec4 ndc_pos = vec4( ( vec3(gl_FragCoord.xy / windowSize, gl_FragDepth) ) * 2 - 1, 1 );
      vec4 viewPos = invProj * ndc_pos;
      viewPos /= viewPos.w;

      color.rgb = fract(viewPos.xyz * 10);
      color.a = 1;
      """

  glSwapWindow(window)
