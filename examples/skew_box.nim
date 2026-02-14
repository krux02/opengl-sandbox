import ../fancygl
import glm/noise

#let (window, context) = defaultSetup(vec2i(640,480))
let (window, context) = defaultSetup()
let windowSize = window.size

glPolygonOffset( 1, 1);
glEnable(GL_POLYGON_OFFSET_FILL)

let faceVertices = arrayBuffer(boxVerticesCornerAtZero)
let numFaceVertices = boxVerticesCornerAtZero.len

let outlineVertices = arrayBuffer([
  vec4f(0,1,1,1),
  vec4f(1,1,1,1),

  vec4f(0,0,1,1),
  vec4f(1,0,1,1),

  vec4f(0,1,0,1),
  vec4f(1,1,0,1),

  vec4f(0,0,0,1),
  vec4f(1,0,0,1),

  vec4f(1,1,1,1),
  vec4f(1,0,1,1),

  vec4f(0,1,1,1),
  vec4f(0,0,1,1),

  vec4f(1,1,0,1),
  vec4f(1,0,0,1),

  vec4f(0,1,0,1),
  vec4f(0,0,0,1),

  vec4f(1,1,1,1),
  vec4f(1,1,0,1),

  vec4f(0,1,1,1),
  vec4f(0,1,0,1),

  vec4f(1,0,1,1),
  vec4f(1,0,0,1),

  vec4f(0,0,1,1),
  vec4f(0,0,0,1),
])

let subLineVertices = arrayBuffer([
  vec4f(0, 0.5f, 1, 1),
  vec4f(1, 0.5f, 1, 1),
  vec4f(0, 0.5f, 0, 1),
  vec4f(1, 0.5f, 0, 1),
  vec4f(0, 0.5f, 1, 1),
  vec4f(0, 0.5f, 0, 1),
  vec4f(1, 0.5f, 1, 1),
  vec4f(1, 0.5f, 0, 1),
])

const GridRadius = 29

var groundGridVertices: ArrayBuffer[Vec4f]

block initGroundGridVertices:
  var buffer: seq[Vec4f] = @[]
  for i in -GridRadius..GridRadius:
    buffer.add vec4f( float32(i), 0, -GridRadius, 1)
    buffer.add vec4f( float32(i), 0,  GridRadius, 1)
    buffer.add vec4f(-GridRadius , 0, float32(i), 1)
    buffer.add vec4f( GridRadius , 0, float32(i), 1)

  groundGridVertices = arrayBuffer(buffer)
  
let groundSurfaceVertices = arrayBuffer([
  vec4f(-GridRadius, 0,-GridRadius, 1),
  vec4f( GridRadius, 0,-GridRadius, 1),
  vec4f( GridRadius, 0, GridRadius, 1),

  vec4f(-GridRadius, 0,-GridRadius, 1),
  vec4f( GridRadius, 0, GridRadius, 1),
  vec4f(-GridRadius, 0, GridRadius, 1)
])

let numOutlineVertices = outlineVertices.len

var runGame: bool = true

let timer = newStopWatch(true)

import glm/noise

var oblique = mat4f(1)
oblique[2].xy = vec2f(-0.5f, -0.5f)

let aspectRatio = float32(windowSize.x / windowSize.y)
let ortho : Mat4f = ortho(-10.0f * aspectRatio, 10.0f * aspectRatio, -10.0f, 10.0f, -128.0f, 128.0f)
let proj : Mat4f = ortho * oblique

var boxPositionsBuffer = createArrayBuffer[Vec4f](length = 21*21, usage = GL_STREAM_DRAW, label = "box positions" )
var boxPositions = newSeq[Vec4f](0)

proc recomputeBoxPositions(time: float32): void =
  boxPositions.setLen(0)
  for x in -10 .. 10:
    for z in -10 .. 10:
      let pos2d = vec2f(x.float32, z.float32)
      let n = perlin(vec3f(pos2d * 0.2, time*0.25f)) * 3
      boxPositions.add vec4f(x.float32, n, z.float32, 0)
  boxPositionsBuffer.setData(boxPositions)

const rotationDuration = 0.675f
  
var rotationStartTime: float32 = -1
var rotationEndTime: float32 = 0
var targetRotation: float32 = 0
var lastRotation: float32 = 0
  
while runGame:
  let time = timer.time.float32
  
  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_ESCAPE:
      runGame = false
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_F10:
      window.screenshot
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_W:
      lastRotation = targetRotation
      targetRotation += float32(PI * 0.5)
      rotationStartTime = time
      rotationEndTime = time + rotationDuration
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_R:
      lastRotation = targetRotation
      targetRotation -= float32(PI * 0.5)
      rotationStartTime = time
      rotationEndTime = time + rotationDuration

  var rotationProgress = clamp((time - rotationStartTime) / (rotationEndTime - rotationStartTime), 0, 1)
  rotationProgress = rotationProgress * rotationProgress * (3 - 2 * rotationProgress)
  let rotation = mix(lastRotation, targetRotation, rotationProgress)

  

  let viewMat = mat4f(1)
    .translate(0,0,0)            # position camera at position 0,1,5
    .rotateY(rotation)               # 
    .inverse                     # the camera matrix needs to be inverted

  recomputeBoxPositions(time)
  
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = groundGridVertices.len
    uniforms:
      modelViewProj = proj * viewMat
    attributes:
      a_vertex = groundGridVertices
    vertexMain:
      """
      gl_Position = modelViewProj * a_vertex;
      """
    fragmentMain:
      """
      color = vec4(vec3(0.5), 1.0);
      """

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = 6
    uniforms:
      modelViewProj = proj * viewMat
    attributes:
      a_vertex = groundSurfaceVertices
    vertexMain:
      """
      gl_Position = modelViewProj * a_vertex;
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = vec4(vec3(0.0), 1.0);
      """
  
  let modelMat = mat4f(1)
  let modelViewProj = proj * modelMat * viewMat

  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = numOutlineVertices
    numInstances  = len(boxPositions)
    uniforms:
      modelViewProj
    attributes:
      a_vertex = outlineVertices
      position = boxPositionsBuffer  {.divisor: 1.}         
    vertexMain:
      """
      gl_Position = modelViewProj * (a_vertex + position);
      """
    fragmentMain:
      """
      color = vec4(1.0);
      """
      
  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = numFaceVertices
    numInstances = len(boxPositions)
    uniforms:
      modelViewProj
    attributes:
      a_vertex = faceVertices
      position = boxPositionsBuffer {.divisor: 1.}
    vertexMain:
      """
      gl_Position = modelViewProj * (a_vertex + position);
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = vec4(vec3(0.0), 1.0);
      """
      
  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = subLineVertices.len
    numInstances = len(boxPositions)
    uniforms:
      modelViewProj
    attributes:
      a_vertex = subLineVertices
      position = boxPositionsBuffer {.divisor: 1.}
    vertexMain:
      """
      gl_Position = modelViewProj * (a_vertex + position);
      """
    fragmentMain:
      """
      color = vec4(0.5, 0.5, 0.5, 1);
      """
  
  glSwapWindow(window)

echo "done"

# Local Variables:
# compile-command: "cd examples; nim c -r skew_box.nim"
# End:
