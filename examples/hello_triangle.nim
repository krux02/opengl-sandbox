import ../fancygl

let (window, context) = defaultSetup(vec2i(640,480))

let vertices = arrayBuffer([
  vec4f(-1,-1, 0, 1),
  vec4f( 1,-1, 0, 1),
  vec4f( 0, 1, 0, 1)
])

let colors   = arrayBuffer([
  vec4f( 1, 0,0,1),
  vec4f(0, 1,0,1),
  vec4f(0,0,1,1)
])

var runGame: bool = true

let timer = newStopWatch(true)

let aspect = window.aspectRatio.float32
let projMat : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

while runGame:

  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_ESCAPE:
      runGame = false
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_F10:
      window.screenshot

  #let time = (frame / 100) * Pi * 2
  let time = timer.time.float32

  let viewMat = mat4f(1)
    .translate(0,1,5)            # position camera at position 0,1,5
    .rotateX(Pi * -0.05)         # look a bit down
    .inverse                     # the camera matrix needs to be inverted

  let modelMat = mat4f(1)
    .rotateY(time)               # rotate the triangle
    .scale(3)                    # scale the triangle to be big enough on screen

  let mvp = projMat * viewMat * modelMat

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = 3
    uniforms:
      mvp
    attributes:
      a_vertex = vertices
      a_color  = colors
    vertexMain:
      """
      gl_Position = mvp * a_vertex;
      v_color = a_color;
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = v_color;
      """

  glSwapWindow(window)

echo "done"
