import ../fancygl

let (window, context) = defaultSetup()
let windowsize = window.size

let vertices = arrayBuffer([vec4f(-1,-1,0,1), vec4f(1,-1,0,1), vec4f(0,1,0,1)])
let colors   = arrayBuffer([vec4f( 1, 0,0,1), vec4f(0, 1,0,1), vec4f(0,0,1,1)])
let moreData = arrayBuffer([vec2f(0,0), vec2f(1,0), vec2f(0,1)])

var runGame: bool = true

let timer = newStopWatch(true)
let textObject = createTextObject("HalloWelt")

let projMat : Mat4f = perspective(45'f32, windowsize.x / windowsize.y, 0.1, 100.0)

var scaler = 1'f32
var offset = vec3f(0,0,-10f)

while runGame:
  let time = timer.time.float32
  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_ESCAPE:
      runGame = false

  var state = getKeyboardState(nil)
  offset.x += (state[SCANCODE_S.int].float - state[SCANCODE_F.int].float) * 0.1f
  offset.y += (state[SCANCODE_D.int].float - state[SCANCODE_E.int].float) * 0.1f
  if state[SCANCODE_KP_PLUS.int] != 0:
    scaler *= 1.1
  if state[SCANCODE_KP_MINUS.int] != 0:
    scaler *= 0.9

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  let modelMat = mat4f(1)
       .scale(scaler)
       .translate(offset)
       .rotateZ(time * 0.1)

  let mvp = projMat * modelMat

  textObject.render(mvp)
  #[
  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = 3
    uniforms:
      time = timer.time.float32
    attributes:
      a_vertex = vertices
      a_color  = colors
      moreData
    vertexMain:
      """
      gl_Position = a_vertex;
      v_color = a_color;
      v_color.rg += moreData;
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = v_color;
      """
  ]#

  glSwapWindow(window)
