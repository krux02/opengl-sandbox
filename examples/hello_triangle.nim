import ../fancygl

let (window, context) = defaultSetup()

let vertices = arrayBuffer([vec4f(-1,-1,0,1), vec4f(1,-1,0,1), vec4f(0,1,0,1)])
let colors   = arrayBuffer([vec4f( 1, 0,0,1), vec4f(0, 1,0,1), vec4f(0,0,1,1)])
let moreData = arrayBuffer([vec2f(0,0), vec2f(1,0), vec2f(0,1)])

var evt: Event
var runGame: bool = true

let timer = newStopWatch(true)

while runGame:
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown and evt.key.keysym.scancode == SDL_SCANCODE_ESCAPE:
      runGame = false

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

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
      gl_Position.x += sin(time) * 0.1;
      gl_Position.y += cos(time) * 0.1;
      v_color = a_color;
      v_color.rg += moreData;
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = v_color;
      """

  glSwapWindow(window)
