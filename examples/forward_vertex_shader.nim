import ../fancygl, colors

let (window, context) = defaultSetup()

let vertices = arrayBuffer([vec4f(-1,-1,0,1), vec4f(1,-1,0,1), vec4f(0,1,0,1)])
let colors   = arrayBuffer([vec4f( 1, 0,0,1), vec4f(0, 1,0,1), vec4f(0,0,1,1)])

var runGame: bool = true

let timer = newStopWatch(true)

while runGame:
  for evt in events():
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
    vertexMain:
      """
      v_color = a_color;
      v_pos = a_vertex;
      """
    vertexOut:
      "out vec4 v_color"
      "out vec4 v_pos"
    geometryMain:
      "layout(line_strip, max_vertices=30) out"
      """
      vec4 center = (v_pos[0] + v_pos[1] + v_pos[2]) * 0.333333;

      for(int i = 0; i < 30; ++i) {
        gl_Position = mix(v_pos[i % 3], center, float(i) * 0.0333333);
        g_color = v_color[i % 3];
        EmitVertex();
      }
      """
    geometryOut:
      "out vec4 g_color"
    fragmentMain:
      """
      color = g_color;
      """

  glSwapWindow(window)
