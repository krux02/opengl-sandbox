# OpenGL example using SDL2

import sdl2, opengl, math, glm, fancygl, sequtils

var vertex = @[
  vec3f(+1, +1, -1), vec3f(-1, +1, -1), vec3f(-1, +1, +1),
  vec3f(+1, +1, +1), vec3f(+1, +1, -1), vec3f(-1, +1, +1),
  vec3f(+1, -1, +1), vec3f(-1, -1, +1), vec3f(-1, -1, -1),
  vec3f(+1, -1, -1), vec3f(+1, -1, +1), vec3f(-1, -1, -1),
  vec3f(+1, +1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, +1),
  vec3f(+1, -1, +1), vec3f(+1, +1, +1), vec3f(-1, -1, +1),
  vec3f(+1, -1, -1), vec3f(-1, -1, -1), vec3f(-1, +1, -1),
  vec3f(+1, +1, -1), vec3f(+1, -1, -1), vec3f(-1, +1, -1),
  vec3f(-1, +1, +1), vec3f(-1, +1, -1), vec3f(-1, -1, -1),
  vec3f(-1, -1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, -1),
  vec3f(+1, +1, -1), vec3f(+1, +1, +1), vec3f(+1, -1, +1),
  vec3f(+1, -1, -1), vec3f(+1, +1, -1), vec3f(+1, -1, +1)
]

var normal = @[
  vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
  vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
  vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
  vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
  vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
  vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
  vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
  vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
  vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
  vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
  vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0),
  vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0)
]

var color = @[
  vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
  vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
  vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
  vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
  vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
  vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
  vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
  vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
  vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
  vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
  vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0),
  vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0)
]

var texcoord = @[
  vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
  vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),
  vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
  vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),
  vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
  vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),
  vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
  vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),
  vec2f(1, 1), vec2f(1, 0), vec2f(0, 0),
  vec2f(0, 1), vec2f(1, 1), vec2f(0, 0),
  vec2f(1, 0), vec2f(1, 1), vec2f(0, 1),
  vec2f(0, 0), vec2f(1, 0), vec2f(0, 1)
]

discard sdl2.init(INIT_EVERYTHING)

var screenWidth: cint = 640
var screenHeight: cint = 480

let window = createWindow("SDL/OpenGL Skeleton", 100, 100, screenWidth, screenHeight, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE)
let context = window.glCreateContext()

# Initialize OpenGL
loadExtensions()

var crateTexture = loadAndBindTexture2DFromFile("crate.png")
nil_Texture2D.bindIt

if 0 != glSetSwapInterval(-1):
  echo "glSetSwapInterval -1 not supported"
  echo sdl2.getError()
  if 0 != glSetSwapInterval(1):
    echo "but glSetSwapInterval 1 is ok"
  else:
    echo "even 1 is not ok"
    echo sdl2.getError()

glClearColor(0.0, 0.0, 0.0, 1.0)                  # Set background color to black and opaque
glClearDepth(1.0)                                 # Set background depth to farthest
glEnable(GL_DEPTH_TEST)                           # Enable depth testing for z-culling
glDepthFunc(GL_LEQUAL)                            # Set the type of depth-test
glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST) # Nice perspective corrections

const glslCode = """
vec4 mymix(vec4 color, float alpha) {
  float a = 3*(alpha/3 - floor(alpha/3));

  float x = 1 - min(1, min(a, 3-a));
  float y = 1 - min(1, abs(a - 1));
  float z = 1 - min(1, abs(a - 2));

  float r = dot(vec4(x,y,z,0), color);
  float g = dot(vec4(y,z,x,0), color);
  float b = dot(vec4(z,x,y,0), color);

  return vec4(r,g,b, color.a);
}
"""

var projection_mat : Mat4x4[float]

proc reshape(newWidth: cint, newHeight: cint) =
  glViewport(0, 0, newWidth, newHeight)   # Set the viewport to cover the new window
  projection_mat = perspective(45.0, newWidth / newHeight, 0.1, 100.0)

var
  mouseX, mouseY: int32
  simulationTime = 0.0
  frameCounter = 0

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  let mouseX_Norm = (mouseX.float32 / screenWidth.float32)
  let mouseY_Norm = (mouseY.float32 / screenHeight.float32)
  let mousePosNorm = vec2(mouseX_Norm, mouseY_Norm)

  #for i in 0..<7:
  block:
    #let newTime = time * (1.0 + i.float64 / 5.0))
    let time = simulationTime

    var modelview_mat = I4()
    modelview_mat = modelview_mat.translate( vec3[float](sin(time)*2, cos(time)*2, -7) )
    modelview_mat = modelview_mat.rotate( vec3[float](0,0,1), time )
    modelview_mat = modelview_mat.rotate( vec3[float](0,1,0), time )
    modelview_mat = modelview_mat.rotate( vec3[float](1,0,0), time )
    #modelview_mat = modelview_mat.scale( vec3[float](50,50,50) )

    #let mvp : Mat4x4[float32] =  modelview_mat * projection_mat;

    shadingDsl(GL_TRIANGLES, vertex.len.GLsizei):
      uniforms:
        modelview = modelview_mat
        projection = projection_mat
        time
        mousePosNorm
        crateTexture
      attributes:
        pos = vertex
        col = color
        texcoord
        normal


      includes:
        glslCode

      vertexMain:
        """
        gl_Position = projection * modelview * vec4(pos, 1);
        v_eyepos = modelview * vec4(pos,1);
        v_eyenormal = modelview * vec4(normal, 0);
        v_col = vec4(col,1);
        v_texcoord = texcoord;
        """

      vertexOut:
        "out vec4 v_col"
        "out vec2 v_texcoord"
        "out vec4 v_eyepos"
        "out vec4 v_eyenormal"

      fragmentMain:
        """
        vec4 t_col = texture(crateTexture, v_texcoord);
        vec2 offset = gl_FragCoord.xy / 32 + mousePosNorm * 10;
        vec4 mix_col = mymix(t_col, time + dot( vec2(cos(time),sin(time)), offset ));
        color = v_col * mix_col;
        //color = (v_eyenormal + vec4(1)) * 0.5;
        """

      fragmentOut:
        "out vec4 color"


    # render face normals using the geometry shader
    shadingDsl(GL_TRIANGLES, vertex.len.GLsizei):
      uniforms:
        modelview = modelview_mat
        projection = projection_mat
      attributes:
        pos = vertex
        normal

      vertexMain:
        """
        gl_Position = modelview * vec4(pos, 1);
        v_eyepos = modelview * vec4(pos,1);
        """

      vertexOut:
        "out vec4 v_eyepos"

      geometryMain:
        "layout(line_strip, max_vertices=2) out"
        """
        vec4 center = v_eyepos[0] + v_eyepos[1] + v_eyepos[2];

        vec3 v1 = (v_eyepos[1] - v_eyepos[0]).xyz;
        vec3 v2 = (v_eyepos[2] - v_eyepos[0]).xyz;
        vec4 normal = vec4(cross(v1,v2),0);

        gl_Position = projection * center;
        EmitVertex();
        gl_Position = projection * (center + normal);
        EmitVertex();
        """
      fragmentMain:
        """
        color = vec4(1);
        """

      fragmentOut:
        "out vec4 color"

  frameCounter += 1
  glSwapWindow(window) # Swap the front and back frame buffers (double buffering)

# Main loop

var
  evt = sdl2.defaultEvent
  runGame = true
  gamePaused = false
  simulationTimeOffset = 0.0
  fpsFrameCounter = 0
  fpsFrameCounterStartTime = 0.0

reshape(screenWidth, screenHeight) # Set up initial viewport and projection

while runGame:
  let time = float64( getTicks() ) / 1000.0

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == WindowEvent:
      let windowEvent = cast[WindowEventPtr](addr(evt))
      if windowEvent.event == WindowEvent_Resized:
        let newWidth = windowEvent.data1
        let newHeight = windowEvent.data2
        reshape(newWidth, newHeight)
    if evt.kind == KeyDown:
      let keyboardEvent = cast[KeyboardEventPtr](addr(evt))
      if keyboardEvent.keysym.scancode == SDL_SCANCODE_ESCAPE:
        runGame = false
        break
      if keyboardEvent.keysym.scancode == SDL_SCANCODE_PAUSE:
        if gamePaused:
          gamePaused = false
          simulationTimeOffset = time - simulationTime
        else:
          gamePaused = true

    if evt.kind == MouseMotion:
      let mouseEvent = cast[MouseMotionEventPtr](addr(evt))
      mouseX = mouseEvent.x
      mouseY = mouseEvent.y


  if not gamePaused:
    simulationTime = time - simulationTimeOffset

  if time - fpsFrameCounterStartTime >= 1:
    echo "FPS: ", fpsFrameCounter
    fpsFrameCounter = 0
    fpsFrameCounterStartTime = time

  render()
  fpsframeCounter += 1



  #limitFrameRate()

