# OpenGL example using SDL2

import sdl2, opengl, math, glm, fancygl

var vertex: seq[Vec3[float]] = @[
  vec3(+1.0, +1.0, -1.0), vec3(-1.0, +1.0, -1.0), vec3(-1.0, +1.0, +1.0),
  vec3(+1.0, +1.0, +1.0), vec3(+1.0, +1.0, -1.0), vec3(-1.0, +1.0, +1.0),
  vec3(+1.0, -1.0, +1.0), vec3(-1.0, -1.0, +1.0), vec3(-1.0, -1.0, -1.0),
  vec3(+1.0, -1.0, -1.0), vec3(+1.0, -1.0, +1.0), vec3(-1.0, -1.0, -1.0),
  vec3(+1.0, +1.0, +1.0), vec3(-1.0, +1.0, +1.0), vec3(-1.0, -1.0, +1.0),
  vec3(+1.0, -1.0, +1.0), vec3(+1.0, +1.0, +1.0), vec3(-1.0, -1.0, +1.0),
  vec3(+1.0, -1.0, -1.0), vec3(-1.0, -1.0, -1.0), vec3(-1.0, +1.0, -1.0),
  vec3(+1.0, +1.0, -1.0), vec3(+1.0, -1.0, -1.0), vec3(-1.0, +1.0, -1.0),
  vec3(-1.0, +1.0, +1.0), vec3(-1.0, +1.0, -1.0), vec3(-1.0, -1.0, -1.0),
  vec3(-1.0, -1.0, +1.0), vec3(-1.0, +1.0, +1.0), vec3(-1.0, -1.0, -1.0),
  vec3(+1.0, +1.0, -1.0), vec3(+1.0, +1.0, +1.0), vec3(+1.0, -1.0, +1.0),
  vec3(+1.0, -1.0, -1.0), vec3(+1.0, +1.0, -1.0), vec3(+1.0, -1.0, +1.0)
]

var color: seq[Vec3[float]] = @[
  vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0),
  vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0),
  vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0),
  vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0),
  vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0),
  vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0),
  vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0),
  vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0),
  vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0),
  vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0),
  vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0),
  vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0)
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

let glslCode = """
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

var projection_mat : Mat4x4[float64]

proc reshape(newWidth: cint, newHeight: cint) =
  glViewport(0, 0, newWidth, newHeight)   # Set the viewport to cover the new window
  projection_mat = perspective(45.0, newWidth / newHeight, 0.1, 100.0)

var mouseX, mouseY: int32
var time = 0.0
var frameCounter = 0


proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  let mouseX_Norm = (mouseX.float32 / screenWidth.float32)
  let mouseY_Norm = (mouseY.float32 / screenHeight.float32)
  let mousePosNorm = vec2(mouseX_Norm, mouseY_Norm)

  for i in 0..<7:
    let newTime = time * (1.0 + i.float64 / 5.0)

    var modelview_mat: Mat4x4[float]  = I4()
    modelview_mat = modelview_mat.transform( vec3(2*sin(newTime), 2*cos(newTime), -7.0) )
    modelview_mat = modelview_mat.rotate( vec3[float](0,0,1), newTime )
    modelview_mat = modelview_mat.rotate( vec3[float](0,1,0), newTime )
    modelview_mat = modelview_mat.rotate( vec3[float](1,0,0), newTime )

    let mvp =  modelview_mat * projection_mat;

    shadingDsl:
      samplers:
        crateTexture
      uniforms:
        mvp
        time
        mousePosNorm
      attributes:
        pos = vertex
        col = color
      varyings:
        var v_col : vec4
      frag_out:
        var color : vec4
      includes:
        glslCode
      vertex_prg:
        """
        gl_Position = mvp * vec4(pos,1);
        v_col = vec4(col,1);
        """
      fragment_prg:
        """
        //vec2 offset = gl_FragCoord.xy / 32 + mousePosNorm * 10;
        //color = mymix(v_col, time + dot( vec2(cos(time),sin(time)), offset ));
        color = v_col;
        """

  frameCounter += 1
  glSwapWindow(window) # Swap the front and back frame buffers (double buffering)

# Main loop

var
  evt = sdl2.defaultEvent
  runGame = true
  fpsFrameCounter = 0
  fpsFrameCounterStartTime = 0.0

reshape(screenWidth, screenHeight) # Set up initial viewport and projection

while runGame:
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
    if evt.kind == MouseMotion:
      let mouseEvent = cast[MouseMotionEventPtr](addr(evt))
      mouseX = mouseEvent.x
      mouseY = mouseEvent.y

  time = float64( getTicks() ) / 1000.0
  if time - fpsFrameCounterStartTime >= 1:
    echo "FPS: ", frameCounter
    fpsFrameCounter = 0
    fpsFrameCounterStartTime = time

  render()
  fpsframeCounter += 1



  #limitFrameRate()

