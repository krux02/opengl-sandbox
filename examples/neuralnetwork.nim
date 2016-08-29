# OpenGL example using SDL2

import sdl2, opengl, math, random, glm, sequtils, ../fancygl

randomize()

discard sdl2.init(INIT_EVERYTHING)

var windowsize = vec2f(320,240)
var viewport = vec4f(0,0,320,240)

doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3)
doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3)
doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_FLAGS        , SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG or SDL_GL_CONTEXT_DEBUG_FLAG)
doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_PROFILE_MASK , SDL_GL_CONTEXT_PROFILE_CORE)

let window = createWindow("SDL/OpenGL Skeleton", 100, 100, windowsize.x.cint, windowsize.y.cint, SDL_WINDOW_OPENGL)
# or SDL_WINDOW_RESIZABL)
if window.isNil:
  echo sdl2.getError()
  system.quit(1)

let context = window.glCreateContext()
if context.isNil:
  echo sdl2.getError()
  system.quit(1)

#Initialize OpenGL
loadExtensions()
enableDefaultDebugCallback()

doAssert 0 == glMakeCurrent(window, context)

let
  vertex = boxVertices.arrayBuffer
  normal = boxNormals.arrayBuffer
  color = boxColors.arrayBuffer
  texcoord = boxTexCoords.arrayBuffer

let indices = toSeq( countup[int8,int8](0, int8(vertex.len-1)) ).elementArrayBuffer

if 0 != glSetSwapInterval(-1):
  stdout.write "glSetSwapInterval -1 (late swap tearing) not supported: "
  echo sdl2.getError()
  if 0 != glSetSwapInterval(1):
    echo "setting glSetSwapInterval 1 (synchronized)"
  else:
    stdout.write "even 1 (synchronized) is not supported: "
    echo sdl2.getError()


glClearColor(0.0, 0.0, 0.0, 1.0)                  # Set background color to black and opaque
glClearDepth(1.0)                                 # Set background depth to farthest
glEnable(GL_DEPTH_TEST)                           # Enable depth testing for z-culling
glDepthFunc(GL_LEQUAL)                            # Set the type of depth-test
#glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST) # Nice perspective corrections

glEnable(GL_CULL_FACE)
glCullFace(GL_BACK)

var projection_mat : Mat4x4[float]

var
  mouseX, mouseY: int32
  simulationTime = 0.0
  frameCounter = 0

const
  layerSize = 16
  numHiddenLayer = 8

var weights = newSeq[float32](layerSize * layerSize * numHiddenLayer)
for i in 0 .. high(weights):
  weights[i] = random(2.0).float32 - 1

var firstWeights = newSeq[float32](4 * layerSize)
for i in 0 .. high(firstWeights):
  firstWeights[i] = random(2.0).float32 - 1

var lastWeights = newSeq[float32](3 * layerSize)
for i in 0 .. high(lastWeights):
  lastWeights[i] = random(2.0).float32 - 1


let weightsTexture = weights.texture1D
let firstWeightsTexture = firstWeights.texture1D
let lastWeightsTexture = lastWeights.texture1D


const glslCode = """
float sig(float x) {
  return x / (1 + abs(x));
}

const int layerSize = 16;
const int numHiddenLayers = 8;
"""

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  let mouse = vec2f(mouseX.float32, windowsize.y - mouseY.float32)
  #let mouseX_Norm = (mouseX.float32 / screenWidth.float32)
  #let mouseY_Norm = (mouseY.float32 / screenHeight.float32)
  let mousePosNorm = (mouse - viewport.xy) / viewport.zw

  let time = simulationTime

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  shadingDsl(GL_TRIANGLES):
    numVertices = 3

    includes:
      glslCode
    uniforms:
      weights = weightsTexture
      firstWeights = firstWeightsTexture
      lastWeights = lastWeightsTexture
      mouse
      time
      viewport

    fragmentMain:
      """

      float inArray[layerSize];
      float outArray[layerSize];

      inArray[0] = texCoord.x * 2.0 - 1.0;
      inArray[1] = texCoord.y * 2.0 - 1.0;
      inArray[2] = (mouse.x / viewport.z) * 2.0 - 1.0;
      inArray[3] = (mouse.y / viewport.w) * 2.0 - 1.0;

      for(int outIdx = 0; outIdx < layerSize; ++outIdx) {
        outArray[outIdx] = 0;
        for(int inIdx = 0; inIdx < 4; ++inIdx) {
          outArray[outIdx] += texelFetch(firstWeights, (outIdx*layerSize)+inIdx,0).r * inArray[inIdx];
        }
        outArray[outIdx] = sig(outArray[outIdx]);
      }


      for(int layer = 0; layer < numHiddenLayers - 1; layer++) {
        for(int i = 0; i < layerSize; i++) {
          inArray[i] = outArray[i];
        }

        for(int outIdx = 0; outIdx < layerSize; ++outIdx) {
          float sum = 0;
          for(int inIdx = 0; inIdx < layerSize; ++inIdx) {
            sum += texelFetch(weights, (layer*layerSize*layerSize)+(outIdx*layerSize)+inIdx,0).r * inArray[inIdx];
          }
          outArray[outIdx] = sig(sum);
        }
      }

      for(int outIdx = 0; outIdx < 3; ++outIdx) {
        float sum = 0;
        for(int inIdx = 0; inIdx < layerSize; ++inIdx) {
          sum += texelFetch(lastWeights, (outIdx*layerSize)+inIdx,0).r * outArray[inIdx];
        }
        color[outIdx] = sig(sum) * 0.5 + 0.5;
      }

      //color.r = outArray[0] * 0.5 + 0.5;
      //color.g = outArray[1] * 0.5 + 0.5;
      //color.b = outArray[2] * 0.5 + 0.5;
      """


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

while runGame:
  let time = float64( getTicks() ) / 1000.0

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == WindowEvent:
      if evt.window.event == WindowEvent_Resized:
        windowsize.x = evt.window.data1.float32
        windowsize.y = evt.window.data2.float32
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SDL_SCANCODE_ESCAPE:
        runGame = false
      of SDL_SCANCODE_PAUSE:
        if gamePaused:
          gamePaused = false
          simulationTimeOffset = time - simulationTime
        else:
          gamePaused = true
      of SDL_SCANCODE_F10:
        window.screenshot("sandbox")
      else:
        discard


    if evt.kind == MouseMotion:
      mouseX = evt.motion.x
      mouseY = evt.motion.y

  if not gamePaused:
    simulationTime = time - simulationTimeOffset

  if time - fpsFrameCounterStartTime >= 1:
    echo "FPS: ", fpsFrameCounter
    fpsFrameCounter = 0
    fpsFrameCounterStartTime = time

  render()
  fpsframeCounter += 1

  #runGame = false

  #limitFrameRate()
