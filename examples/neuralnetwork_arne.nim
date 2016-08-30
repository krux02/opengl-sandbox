# OpenGL example using SDL2

import sdl2, opengl, math, random, glm, sequtils, ../fancygl, fenv

randomize()

discard sdl2.init(INIT_EVERYTHING)

var windowsize = vec2f(960,540)
var viewport   = vec4f(vec2f(0), windowsize)

let renderTargetSize = vec2f(480,270)

doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3)
doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3)
doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_FLAGS        , SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG or SDL_GL_CONTEXT_DEBUG_FLAG)
doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_PROFILE_MASK , SDL_GL_CONTEXT_PROFILE_CORE)

let window = createWindow("neural network", 100, 100, windowsize.x.cint, windowsize.y.cint, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE)

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

declareFramebuffer(RenderTarget):
  depth  = createEmptyDepthTexture2D(renderTargetSize)
  color  = createEmptyTexture2D(renderTargetSize, GL_RGBA8)

let renderTarget = createRenderTarget()

proc generateGaussianNoise(mu, sigma: float64): float64 =
  let epsilon = fenv.epsilon(float64)
  let two_pi = 2.0 * PI

  var
    z0, z1 {. global .}: float64
    generate {. global .}: bool

  generate = not generate;
  if not generate:
    return z1 * sigma + mu;

  var
    u1 = random(1.0)
    u2 = random(1.0)

  while  u1 <= epsilon:
    u1 = random(1.0)
    u2 = random(1.0)

  z0 = sqrt(-2.0 * ln(u1)) * cos(two_pi * u2)
  z1 = sqrt(-2.0 * ln(u1)) * sin(two_pi * u2)

  return z0 * sigma + mu

if 0 != glSetSwapInterval(-1):
  stdout.write "glSetSwapInterval -1 (late swap tearing) not supported: "
  echo sdl2.getError()
  if 0 != glSetSwapInterval(1):
    echo "setting glSetSwapInterval 1 (synchronized)"
  else:
    stdout.write "even 1 (synchronized) is not supported: "
    echo sdl2.getError()


glClearColor(0.0, 0.0, 0.0, 1.0)                   # Set background color to black and opaque
glClearDepth(1.0)                                  # Set background depth to farthest
glEnable(GL_DEPTH_TEST)                            # Enable depth testing for z-culling
glDepthFunc(GL_LEQUAL)                             # Set the type of depth-test

glEnable(GL_CULL_FACE)
glCullFace(GL_BACK)



var projection_mat : Mat4x4[float]

var
  mouseX, mouseY: int32
  simulationTime = 0.0
  frameCounter = 0

const
  layerSize = int32(16)
  numHiddenLayers = int32(8)

var weights = newSeq[float32](layerSize * layerSize * numHiddenLayers)
for i in 0 .. high(weights):
  weights[i] = generateGaussianNoise(0,1.0) #random(2.0).float32 - 1

var firstWeights = newSeq[float32](4 * layerSize)
for i in 0 .. high(firstWeights):
  firstWeights[i] = generateGaussianNoise(0,1.0)

var lastWeights = newSeq[float32](3 * layerSize)
for i in 0 .. high(lastWeights):
  lastWeights[i] = generateGaussianNoise(0,1.0)

let weightsTexture = weights.texture1D
let firstWeightsTexture = firstWeights.texture1D
let lastWeightsTexture = lastWeights.texture1D

const glslCode = """
float sig(float x) {
  //return x / (1.0 + abs(x));
  return 1.0/(1.0+exp(-x));
}
"""

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  let mouse = vec2f(mouseX.float32, windowsize.y - mouseY.float32)
  #let mouseX_Norm = (mouseX.float32 / screenWidth.float32)
  #let mouseY_Norm = (mouseY.float32 / screenHeight.float32)
  let mousePosNorm = (mouse - viewport.xy) / viewport.zw

  let time = simulationTime

  var evalNetwork {. global .} = true

  if evalNetwork:

    bindFramebuffer(renderTarget):
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
          layerSize
          numHiddenLayers

        fragmentMain:
          """

          float inArray[layerSize];
          float outArray[layerSize];

          inArray[0] = texCoord.x - 0.5;
          inArray[1] = texCoord.y - 0.5;
          inArray[2] = 1.0;
          //inArray[2] = (mouse.x / viewport.z) * 2.0 - 1.0;
          //inArray[3] = (mouse.y / viewport.w) * 2.0 - 1.0;

          for(int outIdx = 0; outIdx < layerSize; ++outIdx) {
            float sum = 0;
            for(int inIdx = 0; inIdx < 3; ++inIdx) {
              sum += texelFetch(firstWeights, (outIdx*layerSize)+inIdx,0).r * inArray[inIdx];
            }
            outArray[outIdx] = tanh(sum);
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
              outArray[outIdx] = tanh(sum);
            }
          }

          for(int outIdx = 0; outIdx < 3; ++outIdx) {
            float sum = 0;
            for(int inIdx = 0; inIdx < layerSize; ++inIdx) {
              sum += texelFetch(lastWeights, (outIdx*layerSize)+inIdx,0).r * outArray[inIdx];
            }
            color[outIdx] = sig(sum);// * 0.5 + 0.5;
          }

          //color.r = outArray[0] * 0.5 + 0.5;
          //color.g = outArray[1] * 0.5 + 0.5;
          //color.b = outArray[2] * 0.5 + 0.5;
          """

    renderTarget.color.generateMipmap

  evalNetwork = false

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
  shadingDsl(GL_TRIANGLES):
    numVertices = 3

    uniforms:
      time
      colorTex = renderTarget.color

    fragmentMain:
      """
      float t0 = sin(time + 0.0 * (2 * M_PI / 3.0));
      float t1 = sin(time + 1.0 * (2 * M_PI / 3.0));
      float t2 = sin(time + 2.0 * (2 * M_PI / 3.0));

      vec3 w0 = vec3(t0,t1,t2);
      vec3 w1 = vec3(t1,t2,t0);
      vec3 w2 = vec3(t2,t0,t1);

      vec3 texColor = texture(colorTex, texCoord).rgb;
      color.r = dot(texColor, w0);
      color.g = dot(texColor, w1);
      color.b = dot(texColor, w2);
      """

  frameCounter += 1
  glSwapWindow(window) # Swap the front and back frame buffers (double buffering)

# Main loop

var
  evt: sdl2.Event
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
        viewport.z = windowsize.x
        viewport.w = windowsize.y
        glViewport(viewport.x.int32, viewport.y.int32, viewport.z.int32, viewport.w.int32)
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

  #limitFrameRate()

echo "exit game"
