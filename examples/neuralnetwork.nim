# OpenGL example using SDL2

import sdl2, opengl, math, random, glm, sequtils, ../fancygl, fenv, sdl2/audio, fftw3, math

# randomize()

discard sdl2.init(INIT_EVERYTHING)

var windowsize = vec2f(320,240)
var viewport = vec4f(0,0,windowsize)
let renderTargetSize = vec2f(320,240)

const
  layerSize = 32
  numHiddenLayers = 4

var wav_spec: AudioSpec
var wav_length: uint32
var wav_buffer: ptr uint8
if loadWAV("song2.wav", wav_spec.addr, wav_buffer.addr, wav_length.addr) == nil:
  stderr.write "Could not open song.wav: ", sdl2.getError(), "\n"
var audioBuffer = dataView[int16](wav_buffer.pointer, int(wav_length div 2))

# split stream to left / right channel
var
  streamA = newSeq[float64](audioBuffer.len)
  streamB = newSeq[float64](audioBuffer.len)

for i in 0 ..< (audioBuffer.len div 2):
  let
    x = audioBuffer[i*2].float64
    y = audioBuffer[i*2+1].float64
    z = float64(high(int16))

  streamA[i] = x / z
  streamB[i] = y / z


const N = 4 * (4 * layerSize + layerSize * layerSize * (numHiddenLayers-1) + 3 * layerSize)
var input : array[N, float64]
var output : array[2*N, float64]

let plan = fftw_plan_dft_r2c_1d(N, input[0].addr,
  cast[ptr fftw_complex](output[0].addr), FFTW_ESTIMATE)






doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3)
doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3)
doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_FLAGS        , SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG or SDL_GL_CONTEXT_DEBUG_FLAG)
doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_PROFILE_MASK , SDL_GL_CONTEXT_PROFILE_CORE)

let window = createWindow("neural network graphics", 100, 100, windowsize.x.cint, windowsize.y.cint, SDL_WINDOW_OPENGL)
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

declareFramebuffer(RenderTarget):
  depth  = createEmptyDepthTexture2D(renderTargetSize)
  color  = createEmptyTexture2D(renderTargetSize, GL_RGBA8)

let frambuffer0 = createRenderTarget()

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


glClearColor(0.0, 0.0, 0.0, 1.0)                  # Set background color to black and opaque
glClearDepth(1.0)                                 # Set background depth to farthest
glEnable(GL_DEPTH_TEST)                           # Enable depth testing for z-culling
glDepthFunc(GL_LEQUAL)                            # Set the type of depth-test
#glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST) # Nice perspective corrections

glEnable(GL_CULL_FACE)
glCullFace(GL_BACK)

var
  mouseX, mouseY: int32
  simulationTime = 0.0
  frameCounter = 0

var firstWeights_d0 = newSeq[float32](4 * layerSize)
var firstWeights_d1 = newSeq[float32](4 * layerSize)
var firstWeights_d2 = newSeq[float32](4 * layerSize)
var firstWeights_d3 = newSeq[float32](4 * layerSize)
var weights_d0      = newSeq[float32](layerSize * layerSize * (numHiddenLayers-1))
var weights_d1      = newSeq[float32](layerSize * layerSize * (numHiddenLayers-1))
var weights_d2      = newSeq[float32](layerSize * layerSize * (numHiddenLayers-1))
var weights_d3      = newSeq[float32](layerSize * layerSize * (numHiddenLayers-1))
var lastWeights_d0  = newSeq[float32](3 * layerSize)
var lastWeights_d1  = newSeq[float32](3 * layerSize)
var lastWeights_d2  = newSeq[float32](3 * layerSize)
var lastWeights_d3  = newSeq[float32](3 * layerSize)

for i in 0 .. high(weights_d0):
  weights_d1[i] = generateGaussianNoise(0, 1.0)
for i in 0 .. high(firstWeights_d0):
  firstWeights_d1[i] = generateGaussianNoise(0, 1.0)
for i in 0 .. high(lastWeights_d0):
  lastWeights_d1[i] = generateGaussianNoise(0, 1.0)

const glslCode = """
float sig(float x) {
  //return x / (1.0 + abs(x));
  //return 1.0/(1.0+exp(-x));
  return tanh(x);
}
vec4 sig(vec4 x) {
  //return x / (1.0 + abs(x));
  //return 1.0/(1.0+exp(-x));
  return tanh(x);
}
"""

let weightsTexture      = texture1D(weights_d0.len div 4, GL_RGBA32F)
let firstWeightsTexture = texture1D(firstWeights_d0.len div 4, GL_RGBA32F)
let lastWeightsTexture  = texture1D(lastWeights_d0.len div 4, GL_RGBA32F)

var pos = 0
var offset = 0

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  let mouse = vec2f(mouseX.float32, windowsize.y - mouseY.float32)
  #let mouseX_Norm = (mouseX.float32 / screenWidth.float32)
  #let mouseY_Norm = (mouseY.float32 / screenHeight.float32)
  let mousePosNorm = (mouse - viewport.xy) / viewport.zw

  let time = simulationTime

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  proc linClamp(x, max = 1.0, moritz:float32 = 1.0):float32 =
    return max * tanh(x/moritz)

  # for _ in 0 ..< 10:
  #   if pos < (len(weights_d0) + len(firstWeights_d0) + len(lastWeights_d0)):
  #     if pos < firstWeights_d0.len:
  #       firstWeights_d0[pos] = generateGaussianNoise(0, 1.0)
  #     elif pos < firstWeights_d0.len + weights_d0.len:
  #       weights_d0[pos - firstWeights_d0.len] = generateGaussianNoise(0, 1.0)
  #     else:
  #       lastWeights_d0[pos - firstWeights_d0.len - weights_d0.len] = generateGaussianNoise(0, 0.1)

  #   pos = pos + 1

  # let stdDev:float32 = 0.0001
  # for i in 0 .. high(firstWeights_d0):
  #   firstWeights_d3[i] = generateGaussianNoise(0, stdDev)
  #   firstWeights_d2[i] = linClamp(firstWeights_d2[i] + firstWeights_d3[i], 0.01, 0.01) # acceleration
  #   firstWeights_d1[i] = linClamp(firstWeights_d1[i] + firstWeights_d2[i], 0.1, 0.1) # velocity
  #   firstWeights_d0[i] = linClamp(firstWeights_d0[i] + firstWeights_d1[i], 2, 2) # position
  # for i in 0 .. high(weights_d0):
  #   weights_d3[i] = generateGaussianNoise(0, stdDev)
  #   weights_d2[i] = linClamp(weights_d2[i] + weights_d3[i], 0.01, 0.01) # acceleration
  #   weights_d1[i] = linClamp(weights_d1[i] + weights_d2[i], 0.1, 0.1) # velocity
  #   weights_d0[i] = linClamp(weights_d0[i] + weights_d1[i], 3, 3) # position
  # for i in 0 .. high(lastWeights_d0):
  #   lastWeights_d3[i] = generateGaussianNoise(0, stdDev)
  #   lastWeights_d2[i] = linClamp(lastWeights_d2[i] + lastWeights_d3[i], 0.01, 0.01) # acceleration
  #   lastWeights_d1[i] = linClamp(lastWeights_d1[i] + lastWeights_d2[i], 0.1, 0.1) # velocity
  #   lastWeights_d0[i] = linClamp(lastWeights_d0[i] + lastWeights_d1[i], 0.5, 0.5) # position


  if (offset+N) <= wav_length.int:
    for i in 0 ..< N:
      input[i] = streamA[int(offset) + i]
    fftw_execute(plan)

    for i in 0..< N div 4:
      let im = (output[i] + output[i + 2] + output[i + 4] + output[i + 6]) / 4.0
      let re = (output[i+1] + output[i + 3] + output[i + 5] + output[i + 7]) / 4.0
      let mag = sqrt(im*im + re*re)
      let w = ln(1.5+mag)*0.5
      # echo w
      if i < firstWeights_d0.len:
        firstWeights_d0[i] = w * firstWeights_d1[i]
        discard
      elif i < firstWeights_d0.len + weights_d0.len:
        weights_d0[i - firstWeights_d0.len] = w * weights_d1[i - firstWeights_d0.len]
      else:
        lastWeights_d0[i - firstWeights_d0.len - weights_d0.len] = w * lastWeights_d1[i - firstWeights_d0.len - weights_d0.len]
        discard

    offset += N div 4;

  # echo "--"
  # echo weights_d3[0];
  # echo weights_d2[0];
  # echo weights_d1[0];
  # echo ""
  # echo weights_d0[0];
  # echo weights_d0[weights_d0.len div 2];
  # echo weights_d0[weights_d0.high];

  weightsTexture.setDataRGBA(weights_d0)
  firstWeightsTexture.setDataRGBA(firstWeights_d0)
  lastWeightsTexture.setDataRGBA(lastWeights_d0)

  # frambuffer0.bindFramebuffer:

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
      numHiddenLayers
      layerSize

    fragmentMain:
      """
      const int layerVec4Count = layerSize/4;

      vec4 inArray[layerVec4Count];
      vec4 outArray[layerVec4Count];

      inArray[0] = vec4(
        (texCoord.x - 0.5)*4,
        (texCoord.y - 0.5)*4,
        1.0,
        1.0);
        //(mouse.x / viewport.z) * -2.0 + 1.0,
        //(mouse.y / viewport.w) * -2.0 + 1.0);

      for(int outIdx = 0; outIdx < layerVec4Count; ++outIdx) {
        outArray[outIdx] = sig(vec4(
          dot(texelFetch(firstWeights, (outIdx*layerVec4Count) + 0,0), inArray[0]),
          dot(texelFetch(firstWeights, (outIdx*layerVec4Count) + 1,0), inArray[0]),
          dot(texelFetch(firstWeights, (outIdx*layerVec4Count) + 2,0), inArray[0]),
          dot(texelFetch(firstWeights, (outIdx*layerVec4Count) + 3,0), inArray[0])
        ));
      }


      for(int layer = 0; layer < numHiddenLayers - 1; layer++) {
        for(int i = 0; i < layerVec4Count; i++) {
          inArray[i] = outArray[i];
        }

        for(int outIdx = 0; outIdx < layerVec4Count; ++outIdx) {
          vec4 sum = vec4(0);
          for(int inIdx = 0; inIdx < layerVec4Count; ++inIdx) {
            vec4 w0 = texelFetch(weights, layer*layerSize*layerVec4Count + (outIdx*4*layerVec4Count) + inIdx*4+0, 0);
            vec4 w1 = texelFetch(weights, layer*layerSize*layerVec4Count + (outIdx*4*layerVec4Count) + inIdx*4+1, 0);
            vec4 w2 = texelFetch(weights, layer*layerSize*layerVec4Count + (outIdx*4*layerVec4Count) + inIdx*4+2, 0);
            vec4 w3 = texelFetch(weights, layer*layerSize*layerVec4Count + (outIdx*4*layerVec4Count) + inIdx*4+3, 0);

            sum += vec4(
              dot(inArray[inIdx], w0),
              dot(inArray[inIdx], w1),
              dot(inArray[inIdx], w2),
              dot(inArray[inIdx], w3)
            );
          }
          outArray[outIdx] = sig(sum);
        }
      }

        vec4 sum = vec4(0);
        for(int inIdx = 0; inIdx < layerVec4Count; ++inIdx) {
          sum += vec4(
            dot(outArray[inIdx], texelFetch(lastWeights, (0*layerVec4Count)+inIdx,0)),
            dot(outArray[inIdx], texelFetch(lastWeights, (1*layerVec4Count)+inIdx,0)),
            dot(outArray[inIdx], texelFetch(lastWeights, (2*layerVec4Count)+inIdx,0)),
            0.0
          );
        }
        color = sig(sum) * vec4(0.5) + vec4(0.5);

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
