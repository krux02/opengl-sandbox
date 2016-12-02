# OpenGL example using SDL2

import ../fancygl, fenv, sequtils

# TODO use defaultSetup

var (window, context) = defaultSetup()
let windowSize = window.size
let renderTargetSize = windowSize div 8

glDisable(GL_DEPTH_TEST)

declareFramebuffer(RenderTarget):
  depth  = newDepthTexture2D(renderTargetSize)
  color  = newTexture2D(renderTargetSize, GL_RGBA8)

let framebuffer0 = newRenderTarget()
  
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
    u1 = rand_f64()
    u2 = rand_f64()

  while  u1 <= epsilon:
    u1 = rand_f64()
    u2 = rand_f64()

  z0 = sqrt(-2.0 * ln(u1)) * cos(two_pi * u2)
  z1 = sqrt(-2.0 * ln(u1)) * sin(two_pi * u2)

  return z0 * sigma + mu

glDisable(GL_DEPTH_TEST)

var
  mouseX, mouseY: int32
  gameTimer = newStopWatch(true)
  frameCounter = 0

const
  layerSize = 16
  numHiddenLayers = 8

var firstWeights_d0 = newSeq[float32](4 * layerSize)
var firstWeights_d1 = newSeq[float32](4 * layerSize)
var firstWeights_d2 = newSeq[float32](4 * layerSize)
var firstWeights_d3 = newSeq[float32](4 * layerSize)
var weights_d0      = newSeq[float32](layerSize * layerSize * numHiddenLayers)
var weights_d1      = newSeq[float32](layerSize * layerSize * numHiddenLayers)
var weights_d2      = newSeq[float32](layerSize * layerSize * numHiddenLayers)
var weights_d3      = newSeq[float32](layerSize * layerSize * numHiddenLayers)
var lastWeights_d0  = newSeq[float32](3 * layerSize)
var lastWeights_d1  = newSeq[float32](3 * layerSize)
var lastWeights_d2  = newSeq[float32](3 * layerSize)
var lastWeights_d3  = newSeq[float32](3 * layerSize)

for i in 0 .. high(weights_d0):
  weights_d0[i] = 0.0
for i in 0 .. high(firstWeights_d0):
  firstWeights_d0[i] = 0.0
for i in 0 .. high(lastWeights_d0):
  lastWeights_d0[i] = 0.0

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

let weightsTexture      = newTexture1D(weights_d0.len div 4, GL_RGBA32F)
let firstWeightsTexture = newTexture1D(firstWeights_d0.len div 4, GL_RGBA32F)
let lastWeightsTexture  = newTexture1D(lastWeights_d0.len div 4, GL_RGBA32F)

glDisable(GL_DEPTH_TEST)
    
proc render() =

  let mouse = vec2f(mouseX.float32, window.size.y.float32 - mouseY.float32)
  #let mouseX_Norm = (mouseX.float32 / screenWidth.float32)
  #let mouseY_Norm = (mouseY.float32 / screenHeight.float32)
  #let mousePosNorm = (mouse - viewport.xy) / viewport.zw

  let time = gameTimer.time.float32

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

  let stdDev:float32 = 0.0001
  for i in 0 .. high(firstWeights_d0):
    firstWeights_d3[i] = generateGaussianNoise(0, stdDev)
    firstWeights_d2[i] = linClamp(firstWeights_d2[i] + firstWeights_d3[i], 0.01, 0.01) # acceleration
    firstWeights_d1[i] = linClamp(firstWeights_d1[i] + firstWeights_d2[i], 0.1, 0.1) # velocity
    firstWeights_d0[i] = linClamp(firstWeights_d0[i] + firstWeights_d1[i], 2, 2) # position
  for i in 0 .. high(weights_d0):
    weights_d3[i] = generateGaussianNoise(0, stdDev)
    weights_d2[i] = linClamp(weights_d2[i] + weights_d3[i], 0.01, 0.01) # acceleration
    weights_d1[i] = linClamp(weights_d1[i] + weights_d2[i], 0.1, 0.1) # velocity
    weights_d0[i] = linClamp(weights_d0[i] + weights_d1[i], 3, 3) # position
  for i in 0 .. high(lastWeights_d0):
    lastWeights_d3[i] = generateGaussianNoise(0, stdDev)
    lastWeights_d2[i] = linClamp(lastWeights_d2[i] + lastWeights_d3[i], 0.01, 0.01) # acceleration
    lastWeights_d1[i] = linClamp(lastWeights_d1[i] + lastWeights_d2[i], 0.1, 0.1) # velocity
    lastWeights_d0[i] = linClamp(lastWeights_d0[i] + lastWeights_d1[i], 0.5, 0.5) # position

  # echo "--"
  # echo weights_d3[0];
  # echo weights_d2[0];
  # echo weights_d1[0];
  # echo weights_d0[0];

  weightsTexture.setDataRGBA(weights_d0)
  firstWeightsTexture.setDataRGBA(firstWeights_d0)
  lastWeightsTexture.setDataRGBA(lastWeights_d0)

  framebuffer0.bindFramebuffer:
    
    glViewport(0,0,renderTargetSize.x, renderTargetSize.y)

    shadingDsl:
      
      includes:
        glslCode
      uniforms:
        weights = weightsTexture
        firstWeights = firstWeightsTexture
        lastWeights = lastWeightsTexture
        time
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

  glViewport(0,0,windowSize.x, windowSize.y)

  shadingDsl:

    uniforms:
      
      tex = framebuffer0.color

    fragmentMain:
      """
      color = texture(tex, texCoord);
      """

  frameCounter += 1
  glSwapWindow(window) # Swap the front and back frame buffers (double buffering)

# Main loop

var
  evt = defaultEvent
  runGame = true
  fpsTimer = newStopWatch(true)
  fpsFrameCounter = 0

while runGame:
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SDL_SCANCODE_ESCAPE:
        runGame = false
      of SDL_SCANCODE_PAUSE:
        gameTimer.toggle
      of SDL_SCANCODE_F10:
        window.screenshot("sandbox")
      else:
        discard

    if evt.kind == MouseMotion:
      mouseX = evt.motion.x
      mouseY = evt.motion.y

  
  if fpsTimer.time >= 1:
    echo "FPS: ", fpsFrameCounter
    fpsTimer.reset
    fpsFrameCounter = 0

  render()
  fpsframeCounter += 1

  #runGame = false

  #limitFrameRate()
