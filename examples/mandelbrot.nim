# Based on a renderman shader by Michael Rivero

import ../fancygl

let (window, context) = defaultSetup()

glDisable(GL_DEPTH_TEST)
let aspectRatio = float32(window.size.x / window.size.y)

var evt: Event
var runGame: bool = true

let timer = newStopWatch(true)

let vertices = arrayBuffer([vec4f(-1,-1,0,1), vec4f(1,-1,0,1), vec4f(0,1,0,1)])
let colors   = arrayBuffer([vec4f( 1, 0,0,1), vec4f(0, 1,0,1), vec4f(0,0,1,1)])

var centerScale = vec3f(0,0,1)

while runGame:
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown and evt.key.keysym.scancode == SDL_SCANCODE_ESCAPE:
      runGame = false
  # TODO maybe a mouse interface?

  var state = getKeyboardState()
  if state[SDL_SCANCODE_UP.int] != 0:
    centerScale.y += centerScale.z * 0.005
  if state[SDL_SCANCODE_DOWN.int] != 0:
    centerScale.y -= centerScale.z * 0.005
  if state[SDL_SCANCODE_RIGHT.int] != 0:
    centerScale.x += centerScale.z * 0.005
  if state[SDL_SCANCODE_LEFT.int] != 0:
    centerScale.x -= centerScale.z * 0.005
  if state[SDL_SCANCODE_KP_MINUS.int] != 0:
    centerScale.z *= 1.02
  if state[SDL_SCANCODE_KP_PLUS.int] != 0:
    centerScale.z *= (1 / 1.02)


  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  shadingDsl:
    debugResult
    uniforms:
      time = timer.time.float32
      maxIterations = 200
      aspectRatio
      centerScale
    fragmentMain:
      """
  float tempreal, tempimag, Creal, Cimag;
  float r2;
  vec2 pos = centerScale.xy + (texCoord - 0.5) * vec2(aspectRatio, 1) * centerScale.z;
  //pos.y /= aspectRatio;
  float real = (pos.s * 3.0 );
  float imag = (pos.t * 3.0 );
  Creal = real;
  Cimag = imag;
  int iter = 0;
  for (; iter < maxIterations; iter++) {
    // z = z^2 + c
    tempreal = real;
    tempimag = imag;
    real = (tempreal * tempreal) - (tempimag * tempimag);
    imag = 2 * tempreal * tempimag;
    real += Creal;
    imag += Cimag;
    r2 = (real * real) + (imag * imag);
    if (r2 >= 4)
      break;
  }
  // Base the color on the number of iterations
  if (r2 >= 4) {
    color = vec4(fract(r2 * 10), fract(r2), fract(r2 * 0.1), 1.0); // black
  } else {
    float tmpval = iter / float(maxIterations);
    color = vec4(tmpval, tmpval, tmpval, 1.0);
  }
      """

  glSwapWindow(window)
