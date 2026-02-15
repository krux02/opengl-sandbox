import ../fancygl

proc main*(window: Window): void =
  glDisable(GL_DEPTH_TEST)
  let aspectRatio = window.aspectRatio.float32

  var runGame: bool = true

  let timer = newStopWatch(true)

  var centerScale = vec3f(0,0,1)
  var mousePos: Vec2i

  while runGame:
    var buttonStates:uint32 = getMouseState(mousePos.x.addr, mousePos.y.addr)

    var texCoord: Vec2f = mousePos.vec2f / window.size.vec2f
    texCoord.y = 1 - texCoord.y
    let pos: Vec2f = centerScale.xy + (texCoord - 0.5) * vec2(aspectRatio, 1) * centerScale.z;
    centerScale.xy = pos

    for evt in events():
      if evt.kind == QUIT:
        runGame = false
        break
      if evt.kind == KEY_DOWN:
        if evt.key.keysym.scancode == SCANCODE_ESCAPE:
          runGame = false
        if evt.key.keysym.scancode == SCANCODE_F10:
          window.screenshot

      if evt.kind == MOUSEWHEEL:
        centerScale.z *= pow(1.05f, -evt.wheel.y.float32)

    # TODO maybe a mouse interface?

    let buttonLeft:   bool = (buttonStates and BUTTON_LMASK) != 0
    let buttonMiddle: bool = (buttonStates and BUTTON_MMASK) != 0
    let buttonRight:  bool = (buttonStates and BUTTON_RMASK) != 0

    var state = getKeyboardState(nil)
    if state[SCANCODE_UP] != 0:
      centerScale.y += centerScale.z * 0.005
    if state[SCANCODE_DOWN] != 0:
      centerScale.y -= centerScale.z * 0.005
    if state[SCANCODE_RIGHT] != 0:
      centerScale.x += centerScale.z * 0.005
    if state[SCANCODE_LEFT] != 0:
      centerScale.x -= centerScale.z * 0.005
    if buttonRight or state[SCANCODE_KP_MINUS] != 0:
      centerScale.z *= 1.02
    if buttonLeft or state[SCANCODE_KP_PLUS] != 0:
      centerScale.z *= (1 / 1.02)

    centerScale.xy = pos - (texCoord - 0.5) * vec2(aspectRatio, 1) * centerScale.z;

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

    shadingDsl:
      uniforms:
        maxIterations = 200
        aspectRatio
        centerScale
      fragmentMain:
        """
    float tempreal, tempimag, Creal, Cimag;
    float r2;
    vec2 pos = centerScale.xy + (texCoord - 0.5) * vec2(aspectRatio, 1) * centerScale.z;
    float real = (pos.s * 3.0 );
    float imag = (pos.t * 3.0 );
    Creal = real;
    Cimag = imag;
    int iter = 0;
    for (; iter < maxIterations; iter++) {
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


when isMainModule:
  let (window, context) = defaultSetup()
  main(window)
    
# Local Variables:
# compile-command: "cd examples; nim c -r mandelbrot.nim"
# End:
