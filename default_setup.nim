
proc defaultSetup*(windowsize: Vec2f): tuple[window: WindowPtr, context: GlContextPtr] =
  discard sdl2.init(INIT_EVERYTHING)

  doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3)
  doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3)
  doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_FLAGS        , SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG or SDL_GL_CONTEXT_DEBUG_FLAG)
  doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_PROFILE_MASK , SDL_GL_CONTEXT_PROFILE_CORE)

  result.window = createWindow("SDL/OpenGL Skeleton", 100, 100, windowsize.x.cint, windowsize.y.cint, SDL_WINDOW_OPENGL)
# or SDL_WINDOW_RESIZABL)
  if result.window.isNil:
    echo sdl2.getError()
    system.quit(1)

  result.context = result.window.glCreateContext()
  if result.context.isNil:
    echo sdl2.getError()
    system.quit(1)

  #Initialize OpenGL
  loadExtensions()
  enableDefaultDebugCallback()

  doAssert 0 == glMakeCurrent(result.window, result.context)

  if 0 != glSetSwapInterval(-1):
    stdout.write "glSetSwapInterval -1 (late swap tearing) not supported: "
    echo sdl2.getError()
    if 0 != glSetSwapInterval(1):
      echo "setting glSetSwapInterval 1 (synchronized)"
    else:
      stdout.write "even 1 (synchronized) is not supported: "
      echo sdl2.getError()


  glEnable(GL_DEPTH_TEST)


