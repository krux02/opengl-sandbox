#[
import sdl2, opengl, libarne
proc enableDefaultDebugCallback(): void
type Vec2i = object
  x,y: int32
proc vec2i(x,y: int32): Vec2i = Vec2i(x:x, y:y)
]#

proc defaultSetup*(windowsize: Vec2i = vec2i(-1,-1)): tuple[window: WindowPtr, context: GlContextPtr] =
  discard sdl2.init(INIT_EVERYTHING)

  doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3)
  doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3)
  doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_FLAGS        , SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG or SDL_GL_CONTEXT_DEBUG_FLAG)
  doAssert 0 == glSetAttribute(SDL_GL_CONTEXT_PROFILE_MASK , SDL_GL_CONTEXT_PROFILE_CORE)

  if getNumVideoDisplays() < 1:
    write stderr, "no monitor detected, need at least one, but got: ", getNumVideoDisplays()
    quit(QuitFailure)

  let flags =
    if windowsize.x < 0:
      SDL_WINDOW_OPENGL or SDL_WINDOW_FULLSCREEN_DESKTOP
    else:
      SDL_WINDOW_OPENGL
    
  let posx = SDL_WINDOWPOS_UNDEFINED.cint
  let posy = SDL_WINDOWPOS_UNDEFINED.cint

  result.window = createWindow("SDL/OpenGL Skeleton", posx, posy, windowsize.x, windowsize.y, flags)

  if result.window.isNil:
    echo sdl2.getError()
    system.quit(1)

  result.context = result.window.glCreateContext()
  if result.context.isNil:
    echo sdl2.getError()
    system.quit(1)

  #Initialize OpenGL
  loadExtensions()

  echo "extensions loaded"
  enableDefaultDebugCallback()

  doAssert 0 == glMakeCurrent(result.window, result.context)

  if 0 != glSetSwapInterval(-1):
    stdout.write "late swap tearing not supported: "
    echo sdl2.getError()
    if 0 != glSetSwapInterval(1):
      echo "setting swap interval synchronized"
    else:
      stdout.write "even 1 (synchronized) is not supported: "
      echo sdl2.getError()


  glEnable(GL_DEPTH_TEST)





