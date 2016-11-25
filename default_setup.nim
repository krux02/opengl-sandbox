when isMainModule:
  import sdl2, opengl, glm

proc debugCallback(source: GLenum, `type`: GLenum, id: GLuint, severity: GLenum, length: GLsizei, message: cstring, userParam: pointer): void {. cdecl .} =
  if severity == GL_DEBUG_SEVERITY_NOTIFICATION:
    return

  echo "<gl-debug-callback>"
  echo "message: ", message
  stdout.write "type: "
  case `type`
  of GL_DEBUG_TYPE_ERROR:
    echo "ERROR"
  of GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR:
    echo "DEPRECATED_BEHAVIOR"
  of GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR:
    echo "UNDEFINED_BEHAVIOR"
  of GL_DEBUG_TYPE_PORTABILITY:
    echo "PORTABILITY"
  of GL_DEBUG_TYPE_PERFORMANCE:
    echo "PERFORMANCE"
  of GL_DEBUG_TYPE_MARKER:
    echo "MARKER"
  of GL_DEBUG_TYPE_PUSH_GROUP:
    echo "PUSH_GROUP"
  of GL_DEBUG_TYPE_POP_GROUP:
    echo "POP_GROUP"
  of GL_DEBUG_TYPE_OTHER:
    echo "OTHER"
  else:
    echo "¿ ", `type`.int, " ?"

  echo "id: ", id
  stdout.write "severity: "
  case severity
  of GL_DEBUG_SEVERITY_LOW:
    echo "LOW"
  of GL_DEBUG_SEVERITY_MEDIUM:
    echo "MEDIUM"
  of GL_DEBUG_SEVERITY_HIGH:
    echo "HIGH"
  of GL_DEBUG_SEVERITY_NOTIFICATION:
    echo "NOTIFICATION"
  else:
    echo "¿ ", severity.int, " ?"
  echo "<gl-debug-callback/>"

proc enableDefaultDebugCallback*() =
  glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB)
  glDebugMessageCallbackARB(cast[GLdebugProcArb](debugCallback), nil);


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





