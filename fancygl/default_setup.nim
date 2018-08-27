# included from fancygl.nim


#proc write*(f: File, c: cstring)

proc debugCallbackPrintMessage(
    source: GLenum,
    `type`: GLenum,
    id: GLuint,
    severity: GLenum,
    length: GLsizei,
    message: cstring,
    userParam: pointer): void {. cdecl .} =
  stdout.styledWriteLine(fgYellow, $message)

proc debugCallback(
    source: GLenum,
    `type`: GLenum,
    id: GLuint,
    severity: GLenum,
    length: GLsizei,
    message: cstring,
    userParam: pointer): void {. cdecl .} =

  echo "gl-debug-callback:"
  echo "  message: ", message

  stdout.write "  source: "
  case source
  of GL_DEBUG_SOURCE_API_ARB:
    echo "api"
  of GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB:
    echo "window system"
  of GL_DEBUG_SOURCE_SHADER_COMPILER_ARB:
    echo "shader compiler"
  of GL_DEBUG_SOURCE_THIRD_PARTY_ARB:
    echo "third party"
  of GL_DEBUG_SOURCE_APPLICATION_ARB:
    echo "application"
  of GL_DEBUG_SOURCE_OTHER_ARB:
    echo "other"
  else:
    echo "¿", int(source), "?"

  stdout.write "  type: "
  case `type`
  of GL_DEBUG_TYPE_ERROR_ARB:
    echo "error"
  of GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB:
    echo "deprecated behavior"
  of GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB:
    echo "undefined behavior"
  of GL_DEBUG_TYPE_PORTABILITY_ARB:
    echo "portability"
  of GL_DEBUG_TYPE_PERFORMANCE_ARB:
    echo "performance"
  of GL_DEBUG_TYPE_MARKER:
    echo "marker"
  of GL_DEBUG_TYPE_PUSH_GROUP:
    echo "push group"
  of GL_DEBUG_TYPE_POP_GROUP:
    echo "pop group"
  of GL_DEBUG_TYPE_OTHER_ARB:
    echo "other"
  else:
    echo "¿ ", `type`.int, " ?"

  echo "  id: ", id
  stdout.write "  severity: "
  case severity
  of GL_DEBUG_SEVERITY_LOW_ARB:
    echo "low"
  of GL_DEBUG_SEVERITY_MEDIUM_ARB:
    echo "medium"
  of GL_DEBUG_SEVERITY_HIGH_ARB:
    echo "high"
  of GL_DEBUG_SEVERITY_NOTIFICATION:
    echo "notification"
  else:
    echo "¿ ", severity.int, " ?"

proc enableDefaultDebugCallback*() =
  glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB)
  #glDebugMessageCallbackARB(cast[GLdebugProc](debugCallback), nil)
  glDebugMessageCallbackARB(cast[GLdebugProc](debugCallbackPrintMessage), nil)

proc defaultSetupInternal(windowsize: Vec2i; windowTitle: string): tuple[window: Window, context: GlContext] =
  discard sdl.init(INIT_EVERYTHING)
  if ttf.init() != 0:
    write stderr, ttf.getError()

  doAssert 0 == glSetAttribute(GL_CONTEXT_MAJOR_VERSION, 4)
  doAssert 0 == glSetAttribute(GL_CONTEXT_MINOR_VERSION, 5)
  doAssert 0 == glSetAttribute(GLattr.GL_CONTEXT_FLAGS , GL_CONTEXT_FORWARD_COMPATIBLE_FLAG or GL_CONTEXT_DEBUG_FLAG)
  doAssert 0 == glSetAttribute(GLattr.GL_CONTEXT_PROFILE_MASK , GL_CONTEXT_PROFILE_CORE)
  doAssert 0 == glSetAttribute(GL_STENCIL_SIZE         , 8)

  if getNumVideoDisplays() < 1:
    panic "no monitor detected, need at least one, but got: ", getNumVideoDisplays()

  let flags =
    if windowsize.x < 0:
      WINDOW_OPENGL or WINDOW_FULLSCREEN_DESKTOP
    else:
      WINDOW_OPENGL

  let posx = WINDOWPOS_UNDEFINED.cint
  let posy = WINDOWPOS_UNDEFINED.cint

  result.window = createWindow(windowTitle, posx, posy, windowsize.x.cint, windowsize.y.cint, flags.uint32)

  if result.window.isNil:
    panic sdl.getError()

  result.context = result.window.glCreateContext()
  if result.context.isNil:
    panic sdl.getError()

  #Initialize OpenGL
  doAssert gladLoadGL(glGetProcAddress)
  echo glGetString(GL_VERSION)

  glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DEBUG_SEVERITY_NOTIFICATION, 0, nil, false)

  if glPushDebugGroup != nil:
    glPushDebugGroup(GL_DEBUG_SOURCE_THIRD_PARTY, 1234, -1, "default setup")

  echo "extensions loaded"
  enableDefaultDebugCallback()

  doAssert 0 == glMakeCurrent(result.window, result.context)

  if 0 != glSetSwapInterval(-1):
    stdout.write "late swap tearing not supported: "
    echo sdl.getError()
    if 0 != glSetSwapInterval(1):
      echo "setting swap interval synchronized"
    else:
      stdout.write "even 1 (synchronized) is not supported: "
      echo sdl.getError()

  glEnable(GL_DEPTH_TEST)
  if glPushDebugGroup != nil:
    glPopDebugGroup()

template defaultSetup*(windowsize: Vec2i = vec2i(-1, -1), windowTitle: string = ""): tuple[window: Window, context: GlContext] =
  var name = if windowTitle.len == 0: instantiationInfo().filename else: windowTitle
  name.removeSuffix(".nim")
  defaultSetupInternal(windowsize, name)
