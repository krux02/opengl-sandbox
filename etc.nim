macro debugResult(arg: typed) : stmt =
  for str in arg.repr.split("""\x0A"""):
    echo str
  arg

proc mkString*[T](v : T, prefix, sep, postfix : string) : string =
  result = prefix
  var first = true
  for x in v:
    if not first:
      result.add(sep)
    result.add($x)
    first = false

  result.add(postfix)

proc mkString*[T](v : T, sep : string = ", ") : string =
  mkString(v, "", sep, "")


proc debugCallback(source: GLenum, `type`: GLenum, id: GLuint, severity: GLenum, length: GLsizei, message: cstring, userParam: pointer): void {. cdecl .} =
  echo "---------------------opengl-callback-start------------"
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
  of GL_DEBUG_TYPE_OTHER:
    echo "OTHER"
  else:
    echo "???"

  echo "id: ", id
  stdout.write "severity: "
  case severity
  of GL_DEBUG_SEVERITY_LOW:
    echo "LOW"
  of GL_DEBUG_SEVERITY_MEDIUM:
    echo "MEDIUM"
  of GL_DEBUG_SEVERITY_HIGH:
    echo "HIGH"
  else:
    echo "???"
  echo "---------------------opengl-callback-end--------------"

proc enableDefaultDebugCallback*() =
  glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB)
  glDebugMessageCallbackARB(cast[GLdebugProcArb](debugCallback), nil);
