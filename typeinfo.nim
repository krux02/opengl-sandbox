# returns a string, and true if it is a sample type


proc glslUniformType(value : NimNode): tuple[name: string, isSampler: bool] =
  let tpe = value.getTypeInst
  if tpe.kind == nnkBracketExpr:
    case $tpe[0]
    of "Mat4x4":
      ("mat4", false)
    of "Mat3x3":
      ("mat3", false)
    of "Mat2x2":
      ("mat2", false)
    of "Vec4":
      ("vec4", false)
    of "Vec3":
      ("vec3", false)
    of "Vec2":
      ("vec2", false)
    else:
      ("(unknown:" & $tpe[0] & ")", false)
  else:
    case $tpe
    of "Texture1D":
      ("sampler1D", true)
    of "Texture2D":
      ("sampler2D", true)
    of "Texture3D":
      ("sampler3D", true)
    of "TextureRectangle":
      ("sampler2DRect", true)
    of "float32", "float64", "float":
      ("float", false)
    of "int16", "int32", "int64", "int":
      ("int", false)
    of "Mat4d", "Mat4f":
      ("mat4", false)
    of "Mat3d", "Mat3f":
      ("mat3", false)
    of "Mat2d", "Mat2f":
      ("mat2", false)
    of "Vec4d", "Vec4f":
      ("vec4", false)
    of "Vec3d", "Vec3f":
      ("vec3", false)
    of "Vec2d", "Vec2f":
      ("vec2", false)
    else:
      (($tpe).toLower, false)

proc glslAttribType(value : NimNode): string =
  # result = getAst(glslAttribType(value))[0].strVal
  let tpe = value.getTypeInst
  if $tpe[0] == "seq" or $tpe[0] == "ArrayBuffer":
    tpe[1].glslUniformType[0]
  else:
    echo "not a compatible attribType: "
    echo tpe.repr
    "(error not a seq[..])"

proc attribSize(t: typedesc[Vec4d]) : GLint = 4
proc attribType(t: typedesc[Vec4d]) : GLenum = cGL_DOUBLE
proc attribNormalized(t: typedesc[Vec4d]) : bool = false

proc attribSize(t: typedesc[Vec3d]) : GLint = 3
proc attribType(t: typedesc[Vec3d]) : GLenum = cGL_DOUBLE
proc attribNormalized(t: typedesc[Vec3d]) : bool = false

proc attribSize(t: typedesc[Vec2d]) : GLint = 2
proc attribType(t: typedesc[Vec2d]) : GLenum = cGL_DOUBLE
proc attribNormalized(t: typedesc[Vec2d]) : bool = false

proc attribSize(t: typedesc[Vec4f]) : GLint = 4
proc attribType(t: typedesc[Vec4f]) : GLenum = cGL_FLOAT
proc attribNormalized(t: typedesc[Vec4f]) : bool = false

proc attribSize(t: typedesc[Vec3f]) : GLint = 3
proc attribType(t: typedesc[Vec3f]) : GLenum = cGL_FLOAT
proc attribNormalized(t: typedesc[Vec3f]) : bool = false

proc attribSize(t: typedesc[Vec2f]) : GLint = 2
proc attribType(t: typedesc[Vec2f]) : GLenum = cGL_FLOAT
proc attribNormalized(t: typedesc[Vec2f]) : bool = false

proc attribSize(t: typedesc[Vec4[uint8]]) : GLint = 4
proc attribType(t: typedesc[Vec4[uint8]]) : GLenum = cGL_UNSIGNED_BYTE
proc attribNormalized(t: typedesc[Vec4[uint8]]) : bool = false

