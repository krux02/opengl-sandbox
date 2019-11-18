# included from fancygl.nim

# returns a string, and true if it is a sample type
import glm, ../glad/gl, sdl2/sdl, macros, normalizeType, ast_pattern_matching

template glslPrefix*(t: typedesc[SomeSignedInt   ]): string = "i"
template glslPrefix*(t: typedesc[SomeUnsignedInt ]): string = "u"
template glslPrefix*(t: typedesc[float32         ]): string = ""
template glslPrefix*(t: typedesc[float64         ]): string = "d"
template glslPrefix*(t: typedesc[bool            ]): string = "b"

template glslTypeRepr*[N,T](t: typedesc[Vec[N,T]]): string =
  glslPrefix(type(t.T)) & "vec" & $N

template glslTypeRepr*[M,N,T](t: typedesc[Mat[M,N,T]]): string =
  glslPrefix(type(t.T)) & "mat" & $M & "x" & $N

template glslTypeRepr*(t: typedesc[float32         ]): string = "float"
template glslTypeRepr*(t: typedesc[float64         ]): string = "double"
template glslTypeRepr*(t: typedesc[SomeSignedInt   ]): string = "int"
template glslTypeRepr*(t: typedesc[SomeUnsignedInt ]): string = "uint"
template glslTypeRepr*(t: typedesc[bool            ]): string = "bool"

template glslIsSampler*[N,T]  (t: typedesc[Vec[N,T]  ]): bool = false
template glslIsSampler*[M,N,T](t: typedesc[Mat[M,N,T]]): bool = false
template glslIsSampler*       (t: typedesc[SomeNumber]): bool = false
template glslIsSampler*       (t: typedesc[bool      ]): bool = false

template attribSize*(t: typedesc[SomeNumber]) : GLint        = 1
template attribNormalized*(t: typedesc[SomeNumber]): bool = false

template attribType*(t: typedesc[int8    ]) : GLenum = cGL_BYTE
template attribType*(t: typedesc[int16   ]) : GLenum = cGL_SHORT
template attribType*(t: typedesc[int32   ]) : GLenum = cGL_INT
template attribType*(t: typedesc[float32 ]) : GLenum = cGL_FLOAT
template attribType*(t: typedesc[float64 ]) : GLenum = cGL_DOUBLE
template attribType*(t: typedesc[uint8   ]) : GLenum = GL_UNSIGNED_BYTE
template attribType*(t: typedesc[uint16  ]) : GLenum = GL_UNSIGNED_SHOR
template attribType*(t: typedesc[uint32  ]) : GLenum = GL_UNSIGNED_INT

# singed index types are just interpreted as unsigend index types.
template indexTypeTag*(arg: typedesc[uint8 ]): GLenum = GL_UNSIGNED_BYTE
template indexTypeTag*(arg: typedesc[ int8 ]): GLenum = GL_UNSIGNED_BYTE
template indexTypeTag*(arg: typedesc[uint16]): GLenum = GL_UNSIGNED_SHORT
template indexTypeTag*(arg: typedesc[ int16]): GLenum = GL_UNSIGNED_SHORT
template indexTypeTag*(arg: typedesc[uint32]): GLenum = GL_UNSIGNED_INT
template indexTypeTag*(arg: typedesc[ int32]): GLenum = GL_UNSIGNED_INT

template attribSize*[N,T](t: typedesc[Vec[N,T]]): GLint      = GLint(N)
template attribType*[N,T](t: typedesc[Vec[N,T]]): GLenum    = T.attribType
template attribNormalized*[N,T](t: typedesc[Vec[N,T]]): bool = false

# I hope at some point this will be implemented in the compiler, and i do not need to replicate it here:
# todo insert alignof here

template attribSize*(t : typedesc[Color]): GLint = 4
template attribType*(t:  typedesc[Color]) : GLenum = GL_UNSIGNED_BYTE
template attribNormalized*(t: typedesc[Color]) : GLboolean = true
template glslTypeRepr*(t: typedesc[Color]): string = "vec4"
template glslIsSampler*(t: typedesc[Color]): bool = false

proc `==`*(v1,v2: Color): bool =
  v1.r == v2.r and v1.g == v2.g and v1.b == v2.b and v1.a == v2.a

# API for macros

proc expectIntIn(arg: NimNode; slice: Slice[int]): void =
  if arg.intVal notin slice:
    error("expect integer literal in range: " & $slice.a & " .. " & $slice.b & " but got " & $arg.intVal, arg)

proc glslType*(arg: NimNode): string {.compileTime.} =
  let arg = arg.normalizeType
  arg.matchAst:
  of nnkBracketExpr( ident"array", nnkInfix(ident"..", 0, `highLit`), `innerType`):
    result = glslType(innerType)
    result.add "["
    result.addInt(highLit.intVal+1)
    result.add "]"
    return
  else:
    discard

  arg.matchAst:
  of ident"float32":
    return "float"
  of ident"float64":
    return "double"
  of ident"int32":
    return "int"
  of ident"bool":
    return "bool"
  of ident"Texture1D":
    return "sampler1D"
  of ident"Texture2D":
    return "sampler2D"
  of ident"Texture3D":
    return "sampler3D"
  of ident"TextureCubeMap":
    return "samplerCube"
  of ident"Texture2DShadow":
    return "sampler2DShadow"
  of ident"TextureCubeShadow":
    return "samplerCubeShadow"
  of ident"Texture2DArray":
    return "sampler2DArray"
  of ident"Texture2DArrayShadow":
    return "sampler2DArrayShadow"

  of nnkBracketExpr(ident"Vec", `sizeLit`, `Tsym`):

    Tsym.matchAst:
    of ident"float32":
      result.add "vec"
    of ident"float64":
      result.add "dvec"
    of ident"int32":
      result.add "ivec"
    of ident"bool":
      result.add "bvec"

    sizeLit.expectIntIn 2..4
    result.addInt sizeLit.intVal

  of nnkBracketExpr(ident"Mat", `sizeLit1`, `sizeLit2`, `Tsym`):

    Tsym.matchAst:
    of ident"float32":
      result.add "mat"
    of ident"float64":
      result.add "dmat"
    of ident"int32":
      result.add "imat"
    of ident"bool":
      result.add "bmat"

    sizeLit1.expectIntIn 2..4
    sizeLit2.expectIntIn 2..4

    let intVal1 = sizeLit1.intVal
    let intVal2 = sizeLit2.intVal

    result.addInt intVal1
    if intVal2 != intVal1:
      result.add "x"
      result.addInt intVal2
  else:
    ## well this is definitively wrong
    ## the type needs to be translated to glsl.
    result = arg.repr
