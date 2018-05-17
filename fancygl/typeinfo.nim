# included from fancygl.nim

# returns a string, and true if it is a sample type

template glslPrefix(t: typedesc[SomeSignedInt   ]): string = "i"
template glslPrefix(t: typedesc[SomeUnsignedInt ]): string = "u"
template glslPrefix(t: typedesc[float32         ]): string = ""
template glslPrefix(t: typedesc[float64         ]): string = "d"
template glslPrefix(t: typedesc[bool            ]): string = "b"

template glslTypeRepr[N,T](t: typedesc[Vec[N,T]]): string =
  glslPrefix(type(t.T)) & "vec" & $N

template glslTypeRepr[M,N,T](t: typedesc[Mat[M,N,T]]): string =
  glslPrefix(type(t.T)) & "mat" & $M & "x" & $N

template glslTypeRepr(t: typedesc[float32         ]): string = "float"
template glslTypeRepr(t: typedesc[float64         ]): string = "double"
template glslTypeRepr(t: typedesc[SomeSignedInt   ]): string = "int"
template glslTypeRepr(t: typedesc[SomeUnsignedInt ]): string = "uint"
template glslTypeRepr(t: typedesc[bool            ]): string = "bool"

template glslIsSampler[N,T]  (t: typedesc[Vec[N,T]  ]): bool = false
template glslIsSampler[M,N,T](t: typedesc[Mat[M,N,T]]): bool = false
template glslIsSampler       (t: typedesc[SomeNumber]): bool = false
template glslIsSampler       (t: typedesc[bool      ]): bool = false

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

template attribSize*[N,T](t: typedesc[Vec[N,T]]): GLint      = GLint(N)
template attribType*[N,T](t: typedesc[Vec[N,T]]): GLenum    = T.attribType
template attribNormalized*[N,T](t: typedesc[Vec[N,T]]): bool = false

# I hope at some point this will be implemented in the compiler, and i do not need to replicate it here:
# todo insert alignof here

template attribSize*(t : typedesc[Color]): GLint = 4
template attribType*(t:  typedesc[Color]) : GLenum = GL_UNSIGNED_BYTE
template attribNormalized*(t: typedesc[Color]) : GLboolean = true
template glslTypeRepr(t: typedesc[Color]): string = "vec4"
template glslIsSampler(t: typedesc[Color]): bool = false

proc `==`*(v1,v2: Color): bool =
  v1.r == v2.r and v1.g == v2.g and v1.b == v2.b and v1.a == v2.a
