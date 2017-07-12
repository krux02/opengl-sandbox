# included from fancygl.nim

# returns a string, and true if it is a sample type
template glslTypeRepr(t: typedesc[Vec4f]): string = "vec4"
template glslTypeRepr(t: typedesc[Vec3f]): string = "vec3"
template glslTypeRepr(t: typedesc[Vec2f]): string = "vec2"

template glslTypeRepr(t: typedesc[Vec4i]): string = "ivec4"
template glslTypeRepr(t: typedesc[Vec3i]): string = "ivec3"
template glslTypeRepr(t: typedesc[Vec2i]): string = "ivec2"

template glslTypeRepr(t: typedesc[Vec4d]): string = "dvec4"
template glslTypeRepr(t: typedesc[Vec3d]): string = "dvec3"
template glslTypeRepr(t: typedesc[Vec2d]): string = "dvec2"

template glslTypeRepr(t: typedesc[Vec4b]): string = "bvec4"
template glslTypeRepr(t: typedesc[Vec3b]): string = "bvec3"
template glslTypeRepr(t: typedesc[Vec2b]): string = "bvec2"

template glslPrefix(t: typedesc[float32]): string = ""
template glslPrefix(t: typedesc[int32]): string = "i"
template glslPrefix(t: typedesc[float64]): string = "d"
template glslPrefix(t: typedesc[bool]): string = "b"

template glslTypeRepr[M,N,T](t: typedesc[Mat[M,N,T]]): string =
  glslPrefix(t.T) & "mat" & $M & "x" & $N

# TODO this is wrong but needs fix in mesh_loading_tiny
template glslTypeRepr(t: typedesc[Vec4[uint8]]):   string = "vec4"

template glslTypeRepr(t: typedesc[Mat4f]): string = "mat4"
template glslTypeRepr(t: typedesc[Mat3f]): string = "mat3"
template glslTypeRepr(t: typedesc[Mat2f]): string = "mat2"

template glslTypeRepr(t: typedesc[float32]): string = "float"
template glslTypeRepr(t: typedesc[float64]): string = "double"
template glslTypeRepr(t: typedesc[int8]):   string  = "int"
template glslTypeRepr(t: typedesc[int16]):   string = "int"
template glslTypeRepr(t: typedesc[int32]):   string = "int"
template glslTypeRepr(t: typedesc[int64]):   string = "int"
template glslTypeRepr(t: typedesc[uint32]):  string = "uint"
template glslTypeRepr(t: typedesc[bool]):    string = "bool"

template glslIsSampler(t: typedesc[Vec]): bool = false
template glslIsSampler(t: typedesc[Mat]): bool = false
template glslIsSampler(t: typedesc[Mat2]): bool = false
template glslIsSampler(t: typedesc[Mat3]): bool = false
template glslIsSampler(t: typedesc[Mat4]): bool = false


template glslIsSampler(t: typedesc[float32]): bool = false
template glslIsSampler(t: typedesc[int8]):    bool = false
template glslIsSampler(t: typedesc[int16]):   bool = false
template glslIsSampler(t: typedesc[int32]):   bool = false
template glslIsSampler(t: typedesc[int64]):   bool = false
template glslIsSampler(t: typedesc[bool]):    bool = false

template attribSize(t: typedesc[SomeNumber]) : GLint        = 1
template attribNormalized(t: typedesc[SomeNumber]): bool = false

template attribType(t: typedesc[int8]) : GLenum       = cGL_BYTE
template attribType(t: typedesc[int16]) : GLenum      = cGL_SHORT
template attribType(t: typedesc[int32]) : GLenum      = cGL_INT
template attribType(t: typedesc[float32]) : GLenum    = cGL_FLOAT
template attribType(t: typedesc[float64]) : GLenum    = cGL_DOUBLE
template attribType(t: typedesc[uint8]) : GLenum      = GL_UNSIGNED_BYTE
template attribType(t: typedesc[uint16]) : GLenum     = GL_UNSIGNED_SHOR
template attribType(t: typedesc[uint32]) : GLenum     = GL_UNSIGNED_INT

template attribSize[N,T](t: typedesc[Vec[N,T]]): GLint      = GLint(N)
template attribType[N,T](t: typedesc[Vec[N,T]]): GLenum    = T.attribType
template attribNormalized[N,T](t: typedesc[Vec[N,T]]): bool = false

# I hope at some point this will be implemented in the compiler, and i do not need to replicate it here:

# todo insert alignof here

template attribSize(t : typedesc[Color]): GLint = 4
template attribType(t:  typedesc[Color]) : GLenum = GL_UNSIGNED_BYTE
template attribNormalized(t: typedesc[Color]) : GLboolean = true

template glslTypeRepr(t: typedesc[Color]): string = "vec4"
template glslIsSampler(t: typedesc[Color]): bool = false

proc `==`*(v1,v2: Color): bool =
  v1.r == v2.r and v1.g == v2.g and v1.b == v2.b and v1.a == v2.a
