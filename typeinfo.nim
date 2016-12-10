# included from fancygl.nim

# returns a string, and true if it is a sample type
when isMainModule:
  import glm, opengl

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


# TODO this is wrong but needs fix in mesh_loading_tiny
template glslTypeRepr(t: typedesc[Vec4[uint8]]):   string = "vec4"


template glslTypeRepr(t: typedesc[Mat4x4[float32]]): string = "mat4"
template glslTypeRepr(t: typedesc[Mat3x3[float32]]): string = "mat3"
template glslTypeRepr(t: typedesc[Mat2x2[float32]]): string = "mat2"
  
template glslTypeRepr(t: typedesc[float32]): string = "float"
template glslTypeRepr(t: typedesc[float64]): string = "double"
template glslTypeRepr(t: typedesc[int8]):   string  = "int"
template glslTypeRepr(t: typedesc[int16]):   string = "int"
template glslTypeRepr(t: typedesc[int32]):   string = "int"
template glslTypeRepr(t: typedesc[int64]):   string = "int"
template glslTypeRepr(t: typedesc[uint32]):  string = "uint"
template glslTypeRepr(t: typedesc[bool]):    string = "bool"

template glslTypeRepr(t: typedesc[Texture1D]): string = "sampler1D"
template glslTypeRepr(t: typedesc[Texture2D]): string = "sampler2D"
template glslTypeRepr(t: typedesc[Texture3D]): string = "sampler3D"

template glslTypeRepr(t: typedesc[TextureRectangle]): string = "sampler2DRect"

template glslTypeRepr(t: typedesc[Texture2DArray]): string = "sampler2DArray"
template glslTypeRepr(t: typedesc[Texture1DArray]): string = "sampler1DArray"

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
  
template glslIsSampler(t: typedesc[Texture1D]): bool = true
template glslIsSampler(t: typedesc[Texture2D]): bool = true
template glslIsSampler(t: typedesc[Texture3D]): bool = true
template glslIsSampler(t: typedesc[TextureRectangle]): bool = true
template glslIsSampler(t: typedesc[Texture2DArray]): bool = true
template glslIsSampler(t: typedesc[Texture2DArrayShadow]): bool = true

template attribSize[N,T](t: typedesc[Vec[N,T]]): GLint      = GLint(N)
template attribNormalized[N,T](t: typedesc[Vec[N,T]]): bool = false

template attribType[N](t: typedesc[Vec[N,int8]]): GLenum    = cGL_BYTE
template attribType[N](t: typedesc[Vec[N,uint8]]): GLenum   = GL_UNSIGNED_BYTE
template attribType[N](t: typedesc[Vec[N,int16]]): GLenum    = cGL_SHORT
template attribType[N](t: typedesc[Vec[N,uint16]]): GLenum   = GL_UNSIGNED_SHORT
template attribType[N](t: typedesc[Vec[N,int32]]): GLenum    = cGL_INT
template attribType[N](t: typedesc[Vec[N,uint32]]): GLenum   = GL_UNSIGNED_INT
template attribType[N](t: typedesc[Vec[N,float32]]): GLenum  = cGL_FLOAT
template attribType[N](t: typedesc[Vec[N,float64]]): GLenum  = cGL_DOUBLE

template attribSize(t: typedesc[float64]) : GLint     = 1
template attribType(t: typedesc[float64]) : GLenum    = cGL_DOUBLE
template attribNormalized(t: typedesc[float64]): bool = false
  
template attribSize(t: typedesc[float32]) : GLint     = 1
template attribType(t: typedesc[float32]) : GLenum    = cGL_FLOAT
template attribNormalized(t: typedesc[float32]): bool = false

template attribSize(t: typedesc[int32]) : GLint       = 1
template attribType(t: typedesc[int32]) : GLenum      = cGL_INT
template attribNormalized(t: typedesc[int32]): bool   = false

template attribSize(t: typedesc[int16]) : GLint       = 1
template attribType(t: typedesc[int16]) : GLenum      = cGL_SHORT
template attribNormalized(t: typedesc[int16]): bool   = false

template attribSize(t: typedesc[int8]) : GLint        = 1
template attribType(t: typedesc[int8]) : GLenum       = cGL_BYTE
template attribNormalized(t: typedesc[int8]): bool    = false

template attribSize(t: typedesc[uint32]) : GLint      = 1
template attribType(t: typedesc[uint32]) : GLenum     = GL_UNSIGNED_INT
template attribNormalized(t: typedesc[int32]): bool   = false

template attribSize(t: typedesc[uint16]) : GLint      = 1
template attribType(t: typedesc[uint16]) : GLenum     = GL_UNSIGNED_SHORT
template attribNormalized(t: typedesc[int16]): bool   = false

template attribSize(t: typedesc[uint8]) : GLint       = 1
template attribType(t: typedesc[uint8]) : GLenum      = GL_UNSIGEND_BYTE
template attribNormalized(t: typedesc[uint8]): bool   = false


  

template attribSize(t : typedesc[Color]): GLint = 4
template attribType(t:  typedesc[Color]) : GLenum = GL_UNSIGNED_BYTE
template attribNormalized(t: typedesc[Color]) : GLboolean = true

template glslTypeRepr(t: typedesc[Color]): string = "vec4"
template glslIsSampler(t: typedesc[Color]): bool = false

proc `==`*(v1,v2: Color): bool =
  v1.r == v2.r and v1.g == v2.g and v1.b == v2.b and v1.a == v2.a
