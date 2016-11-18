# included from fancygl.nim

# returns a string, and true if it is a sample type
import glm

template glslTypeRepr(t: typedesc[Vec4[float32]]): string = "vec4"
template glslTypeRepr(t: typedesc[Vec3[float32]]): string = "vec3"
template glslTypeRepr(t: typedesc[Vec2[float32]]): string = "vec2"
# TODO this is wrong but needs fix in mesh_loading_tiny
template glslTypeRepr(t: typedesc[Vec4[uint8]]):   string = "vec4"

template glslTypeRepr(t: typedesc[Mat4x4[float32]]): string = "mat4"
template glslTypeRepr(t: typedesc[Mat3x3[float32]]): string = "mat3"
template glslTypeRepr(t: typedesc[Mat2x2[float32]]): string = "mat2"
  
template glslTypeRepr(t: typedesc[float32]): string = "float"
template glslTypeRepr(t: typedesc[int8]):   string = "int"
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

template glslIsSampler(t: typedesc[Vec4[float32]]): bool = false
template glslIsSampler(t: typedesc[Vec3[float32]]): bool = false
template glslIsSampler(t: typedesc[Vec2[float32]]): bool = false
template glslIsSampler(t: typedesc[Vec4[uint8]]):   bool = false

template glslIsSampler(t: typedesc[Mat4x4[float32]]): bool = false
template glslIsSampler(t: typedesc[Mat3x3[float32]]): bool = false
template glslIsSampler(t: typedesc[Mat2x2[float32]]): bool = false
  
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

proc attribSize(t: typedesc[Vec4d]) : GLint       = 4
proc attribType(t: typedesc[Vec4d]) : GLenum      = cGL_DOUBLE
proc attribNormalized(t: typedesc[Vec4d]) : bool  = false

proc attribSize(t: typedesc[Vec3d]) : GLint       = 3
proc attribType(t: typedesc[Vec3d]) : GLenum      = cGL_DOUBLE
proc attribNormalized(t: typedesc[Vec3d]) : bool  = false

proc attribSize(t: typedesc[Vec2d]) : GLint       = 2
proc attribType(t: typedesc[Vec2d]) : GLenum      = cGL_DOUBLE
proc attribNormalized(t: typedesc[Vec2d]) : bool  = false

proc attribSize(t: typedesc[float64]) : GLint     = 1
proc attribType(t: typedesc[float64]) : GLenum    = cGL_DOUBLE
proc attribNormalized(t: typedesc[float64]): bool = false

proc attribSize(t: typedesc[Vec4f]) : GLint       = 4
proc attribType(t: typedesc[Vec4f]) : GLenum      = cGL_FLOAT
proc attribNormalized(t: typedesc[Vec4f]) : bool  = false

proc attribSize(t: typedesc[Vec3f]) : GLint       = 3
proc attribType(t: typedesc[Vec3f]) : GLenum      = cGL_FLOAT
proc attribNormalized(t: typedesc[Vec3f]) : bool  = false

proc attribSize(t: typedesc[Vec2f]) : GLint       = 2
proc attribType(t: typedesc[Vec2f]) : GLenum      = cGL_FLOAT
proc attribNormalized(t: typedesc[Vec2f]) : bool  = false

proc attribSize(t: typedesc[float32]) : GLint     = 1
proc attribType(t: typedesc[float32]) : GLenum    = cGL_FLOAT
proc attribNormalized(t: typedesc[float32]): bool = false

proc attribSize(t: typedesc[Vec4[uint8]]) : GLint  = 4
proc attribType(t: typedesc[Vec4[uint8]]) : GLenum = GL_UNSIGNED_BYTE
proc attribNormalized(t: typedesc[Vec4[uint8]]) : bool = false



proc `==`*(v1,v2: Color): bool =
  v1.r == v2.r and v1.g == v2.g and v1.b == v2.b and v1.a == v2.a

proc attribSize(t : typedesc[Color]): GLint = 4
proc attribType(t:  typedesc[Color]) : GLenum = GL_UNSIGNED_BYTE
proc attribNormalized(t: typedesc[Color]) : GLboolean = true

template glslTypeRepr(t: typedesc[Color]): string = "vec4"
template glslIsSampler(t: typedesc[Color]): bool = false
