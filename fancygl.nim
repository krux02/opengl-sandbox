import opengl, glm, math, strutils, nre, macros, macroutils, sdl2, sdl2/image

#### glm additions ####

type Vec4f* = Vec4[float32]
type Vec3f* = Vec3[float32]
type Vec2f* = Vec2[float32]

proc vec4f*(x,y,z,w:float32)             : Vec4f = [  x,   y,   z,   w].Vec4f
proc vec4f*(v:Vec3f,w:float32)           : Vec4f = [v.x, v.y, v.z,   w].Vec4f
proc vec4f*(x:float32,v:Vec3f)           : Vec4f = [  x, v.x, v.y, v.z].Vec4f
proc vec4f*(a,b:Vec2f)                   : Vec4f = [a.x, a.y, b.x, b.y].Vec4f
proc vec4f*(v:Vec2f,z,w:float32)         : Vec4f = [v.x, v.y,   z,   w].Vec4f
proc vec4f*(x:float32,v:Vec2f,w:float32) : Vec4f = [  x, v.x, v.y,   w].Vec4f
proc vec4f*(x,y:float32,v:Vec2f)         : Vec4f = [  x,   y, v.x, v.y].Vec4f
proc vec4f*(x:float32)                   : Vec4f = [  x,   x,   x,   x].Vec4f

proc vec3f*(x,y,z:   float32)  : Vec3f = [  x,   y,   z].Vec3f
proc vec3f*(v:Vec2f,z:float32) : Vec3f = [v.x, v.y,   z].Vec3f
proc vec3f*(x:float32,v:Vec2f) : Vec3f = [  x, v.x, v.y].Vec3f
proc vec3f*(x:float32)         : Vec3f = [  x,   x,   x].Vec3f

proc vec2f*(x,y:float32) : Vec2f = [x,y].Vec2f
proc vec2f*(x:float32)   : Vec2f = [x,x].Vec2f

type Vec4d* = Vec4[float64]
type Vec3d* = Vec3[float64]
type Vec2d* = Vec2[float64]

proc vec4d*(x,y,z,w:float64)             : Vec4d = [  x,   y,   z,   w].Vec4d
proc vec4d*(v:Vec3d,w:float64)           : Vec4d = [v.x, v.y, v.z,   w].Vec4d
proc vec4d*(x:float64,v:Vec3d)           : Vec4d = [  x, v.x, v.y, v.z].Vec4d
proc vec4d*(a,b:Vec2d)                   : Vec4d = [a.x, a.y, b.x, b.y].Vec4d
proc vec4d*(v:Vec2d,z,w:float64)         : Vec4d = [v.x, v.y,   z,   w].Vec4d
proc vec4d*(x:float64,v:Vec2d,w:float64) : Vec4d = [  x, v.x, v.y,   w].Vec4d
proc vec4d*(x,y:float64,v:Vec2d)         : Vec4d = [  x,   y, v.x, v.y].Vec4d
proc vec4d*(x:float64)                   : Vec4d = [  x,   x,   x,   x].Vec4d

proc vec3d*(x,y,z:   float64)  : Vec3d = [  x,   y,   z].Vec3d
proc vec3d*(v:Vec2d,z:float64) : Vec3d = [v.x, v.y,   z].Vec3d
proc vec3d*(x:float64,v:Vec2d) : Vec3d = [  x, v.x, v.y].Vec3d
proc vec3d*(x:float64)         : Vec3d = [  x,   x,   x].Vec3d

proc vec2d*(x,y:float64) : Vec2d = [x,y].Vec2d
proc vec2d*(x:float64)   : Vec2d = [x,x].Vec2d

proc vec4f*(v: Vec4d) : Vec4f = [v.x.float32,v.y.float32,v.z.float32,v.w.float32].Vec4f
proc vec3f*(v: Vec3d) : Vec3f = [v.x.float32,v.y.float32,v.z.float32].Vec3f
proc vec2f*(v: Vec2d) : Vec2f = [v.x.float32,v.y.float32].Vec2f
proc vec4d*(v: Vec4f) : Vec4d = [v.x.float64,v.y.float64,v.z.float64,v.w.float64].Vec4d
proc vec3d*(v: Vec3f) : Vec3d = [v.x.float64,v.y.float64,v.z.float64].Vec3d
proc vec2d*(v: Vec2f) : Vec2d = [v.x.float64,v.y.float64].Vec2d

type Mat4f* = Mat4x4[float32]
type Mat3f* = Mat3x3[float32]
type Mat2f* = Mat2x2[float32]
type Mat4d* = Mat4x4[float64]
type Mat3d* = Mat3x3[float64]
type Mat2d* = Mat2x2[float64]

proc mat4f*(mat: Mat4d): Mat4f =
  for i in 0..<4:
   for j in 0..<4:
     result[i][j] = mat[i][j]

proc I4*() : Mat4d = mat4x4(
  vec4d(1, 0, 0, 0),
  vec4d(0, 1, 0, 0),
  vec4d(0, 0, 1, 0),
  vec4d(0, 0, 0, 1)
)

proc I4f*() : Mat4f = mat4x4[float32](
  vec4f(1, 0, 0, 0),
  vec4f(0, 1, 0, 0),
  vec4f(0, 0, 1, 0),
  vec4f(0, 0, 0, 1)
)

################################################################################
#### primitive objects #########################################################
################################################################################

### uv sphere ###

proc uvSphereVertices*(segments, rings: int): seq[Vec3f] =
  result.newSeq(segments * rings)
  result.setLen(0)

  for j in 0 .. < segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32

    for i in 0 .. < rings:
      let
        alpha = (i / (rings-1)) * PI
        h = cos(alpha).float32
        r = sin(alpha).float32

      result.add( vec3f(x * r, y * r, h) )


proc uvSphereNormals*(segments, rings: int): seq[Vec3f] =
  uvSphereVertices(segments, rings)

proc uvSphereTexCoords*(segments, rings: int): seq[Vec2f] =
  result.newSeq(segments * rings)
  result.setLen(0)

  for j in 0 .. < segments:
    let beta = (j / segments).float32

    for i in 0 .. < rings:
      let alpha = (i / (rings-1)).float32

      result.add( vec2f(alpha,beta) )


proc uvSphereIndices*(segments, rings: int): seq[int16] =
  result.newSeq(segments * rings * 6)
  result.setLen(0)

  for segment in 0 ..< segments - 1:
    for ring in 0 ..< rings - 1:
      let
        i1 = int16( ring +     segment * rings )
        i2 = int16( ring + 1 + segment * rings )
        i3 = int16( ring +     segment * rings + rings )
        i4 = int16( ring + 1 + segment * rings + rings )
      result.add([i1,i2,i3,i3,i2,i4])

  for ring in 0 ..< rings - 1:
    let
      i1 = int16( ring +     segments * rings - rings )
      i2 = int16( ring + 1 + segments * rings - rings )
      i3 = int16( ring +     0 )
      i4 = int16( ring + 1 + 0 )

    result.add([i1,i2,i3,i3,i2,i4])

### cylinder ###

proc cylinderVertices*(segments: int, topRadius: float32 = 1): seq[Vec3f] =
  result.newSeq(segments * 4 + 2)

  result[2 * segments] = vec3f(0,0,-1)
  result[3 * segments + 1] = vec3f(0,0, 1)

  for j in 0 .. < segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32
      top =    vec3f(vec2f(x,y) * topRadius,  1)
      bottom = vec3f(x, y, -1)

    result[2*j+0] = bottom
    result[2*j+1] = top
    result[2*segments + 1 + j] = bottom
    result[3*segments + 2 + j] = top

proc cylinderNormals*(segments: int, topRadius: float32 = 1): seq[Vec3f] =
  result.newSeq(segments * 4 + 2)

  result[2 * segments] = vec3f(0,0,-1)
  result[3 * segments + 1] = vec3f(0,0, 1)

  let n = vec2f(2,1-topRadius).normalize

  echo n

  for j in 0 .. < segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32

    result[2*j+0] = vec3f( vec2(x, y) * n.x, n.y)
    result[2*j+1] = vec3f( vec2(x, y) * n.x, n.y)
    result[2*segments + 1 + j] = vec3f(0,0,-1)
    result[3*segments + 2 + j] = vec3f(0,0, 1)

proc cylinderTexCoords*(segments: int): seq[Vec2f] =
  result.newSeq(segments * 4 + 2)

  result[2 * segments] = vec2f(0.5f)
  result[3 * segments + 1] = vec2f(0.5f)

  for j in 0 .. < segments:
    let
      u = (j / segments).float32
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32 * 0.5f + 0.5f
      y = sin(beta).float32 * 0.5f + 0.5f

    result[2*j+0] = vec2f(u, 0)
    result[2*j+1] = vec2f(u, 1)
    result[2*segments + 1 + j] = vec2f(x,y)
    result[3*segments + 2 + j] = vec2f(x,y)

proc cylinderIndices*(segments: int): seq[int16] =
  result.newSeq(0)

  for i in 0 ..< segments - 1:
    let
      i1 = int16( i * 2 + 0 )
      i2 = int16( i * 2 + 1 )
      i3 = int16( i * 2 + 2 )
      i4 = int16( i * 2 + 3 )

    result.add([i1,i3,i2,i2,i3,i4])

  let
    i1 = int16( segments * 2 - 2 )
    i2 = int16( segments * 2 - 1 )
    i3 = int16( 0 )
    i4 = int16( 1 )

  result.add([i1,i3,i2,i2,i3,i4])


  var base = int16(2 * segments)

  for i in 0 ..< int16(segments - 1):
    let ii = i.int16
    result.add( [ base , base + ii + 2, base + ii + 1 ] )

  result.add( [ base , base + 1, base + segments.int16 ] )

  base = int16(3 * segments + 1)

  for i in 0 ..< segments - 1:
    let ii = i.int16
    result.add( [ base, base + ii + 1, base + ii + 2 ] )

  result.add( [ base , base + segments.int16, base + 1 ] )


### box ###

const
  boxVertices* = @[
    vec3f(+1, +1, -1), vec3f(-1, +1, -1), vec3f(-1, +1, +1),
    vec3f(+1, +1, +1), vec3f(+1, +1, -1), vec3f(-1, +1, +1),
    vec3f(+1, -1, +1), vec3f(-1, -1, +1), vec3f(-1, -1, -1),
    vec3f(+1, -1, -1), vec3f(+1, -1, +1), vec3f(-1, -1, -1),
    vec3f(+1, +1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, +1),
    vec3f(+1, -1, +1), vec3f(+1, +1, +1), vec3f(-1, -1, +1),
    vec3f(+1, -1, -1), vec3f(-1, -1, -1), vec3f(-1, +1, -1),
    vec3f(+1, +1, -1), vec3f(+1, -1, -1), vec3f(-1, +1, -1),
    vec3f(-1, +1, +1), vec3f(-1, +1, -1), vec3f(-1, -1, -1),
    vec3f(-1, -1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, -1),
    vec3f(+1, +1, -1), vec3f(+1, +1, +1), vec3f(+1, -1, +1),
    vec3f(+1, -1, -1), vec3f(+1, +1, -1), vec3f(+1, -1, +1)
  ]

  boxNormals* = @[
    vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
    vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
    vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
    vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
    vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
    vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
    vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
    vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
    vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
    vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
    vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0),
    vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0)
  ]

  boxColors* = @[
    vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
    vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
    vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
    vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
    vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
    vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
    vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
    vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
    vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
    vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
    vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0),
    vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0)
  ]

  boxTexCoords* = @[
    vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),
    vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(1, 0), vec2f(0, 0),
    vec2f(0, 1), vec2f(1, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(1, 1), vec2f(0, 1),
    vec2f(0, 0), vec2f(1, 0), vec2f(0, 1)
  ]

################################################################################
#### macro utils ###############################################################
################################################################################

macro debugResult(arg: typed) : stmt =
  echo arg.repr
  arg

################################################################################
#### Sampler Types #############################################################
################################################################################

macro nilName(name:expr) : expr =
  name.expectKind(nnkIdent)
  !!("nil_" & $name)

template textureTypeTemplate(name, nilName, target:expr, shadername:string): stmt =
  type name* = distinct GLuint
  const nilName* = name(0)

  proc bindIt*(texture: name) =
    glBindTexture(target, texture.GLuint)

  proc parameter*(texture: name, pname: GLenum, param: GLint): void =
    glTextureParameteriEXT(texture.GLuint, target, pname, param)

  proc generateMipmap*(texture: name): void =
    glGenerateTextureMipmapEXT(texture.GLuint, target)

template textureTypeTemplate(name: expr, target:expr, shadername:string): stmt =
  textureTypeTemplate(name, nilName(name), target, shadername)

proc geometryNumVerts(mode: GLenum): int =
  case mode
  of GL_POINTS: 1
  of GL_LINE_STRIP: 2
  of GL_LINE_LOOP: 2
  of GL_LINES: 2
  of GL_LINE_STRIP_ADJACENCY: 4
  of GL_LINES_ADJACENCY: 4
  of GL_TRIANGLE_STRIP: 3
  of GL_TRIANGLE_FAN: 3
  of GL_TRIANGLES: 3
  of GL_TRIANGLE_STRIP_ADJACENCY: 6
  of GL_TRIANGLES_ADJACENCY: 6
  of GL_PATCHES: -1
  else: -1128

proc geometryPrimitiveLayout(mode: GLenum): string =
  case mode
  of GL_POINTS:
    "points"
  of GL_LINE_STRIP, GL_LINE_LOOP, GL_LINES:
    "lines"
  of GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY:
    "lines_adjacency"
  of GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES:
    "triangles"
  of GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY:
    "triangles_adjacency"
  else:
    ""


textureTypeTemplate(Texture1D,                 nil_Texture1D,
    GL_TEXTURE_1D, "sampler1D")
textureTypeTemplate(Texture2D,                 nil_Texture2D,
    GL_TEXTURE_2D, "sampler2D")
textureTypeTemplate(Texture3D,                 nil_Texture3D,
    GL_TEXTURE_3D, "sampler3D")
textureTypeTemplate(Texture1DArray,             nil_Texture1DArray,
    GL_Texture_1D_ARRAY, "sampler1DArray")
textureTypeTemplate(Texture2DArray,            nil_Texture2DArray,
    GL_TEXTURE_2D_ARRAY, "sampler2DArray")
textureTypeTemplate(TextureRectangle,          nil_TextureRectangle,
    GL_TEXTURE_RECTANGLE, "sampler2DRect")
textureTypeTemplate(TextureCubeMap,            nil_TextureCubeMap,
    GL_TEXTURE_CUBE_MAP, "samplerCube")
textureTypeTemplate(TextureCubeMapArray,       nil_TextureCubeMapArray,
    GL_TEXTURE_CUBE_MAP_ARRAY , "samplerCubeArray")
textureTypeTemplate(TextureBuffer,             nil_TextureBuffer,
    GL_TEXTURE_BUFFER, "samplerBuffer")
textureTypeTemplate(Texture2DMultisample,      nil_Texture2DMultisample,
    GL_TEXTURE_2D_MULTISAMPLE, "sampler2DMS")
textureTypeTemplate(Texture2DMultisampleArray, nil_Texture2DMultisampleArray,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY, "sampler2DMSArray")


textureTypeTemplate(Texture1DShadow,        nil_Texture1DShadow,        GL_TEXTURE_1D,             "sampler1DShadow​")
textureTypeTemplate(Texture2DShadow,        nil_Texture2DShadow,        GL_TEXTURE_2D,             "sampler2DShadow​")
textureTypeTemplate(TextureCubeShadow,      nil_TextureCubeShadow,      GL_TEXTURE_CUBE_MAP,       "samplerCubeShadow​")
textureTypeTemplate(Texture2DRectShadow,    nil_Texture2DRectShadow,    GL_TEXTURE_RECTANGLE,      "sampler2DRectShadow​")
textureTypeTemplate(Texture1DArrayShadow,   nil_Texture1DArrayShadow,   GL_TEXTURE_1D_ARRAY,       "sampler1DArrayShadow​")
textureTypeTemplate(Texture2DArrayShadow,   nil_Texture2DArrayShadow,   GL_TEXTURE_2D_ARRAY,       "sampler2DArrayShadow​")
textureTypeTemplate(TextureCubeArrayShadow, nil_TextureCubeArrayShadow, GL_TEXTURE_CUBE_MAP_ARRAY, "samplerCubeArrayShadow​")

proc loadTextureRectangleFromFile*(filename: string): TextureRectangle =
  let surface = image.load(filename)
  defer: freeSurface(surface)
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)
  glGenTextures(1, cast[ptr GLuint](result.addr))
  glTextureImage2DEXT(result.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_RGBA, surface2.w, surface2.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)

proc loadTexture2DFromFile*(filename: string): Texture2D =
  let surface = image.load(filename)
  defer: freeSurface(surface)
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)
  glGenTextures(1, cast[ptr GLuint](result.addr))
  glTextureImage2DEXT(result.GLuint, GL_TEXTURE_2D, 0, GL_RGBA, surface2.w, surface2.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.generateMipmap

proc size*(tex: Texture2D): Vec2f =
  var w,h: GLint
  glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, w.addr)
  glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2f(w.float32, h.float32)

proc resize*(tex: Texture2D, size: Vec2f) =
  var internalFormat: GLint
  glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, internalFormat.addr)
  glTextureImage2DEXT(tex.GLuint, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0, internalFormat.GLenum, cGL_UNSIGNED_BYTE, nil)

proc createEmptyTexture2D*(size: Vec2f, internalFormat: GLint = GL_RGB) : Texture2D =
  glGenTextures(1, cast[ptr GLuint](result.addr))
  glTextureImage2DEXT(result.GLuint, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0,GL_RGB, cGL_UNSIGNED_BYTE, nil)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)

proc createEmptyDepthTexture2D*(size: Vec2f, internalFormat: GLint = GL_DEPTH_COMPONENT) : Texture2D =
  glGenTextures(1, cast[ptr GLuint](result.addr))
  glTextureImage2DEXT(result.GLuint, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0,GL_DEPTH_COMPONENT, cGL_FLOAT, nil)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)

proc size*(tex: TextureRectangle): Vec2f =
  var w,h: GLint
  glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_TEXTURE_WIDTH, w.addr)
  glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2f(w.float32, h.float32)

proc resize*(tex: TextureRectangle, size: Vec2f) =
  var internalFormat: GLint
  glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_TEXTURE_INTERNAL_FORMAT, internalFormat.addr)
  glTextureImage2DEXT(tex.GLuint, GL_TEXTURE_RECTANGLE, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0, internalFormat.GLenum, cGL_UNSIGNED_BYTE, nil)

proc saveToBmpFile*(tex: Texture2D, filename: string): void =
  let s = tex.size
  var surface = createRGBSurface(0, s.x.int32, s.y.int32, 32, 0xff000000.uint32, 0x00ff0000, 0x0000ff00, 0x000000ff)  # no alpha, rest default
  glGetTextureImageEXT(tex.GLuint, GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface.pixels)
  surface.saveBMP(filename)

proc saveToBmpFile*(tex: TextureRectangle, filename: string): void =
  let s = tex.size
  var surface = createRGBSurface(0, s.x.int32, s.y.int32, 32, 0xff000000.uint32, 0x00ff0000, 0x0000ff00, 0x000000ff)  # no alpha, rest default
  glGetTextureImageEXT(tex.GLuint, GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface.pixels)
  surface.saveBMP(filename)

#### framebuffer ####

type DepthRenderbuffer* = distinct GLuint

proc bindIt*(drb: DepthRenderbuffer): void =
  glBindRenderbuffer(GL_RENDERBUFFER, drb.GLuint)

proc createDepthRenderBuffer*(size: Vec2f) : DepthRenderbuffer =
  glGenRenderbuffers(1, cast[ptr GLuint](result.addr))
  glNamedRenderbufferStorageEXT(result.GLuint, GL_DEPTH_COMPONENT, size.x.GLsizei, size.y.GLsizei)

type FrameBuffer* = distinct GLuint

proc bindIt*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_FRAMEBUFFER, fb.GLuint)

proc bindDraw*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fb.GLuint)

proc bindRead*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_READ_FRAMEBUFFER, fb.GLuint)

proc createFrameBuffer*(): FrameBuffer =
  glGenFramebuffers(1, cast[ptr GLuint](result.addr))

proc drawBuffers*(fb: FrameBuffer, args : varargs[GLenum]) =
  var tmp = newSeq[GLenum](args.len)
  for i, arg in args:
    tmp[i] = arg

  if tmp.len > 0:
    glFramebufferDrawBuffersEXT(fb.GLuint, tmp.len.GLsizei, tmp[0].addr)

# returns a string, and true if it is a sample type
proc glslUniformType(value : NimNode): (string, bool) =
  let tpe = value.getType2
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
  let tpe = value.getType2

  if $tpe[0] == "seq" or $tpe[0] == "ArrayBuffer":
    tpe[1].glslUniformType[0]
  else:
    echo "not a compatible attribType: "
    echo tpe.repr
    "(error not a seq[..])"

#### Uniform ####

proc uniform(location: GLint, mat: Mat4x4[float64]) =
  var mat_float32 = mat4f(mat)
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat_float32.addr))

proc uniform(location: GLint, mat: var Mat4x4[float32]) =
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat.addr))

proc uniform(location: GLint, value: float32) =
  glUniform1f(location, value)

proc uniform(location: GLint, value: float64) =
  glUniform1f(location, value)

proc uniform(location: GLint, value: int32) =
  glUniform1i(location, value)

proc uniform(location: GLint, value: Vec2f) =
  glUniform2f(location, value[0], value[1])

proc uniform(location: GLint, value: Vec3f) =
  glUniform3f(location, value[0], value[1], value[2])

proc uniform(location: GLint, value: Vec4f) =
  glUniform4f(location, value[0], value[1], value[2], value[3])


#### Vertex Array Object ####


type VertexArrayObject* = distinct GLuint

proc newVertexArrayObject*() : VertexArrayObject =
  glGenVertexArrays(1, cast[ptr GLuint](result.addr))

const nil_vao* = VertexArrayObject(0)

proc bindIt*(vao: VertexArrayObject) =
  glBindVertexArray(GLuint(vao))

proc delete*(vao: VertexArrayObject) =
  var raw_vao = GLuint(vao)
  glDeleteVertexArrays(1, raw_vao.addr)


proc divisor(vao: VertexArrayObject, index, divisor: GLuint) : void =
  glVertexArrayVertexBindingDivisorEXT(vao.GLuint, index, divisor)

proc enableAttrib(vao: VertexArrayObject, index: GLuint) : void =
  glEnableVertexArrayAttribEXT(vao.GLuint, index)

#proc divisor(vao: VertexArrayObject, index: GLuint) : GLuint =


template blockBind*(vao: VertexArrayObject, blk: stmt) : stmt =
  vao.bindIt
  blk
  nil_vao.bindIt
  glBufferData(GL_ARRAY_BUFFER, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)

#### Array Buffers ####

type ArrayBuffer*[T]        = distinct GLuint
type ElementArrayBuffer*[T] = distinct GLuint
type UniformBuffer*[T]      = distinct GLuint

type SeqLikeBuffer[T] = ArrayBuffer[T] | ElementArrayBuffer[T]
type AnyBuffer[T] = ArrayBuffer[T] | ElementArrayBuffer[T] | UniformBuffer[T]

proc create*[T](arrayBuffer: var ArrayBuffer[T] ) : void =
  glGenBuffers(1, cast[ptr GLuint](addr(arrayBuffer)))

proc create*[T](arrayBuffer: var ElementArrayBuffer[T] ) : void =
  glGenBuffers(1, cast[ptr GLuint](addr(arrayBuffer)))

proc create*[T](arrayBuffer: var UniformBuffer[T] ) : void =
  glGenBuffers(1, cast[ptr GLuint](addr(arrayBuffer)))

proc createArrayBuffer*[T](len: int, usage: GLenum): ArrayBuffer[T] =
  result.create
  glNamedBufferDataEXT(result.GLuint, len * GLsizeiptr(sizeof(T)), nil, usage)

proc createElementArrayBuffer*[T](len: int, usage: GLenum): ElementArrayBuffer[T] =
  result.create
  glNamedBufferDataEXT(result.GLuint, len * GLsizeiptr(sizeof(T)), nil, usage)

proc createUniformBuffer*[T](usage: GLenum): UniformBuffer[T] =
  result.create
  glNamedBufferDataEXT(result.GLuint, GLsizeiptr(sizeof(T)), nil, usage)

proc currentArrayBuffer*[T](): ArrayBuffer[T] =
  glGetIntegerv(GL_ARRAY_BUFFER_BINDING, cast[ptr GLint](result.addr))

proc currentElementArrayBuffer*[T](): ElementArrayBuffer[T] =
  glGetIntegerv(GL_ELEMENT_ARRAY_BUFFER_BINDING, cast[ptr GLint](result.addr))

proc currentUniformBuffer*[T](): UniformBuffer[T] =
  glGetIntegerv(GL_UNIFORM_BUFFER_BINDING, cast[ptr GLint](result.addr))

proc bindingKind*[T](buffer: ArrayBuffer[T]) : GLenum {. inline .} =
  GL_ARRAY_BUFFER_BINDING

proc bindingKind*[T](buffer: ElementArrayBuffer[T]) : GLenum {. inline .} =
  GL_ELEMENT_ARRAY_BUFFER_BINDING

proc bindingKind*[T](buffer: UniformBuffer[T]) : GLenum {. inline .} =
  GL_UNIFORM_BUFFER_BINDING

proc bufferKind*[T](buffer: ArrayBuffer[T]) : GLenum {. inline .} =
  GL_ARRAY_BUFFER

proc bufferKind*[T](buffer: ElementArrayBuffer[T]) : GLenum {. inline .} =
  GL_ELEMENT_ARRAY_BUFFER

proc bufferKind*[T](buffer: UniformBuffer[T]) : GLenum {. inline .} =
  GL_UNIFORM_BUFFER

proc bindIt*[T](buffer: AnyBuffer[T]) =
  glBindBuffer(buffer.bufferKind, GLuint(buffer))

template bindBlock[T](buffer : AnyBuffer[T], blk:untyped) =
  let buf = buffer
  var outer : GLint
  glGetIntegerv(buf.bindingKind, outer.addr)
  buf.bindIt
  blk
  glBindBuffer(buf.bufferKind, GLuint(outer))

proc bufferData*[T](buffer: SeqLikeBuffer[T], usage: GLenum, data: openarray[T]) =
  if buffer.int > 0:
    glNamedBufferDataEXT(buffer.GLuint, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)

proc bufferData*[T](buffer: UniformBuffer[T], usage: GLenum, data: T) =
  if buffer.int > 0:
    glNamedBufferDataEXT(buffer.GLuint, GLsizeiptr(sizeof(T)), unsafeAddr(data), usage)

proc len*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]) : int =
  var size: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_SIZE, size.addr)
  return size.int div sizeof(T).int

proc arrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW): ArrayBuffer[T] =
  result.create
  result.bufferData(usage, data)

proc elementArrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW): ElementArrayBuffer[T] =
  result.create
  result.bufferData(usage, data)

proc uniformBuffer*[T](data : T, usage: GLenum = GL_STATIC_DRAW): UniformBuffer[T] =
  result.create
  result.bufferData(usage, data)

proc access[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_ACCESS, tmp.addr)
  return tmp.GLenum

proc accessFlags[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_ACCESS_FLAGS, tmp.addr)
  return tmp.GLenum

proc immutableStorage[T](buffer: ArrayBuffer[T]) : bool =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_IMMUTABLE_STORAGE, tmp.addr)
  return tmp != GL_FALSE

proc mapped[T](buffer: ArrayBuffer[T]) : bool =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_MAPPED, tmp.addr)
  return tmp != GL_FALSE

proc mapLength[T](buffer: ArrayBuffer[T]) : int =
  var tmp: pointer
  glGetNamedBufferPointervEXT(buffer.GLuint, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc mapOffset[T](buffer: ArrayBuffer[T]) : int =
  var tmp: pointer
  glGetNamedBufferPointervEXT(buffer.GLuint, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc size[T](buffer: ArrayBuffer[T]) : int =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_MAPPED, tmp.addr)
  return int(tmp)

proc storageFlags[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_STORAGE_FLAGS, tmp.addr)
  return tmp.GLenum

proc usage[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_USAGE, tmp.addr)
  return tmp.GLenum

type
  UncheckedArray {.unchecked.} [t] = array[0,t]

  MappedReadBuffer*[T] = object
    data: ptr UncheckedArray[T]
    size: int

  MappedWriteBuffer*[T] = object
    data: ptr UncheckedArray[T]
    size: int

  MappedReadWriteBuffer*[T] = object
    data: ptr UncheckedArray[T]
    size: int

proc len*(mb : MappedReadBuffer | MappedWriteBuffer | MappedReadWriteBuffer) : int =
  mb.size

proc `[]`*[T](mb : MappedReadBuffer[T], index: int) : T =
  mb.data[index]

proc `[]=`*[T](mb : MappedWriteBuffer[T], index: int, val: T) : void =
  mb.data[index] = val

proc `[]`*[T](mb : MappedReadWriteBuffer[T]; index: int) : var T =
  mb.data[index]

proc `[]=`*[T](mb : MappedReadWriteBuffer[T], index: int, val: T) : void =
  mb.data[index] = val

proc compileTest() =
  var mb = MappedReadWriteBuffer[int](data:nil, size:1)
  let i : int = mb[0] # read
  mb[0] += 17         # modify
  mb[0] = 18          # write(error)


proc unmap*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): bool =
  glUnmapNamedBufferEXT(buffer.GLuint) != GL_FALSE.GLboolean

proc mapRead*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): MappedReadBuffer[T] =
  result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.GLuint, GL_READ_ONLY))
  result.size = buffer.size div sizeof(T)

proc mapWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): MappedWriteBuffer[T] =
  result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.GLuint, GL_WRITE_ONLY))
  result.size = buffer.size div sizeof(T)

proc mapReadWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): MappedReadWriteBuffer[T] =
  result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.GLuint, GL_READ_WRITE))
  result.size = buffer.size div sizeof(T)

template mapReadBufferBlock*(buffer, blck: untyped) : stmt =
  block:
    let mappedBuffer {. inject .} = buffer.mapRead
    blck

  discard buffer.unmap

template mapWriteBufferBlock*(buffer: untyped, blck: untyped) : stmt =
  block:
    let mappedBuffer {. inject .} = buffer.mapWrite
    blck

  discard buffer.unmap

template mapReadWriteBufferBlock*(buffer: untyped, blck: untyped) : stmt =
  block:
    let mappedBuffer {. inject .} = buffer.mapReadWrite
    blck

  discard buffer.unmap


####################################################################################
#### framebuffer ###################################################################
####################################################################################


const currentFramebuffer* = 0

# default fragment Outputs
const fragmentOutputs* = ["color"]

macro declareFramebuffer*(typename,arg:untyped) : untyped =
  typename.expectKind nnkIdent

  result = newStmtList()

  var fragmentOutputs = newSeq[string]()

  var depthType:NimNode = nil
  var depthCreateExpr:NimNode = nil
  var useDepthRenderbuffer = true

  for asgn in arg:
    asgn.expectKind nnkAsgn

    let lhs = asgn[0]
    let rhs = asgn[1]

    if lhs.ident == !"depth":
        echo rhs.treerepr
        rhs.expectKind(nnkCall)
        depthCreateExpr = rhs;

        if rhs[0].ident == !"createDepthRenderBuffer":
          depthType = bindSym"DepthRenderbuffer"
          useDepthRenderbuffer = true
        elif rhs[0].ident == !"createEmptyDepthTexture2D":
          depthType = bindSym"Texture2D"
          useDepthRenderbuffer = false
        else:
          error "expected call to either createDepthRenderBuffer or createEmptyDepthTexture2D"

    else:
      fragmentOutputs.add($asgn[0])

  let recList = newNimNode(nnkRecList)
  recList.add( newExpIdentDef(!"glname", bindSym"FrameBuffer") )
  recList.add( newExpIdentDef(!"depth", depthType) )

  for fragOut in fragmentOutputs:
    recList.add( newExpIdentDef(!fragOut, bindSym"Texture2D") )

  result.add newObjectTy(typename, recList)


  result.add(
    newNimNode2(nnkTemplateDef,
      !!"fragmentOutputSeq",
      newEmptyNode(),
      newEmptyNode(),
      newNimNode2(nnkFormalParams,
        newNimNode2(nnkBracketExpr, bindSym"seq", bindSym"string"),
        newNimNode2(nnkIdentDefs,
          !!"t",
          newNimnode2(nnkBracketExpr,
            bindSym"typedesc",
            typename),
          newEmptyNode()
        )
      ),
      newEmptyNode(),
      newEmptyNode(),
      newNimNode2(nnkStmtList,
        fragmentOutputs.toConstExpr
      )
    )
  )

  #result.add newConstStmt(!!"fragmentOutputs", fragmentOutputs.toConstExpr)

  let branchStmtList = newStmtList()

  branchStmtList.add(newAssignment(newDotExpr(!!"result", !!"glname"),
    newCall(bindSym"createFrameBuffer")
  ))

  branchStmtList.add(newAssignment(newDotExpr(!!"result", !!"depth"),
    depthCreateExpr
  ))

  if useDepthRenderbuffer:
    branchStmtList.add(newCall(bindSym"glNamedFramebufferRenderbufferEXT",
      newDotExpr(!!"result", !!"glname", bindSym"GLuint"), bindSym"GL_DEPTH_ATTACHMENT", bindSym"GL_RENDERBUFFER",
      newDotExpr(!!"result", !!"depth", bindSym"GLuint")
    ))
  else:
    branchStmtList.add(newCall(bindSym"glNamedFramebufferTextureEXT",
      newDotExpr(!!"result", !!"glname", bindSym"GLuint"), bindSym"GL_DEPTH_ATTACHMENT",
      newDotExpr(!!"result", !!"depth", bindSym"GLuint"), newLit(0)
    ))

  let drawBuffersCall = newCall(bindSym"drawBuffers", newDotExpr(!!"result", !!"glname"))

  var i = 0
  for asgn in arg:
    let lhs = asgn[0]
    let rhs = asgn[1]

    if lhs.ident != !"depth":
      let name = $lhs
      branchStmtList.add(newAssignment( newDotExpr( !!"result", !! name ), rhs))

      branchStmtList.add(newCall(bindSym"glNamedFramebufferTextureEXT",
        newDotExpr(!!"result", !!"glname", bindSym"GLuint"), !!("GL_COLOR_ATTACHMENT" & $i),
        newDotExpr(!!"result", !! name, bindSym"GLuint"), newLit(0)
      ))
      drawBuffersCall.add( newCall(bindSym"GLenum", !!("GL_COLOR_ATTACHMENT" & $i)) )
      i += 1

  branchStmtList.add( drawBuffersCall )

  let ifStmt = newNimNode2( nnkIfStmt,
    newNimNode2(nnkElifBranch,
      newInfix( !!"==", newDotExpr( !!"result", !!"glname", !!"int" ), newLit(0) ),
      branchStmtList
    )
  )

  result.add(
    newNimNode2( nnkProcDef,
      !!( join(["create",$typename]) ),
      newEmptyNode(),
      newEmptyNode(),
      newNimNode2( nnkFormalParams,
        typename
      ),
      newEmptyNode(),
      newEmptyNode(),
      branchStmtList
    )
  )

  echo result.repr

  let resizeStmtList = newStmtList()
  resizeStmtList.add( newCall(bindSym"resize", newDotExpr(!!"fb", !!"depth"), !!"newsize") )
  for fragOut in fragmentOutputs:
    resizeStmtList.add( newCall(bindSym"resize", newDotExpr(!!"fb", !!fragOut), !!"newsize") )

  result.add(
    newNimNode2( nnkProcDef,
      !!"resize",
      newEmptyNode(),
      newEmptyNode(),
      newNimNode2( nnkFormalParams,
        bindSym"void",
        newNimNode2(nnkIdentDefs,
          !!"fb",
          typename,
          newEmptyNode()
        ),
        newNimNode2(nnkIdentDefs,
          !!"newsize",
          bindSym"Vec2f",
          newEmptyNode()
        )
      ),
      newEmptyNode(),
      newEmptyNode(),
      resizeStmtList
    )
  )

  result = newCall( bindSym"debugResult", result )

template bindFramebuffer*(name, blok: untyped): untyped =
  var drawfb, readfb: GLint
  glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, drawfb.addr)
  glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, readfb.addr)

  name.glname.bindIt
  block:
    let currentFramebuffer {. inject .} = name
    const fragmentOutputs {.inject.} = name.type.fragmentOutputSeq
    blok

  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, drawfb.GLuint)
  glBindFramebuffer(GL_READ_FRAMEBUFFER, readfb.GLuint)

####################################################################################
#### etc ###########################################################################
####################################################################################

type ShaderParam* = tuple[name: string, gl_type: string]

const
  sourceHeader = """
#version 330
#define M_PI 3.1415926535897932384626433832795
"""

  screenTriagleVertexSource = """
#version 330

const vec4 positions[3] = vec4[](
  vec4(-1.0, -1.0, 1.0, 1.0),
  vec4( 3.0, -1.0, 1.0, 1.0),
  vec4(-1.0,  3.0, 1.0, 1.0)
);

const vec2 texCoords[3] = vec2[](
  vec2(0.0, 0.0),
  vec2(2.0, 0.0),
  vec2(0.0, 2.0)
);

out vec2 texCoord;

void main() {
  gl_Position = positions[gl_VertexID];
  texCoord = texCoords[gl_VertexID];
}
"""

proc `$`(args: openArray[string]) : string =
  result = "["
  for arg in args:
    result.add("\"" & arg & "\",")
  result.add("]")

macro printVar(args: varargs[untyped]) : stmt =

  result = newStmtList()
  for arg in args:
    result.add newCall(bindSym"echo", newLit($arg.ident & ": "), arg)

proc genShaderSource(
    sourceHeader: string,
    uniforms : openArray[string],
    inParams : openArray[string], arrayLength: int,  # for geometry shader, -1 otherwise
    outParams: openArray[string],
    includes: openArray[string], mainSrc: string): string =

  result = sourceHeader

  for i, u in uniforms:
    result.add( u & ";\n" )
  for i, paramRaw in inParams:
    let param = paramRaw.replaceWord("out", "in")
    if arrayLength >= 0:
      result.add format("$1[$2];\n", param, arrayLength)
    else:
      result.add(param & ";\n")
  for param in outParams:
    result.add(param & ";\n")
  for incl in includes:
    result.add incl

  result.add("void main() {\n")
  result.add(mainSrc)
  result.add("\n}\n")


proc forwardVertexShaderSource(sourceHeader: string,
    attribNames, attribTypes : openArray[string] ): string =

  result = sourceHeader
  for i, name in attribNames:
    let tpe = attribTypes[i]
    result.add("in " & tpe & " " & name & ";\n")

  result.add("\nout VertexData {\n")
  for i, name in attribNames:
    let tpe = attribTypes[i]
    result.add(tpe & " " & name & ";\n")
  result.add("} VertexOut;\n")

  result.add("\nvoid main() {\n")
  for name in attribNames:
    result.add("VertexOut." & name & " = " & name & ";\n")
  result.add("}\n")

  echo "forwardVertexShaderSource:\n", result


proc shaderSource(shader: GLuint, source: string) =
  var source_array: array[1, string] = [source]
  var c_source_array = allocCStringArray(source_array)
  defer: deallocCStringArray(c_source_array)
  glShaderSource(shader, 1, c_source_array, nil)

proc compileStatus(shader:GLuint): bool =
  var status: GLint
  glGetShaderiv(shader, GL_COMPILE_STATUS, status.addr)
  status != 0

proc linkStatus(program:GLuint): bool =
  var status: GLint
  glGetProgramiv(program, GL_LINK_STATUS, status.addr)
  status != 0

proc shaderInfoLog(shader: GLuint): string =
  var length: GLint = 0
  glGetShaderiv(shader, GL_INFO_LOG_LENGTH, length.addr)
  result = newString(length.int)
  glGetShaderInfoLog(shader, length, nil, result)

proc showError(log: string, source: string): void =
  let lines = source.splitLines
  for match in log.findIter(re"(\d+)\((\d+)\).*"):
    let line_nr = match.captures[1].parseInt;
    echo lines[line_nr - 1]
    echo match.match

proc programInfoLog(program: GLuint): string =
  var length: GLint = 0
  glGetProgramiv(program, GL_INFO_LOG_LENGTH, length.addr);
  result = newString(length.int)
  glGetProgramInfoLog(program, length, nil, result);

proc compileShader(shaderType: GLenum, source: string): GLuint =
  result = glCreateShader(shaderType)
  result.shaderSource(source)
  glCompileShader(result)

  if not result.compileStatus:
    echo "==== start Shader Problems ======================================="
    echo source
    echo "------------------------------------------------------------------"
    showError(result.shaderInfoLog, source)
    echo "==== end Shader Problems ========================================="

proc linkShader(shaders: varargs[GLuint]): GLuint =
  result = glCreateProgram()

  for shader in shaders:
    glAttachShader(result, shader)
    glDeleteShader(shader)
  glLinkProgram(result)

  if not result.linkStatus:
    echo "Log: ", result.programInfoLog
    glDeleteProgram(result)
    result = 0

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

proc makeAndBindBuffer[T](buffer: var ArrayBuffer[T], index: GLint) =
  if index >= 0:
    buffer.create
    buffer.bindIt
    glVertexAttribPointer(index.GLuint, attribSize(T), attribType(T), attribNormalized(T), 0, nil)

proc bindAndAttribPointer[T](buffer: ArrayBuffer[T], index: GLint) =
  if index >= 0:
    buffer.bindIt
    glVertexAttribPointer(index.GLuint, attribSize(T), attribType(T), attribNormalized(T), 0, nil)

proc makeAndBindElementBuffer[T](buffer: var ElementArraybuffer[T]) =
  buffer.create
  buffer.bindIt

proc myEnableVertexAttribArray(vao: VertexArrayObject, index: GLint, divisorval: GLuint): void =
  if index >= 0:
    vao.enableAttrib(index.GLuint)
    vao.divisor(index.GLuint, divisorval)

template renderBlockTemplate(numLocations: int, globalsBlock, linkShaderBlock, bufferCreationBlock,
               initUniformsBlock, setUniformsBlock, drawCommand: expr): stmt {. dirty .} =
  block:
    var vao {.global.}: VertexArrayObject
    var glProgram {.global.}: GLuint  = 0
    var locations {.global.}: array[numLocations, GLint]

    globalsBlock

    if glProgram == 0:

      gl_program = linkShaderBlock
      glUseProgram(gl_program)

      initUniformsBlock

      vao = newVertexArrayObject()
      vao.bindIt

      bufferCreationBlock

      glBindBuffer(GL_ARRAY_BUFFER, 0)

      nil_vao.bindIt

      glUseProgram(0)

      #for i, loc in locations:
      #  echo "location(", i, "): ", loc

    glUseProgram(gl_program)

    bindIt(vao)

    setUniformsBlock

    drawCommand

    bindIt(nil_vao)
    glUseProgram(0);

################################################################################
## Shading Dsl #################################################################
################################################################################

proc attribute[T](name: string, value: T, divisor: GLuint) : int = 0
proc attributes(args : varargs[int]) : int = 0
proc shaderArg[T](name: string, value: T): int = 0
proc uniforms(args: varargs[int]): int = 0
proc vertexOut(args: varargs[string]): int = 0
proc geometryOut(args: varargs[string]): int = 0
proc fragmentOut(args: varargs[string]): int = 0
proc vertexMain(src: string): int = 0
proc fragmentMain(src: string): int = 0
proc geometryMain(layout, src: string): int = 0
proc includes(args: varargs[int]): int = 0
proc incl(arg: string): int = 0
proc numVertices(num: GLSizei): int = 0
proc numInstances(num: GLSizei): int = 0

################################################################################
## Shading Dsl Inner ###########################################################
################################################################################

macro shadingDslInner(mode: GLenum, fragmentOutputs: static[openArray[string]], statement: varargs[int] ) : stmt =

  var numSamplers = 0
  var numLocations = 0
  var uniformsSection : seq[string] = @[]
  var initUniformsBlock = newStmtList()
  var setUniformsBlock = newStmtList()
  var attribNames = newSeq[string](0)
  var attribTypes = newSeq[string](0)
  #var attributesSection : seq[object(name:string, tpe: string)] = @[]
  var globalsBlock = newStmtList()
  var bufferCreationBlock = newStmtList()
  var vertexOutSection : seq[string] = @[]
  var geometryOutSection : seq[string] = @[]
  var fragmentOutSection : seq[string] = @[]
  for i,fragout in fragmentOutputs:
    fragmentOutSection.add format("layout(location = $1) out vec4 $2", $i, fragout)
  var includesSection : seq[string] = @[]
  var vertexMain: string
  var geometryLayout: string
  var geometryMain: string
  var fragmentMain: string
  var hasIndices = false
  var indexType: NimNode = nil
  var numVertices, numInstances: NimNode
  var hasInstanceData = false

  #### BEGIN PARSE TREE ####

  proc locations(i: int) : NimNode =
    newTree(nnkBracketExpr, !!"locations", newLit(i))

  for call in statement.items:
    call.expectKind nnkCall
    case $call[0]
    of "numVertices":
      numVertices = call[1]

    of "numInstances":
      numInstances = call[1]

    of "uniforms":
      for innerCall in call[1][1].items:
        innerCall[1].expectKind nnkStrLit
        let name = $innerCall[1]
        let value = innerCall[2]


        let (glslType, isSample) = value.glslUniformType
        let baseString = "uniform " & glslType & " " & name

        initUniformsBlock.add( newAssignment(
          locations(numLocations),
          newCall( bindSym"glGetUniformLocation", !!"glProgram", newLit(name) )
        ))

        if isSample:
          initUniformsBlock.add( newCall( bindSym"glUniform1i", locations(numLocations), newLit(numSamplers) ) )

          proc activeTexture(texture: int): void =
            glActiveTexture( (GL_TEXTURE0 + texture).GLenum )

          setUniformsBlock.add( newCall( bindSym"activeTexture", newLit(numSamplers) ) )
          setUniformsBlock.add( newCall( bindSym"bindIt", value ) )
          numSamplers += 1
        else:
          setUniformsBlock.add( newCall( bindSym"uniform", locations(numLocations), value ) )

        uniformsSection.add( baseString )

        numLocations += 1

    of "attributes":
      for innerCall in call[1][1].items:
        innerCall[1].expectKind nnkStrLit
        let name = $innerCall[1]
        let value = innerCall[2]
        let divisor: int =
          if innerCall[3].kind == nnkHiddenStdConv:
            innerCall[3][1].intVal.int
          elif innerCall[3].kind == nnkIntLit:
            innerCall[3].intVal.int
          else:
            0

        let buffername = !(name & "Buffer")

        let isAttrib = name != "indices"
        #echo "attribute ", value.glslAttribType, " ", name

        if not isAttrib:
          if hasIndices:
            echo "error, has already indices"

          hasIndices = true

          case value.getType2[1].typeKind
          of ntyInt8, ntyUInt8:
            indexType = bindSym"GL_UNSIGNED_BYTE"
          of ntyInt16, ntyUInt16:
            indexType = bindSym"GL_UNSIGNED_SHORT"
          of ntyInt32, ntyUInt32:
            indexType = bindSym"GL_UNSIGNED_INT"
          of ntyInt, ntyUInt:
            echo "error int type has to be explicity sized uint8 uint16 or uint32"
          of ntyInt64, ntyUInt64:
            echo "error 64 bit indices not supported"
          else:
            echo "error unknown type kind: ", value.getType2[1].typeKind


        template foobarTemplate( lhs, rhs, bufferType : expr ) : stmt {.dirty.} =
          var lhs {.global.}: bufferType[rhs[0].type]

        let isSeq:bool = $value.getType2[0] == "seq"

        if isSeq:
          let bufferType =
            if isAttrib:
              bindSym"ArrayBuffer"
            else:
              bindSym"ElementArrayBuffer"

          globalsBlock.add(getAst(foobarTemplate( !! buffername, value, bufferType )))

        let attribCount = attribNames.len

        if isAttrib:
          bufferCreationBlock.add( newAssignment(
            locations(numLocations),
            newCall( bindSym"glGetAttribLocation", !! "glProgram", newLit(name) )
          ))

          bufferCreationBlock.add(newCall(bindSym"myEnableVertexAttribArray", !!"vao", locations(numLocations), newLit(divisor)))

        if isSeq:
          if isAttrib:
            bufferCreationBlock.add(newCall(bindSym"makeAndBindBuffer",
              !! buffername,
              locations(numLocations),
            ))

          else:
            bufferCreationBlock.add(newCall(bindSym"makeAndBindElementBuffer",
              !! buffername,
            ))

          setUniformsBlock.add(newCall(bindSym"bindIt", !! buffername))
          setUniformsBlock.add(newCall(bindSym"bufferData", !! buffername, bindSym"GL_STREAM_DRAW", value))

        else:
          if isAttrib:
            bufferCreationBlock.add(newCall(bindSym"bindAndAttribPointer",
              value,
              locations(numLocations),
            ))
          else:
            bufferCreationBlock.add(newCall(bindSym"bindIt", value))

        if isAttrib:
          attribNames.add( name )
          attribTypes.add( value.glslAttribType )
          # format("in $1 $2", value.glslAttribType, name) )
          numLocations += 1

    of "vertexOut":
      #echo "vertexOut"

      for innerCall in call[1][1].items:
        vertexOutSection.add( innerCall.strVal )

    of "geometryOut":

      for innerCall in call[1][1].items:
        geometryOutSection.add( innerCall.strVal )

    of "fragmentOut":

      fragmentOutSection = @[]
      for innerCall in call[1][1].items:
        fragmentOutSection.add( innerCall.strVal )

    of "includes":

      for innerCall in call[1][1].items:
        if innerCall[1].kind == nnkSym:
          let sym = innerCall[1].symbol
          includesSection.add(sym.getImpl.strVal)


    of "vertexMain":
      vertexMain = call[1].strVal

    of "fragmentMain":
      fragmentMain = call[1].strVal

    of "geometryMain":

      geometryLayout = call[1].strVal
      geometryMain = call[2].strVal

    else:
      echo "unknownSection"
      echo call.repr


  if hasIndices and indexType == nil:
    error "has indices, but index Type was never set to anything"

  var vertexShaderSource : string

  if vertexMain == nil and geometryMain == nil:

    if vertexOutSection.len > 0:
      error("cannot create implicit screen space quad renderer with vertex out section")

    vertexShaderSource = screenTriagleVertexSource
    vertexOutSection.add("out vec2 texCoord")


  elif vertexMain == nil:
    vertexShaderSource = forwardVertexShaderSource(sourceHeader, attribNames, attribTypes)


    vertexOutSection.newSeq(attribNames.len)
    for i in 0..<attribNames.len:
       vertexOutSection[i] = format("out $1 $2", attribTypes[i], attribNames[i])

  else:
    var attributesSection = newSeq[string](attribNames.len)
    for i in 0..<attribNames.len:
       attributesSection[i] = format("in $1 $2", attribTypes[i], attribNames[i])

    vertexShaderSource = genShaderSource(sourceHeader, uniformsSection, attributesSection, -1, vertexOutSection, includesSection, vertexMain)

  var linkShaderBlock : NimNode

  if geometryMain == nil:

    let fragmentShaderSource = genShaderSource(sourceHeader, uniformsSection, vertexOutSection, -1, fragmentOutSection, includesSection, fragmentMain)

    linkShaderBlock = newCall( bindSym"linkShader",
      newCall( bindSym"compileShader", bindSym"GL_VERTEX_SHADER", newLit(vertexShaderSource) ),
      newCall( bindSym"compileShader", bindSym"GL_FRAGMENT_SHADER", newLit(fragmentShaderSource) ),
    )

  else:
    let geometryHeader = format("$1\nlayout($2) in;\n$3;\n", sourceHeader, geometryPrimitiveLayout(mode.intVal.GLenum), geometryLayout)
    let geometryShaderSource = genShaderSource(geometryHeader, uniformsSection, vertexOutSection, geometryNumVerts(mode.intVal.GLenum), geometryOutSection, includesSection, geometryMain)
    let fragmentShaderSource = genShaderSource(sourceHeader, uniformsSection, geometryOutSection, -1, fragmentOutSection, includesSection, fragmentMain)

    linkShaderBlock = newCall( bindSym"linkShader",
      newCall( bindSym"compileShader", bindSym"GL_VERTEX_SHADER", newLit(vertexShaderSource) ),
      newCall( bindSym"compileShader", bindSym"GL_GEOMETRY_SHADER", newLit(geometryShaderSource) ),
      newCall( bindSym"compileShader", bindSym"GL_FRAGMENT_SHADER", newLit(fragmentShaderSource) ),
    )



  let drawCommand =
    if hasIndices:
      if numInstances != nil:
        newCall( bindSym"glDrawElementsInstanced", mode, numVertices, indexType, newNilLit(), numInstances )
      else:
        newCall( bindSym"glDrawElements", mode, numVertices, indexType, newNilLit() )

    else:
      if numInstances != nil:
        newCall( bindSym"glDrawArraysInstanced", mode, newLit(0), numVertices, numInstances )
      else:
        newCall( bindSym"glDrawArrays", mode, newLit(0), numVertices )

  result = getAst(renderBlockTemplate(numLocations, globalsBlock, linkShaderBlock,
         bufferCreationBlock, initUniformsBlock, setUniformsBlock, drawCommand))

  # result = newCall( bindSym"debugResult", result )

################################################################################
## Shading Dsl Outer ###########################################################
################################################################################

macro shadingDsl*(mode:GLenum, statement: stmt) : stmt {.immediate.} =

  result = newCall(bindSym"shadingDslInner", mode, !! "fragmentOutputs" )
  # numVertices = result[2]
  # numInstances = result[3]

  for section in statement.items:
    section.expectKind({nnkCall, nnkAsgn})

    if section.kind == nnkAsgn:
      section.expectLen(2)
      let ident = section[0]
      ident.expectKind nnkIdent
      case $ident.ident
      of "numVertices":
        result.add( newCall(bindSym"numVertices", section[1] ) )
      of "numInstances":
        result.add( newCall(bindSym"numInstances", section[1] ) )
      else:
        error("unknown named parameter " & $ident.ident)

    elif section.kind == nnkCall:
      let ident = section[0]
      ident.expectKind nnkIdent
      let stmtList = section[1]
      stmtList.expectKind nnkStmtList

      case $ident
      of "uniforms":
        let uniformsCall = newCall(bindSym"uniforms")

        for capture in stmtList.items:
          capture.expectKind({nnkAsgn, nnkIdent})
          if capture.kind == nnkAsgn:
            capture.expectLen 2
            capture[0].expectKind nnkIdent
            uniformsCall.add( newCall(bindSym"shaderArg", newLit($capture[0]), capture[1] ) )
          elif capture.kind == nnkIdent:
            uniformsCall.add( newCall(bindSym"shaderArg",  newLit($capture), capture) )

        result.add(uniformsCall)

      of "attributes":
        let attributesCall = newCall(bindSym"attributes")

        proc handleCapture(attributesCall, capture: NimNode, divisor: int) =
          capture.expectKind({nnkAsgn, nnkIdent})
          if capture.kind == nnkAsgn:
            capture.expectLen 2
            capture[0].expectKind nnkIdent
            attributesCall.add( newCall(bindSym"attribute", newLit($capture[0]), capture[1], newLit(divisor) ) )
          elif capture.kind == nnkIdent:
            attributesCall.add( newCall(bindSym"attribute",  newLit($capture), capture, newLit(divisor)) )


        for capture in stmtList.items:
          if capture.kind == nnkCall:
            if $capture[0] == "instanceData":
              let stmtList = capture[1]
              stmtList.expectKind nnkStmtList
              for capture in stmtList.items:
                handleCapture(attributesCall, capture, 1)

            else:
              echo "error expected call to instanceData, but got: ", capture.repr
          else:
            handleCapture(attributesCall, capture, 0)

        result.add(attributesCall)

      of "vertexOut", "geometryOut", "fragmentOut":

        let outCall =
          case $ident
          of "vertexOut": newCall(bindSym"vertexOut")
          of "geometryOut": newCall(bindSym"geometryOut")
          of "fragmentOut": newCall(bindSym"fragmentOut")
          else: nil

        for section in stmtList.items:
          section.expectKind({nnkVarSection, nnkStrLit, nnkTripleStrLit})
          case section.kind
          of nnkVarSection:
            for def in section:
              def.expectKind nnkIdentDefs
              def[0].expectKind nnkIdent
              def[1].expectKind nnkIdent
              outCall.add format("out $2 $1", $def[0], $def[1]).newLit
          of nnkStrLit:
            outCall.add section
          of nnkTripleStrLit:
            for line in section.strVal.splitLines:
              outCall.add line.strip.newLit
          else:
            error("unreachable")


        result.add(outCall)

      of "vertexMain":
        stmtList.expectLen(1)
        stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
        result.add( newCall(bindSym"vertexMain", stmtList[0]) )

      of "geometryMain":
        stmtList.expectLen(2)
        stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
        stmtList[1].expectKind({nnkTripleStrLit, nnkStrLit})
        result.add( newCall(bindSym"geometryMain", stmtList[0], stmtList[1]) )

      of "fragmentMain":
        stmtList.expectLen(1)
        stmtList[0].expectKind({ nnkTripleStrLit, nnkStrLit })
        result.add( newCall(bindSym"fragmentMain", stmtList[0]) )

      of "includes":
        let includesCall = newCall(bindSym"includes")

        for statement in stmtList:
          statement.expectKind( nnkIdent )

          includesCall.add( newCall(bindSym"incl", statement) )

        result.add(includesCall)
      else:
        error("unknown section " & $ident.ident)
