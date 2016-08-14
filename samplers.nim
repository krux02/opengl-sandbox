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
    when false:
      glTextureParameteriEXT(texture.GLuint, target, pname, param)
    else:
      var prevTexture:GLint
      glGetIntegerv(GL_ACTIVE_TEXTURE, prevTexture.addr)
      glBindTexture(target, texture.GLuint)
      glTexParameteri(target, pname, param)
      glBindTexture(target, prevTexture.GLuint)




  proc generateMipmap*(texture: name): void =
    when false:
      glGenerateTextureMipmapEXT(texture.GLuint, target)
    else:
      var prevTexture:GLint
      glGetIntegerv(GL_ACTIVE_TEXTURE, prevTexture.addr)
      glBindTexture(target, texture.GLuint)
      glGenerateMipmap(target)
      glBindTexture(target, prevTexture.GLuint)

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
  when false:
    glTextureImage2DEXT(result.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_RGBA.GLint, surface2.w, surface2.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  else:
    glTextureStorage2D(result.GLuint, 1, GL_RGBA, surface2.w, surface2.h)
    glTextureSubImage2D(result.GLuint, 0, 0, 0, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)

proc texture2D*(surface: sdl2.SurfacePtr): Texture2D =
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)
  glGenTextures(1, cast[ptr GLuint](result.addr))
  when false:
    glTextureImage2DEXT(result.GLuint, GL_TEXTURE_2D, 0, GL_RGBA.GLint, surface2.w, surface2.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  else:
    glTextureStorage2D(result.GLuint, 1, GL_RGBA, surface2.w, surface2.h)
    glTextureSubImage2D(result.GLuint, 0, 0, 0, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.generateMipmap

proc textureRectangle*(surface: sdl2.SurfacePtr): TextureRectangle =
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)
  glGenTextures(1, cast[ptr GLuint](result.addr))
  when false:
    glTextureImage2DEXT(result.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_RGBA.GLint, surface2.w, surface2.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  else:
    glTextureStorage2D(result.GLuint, 1, GL_RGBA, surface2.w, surface2.h)
    glTextureSubImage2D(result.GLuint, 0, 0, 0, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)


proc textureRectangle*(size: Vec2i; internalFormat : GLint = GL_RGBA.GLint): TextureRectangle =
  glGenTextures(1, cast[ptr GLuint](result.addr))
  when false:
    glTextureImage2DEXT(result.GLuint, GL_TEXTURE_RECTANGLE, 0, internalFormat, size.x, size.y, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, nil)
  else:
    glTextureStorage2D(result.GLuint, 1, internalFormat.GLenum, size.x.GLsizei, size.y.GLsizei)
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)


proc subImage*( texture : TextureRectangle; data : var seq[Mat4f] ) =
  when false:
    glTextureSubImage2DEXT(texture.GLuint, GL_TEXTURE_RECTANGLE, 0, 0, 0, 4, data.len.GLsizei, GL_RGBA, cGL_FLOAT, data[0].addr.pointer )
  else:
    glTextureSubImage2D(texture.GLuint, 0, 0, 0, 4, data.len.GLsizei, GL_RGBA, cGL_FLOAT, data[0].addr.pointer )


proc loadTexture2DFromFile*(filename: string): Texture2D =
  let surface = image.load(filename)
  if surface.isNil:
    echo "can't load texture ", filename, ": ", sdl2.getError()
    return nilTexture2D

  defer: freeSurface(surface)
  texture2D(surface)

proc size*(tex: Texture2D): Vec2f =
  var w,h: GLint
  glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, w.addr)
  glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2f(w.float32, h.float32)

proc resize*(tex: Texture2D, size: Vec2f) =
  var internalFormat: GLint
  glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, internalFormat.addr)
  when false:
    glTextureImage2DEXT(tex.GLuint, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0, internalFormat.GLenum, cGL_UNSIGNED_BYTE, nil)
  else:
    glTextureStorage2D(tex.GLuint, 1, internalFormat.GLenum, size.x.GLsizei, size.y.GLsizei)
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)

proc createEmptyTexture2D*(size: Vec2f, internalFormat: GLint = GL_RGB.GLint) : Texture2D =
  glGenTextures(1, cast[ptr GLuint](result.addr))
  when false:
    glTextureImage2DEXT(result.GLuint, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0,GL_RGB, cGL_UNSIGNED_BYTE, nil)
  else:
    glTextureStorage2D(result.GLuint, 1, internalFormat.GLenum, size.x.GLsizei, size.y.GLsizei)
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)

proc createEmptyDepthTexture2D*(size: Vec2f, internalFormat: GLint = GL_DEPTH_COMPONENT.GLint) : Texture2D =
  glGenTextures(1, cast[ptr GLuint](result.addr))
  when false:
    glTextureImage2DEXT(result.GLuint, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0,GL_DEPTH_COMPONENT, cGL_FLOAT, nil)
  else:
    glTextureStorage2D(result.GLuint, 1, internalFormat.GLenum, size.x.GLsizei, size.y.GLsizei)
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)

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
  when false:
    glTextureImage2DEXT(tex.GLuint, GL_TEXTURE_RECTANGLE, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0, internalFormat.GLenum, cGL_UNSIGNED_BYTE, nil)
  else:
    glTextureStorage2D(tex.GLuint, 1, internalFormat.GLenum, size.x.GLsizei, size.y.GLsizei)
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)


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
