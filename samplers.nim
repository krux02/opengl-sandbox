################################################################################
#### Sampler Types #############################################################
################################################################################


#macro nilName(name:untyped) : untyped =
#  name.expectKind(nnkIdent)
#  !!("nil_" & $name)

macro createTextureMacro(name, target: untyped): untyped =
  let procName = newIdentNode("create" & $name.ident)
  result = quote do:
    proc `procName`*() : `name` =
      var id : GLuint
      glCreateTextures(`target`, 1,  id.addr)
      `name`(handle: id)
  
template textureTypeTemplate(name, nilName, target:untyped, shadername:string): untyped =
  type
    name* = object
      handle: GLuint
  #const nilName* = name(0)
  proc `$`*(texture: name): string =
    $texture.handle

  createTextureMacro(name, target)

  proc isValid*(texture: name): bool =
    texture.handle.int > 0 and glIsTexture(texture.handle)
    

  proc bindToActiveUnit*(texture: name): void =
    glBindTexture(target, texture.handle)

  proc bindToUnit*(texture: name, unit: int): void =
    glBindTextureUnit(unit.GLuint, texture.handle)

  proc parameter*(texture: name, pname: GLenum, param: GLint): void =
    when false:
      glTextureParameteriEXT(texture.handle, target, pname, param)
    else:
      glTextureParameteri(texture.handle, pname, param)

  proc generateMipmap*(texture: name): void =
    when false:
      glGenerateTextureMipmapEXT(texture.handle, target)
    else:
      glGenerateTextureMipmap(texture.handle)

  proc delete*(texture: name): void =
    var id = texture.handle
    glDeleteTextures(1, id.addr)

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
    glTextureStorage2D(result.handle, 1, GL_RGBA, surface2.w, surface2.h)
    glTextureSubImage2D(result.handle, 0, 0, 0, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)


proc texture2D*(surface: sdl2.SurfacePtr): Texture2D =
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)


  when false:
    glGenTextures(1, cast[ptr GLuint](result.addr))
    glTextureImage2DEXT(result.GLuint, GL_TEXTURE_2D, 0, GL_RGBA.GLint, surface2.w, surface2.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  else:
    glCreateTextures(GL_TEXTURE_2D, 1, cast[ptr GLuint](result.addr))
    glTextureStorage2D(result.handle, 1, GL_RGBA8, surface2.w, surface2.h)
    glTextureSubImage2D(result.handle, 0, 0, 0, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.generateMipmap

proc textureRectangle*(surface: sdl2.SurfacePtr): TextureRectangle =
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)

  when false:
    glGenTextures(1, cast[ptr GLuint](result.addr))
    glTextureImage2DEXT(result.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_RGBA.GLint, surface2.w, surface2.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  else:
    glCreateTextures(GL_TEXTURE_RECTANGLE, 1, cast[ptr GLuint](result.addr))
    glTextureStorage2D(result.handle, 1, GL_RGBA8, surface2.w, surface2.h)
    glTextureSubImage2D(result.handle, 0, 0, 0, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)


proc texture1D*(size: int, internalFormat: GLenum = GL_RGBA8): Texture1D =
  when false:
    discard
  else:
    glCreateTextures(GL_TEXTURE_1D, 1, cast[ptr GLuint](result.addr))
    glTextureStorage1D(result.handle, 1, internalFormat, size.GLsizei)

proc setDataRGBA*( texture: Texture1D, data: seq[float32]) =
  when false:
    discard
  else:
    glTextureSubImage1D(texture.handle, 0, 0, data.len.GLsizei div 4, GL_RGBA, cGL_FLOAT, data[0].unsafeAddr)

proc setData*( texture: Texture1D, data: seq[float32]) =
  when false:
    discard
  else:
    glTextureSubImage1D(texture.handle, 0, 0, data.len.GLsizei, GL_RED, cGL_FLOAT, data[0].unsafeAddr)

proc setData*( texture: Texture1D, data: seq[Vec4u8]) =
  when false:
    discard
  else:
    glTextureSubImage1D(texture.handle, 0, 0, data.len.GLsizei, GL_RGBA, GL_UNSIGNED_BYTE, data[0].unsafeAddr)

    
proc textureRectangle*(size: Vec2i; internalFormat : GLenum = GL_RGBA8): TextureRectangle =
  when false:
    glGenTextures(1, result.handle.addr)
    glTextureImage2DEXT(result.handle, GL_TEXTURE_RECTANGLE, 0, internalFormat, size.x, size.y, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, nil)
  else:
    glCreateTextures(GL_TEXTURE_RECTANGLE, 1, result.handle.addr)
    glTextureStorage2D(result.handle, 1, internalFormat, size.x.GLsizei, size.y.GLsizei)
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)


proc subImage*( texture : TextureRectangle; data : var seq[Mat4f] ) =
  when false:
    glTextureSubImage2DEXT(texture.handle, GL_TEXTURE_RECTANGLE, 0, 0, 0, 4, data.len.GLsizei, GL_RGBA, cGL_FLOAT, data[0].addr.pointer )
  else:
    glTextureSubImage2D(texture.handle, 0, 0, 0, 4, data.len.GLsizei, GL_RGBA, cGL_FLOAT, data[0].addr.pointer )


proc loadTexture2DFromFile*(filename: string): Texture2D =
  let surface = image.load(filename)
  if surface.isNil:
    echo "can't load texture ", filename, ": ", sdl2.getError()
    return Texture2D(handle:0)

  defer: freeSurface(surface)
  texture2D(surface)

proc size*(tex: Texture2D): Vec2f =
  var w,h: GLint
  when false:
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, w.addr)
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, h.addr)
  else:
    glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
    glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2f(w.float32, h.float32)

proc saveToBmpFile*(tex: Texture2D, filename: string): void =
  let s = tex.size
  var surface = createRGBSurface(0, s.x.int32, s.y.int32, 32, 0xff000000.uint32, 0x00ff0000, 0x0000ff00, 0x000000ff)  # no alpha, rest default
  when false:
    glGetTextureImageEXT(tex.handle, GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface.pixels)
  else:
    let bufferSize = GLsizei(s.x * s.y * 4)
    glGetTextureImage(tex.handle, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, bufferSize, surface.pixels)
  surface.saveBMP(filename)

when false:
  # ARB_direct_state_access textures have immutable size :/
  proc resize*(tex: Texture2D, size: Vec2f) =
    var internalFormat: GLint
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, internalFormat.addr)
    glTextureImage2DEXT(tex.handle, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0, internalFormat.GLenum, cGL_UNSIGNED_BYTE, nil)

  proc resize*(tex: TextureRectangle, size: Vec2f) =
    var internalFormat: GLint
    glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_TEXTURE_INTERNAL_FORMAT, internalFormat.addr)
    glTextureImage2DEXT(tex.handle, GL_TEXTURE_RECTANGLE, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0, internalFormat.GLenum, cGL_UNSIGNED_BYTE, nil)

proc createEmptyTexture2D*(size: Vec2f, internalFormat: GLenum = GL_RGB8) : Texture2D =
  when false:
    glGenTextures(1, cast[ptr GLuint](result.addr))
    glTextureImage2DEXT(result.handle, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0,GL_RGB, cGL_UNSIGNED_BYTE, nil)
  else:
    glCreateTextures(GL_TEXTURE_2D, 1, cast[ptr GLuint](result.addr))
    glTextureStorage2D(
      result.handle, 1,
      internalFormat,
      size.x.GLsizei, size.y.GLsizei
    )
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)

proc createEmptyDepthTexture2D*(size: Vec2f, internalFormat: GLenum = GL_DEPTH_COMPONENT24) : Texture2D =
  when false:
    glGenTextures(1, result.handle.addr)
    glTextureImage2DEXT(result.handle, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0,GL_DEPTH_COMPONENT, cGL_FLOAT, nil)
  else:
    glCreateTextures(GL_TEXTURE_2D, 1, result.handle.addr)
    glTextureStorage2D(result.handle, 1, internalFormat, size.x.GLsizei, size.y.GLsizei)
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)

  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)

proc size*(tex: TextureRectangle): Vec2f =
  var w,h: GLint
  when false:
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_RECTANGLE, 0, GL_TEXTURE_WIDTH, w.addr)
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_RECTANGLE, 0, GL_TEXTURE_HEIGHT, h.addr)
  else:
    glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
    glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2f(w.float32, h.float32)

proc saveToBmpFile*(tex: TextureRectangle, filename: string): void =
  let s = tex.size
  var surface = createRGBSurface(0, s.x.int32, s.y.int32, 32, 0xff000000.uint32, 0x00ff0000, 0x0000ff00, 0x000000ff)  # no alpha, rest default
  when false:
    glGetTextureImageEXT(tex.handle, GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface.pixels)
  else:
    let bufferSize = GLsizei(s.x * s.y * 4)
    glGetTextureImage(tex.handle, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, bufferSize, surface.pixels)
  surface.saveBMP(filename)
