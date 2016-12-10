# included from fancygl.nim

################################################################################
#### Sampler Types #############################################################
################################################################################

#[
macro createTextureMacro(name, target: untyped): untyped =
  let procName = newIdentNode("new" & $name.ident)
  result = quote do:
    proc `procName`*() : `name` =
      var id : GLuint
      glCreateTextures(`target`, 1,  id.addr)
      `name`(handle: id)
]#

template textureTypeTemplate(name, nilName, target:untyped, shadername:string): untyped =
  type
    name* = object
      handle*: GLuint
  #const nilName* = name(0)
  proc `$`*(texture: name): string =
    $texture.handle

  #createTextureMacro(name, target)

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

proc unbindTextures(first,count: int): void =
  when true:
    glBindTextures(first.GLuint, count.GLsizei, nil)
  else:
    for textureUnit in first ..< first + count:
      glBindTextureUnit(GLuint(textureUnit), 0);

proc bindTextures(first: int; handles: openarray[GLuint]): void =
  when true:
    glBindTextures(first.GLuint, handles.len.GLsizei, handles[0].unsafeaddr)
  else:
    for i in 0 ..< handles.len:
      let textureUnit = GLuint(first + i)
      let texture: GLuint = handles[i]
      glBindTextureUnit(textureUnit, texture)
    
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

proc loadSurfaceFromFile*(filename: string): SurfacePtr =
  let surface = image.load(filename)
  defer: freeSurface(surface)
  result = surface.convertSurfaceFormat(SDL_PIXELFORMAT_RGBA8888, 0)

proc loadTextureRectangleFromFile*(filename: string): TextureRectangle =
  let surface = loadSurfaceFromFile(filename)
  defer: freeSurface(surface)
  glGenTextures(1, cast[ptr GLuint](result.addr))
  when false:
    glTextureImage2DEXT(result.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_RGBA.GLint, surface.w, surface.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface.pixels)
  else:
    let levels = min(surface.w, surface.h).float32.log2.floor.GLint
    glTextureStorage2D(result.handle, levels, GL_RGBA, surface.w, surface.h)
    glTextureSubImage2D(result.handle, 0, 0, 0, surface.w, surface.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface.pixels)
    glGenerateTextureMipmap(result.handle)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)


proc subImage*(this: Texture2D; surface: sdl2.SurfacePtr; pos: Vec2i = vec2i(0); level: int = 0): void =
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)

  glTextureSubImage2D(this.handle, level.GLint, pos.x, pos.y, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

  
proc texture2D*(surface: sdl2.SurfacePtr): Texture2D =
  glCreateTextures(GL_TEXTURE_2D, 1, cast[ptr GLuint](result.addr))
  let levels = min(surface.w, surface.h).float32.log2.floor.GLint
  glTextureStorage2D(result.handle, levels, GL_RGBA8, surface.w, surface.h)
  result.subImage(surface)
  glGenerateTextureMipmap(result.handle)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.generateMipmap
  
proc texture2D*(size: Vec2i, internalFormat: GLenum = GL_RGBA8): Texture2D =
  when false:
    discard
  else:
    glCreateTextures(GL_TEXTURE_2D, 1, result.handle.addr)
    let levels = GLint(floor(log2(float32(min(size.x, size.y)))))
    glTextureStorage2D(result.handle, levels, internalFormat, size.x.GLsizei, size.y.GLsizei)

proc size*(tex: Texture2D): Vec2i =
  var w,h: GLint
  when false:
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, w.addr)
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, h.addr)
  else:
    glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
    glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2i(w, h)
    
proc setData*(texture: Texture2D, data: seq[float32], level: int = 0) =
  let
    s = texture.size
    w = s.x.GLint
    h = s.y.GLint
    l = level.GLint
  glTextureSubImage2D(texture.handle, l, 0, 0, w, h, GL_RED, cGL_FLOAT, data[0].unsafeAddr)

proc setData*(texture: Texture2D; data: seq[Vec4u8], level: int = 0): void =
  let
    s = texture.size
    w = s.x.GLint
    h = s.y.GLint
    l = level.GLint
  glTextureSubImage2D(texture.handle, l, 0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, data[0].unsafeAddr)

proc setData*(texture: Texture2D; data: seq[uint8]; level: int = 0): void =
  let
    s = texture.size
    w = s.x.GLint
    h = s.y.GLint
    l = level.GLint
  glTextureSubImage2D(texture.handle, l, 0, 0, w, h, GL_RED, GL_UNSIGNED_BYTE, data[0].unsafeAddr)

proc newTexture2DArray*(size: Vec2i; depth: int; levels: int; internalFormat: GLenum = GL_RGBA8): Texture2DArray =
  glCreateTextures(GL_TEXTURE_2D_ARRAY, 1, result.handle.addr)
  glTextureStorage3D(result.handle, levels.GLint, internalFormat, size.x.GLsizei, size.y.GLsizei, depth.GLsizei)

proc newTexture2DArray*(size: Vec2i; depth: int; internalFormat: GLenum = GL_RGBA8): Texture2DArray =
  let levels = max(min(size.x, size.y).float32.log2.floor.int, 1)
  newTexture2DArray(size, depth, levels, internalFormat)

proc size*(tex: Texture2DArray): tuple[size : Vec2i, depth: int] =
  var w,h,d: GLint
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_HEIGHT, h.addr)
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_DEPTH, d.addr)
  result = (size: vec2i(w,h), depth: int(d))
  
proc subImage*(this: Texture2DArray; surface: sdl2.SurfacePtr; pos: Vec2i = vec2i(0); layer: int; level: int = 0): void =
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)

  let (a,b) = this.size
  #echo glm.`$` a, " ", b

  #echo(this.handle, " ", level.GLint, " ", pos.x.GLint, pos.y.GLint, layer.GLint, " ", surface2.w.GLsizei, " ", surface2.h.GLsizei, " ", 1)
  glTextureSubImage3D(this.handle, level.GLint, pos.x.GLint, pos.y.GLint, layer.GLint, surface2.w.GLsizei, surface2.h.GLsizei, 1, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  
proc `[]=`*(tex: Texture2DArray; i: int; surface: sdl2.SurfacePtr): void =
  let (size, d) = tex.size
  assert size.x == surface.w
  assert size.y == surface.h
  let
    level = GLint(0)
    xoffset = GLint(0)
    yoffset = GLint(0)
    zoffset = GLint(i)
    width   = GLsizei(size.x)
    height  = GLsizei(size.y)
    depth   = GLsizei(1)
    format  = GL_RGBA
    typ = GL_UNSIGNED_INT_8_8_8_8
    
  glTextureSubImage3D(tex.handle, level, xoffset, yoffset, zoffset, width, height, depth, format, typ, surface.pixels)

proc texture2DArray*(surfaces: openarray[sdl2.SurfacePtr]; internalFormat: GLenum = GL_RGBA8): Texture2DArray =
  glCreateTextures(GL_TEXTURE_2D_ARRAY, 1, result.handle.addr)
  let w = surfaces[0].w
  let h = surfaces[0].h
  let levels = min(w, h).float32.log2.floor.GLint
  glTextureStorage3D(result.handle, levels, internalFormat, w, h, surfaces.len.GLsizei)

  for i, surf in surfaces:
    result[i] = surf

  glGenerateTextureMipmap(result.handle)
  
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
    
proc newTexture1D*(size: int, internalFormat: GLenum = GL_RGBA8): Texture1D =
  when false:
    discard
  else:
    glCreateTextures(GL_TEXTURE_1D, 1, result.handle.addr)
    let levels = GLint(floor(log2(float32(size))))
    glTextureStorage1D(result.handle, levels, internalFormat, size.GLsizei)

proc size*(tex: Texture1D): int =
  var w: GLint
  when false:
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, w.addr)
  else:
    glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
  result = w.int

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


proc size*(tex: TextureRectangle): Vec2i =
  var w,h: GLint
  when false:
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, w.addr)
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, h.addr)  
  else:
    glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
    glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2i(w, h)

proc subImage*( texture : TextureRectangle; data : var seq[Mat4f] ) =
  when false:
    glTextureSubImage2DEXT(texture.handle, GL_TEXTURE_RECTANGLE, 0, 0, 0, 4, data.len.GLsizei, GL_RGBA, cGL_FLOAT, data[0].addr.pointer )
  else:
    glTextureSubImage2D(texture.handle, 0, 0, 0, 4, data.len.GLsizei, GL_RGBA, cGL_FLOAT, data[0].addr.pointer )


proc createErrorSurface*(message: string = nil): sdl2.SurfacePtr =
  result = createRGBSurface(0, 512, 512, 32, 0,0,0,0)
  if result.isNil:
    echo "SDL_CreateRGBSurface() failed: ", getError()
    quit(QUIT_FAILURE)

  let pixels = cast[ptr array[512*512,uint32]](result.pixels)
  for i in 0 ..< 512*512:
    pixels[i] = rand_u32() or 0xff000000'u32

proc loadTexture2DFromFile*(filename: string): Texture2D =
  var surface = image.load(filename)
  if surface.isNil:
    let message = s"can't load texture $filename: ${sdl2.getError()}"
    echo message
    surface = createErrorSurface(message)
    

  defer: freeSurface(surface)
  texture2D(surface)
  
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
  proc resize*(tex: Texture2D, size: Vec2i) =
    var internalFormat: GLint
    glGetTextureLevelParameterivEXT(tex.handle, GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, internalFormat.addr)
    glTextureImage2DEXT(tex.handle, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0, internalFormat.GLenum, cGL_UNSIGNED_BYTE, nil)

  proc resize*(tex: TextureRectangle, size: Vec2i) =
    var internalFormat: GLint
    glGetTextureLevelParameterivEXT(tex.GLuint, GL_TEXTURE_RECTANGLE, 0, GL_TEXTURE_INTERNAL_FORMAT, internalFormat.addr)
    glTextureImage2DEXT(tex.handle, GL_TEXTURE_RECTANGLE, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0, internalFormat.GLenum, cGL_UNSIGNED_BYTE, nil)

proc newTexture2D*(size: Vec2i, internalFormat: GLenum = GL_RGB8) : Texture2D =
  when false:
    glGenTextures(1, cast[ptr GLuint](result.addr))
    glTextureImage2DEXT(result.handle, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0,GL_RGB, cGL_UNSIGNED_BYTE, nil)
  else:
    glCreateTextures(GL_TEXTURE_2D, 1, cast[ptr GLuint](result.addr))
    let levels = min(size.x, size.y).float32.log2.floor.GLint
    glTextureStorage2D(
      result.handle, levels,
      internalFormat,
      size.x.GLsizei, size.y.GLsizei
    )
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)

proc newDepthTexture2D*(size: Vec2i, internalFormat: GLenum = GL_DEPTH_COMPONENT24) : Texture2D =
  let levels = min(size.x, size.y).float32.log2.floor.GLint
  when false:
    glGenTextures(1, result.handle.addr)
    glTextureImage2DEXT(result.handle, GL_TEXTURE_2D, 0, internalFormat, size.x.GLsizei, size.y.GLsizei, 0,GL_DEPTH_COMPONENT, cGL_FLOAT, nil)
  else:
    glCreateTextures(GL_TEXTURE_2D, 1, result.handle.addr)
    glTextureStorage2D(result.handle, levels, internalFormat, size.x.GLsizei, size.y.GLsizei)
    # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)

  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)

proc saveToBmpFile*(tex: TextureRectangle, filename: string): void =
  let s = tex.size
  var surface = createRGBSurface(0, s.x.int32, s.y.int32, 32, 0xff000000.uint32, 0x00ff0000, 0x0000ff00, 0x000000ff)  # no alpha, rest default
  when false:
    glGetTextureImageEXT(tex.handle, GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface.pixels)
  else:
    let bufferSize = GLsizei(s.x * s.y * 4)
    glGetTextureImage(tex.handle, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, bufferSize, surface.pixels)
  surface.saveBMP(filename)
