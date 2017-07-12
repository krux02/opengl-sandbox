# included from fancygl.nim

################################################################################
#### Sampler Types #############################################################
################################################################################

proc createErrorSurface*(message: string = nil): sdl2.SurfacePtr =
  # TODO message is still unsued
  result = createRGBSurface(0, 512, 512, 32, 0,0,0,0)
  if result.isNil:
    panic "SDL_CreateRGBSurface() failed: ", getError()

  let pixels = cast[ptr array[512*512,uint32]](result.pixels)
  for i in 0 ..< 512*512:
    pixels[i] = rand_u32() or 0xff000000'u32


const typeConst = [GL_RED, GL_RG, GL_RGB, GL_RGBA]

template textureTypeTemplate(name: untyped, target: GLenum, shadername:string): untyped =
  type
    name* = object
      handle*: GLuint

  proc `$`*(texture: name): string =
    $texture.handle


proc unbindTextures*(first,count: int): void =
  glBindTextures(first.GLuint, count.GLsizei, nil)

proc bindTextures*(first: int; handles: openarray[GLuint]): void =
  glBindTextures(first.GLuint, handles.len.GLsizei, handles[0].unsafeaddr)

proc geometryNumVerts(mode: GLenum): int =
  case mode
  of GL_POINTS:                   1
  of GL_LINE_STRIP:               2
  of GL_LINE_LOOP:                2
  of GL_LINES:                    2
  of GL_LINE_STRIP_ADJACENCY:     4
  of GL_LINES_ADJACENCY:          4
  of GL_TRIANGLE_STRIP:           3
  of GL_TRIANGLE_FAN:             3
  of GL_TRIANGLES:                3
  of GL_TRIANGLE_STRIP_ADJACENCY: 6
  of GL_TRIANGLES_ADJACENCY:      6
  of GL_PATCHES: -                1
  else: -                         1128

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

textureTypeTemplate(Texture1D,
    GL_TEXTURE_1D, "sampler1D")
textureTypeTemplate(Texture2D,
    GL_TEXTURE_2D, "sampler2D")
textureTypeTemplate(Texture3D,
    GL_TEXTURE_3D, "sampler3D")
textureTypeTemplate(Texture1DArray,
    GL_Texture_1D_ARRAY, "sampler1DArray")
textureTypeTemplate(Texture2DArray,
    GL_TEXTURE_2D_ARRAY, "sampler2DArray")
textureTypeTemplate(TextureRectangle,
    GL_TEXTURE_RECTANGLE, "sampler2DRect")
textureTypeTemplate(TextureCubeMap,
    GL_TEXTURE_CUBE_MAP, "samplerCube")
textureTypeTemplate(TextureCubeMapArray,
    GL_TEXTURE_CUBE_MAP_ARRAY , "samplerCubeArray")
textureTypeTemplate(TextureBuffer,
    GL_TEXTURE_BUFFER, "samplerBuffer")
textureTypeTemplate(Texture2DMultisample,
    GL_TEXTURE_2D_MULTISAMPLE, "sampler2DMS")
textureTypeTemplate(Texture2DMultisampleArray,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY, "sampler2DMSArray")


textureTypeTemplate(Texture1DShadow,        GL_TEXTURE_1D,             "sampler1DShadow​")
textureTypeTemplate(Texture2DShadow,        GL_TEXTURE_2D,             "sampler2DShadow​")
textureTypeTemplate(TextureCubeShadow,      GL_TEXTURE_CUBE_MAP,       "samplerCubeShadow​")
textureTypeTemplate(Texture2DRectShadow,    GL_TEXTURE_RECTANGLE,      "sampler2DRectShadow​")
textureTypeTemplate(Texture1DArrayShadow,   GL_TEXTURE_1D_ARRAY,       "sampler1DArrayShadow​")
textureTypeTemplate(Texture2DArrayShadow,   GL_TEXTURE_2D_ARRAY,       "sampler2DArrayShadow​")
textureTypeTemplate(TextureCubeArrayShadow, GL_TEXTURE_CUBE_MAP_ARRAY, "samplerCubeArrayShadow​")


type
  AnyTexture =
    Texture1D | Texture2D | Texture3D |
    Texture1DArray | Texture2DArray |
    TextureRectangle | TextureCubeMap | TextureCubeMapArray |
    TextureBuffer | Texture2DMultisample | Texture2DMultisampleArray |
    Texture1DShadow | Texture2DShadow | TextureCubeShadow |
    Texture2DRectShadow | Texture1DArrayShadow | Texture2DArrayShadow |
    TextureCubeArrayShadow

proc isValid*(texture: AnyTexture): bool =
  texture.handle.int > 0 and glIsTexture(texture.handle)

proc bindToUnit*(texture: AnyTexture, unit: int): void =
  glBindTextureUnit(unit.GLuint, texture.handle)

proc parameter*(texture: AnyTexture, pname: GLenum, param: GLint): void =
  glTextureParameteri(texture.handle, pname, param)

proc generateMipmap*(texture: AnyTexture): void =
  glGenerateTextureMipmap(texture.handle)

proc delete*(texture: AnyTexture): void =
  var id = texture.handle
  glDeleteTextures(1, id.addr)

proc label*(arg: AnyTexture): string =
  const bufsize = 255
  result = newString(bufsize)
  var length: GLsizei
  glGetObjectLabel(GL_TEXTURE, arg.handle, bufsize, length.addr, result[0].addr)
  result.setLen(length)

proc `label=`*(arg: AnyTexture; label: string): void =
    ## does nothing when label is nil
    if not isNil label:
      glObjectLabel(GL_TEXTURE, arg.handle, GLsizei(label.len), label[0].unsafeAddr)

proc loadSurfaceFromFile*(filename: string): SurfacePtr =
  let surface = image.load(filename)
  defer: freeSurface(surface)
  result = surface.convertSurfaceFormat(SDL_PIXELFORMAT_RGBA8888, 0)

proc size*(tex: Texture2D): Vec2i =
  var w,h: GLint
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2i(w, h)


proc subImage*(this: Texture2D; surface: sdl2.SurfacePtr; pos: Vec2i = vec2i(0); level: int = 0): void =
  case surface.format.format
  of SDL_PIXELFORMAT_RGBA8888:
    glTextureSubImage2D(this.handle, level.GLint, pos.x, pos.y, surface.w, surface.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface.pixels)
  of SDL_PIXELFORMAT_RGB24:
    glTextureSubImage2D(this.handle, level.GLint, pos.x, pos.y, surface.w, surface.h, GL_RGB,  GL_UNSIGNED_BYTE, surface.pixels)
  else:
    echo "converting surface format to RGBA8888, from: ", getPixelFormatName(surface.format.format)
    let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
    defer: freeSurface(surface2)
    glTextureSubImage2D(this.handle, level.GLint, pos.x, pos.y, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

proc subImageGrayscale*(this: Texture2D; surface: sdl2.SurfacePtr; pos: Vec2i = vec2i(0); level: int = 0): void =
  assert(surface.format.format == SDL_PIXELFORMAT_INDEX8, $getPixelFormatName(surface.format.format))
  glTextureSubImage2D(this.handle, level.GLint, pos.x, pos.y, surface.w, surface.h, GL_RED, GL_UNSIGNED_BYTE, surface.pixels)

proc texture2D*(surface: sdl2.SurfacePtr): Texture2D =
  glCreateTextures(GL_TEXTURE_2D, 1, result.handle.addr)
  let levels = min(surface.w, surface.h).float32.log2.floor.GLint
  glTextureStorage2D(result.handle, levels, GL_RGBA8, surface.w, surface.h)
  result.subImage(surface)
  glGenerateTextureMipmap(result.handle)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.generateMipmap

proc texture2D*(size: Vec2i, internalFormat: GLenum = GL_RGBA8): Texture2D =
  glCreateTextures(GL_TEXTURE_2D, 1, result.handle.addr)
  let levels = GLint(floor(log2(float32(min(size.x, size.y)))))
  glTextureStorage2D(result.handle, levels, internalFormat, size.x.GLsizei, size.y.GLsizei)

proc setData*[T](texture: Texture2D, data: seq[T]; level: int = 0) =
  let
    s = texture.size
    w = s.x.GLint
    h = s.y.GLint
    lev = level.GLint

  glTextureSubImage2D(texture.handle, lev, 0, 0, w, h, typeConst[T.attribSize-1], T.attribType, data[0].unsafeAddr)

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

  #let (a,b) = this.size
  #echo glm.`$` a, " ", b

  #echo(this.handle, " ", level.GLint, " ", pos.x.GLint, pos.y.GLint, layer.GLint, " ", surface2.w.GLsizei, " ", surface2.h.GLsizei, " ", 1)
  glTextureSubImage3D(this.handle, level.GLint, pos.x.GLint, pos.y.GLint, layer.GLint, surface2.w.GLsizei, surface2.h.GLsizei, 1, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

proc `[]=`*(tex: Texture2DArray; i: int; surface: sdl2.SurfacePtr): void =
  let (size, _) = tex.size # ignore depth
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

  glCreateTextures(GL_TEXTURE_RECTANGLE, 1, cast[ptr GLuint](result.addr))
  glTextureStorage2D(result.handle, 1, GL_RGBA8, surface2.w, surface2.h)
  glTextureSubImage2D(result.handle, 0, 0, 0, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

proc loadTextureRectangleFromFile*(filename: string): TextureRectangle =
  var surface = image.load(filename)
  if surface.isNil:
    let message = s"can't load texture $filename: ${sdl2.getError()}"
    echo message
    surface = createErrorSurface(message)

  defer: freeSurface(surface)
  result = textureRectangle(surface)
  result.label = filename

proc newTexture1D*(size: int, internalFormat: GLenum = GL_RGBA8): Texture1D =
  glCreateTextures(GL_TEXTURE_1D, 1, result.handle.addr)
  let levels = GLint(floor(log2(float32(size))))
  glTextureStorage1D(result.handle, levels, internalFormat, size.GLsizei)

proc size*(tex: Texture1D): int =
  var w: GLint
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
  result = w.int

proc setDataRGBA*( texture: Texture1D, data: seq[float32]) =
  glTextureSubImage1D(texture.handle, 0, 0, data.len.GLsizei div 4, GL_RGBA, cGL_FLOAT, data[0].unsafeAddr)

proc setData*( texture: Texture1D, data: seq[float32]) =
  glTextureSubImage1D(texture.handle, 0, 0, data.len.GLsizei, GL_RED, cGL_FLOAT, data[0].unsafeAddr)

proc setData*( texture: Texture1D, data: seq[Vec4u8]) =
  glTextureSubImage1D(texture.handle, 0, 0, data.len.GLsizei, GL_RGBA, GL_UNSIGNED_BYTE, data[0].unsafeAddr)

proc newTextureRectangle*(size: Vec2i; internalFormat : GLenum = GL_RGBA8): TextureRectangle =
  glCreateTextures(GL_TEXTURE_RECTANGLE, 1, result.handle.addr)
  glTextureStorage2D(result.handle, 1, internalFormat, size.x.GLsizei, size.y.GLsizei)

proc size*(tex: TextureRectangle): Vec2i =
  var w,h: GLint
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2i(w,h)

proc subImage*( texture : TextureRectangle; data : var seq[Mat4f] ) =
  glTextureSubImage2D(texture.handle, 0, 0, 0, 4, data.len.GLsizei, GL_RGBA, cGL_FLOAT, data[0].addr.pointer )

proc setData*[T](texture: TextureRectangle; data: seq[T]) =
  let
    s = texture.size
    w = s.x.GLint
    h = s.y.GLint

  glTextureSubImage2D(texture.handle, 0, 0, 0, w, h, typeConst[T.attribSize-1], T.attribType, data[0].unsafeAddr)

proc setPixel*[T](texture: TextureRectangle; pos: Vec2i; pixel: T) =
  glTextureSubImage2D(texture.handle, 0, pos.x, pos.y, 1, 1, typeConst[T.attribSize-1], T.attribType, pixel.unsafeAddr)

#proc readPixel*[T](texture: TextureRectangle; pos: Vec2i): T =
#  glGetTextureSubImage(texture.handle, 0, pos.x, pos.y, 0, 1, 1, 1, typeConst[T.attribSize-1], T.attribType, result.addr)

proc loadTexture2DFromFile*(filename: string): Texture2D =
  var surface = image.load(filename)
  if surface.isNil:
    let message = s"can't load texture $filename: ${sdl2.getError()}"
    echo message
    surface = createErrorSurface(message)


  defer: freeSurface(surface)
  result = texture2D(surface)
  result.label = filename

proc saveToBmpFile*(tex: Texture2D | TextureRectangle; filename: string): void =
  let s = tex.size
  var surface = createRGBSurface(0, s.x.int32, s.y.int32, 32, 0xff000000.uint32, 0x00ff0000, 0x0000ff00, 0x000000ff)  # no alpha, rest default
  let bufferSize = GLsizei(s.x * s.y * 4)
  glGetTextureImage(tex.handle, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, bufferSize, surface.pixels)
  surface.saveBMP(filename)

proc saveToGrayscaleBmpFile*(tex: Texture2D | TextureRectangle; filename: string): void =
  var colors: array[256,Color]
  for i, color in colors.mpairs:
    color.r = uint8(i)
    color.g = uint8(i)
    color.b = uint8(i)

  let s = tex.size
  var surface = createRGBSurface(0, s.x.int32, s.y.int32, 8, 0, 0, 0, 0)
  setPaletteColors(surface.format.palette, colors[0].addr, 0, 256);

  let bufferSize = GLsizei(s.x * s.y)
  glGetTextureImage(tex.handle, 0, GL_RED, GL_UNSIGNED_BYTE, bufferSize, surface.pixels)
  surface.saveBMP(filename)

proc newTexture2D*(size: Vec2i, internalFormat: GLenum = GL_RGBA8; levels: int = -1) : Texture2D =
  glCreateTextures(GL_TEXTURE_2D, 1, result.handle.addr)
  let levelsArg = if levels < 0: min(size.x, size.y).float32.log2.floor.GLint else: levels.GLint
  glTextureStorage2D(
    result.handle, levelsArg,
    internalFormat,
    size.x.GLsizei, size.y.GLsizei
  )

  # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)
  # result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  # result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)

proc newDepthTexture2D*(size: Vec2i, internalFormat: GLenum = GL_DEPTH_COMPONENT24) : Texture2D =
  let levels = min(size.x, size.y).float32.log2.floor.GLint
  glCreateTextures(GL_TEXTURE_2D, 1, result.handle.addr)
  glTextureStorage2D(result.handle, levels, internalFormat, size.x.GLsizei, size.y.GLsizei)
  # glTextureSubImage2D(tex.GLuint, 0, 0, 0, size.x.GLsizei, size.y.GLsizei, internalFormat,cGL_UNSIGNED_BYTE, nil)

  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)

proc saveToBmpFile*(tex: TextureRectangle, filename: string): void =
  let s = tex.size
  var surface = createRGBSurface(0, s.x.int32, s.y.int32, 32, 0xff000000.uint32, 0x00ff0000, 0x0000ff00, 0x000000ff)  # no alpha, rest default
  let bufferSize = GLsizei(s.x * s.y * 4)
  glGetTextureImage(tex.handle, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, bufferSize, surface.pixels)
  surface.saveBMP(filename)
