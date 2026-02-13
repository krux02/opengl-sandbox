# included from fancygl.nim

################################################################################
#### Sampler Types #############################################################
################################################################################

proc createErrorSurface*(message: string = "", size: Vec2i = vec2i(-1)): sdl.Surface =
  # TODO message is still unsued
  var w,h: int32 = 512

  if size.x != -1:
    w = size.x
  if size.y != -1:
    h = size.y

  result = createRGBSurface(0, w, h, 32, 0,0,0,0)
  if result.isNil:
    panic "SDL_CreateRGBSurface() failed: ", sdl.getError()

  let pixels = cast[ptr array[512*512,uint32]](result.pixels)
  for i in 0 ..< size.x * size.y:
    pixels[i] = rand_u32() or 0xff000000'u32


const typeConst = [GL_RED, GL_RG, GL_RGB, GL_RGBA]

proc unbindTextures*(first,count: int): void =
  if glBindTextures != nil:
    glBindTextures(first.GLuint, count.GLsizei, nil)
  else:
    for i in 0 ..< count:
      glBindTextureUnit(GLuint(first + i), 0)

proc bindTextures*(first: int; handles: openarray[GLuint]): void =
  if glBindTextures != nil:
    glBindTextures(first.GLuint, handles.len.GLsizei, handles[0].unsafeaddr)
  else:
    for i, texture in handles:
      glBindTextureUnit(GLuint(first + i), texture)

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
  #of GL_PATCHES: -                1
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

type
  Texture1D* = object
    handle*: GLuint

  Texture2D* = object
    handle*: GLuint

  Texture3D* = object
    handle*: GLuint

  Texture1DArray* = object
    handle*: GLuint

  Texture2DArray* = object
    handle*: GLuint

  TextureRectangle* = object
    handle*: GLuint

  TextureCubeMap* = object
    handle*: GLuint

  TextureCubeMapArray* = object
    handle*: GLuint

  TextureBuffer* = object
    handle*: GLuint

  Texture2DMultisample* = object
    handle*: GLuint

  Texture2DMultisampleArray* = object
    handle*: GLuint

  Texture1DShadow* = object
    handle*: GLuint

  Texture2DShadow* = object
    handle*: GLuint

  TextureCubeShadow* = object
    handle*: GLuint

  TextureCubeArrayShadow* = object
    handle*: GLuint

  Texture2DRectShadow* = object
    handle*: GLuint

  Texture1DArrayShadow* = object
    handle*: GLuint

  Texture2DArrayShadow* = object
    handle*: GLuint

type
  AnyTexture =
    Texture1D | Texture2D | Texture3D |
    Texture1DArray | Texture2DArray |
    TextureRectangle | TextureCubeMap | # TextureCubeMapArray |
    TextureBuffer | Texture2DMultisample | Texture2DMultisampleArray |
    Texture1DShadow | Texture2DShadow | TextureCubeShadow |
    Texture2DRectShadow | Texture1DArrayShadow | Texture2DArrayShadow
    #TextureCubeArrayShadow

proc `$`*(texture: AnyTexture): string =
  $texture.handle

proc isValid*(texture: AnyTexture): bool =
  texture.handle.int > 0 and glIsTexture(texture.handle)

proc bindToUnit*(texture: AnyTexture, unit: int): void =
  glBindTextureUnit(unit.GLuint, texture.handle)

proc parameter*(texture: AnyTexture, pname: GLenum, param: GLenum): void =
  glTextureParameteri(texture.handle, pname, GLint(param))

proc generateMipmap*(texture: AnyTexture): void =
  glGenerateTextureMipmap(texture.handle)

proc delete*(texture: AnyTexture): void =
  var id = texture.handle
  glDeleteTextures(1, id.addr)

proc loadSurfaceFromFile*(filename: string): Surface =
  let surface = img.load(filename)
  defer: freeSurface(surface)
  ## TODO: well this is bad
  result = surface.convertSurfaceFormat(PIXELFORMAT_RGBA8888, 0)

proc size*(tex: Texture2D): Vec2i =
  var w,h: GLint

  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_HEIGHT, h.addr)
  result = vec2i(w, h)


proc subImage*(this: Texture2D; surface: Surface; pos: Vec2i = vec2i(0); level: int = 0): void =
  case surface.format.format
  of PIXELFORMAT_RGBA8888:
    glTextureSubImage2D(this.handle, level.GLint, pos.x, pos.y, surface.w, surface.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface.pixels)
  of PIXELFORMAT_RGB24:
    glTextureSubImage2D(this.handle, level.GLint, pos.x, pos.y, surface.w, surface.h, GL_RGB,  GL_UNSIGNED_BYTE, surface.pixels)
  else:
    echo "converting surface format to RGBA8888, from: ", getPixelFormatName(surface.format.format)
    let surface2 = sdl.convertSurfaceFormat(surface, PIXELFORMAT_RGBA8888, 0)
    defer: freeSurface(surface2)
    glTextureSubImage2D(this.handle, level.GLint, pos.x, pos.y, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

proc subImageGrayscale*(this: Texture2D; surface: sdl.Surface; pos: Vec2i = vec2i(0); level: int = 0): void =
  assert(surface.format.format == PIXELFORMAT_INDEX8, $getPixelFormatName(surface.format.format))
  # use surface.pitch instead of surface.w, as this descrips the data layout, not the semantic size of the usable area of the texture
  glTextureSubImage2D(this.handle, level.GLint, pos.x, pos.y, surface.pitch, surface.h, GL_RED, GL_UNSIGNED_BYTE, surface.pixels)

proc texture2D*(surface: sdl.Surface): Texture2D =
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

proc subImage*(this: Texture2DArray; surface: sdl.Surface; pos: Vec2i = vec2i(0); layer: int; level: int = 0): void =
  let surface2 = sdl.convertSurfaceFormat(surface, PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)
  glTextureSubImage3D(this.handle, level.GLint, pos.x.GLint, pos.y.GLint, layer.GLint, surface2.w.GLsizei, surface2.h.GLsizei, 1, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

proc `[]=`*(tex: Texture2DArray; i: int; surface: sdl.Surface): void =
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

proc texture2DArray*(surfaces: openarray[sdl.Surface]; internalFormat: GLenum = GL_RGBA8): Texture2DArray =
  glCreateTextures(GL_TEXTURE_2D_ARRAY, 1, result.handle.addr)
  let w = surfaces[0].w
  let h = surfaces[0].h
  let levels = min(w, h).float32.log2.floor.GLint
  glTextureStorage3D(result.handle, levels, internalFormat, w, h, surfaces.len.GLsizei)

  for i, surf in surfaces:
    result[i] = surf

  glGenerateTextureMipmap(result.handle)

proc textureRectangle*(surface: sdl.Surface): TextureRectangle =
  let surface2 = sdl.convertSurfaceFormat(surface, PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)

  glCreateTextures(GL_TEXTURE_RECTANGLE, 1, cast[ptr GLuint](result.addr))
  glTextureStorage2D(result.handle, 1, GL_RGBA8, surface2.w, surface2.h)
  glTextureSubImage2D(result.handle, 0, 0, 0, surface2.w, surface2.h, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)

proc `label=`*[T](arg: T; label: string): void

proc loadTextureRectangleFromFile*(filename: string): TextureRectangle =
  var surface = img.load(filename)
  if surface.isNil:
    let message = s"can't load texture $filename: ${sdl.getError()}"
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

proc setDataRGBA*( texture: Texture1D, data: openarray[float32]) =
  glTextureSubImage1D(texture.handle, 0, 0, data.len.GLsizei div 4, GL_RGBA, cGL_FLOAT, data[0].unsafeAddr)

proc setData*( texture: Texture1D, data: openarray[float32]) =
  glTextureSubImage1D(texture.handle, 0, 0, data.len.GLsizei, GL_RED, cGL_FLOAT, data[0].unsafeAddr)

proc setData*( texture: Texture1D, data: openarray[Vec4u8 | Color]) =
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
  mixin attribSize
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
  var surface = img.load(filename)
  if surface.isNil:
    let message = s"can't load texture $filename: ${sdl.getError()}"
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
  discard setPaletteColors(surface.format.palette, colors[0].addr, 0, 256);

  let bufferSize = GLsizei(s.x * s.y)
  glGetTextureImage(tex.handle, 0, GL_RED, GL_UNSIGNED_BYTE, bufferSize, surface.pixels)
  discard surface.saveBMP(filename)

proc newTexture2D*(size: Vec2i, internalFormat: GLenum = GL_RGBA8; levels: int = -1) : Texture2D =
  glCreateTextures(GL_TEXTURE_2D, 1, result.handle.addr)
  let levelsArg = if levels < 0: min(size.x, size.y).float32.log2.floor.GLint else: levels.GLint
  glTextureStorage2D(
    result.handle, levelsArg,
    internalFormat,
    size.x.GLsizei, size.y.GLsizei
  )

proc noiseTexture2D*(size: Vec2i): Texture2D =
  var data : seq[uint32] = newSeq[uint32](size.x * size.y)
  for it in data.mitems:
    it = rand_u32()

  result = newTexture2D(size)
  result.setData(data)
  result.parameter(GL_TEXTURE_WRAP_S, GL_REPEAT)
  result.parameter(GL_TEXTURE_WRAP_T, GL_REPEAT)
  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.generateMipmap
  result.label = "RGBA noise"


proc newTexture3D*(size: Vec3i, internalFormat: GLenum = GL_RGBA8; levels: int = -1): Texture3D =
  glCreateTextures(GL_TEXTURE_3D, 1, result.handle.addr)
  let levelsArg = if levels < 0: min(min(size.x, size.y),size.z).float32.log2.floor.GLint else: levels.GLint
  glTextureStorage3D(result.handle, levelsArg, internalFormat,
                     GLsizei(size.x), GLsizei(size.y), GLsizei(size.z))


proc size*(tex: Texture3D): Vec3i =
  var w,h,d: GLint
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_WIDTH, w.addr)
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_HEIGHT, h.addr)
  glGetTextureLevelParameteriv(tex.handle, 0, GL_TEXTURE_DEPTH, d.addr)
  result = vec3i(w,h,d)


proc setData*[T](texture: Texture3D, data: seq[T]; level: int = 0) =
  let
    s = texture.size
    w = s.x.GLint
    h = s.y.GLint
    d = s.z.GLint
    lev = level.GLint

  glTextureSubImage3D(
    texture.handle, lev, 0, 0, 0, w, h, d, typeConst[T.attribSize-1], T.attribType, data[0].unsafeAddr
  )

proc loadTextureCubeMapFromFiles*(filenames: openarray[string]): TextureCubeMap =
  assert filenames.len == 6

  var surfaces: array[6, sdl.Surface]
  var size: int32 = -1

  for i, filename in filenames:
    let surface = img.load(filename)
    if surface.isNil:
      echo s"can't load texture $filename: ${sdl.getError()}"
    else:
      if surface.w != surface.h:
        echo s"need square textures for cube maps got ${surface.w}x${surface.h} in $filename"
        freeSurface(surface)
      elif size > 0 and size != surface.w:
        echo s"all surfaces need to be the same size ${size}x${size}, but $filename is ${surface.w}x${surface.h}"
        freeSurface(surface)
      else:
        surfaces[i] = surface
        size = surface.w

  let arraySurface = sdl.createRGBSurface(0, size, size * 6, 32, 0, 0, 0, 0);
  var srcRect: Rect = Rect(x: 0, y: 0, w: size, h: size)
  for i, surface in surfaces.mPairs:
    # remove nil, make sure the surface is useful
    if surface == nil:
      surface = createErrorSurface(filenames[i], vec2i(size))

    var dstRect: Rect = Rect(x: 0, y: i * size, w: size, h: size)
    discard blitSurface(surface, srcrect.addr, arraySurface, dstrect.addr)
    freeSurface(surface)
    surface = nil

  glCreateTextures(GL_TEXTURE_CUBE_MAP, 1, result.handle.addr)
  let levels = size.float32.log2.floor.GLint
  glTextureStorage2D(result.handle, levels, GL_RGBA8, size, size)
  glTextureSubImage3D(result.handle, 0, 0,0,0, size, size, 6, GL_RGBA, GL_UNSIGNED_BYTE, arraySurface.pixels)
  freeSurface(arraySurface)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.generateMipmap

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
  discard surface.saveBMP(filename)

## glsl only procs

proc texture*(sampler: Texture2D;            P: Vec2f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture3D;            P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: TextureCubeMap;          P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture2DShadow;      P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: TextureCubeShadow;    P: Vec4f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture2DArray;       P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture2DArrayShadow; P: Vec4f): Vec4f =
  quit("only implemented in shader")

proc fwidth*(p: SomeFloat): SomeFloat =
  quit("only implemented in shader")
proc fwidthCoarse*(p: SomeFloat): SomeFloat =
  quit("only implemented in shader")
proc fwidthFine*(p: SomeFloat): SomeFloat =
  quit("only implemented in shader")
proc dFdx*(p: SomeFloat): SomeFloat =
  quit("only implemented in shader")
proc dFdy*(p: SomeFloat): SomeFloat =
  quit("only implemented in shader")
proc dFdxCoarse*(p: SomeFloat): SomeFloat =
  quit("only implemented in shader")
proc dFdyCoarse*(p: SomeFloat): SomeFloat =
  quit("only implemented in shader")
proc dFdxFine*(p: SomeFloat): SomeFloat =
  quit("only implemented in shader")
proc dFdyFine*(p: SomeFloat): SomeFloat =
  quit("only implemented in shader")

proc fwidth*[N,T](p: Vec[N,T]): Vec[N,T] =
  quit("only implemented in shader")
proc fwidthCoarse*[N,T](p: Vec[N,T]): Vec[N,T] =
  quit("only implemented in shader")
proc fwidthFine*[N,T](p: Vec[N,T]): Vec[N,T] =
  quit("only implemented in shader")
proc dFdx*[N,T](p: Vec[N,T]): Vec[N,T] =
  quit("only implemented in shader")
proc dFdy*[N,T](p: Vec[N,T]): Vec[N,T] =
  quit("only implemented in shader")
proc dFdxCoarse*[N,T](p: Vec[N,T]): Vec[N,T] =
  quit("only implemented in shader")
proc dFdyCoarse*[N,T](p: Vec[N,T]): Vec[N,T] =
  quit("only implemented in shader")
proc dFdxFine*[N,T](p: Vec[N,T]): Vec[N,T] =
  quit("only implemented in shader")
proc dFdyFine*[N,T](p: Vec[N,T]): Vec[N,T] =
  quit("only implemented in shader")
