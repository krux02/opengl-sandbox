# import fancygl, opengl, sdl2, sdl2/ttf

type
  TextRenderer = object
    textHeight : int
    fontPath: string
    font: Font
    fg: Color
    bg: Color
    texCoordBuffer: ArrayBuffer[Vec2f]

    textureWidth: int
    texture: Texture2D

# macro getSymbolLineinfo(arg: typed): string =
#   result = newLit(arg.getImpl.lineinfo)

proc init(self: ptr TextRenderer; textHeight: int): void =
  self.textHeight = textHeight
  self.fontPath = getResourcePath("Inconsolata-Regular.ttf")
  self.font = ttf.openFont(self.fontPath, self.textHeight.cint)

  if self.font.isNil:
    var msg = newString(0)
    msg.add "could not load font: "
    msg.add self.fontPath
    msg.add "\n"
    msg.add ttf.getError()
    panic(msg)

  self.fg = Color(r: 255, g: 255, b:255, a:255)
  self.bg = Color(r:   0, g:   0, b:  0, a:255)

  self.texCoordBuffer = arrayBuffer([vec2f(0,0), vec2f(1,0), vec2f(0,-1), vec2f(1,-1)], GL_STATIC_DRAW)

  self.textureWidth = 256
  self.texture = newTexture2D(size = vec2i(self.textureWidth.int32, self.textHeight.int32 + 2), internalFormat = GL_R8, levels = 1)

proc textRenderer(): ptr TextRenderer =
  var this {.global.}: ptr TextRenderer = nil
  if this.isNil:
    this = create(TextRenderer)
    this.init(32)
  result = this

# proc textRendererLarge(): var TextRenderer =
#   var this {.global.}: ptr TextRenderer = nil
#   if this.isNil:
#     this = create(TextRenderer)
#     this.init(100)
#   this[]

proc textureArray(this: TextRenderer; strings: openarray[string]): Texture2DArray =
  if strings.len == 0:
    return

  var maxSize: Vec2i
  var surfaces = newSeq[Surface](strings.len)

  for i, str in strings:
    if str.len > 0:
      let surf = renderTextShaded(this.font, str, this.fg, this.bg)
      assert surf != nil
      surf.flipY
      maxSize = max(maxSize, surf.size)
      surfaces[i] = surf

  result = newTexture2DArray(maxSize, strings.len, 1)

  for i, surf in surfaces:
    if not surf.isNil:
      result.subImage(surf, layer = i)
      freeSurface surf

proc textureArray*(strings: openarray[string]): Texture2DArray =
  textureArray(textRenderer()[], strings)

# var timer = newStopWatch(true)

proc text(this: var TextRenderer; str: string; pixelPos: Vec2i): void =
  # TODO add print statement to render text at screen

  let surface = renderTextShaded(this.font, str, this.fg, this.bg)
  
  defer: freeSurface(surface)

  assert glIsTexture(this.texture.handle)

  # ensure minimum width
  if surface.w > this.textureWidth:
    while surface.w > this.textureWidth:
      this.textureWidth *= 2

    let newSize = vec2i(this.textureWidth, this.textHeight.int32 + 2)
    delete this.texture
    this.texture = newTexture2D(newSize)

  assert glIsTexture(this.texture.handle)
  #assert this.texture.size.y == surface.size.y

  this.texture.subImageGrayscale(surface)

  var viewport : Vec4i
  glGetIntegerv(GL_VIEWPORT, viewport[0].addr)

  let vpPos = viewport.xy
  let vpSize = viewport.zw
  let textSize = surface.size

  let rectPixelPos = vec2i(pixelPos.x, vpSize.y - pixelPos.y)
  let rectPos  = vec2f(rectPixelPos) / vec2f(vpSize) * 2.0f - vec2f(1)
  let rectSize = vec2f(textSize) / vec2f(vpSize) * 2.0f

  shadingDsl:
    primitiveMode = GL_TRIANGLE_STRIP
    numVertices = 4
    debugGroup = "renderText"

    uniforms:
      rectPixelPos
      rectPos
      rectSize
      vpPos
      tex = this.texture

    attributes:
      a_texcoord = this.texCoordBuffer

    vertexMain:
      """
      gl_Position.xy = rectPos + a_texcoord * rectSize;
      gl_Position.zw = vec2(-1,1);
      """

    fragmentMain:
      """
      ivec2 texcoord;
      texcoord.x = int(gl_FragCoord.x) - rectPixelPos.x - vpPos.x;
      texcoord.y = rectPixelPos.y - int(gl_FragCoord.y);
      color = texelFetch(tex, texcoord, 0).rrrr;
      """


proc renderText*(str: string, pos: Vec2i): void =
  var renderer = textRenderer()
  renderer[].text(str, pos)
