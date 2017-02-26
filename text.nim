import fancygl, opengl, sdl2, sdl2/ttf

type
  TextRenderer = object
    textHeight : int
    font: FontPtr
    fg: Color
    bg: Color
    texCoordBuffer: ArrayBuffer[Vec2f]

    textureWidth: int
    texture: Texture2D

const fontPaths = ["/usr/share/fonts/truetype/inconsolata/Inconsolata.otf", "/usr/share/fonts/TTF/Inconsolata-Regular.ttf"]

proc init(self: ptr TextRenderer): void =
  self.textHeight = 14

  for path in fontPaths:
    if not self.font.isNil:
      break
    self.font = ttf.openFont(path, self.textHeight.cint)
    echo sdl2.getError()

  if self.font.isNil:
    stderr.writeLine "could not load font: ", sdl2.getError()
    stderr.writeLine "sorry system font locations are hard coded into the program, change that to fix this problem"
    system.quit(QUIT_FAILURE)

  self.fg = (255.uint8, 255.uint8, 255.uint8, 255.uint8)
  self.bg = (0.uint8, 0.uint8, 0.uint8, 255.uint8)

  self.texCoordBuffer = arrayBuffer([vec2f(0,0), vec2f(1,0), vec2f(0,-1), vec2f(1,-1)], GL_STATIC_DRAW)

  self.textureWidth = 256
  self.texture = newTexture2D(size = vec2i(self.textureWidth.int32, self.textHeight.int32 + 2), internalFormat = GL_R8, levels = 1)

proc textRenderer(): var TextRenderer =
  var this {.global.}: ptr TextRenderer = nil
  if this.isNil:
    this = create(TextRenderer)
    this.init()
  this[]

proc textureArray(this: TextRenderer; strings: openarray[string]): Texture2DArray =
  var maxWidth = 0
  var surfaces = newSeq[SurfacePtr](strings.len)

  for i, str in strings:
    if str.len > 0:
      let surf = renderTextShaded(this.font, str, this.fg, this.bg)
      assert surf != nil
      surf.flipY
      maxWidth = max(maxWidth, surf.w)
      surfaces[i] = surf

  result = newTexture2DArray(vec2i(maxWidth.int32, this.textHeight.int32 + 2), strings.len, 1)

  for i, surf in surfaces:
    if not surf.isNil:
      result.subImage(surf, layer = i)
      freeSurface surf

proc textureArray*(strings: openarray[string]): Texture2DArray =
  textureArray(textRenderer(), strings)

proc text(this: var TextRenderer; str: string; pixelPos: Vec2i): void =
  # TODO add print statement to render text at screen

  let surface = this.font.renderTextShaded(str, this.fg, this.bg)
  defer: freeSurface(surface)

  #surface.flipY
  surface.saveBmp("text.bmp")

  if surface.w > this.textureWidth:
    while surface.w > this.textureWidth:
      this.textureWidth *= 2

    delete this.texture
    this.texture = newTexture2D(vec2i(this.textureWidth.int32, this.textHeight.int32 + 2))

  assert this.texture.size.y == surface.size.y

  this.texture.subImageGrayscale(surface)

  var viewport : Vec4i
  glGetIntegerv(GL_VIEWPORT, viewport[0].addr)

  let vpPos = viewport.xy
  let vpSize = viewport.zw
  let textSize = surface.size

  let rectPixelPos = vec2i(pixelPos.x, vpSize.y - pixelPos.y)
  let rectPos  = vec2f(rectPixelPos-vpPos) / vec2f(vpSize) * 2.0f - vec2f(1)
  let rectSize = vec2f(textSize) / vec2f(vpSize) * 2.0f

  shadingDsl:
    primitiveMode = GL_TRIANGLE_STRIP
    numVertices = 4

    uniforms:
      rectPixelPos
      rectPixelSize = textSize
      rectPos
      rectSize
      tex = this.texture
    attributes:
      a_texcoord = this.texCoordBuffer

    vertexMain:
      """
      gl_Position.xy = rectPos + a_texcoord * rectSize;
      gl_Position.zw = vec2(-1,1);
      v_texcoord = a_texcoord;
      """

    vertexOut:
      "out vec2 v_texcoord"

    fragmentMain:
      """
      ivec2 texcoord;
      texcoord.x = int(gl_FragCoord.x) - rectPixelPos.x;
      texcoord.y = rectPixelPos.y - int(gl_FragCoord.y);
      color = texelFetch(tex, texcoord, 0).rrrr;
      """

proc renderText*(str: string, pos: Vec2i): void =
  var renderer = textRenderer()
  renderer.text(str, pos)
