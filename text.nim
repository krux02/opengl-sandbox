import fancygl, sdl2/ttf

template orElse[T](a,b: ptr T): ptr T =
  let aval = a
  if not aval.isNil:
    aval
  else:
    b

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
  
proc newTextRenderer(): TextRenderer =
  result.textHeight = 14
  for path in fontPaths:
    if not result.font.isNil:
      break
    result.font = ttf.openFont(path, result.textHeight.cint)

  if result.font.isNil:
    echo "could not load font: ", fancygl.getError()
    echo "sorry system font locations are hard coded into the program, change that to fix this problem"
    system.quit(QUIT_FAILURE)

  result.fg = (255.uint8, 255.uint8, 255.uint8, 255.uint8)
  result.bg = (0.uint8, 0.uint8, 0.uint8, 255.uint8)

  result.texCoordBuffer = arrayBuffer(@[vec2f(0,0), vec2f(1,0), vec2f(0,1), vec2f(1,1)], GL_STATIC_DRAW)

  result.textureWidth = 256
  result.texture = newTexture2D(vec2i(result.textureWidth.int32, result.textHeight.int32))

proc textRenderer(): var TextRenderer =
  var this {.global.}: ptr TextRenderer = nil
  if this.isNil:
    this = cast[ptr TextRenderer](alloc(sizeof(TextRenderer)))
    this[] = newTextRenderer()
  this[]

proc textureArray(this: TextRenderer; strings: openarray[string]): Texture2DArray =
  var maxWidth = 0
  var surfaces = newSeqOfCap[SurfacePtr](strings.len)

  for str in strings:

    surfaces.add this.font.renderTextShaded(str, this.fg, this.bg)
    if not surfaces.back.isNil:
      surfaces.back.flipY
      maxWidth = max(maxWidth, surfaces.back.w)

  result = newTexture2DArray(vec2i(maxWidth.int32, this.textHeight.int32 + 2), strings.len, 1)
    
  for i, surf in surfaces:
    if not surf.isNil:
      result.subImage(surf, layer = i)
      freeSurface surf
    
proc textureArray*(strings: openarray[string]): Texture2DArray =
  textureArray(textRenderer(), strings)
    
proc text(this: var TextRenderer; str: string; x,y: int): void =

  
  if this.texture.handle == 0:
    this.texture = newTexture2D(vec2i(this.textureWidth.int32, this.textHeight.int32))

  # TODO add print statement to render text at screen

  let surface = this.font.renderTextShaded(str, this.fg, this.bg)
  defer: freeSurface(surface)

  surface.flipY

  if surface.w > this.textureWidth:
    delete this.texture

    while surface.w > this.textureWidth:
      this.textureWidth *= 2
    
    this.texture = newTexture2D(vec2i(this.textureWidth.int32, this.textHeight.int32))

  this.texture.subImage(surface)

  var viewport : Vec4i
  glGetIntegerv(GL_VIEWPORT, viewport[0].addr)
  
  let vpPos = viewport.xy
  let vpSize = viewport.zw
  let textPos = vec2i(x.int32, y.int32)
  let textSize = surface.size
  
  let rectPos  = vec2f(textPos-vpPos) / vec2f(vpSize) * 2.0f - vec2f(1)
  let rectSize = vec2f(textSize) / vec2f(vpSize) * 2.0f - vec2f(1)

  shadingDsl:
    primitiveMode = GL_TRIANGLE_STRIP
    numVertices = 4

    uniforms:
      rectPixelPos = textPos
      rectPos
      rectSize
      tex = this.texture
    attributes:
      a_texcoord = this.texCoordBuffer

    vertexMain:
      """
      gl_Position.xy = rectPos + a_texcoord * rectSize;
      gl_Position.zw = vec2(0,1);
      v_texcoord = a_texcoord * rectSize;
      """

    vertexOut:
      "out vec2 v_texcoord"

    fragmentMain:
      """
      vec2 texcoord = ivec2(gl_FragCoord.xy) - rectPixelPos;
      color = texture(tex, v_texcoord);
      //color.xy = v_texcoord;
      """


proc text*(this: var TextRenderer; str: string; pos: Vec2i): void =
  text(this, str, pos.x.int, pos.y.int)
