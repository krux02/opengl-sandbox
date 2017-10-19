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

const fontPaths = [
  "/usr/share/fonts/truetype/inconsolata/Inconsolata.otf",
  "/usr/share/fonts/TTF/Inconsolata-Regular.ttf"
]

macro getSymbolLineinfo(arg: typed): string =
  result = newLit(arg.symbol.getImpl.lineinfo)

proc init(self: ptr TextRenderer; textHeight: int): void =
  self.textHeight = textHeight

  for path in fontPaths:
    self.font = ttf.openFont(path, self.textHeight.cint)

    if self.font.isNil:
      echo ttf.getError()
    else:
      self.fontPath = path
      break

  if self.font.isNil:
    var msg = newString(0)
    msg.add "could not load font and font from hard coded font paths: \n"
    msg.add getSymbolLineinfo(fontPaths)
    msg.add "\nyou could add a path to a font that exists on your system."
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
    this.init(14)
  result = this

proc textRendererLarge(): var TextRenderer =
  var this {.global.}: ptr TextRenderer = nil
  if this.isNil:
    this = create(TextRenderer)
    this.init(100)
  this[]

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

proc distanceTransform(image: seq[uint8]; dist: var seq[uint8]; insize, outsize: Vec2i; searchsize: int) =
  assert(image.len == insize.x * insize.y, "invalid argument")
  assert(dist.len == outsize.x * outsize.y, "invalid argument")

  ## this does not work, so don't use it
  let scale = vec2f(insize / outsize)
  var hood = newSeq[float32](outsize.x * outsize.y)
  # Raw distance map.
  var min =  1e20'f32
  var max = -1e20'f32

  template getImage(X,Y: int32): uint8 =
    image[X + Y * insize.x]

  template getDist(X,Y: int32): uint8 =
    dist[X + Y * outsize.x]

  proc lerp(pos: Vec2f): float32 =
    let
      posi = vec2i(floor(pos))
      p0 = max(posi    , vec2i(0)  )  # clamp
      p1 = min(posi + 1, insize - 1)  # clamp
      f = fract(pos)

    let
      g00 = float32(getImage(p0.x, p0.y))
      g10 = float32(getImage(p1.x, p0.y))
      g11 = float32(getImage(p1.x, p1.y))
      g01 = float32(getImage(p0.x, p1.y))

    return
      mix( mix(g00, g01, f.y), mix(g10, g11, f.y), f.x)

  var deltas = newSeq[tuple[x: float32; y: seq[Vec2f]]](0)
  for i in 1 .. searchsize * 2:
    let r: float32 = ((float32)i) / 2;
    let N: int = int(2 * 3.14159265358979323846 * r);
    var ring = newSeq[Vec2f](N);
    for j, it in ring.mpairs:
      it = vec2f(
        r * cos(j.float32 * Pi * 2 / N.float32),
        r * sin(j.float32 * Pi * 2 / N.float32)
      )
    deltas.add((x: r, y: ring));

  for iy in 0 ..< outsize.y:
    for ix in 0 ..< outsize.x:
      # Pixel location on src image.
      let b = vec2f(ix.float32,iy.float32) * scale

      let m : float32 = lerp(b);
      let inside: bool = m >= 0;
      var minDistance = 1e20f;
      if m != 0:
        # Radial outwards search.
        for ring in deltas:
          for delta in ring.y:
            proc binary(N: int32; v0, v1, k0, k1: float32): float32 =
              let k: float32 = k0 + (k1 - k0) * (0 - v0) / (v1 - v0);
              let vk: float32 = lerp(b + delta * k)

              if vk*vk < 1e-5 or N > 20:
                return k;
              if v0*vk < 0:
                return binary(N + 1, v0, vk, k0, k);
              if vk*v1 < 0:
                return binary(N+1,vk, v1, k, k1);

              assert(false);
              return k;

            let n: float32 = lerp(b + delta)
            if n != 1e20f and n*m <= 0:
              let dist: float32 = ring.x;
              if n == 0:
                minDistance = dist;
              else:
                minDistance = dist * binary(0, m, n, 0, 1);
          if minDistance != 1e20f:
            break;
      else:
        minDistance = 0;

      if minDistance != 1e20f:
        if not inside:
          minDistance = -minDistance;
        min = min(minDistance, min)
        max = max(minDistance, max)
      elif not inside:
        minDistance = -minDistance;
      hood[ix + iy * outsize.x] = minDistance;

  # Perform scaling.
  let mm: float32 = max(abs(min), abs(max));
  min = -mm;
  max = mm;
  let det = 255.0f/(max-min);

  assert(dist.len == hood.len)

  var a,b,c: int
  for i, m in hood:
    if m == 1e20f:
      a += 1
      dist[i] = 255
    elif m == -1e20f:
      c += 1
      dist[i] = 0
    else:
      b += 1
      dist[i] = uint8((m-min) * det)

type
  TextObject = object
    width: float32
    texture: Texture2D

# (iimage-mode-buffer 1)
# (iimage-mode 1) (iimage-mode 0)

proc createTextObject(this: TextRenderer; arg: string): TextObject =
  let surface = renderTextShaded(this.font, arg, this.fg, this.bg)
  assert surface != nil
  defer:
    freeSurface(surface)

  var pixeldata = newSeq[uint8](surface.w * surface.h)
  copyMem(pixelData[0].addr, surface.pixels, pixelData.len)
  var signedDistancePixels = newSeq[uint8](surface.w * surface.h)
  let size = vec2i(surface.w, surface.h)
  distanceTransform(pixeldata, signedDistancePixels, size, size, 8)

  #result.texture = texture2D(surface)
  #discard surface.savePng(arg & "_a.png")
  copyMem(surface.pixels, signedDistancePixels[0].addr, pixelData.len)
  #discard surface.savePng(arg & "_b.png")

  surface.flipY
  result.texture = texture2D(surface)
  result.texture.parameter(GL_TEXTURE_MAG_FILTER, GL_NEAREST)
  result.width = float32(surface.w / surface.h)

proc createTextObject*(arg: string): TextObject =
  createTextObject(textRendererLarge(), arg)

proc render(tr: TextRenderer; to: TextObject; mvp: Mat4f): void =
  shadingDsl:
    primitiveMode = GL_TRIANGLE_STRIP
    numVertices = 4

    uniforms:
      mvp
      tex = to.texture
      width = to.width

    attributes:
      a_texCoord = tr.texCoordBuffer

    vertexMain:
      """
      gl_Position = mvp * vec4(a_texCoord.x * width, a_texCoord.y, 0, 1);
      v_texCoord = a_texCoord;
      """
    vertexOut:
      "out vec2 v_texCoord"

    fragmentMain:
      """
      //if(texture(tex, v_texCoord).r < 0.5)
      //  discard;
      //color = vec4(1);
      color = texture(tex, v_texCoord);
      """

proc render*(textObject: TextObject; mvp: Mat4f): void =
  render(textRendererLarge(), textObject, mvp)

var timer = newStopWatch(true)

proc text(this: var TextRenderer; str: string; pixelPos: Vec2i): void =
  # TODO add print statement to render text at screen

  let surface = this.font.renderTextShaded(str, this.fg, this.bg)
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
      """

    fragmentMain:
      """
      ivec2 texcoord;
      texcoord.x = int(gl_FragCoord.x) - rectPixelPos.x;
      texcoord.y = rectPixelPos.y - int(gl_FragCoord.y);
      color = texelFetch(tex, texcoord, 0).rrrr;
      """


proc renderText*(str: string, pos: Vec2i): void =
  var renderer = textRenderer()
  renderer[].text(str, pos)
