import ../fancygl, sdl2/image

let (window, context) = defaultSetup()

proc setup(): void =
  glEnable(GL_DEPTH_TEST)
  #glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  glProvokingVertex(GL_FIRST_VERTEX_CONVENTION)

proc loadTilePaletteFromFile*(filename: string; tilesize: Vec2i; levels: int): Texture2DArray =
  glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "loadTilemapFromFile");
  defer:
    glPopDebugGroup()
  var surface = image.load(filename)
  if surface.isNil:
    let message = s"can't load texture $filename: ${getError()}"
    echo message
    surface = createErrorSurface(message)
  defer: freeSurface(surface)

  var rect: Rect
  rect.x = 0
  rect.y = 0
  rect.w = tilesize.x
  rect.h = tilesize.y

  var layerSurface = createRGBSurface(0, tilesize.x, tilesize.y, 32, 0,0,0,0)
  defer: freeSurface(layerSurface)


  let rows = (surface.h div tilesize.y)
  let cols = (surface.w div tilesize.x)

  result = newTexture2DArray(tilesize, rows * cols, levels = levels)

  var i = 0
  for y in 0 ..< rows:
    for x in 0 ..< cols:
      rect.x = cint(x) * tilesize.x
      rect.y = cint(y) * tilesize.y
      blitSurface(surface, rect.addr, layerSurface, nil)
      result.subImage(layerSurface, layer = i)
      i += 1

const mapwidth  = 1024
const scaling   = 2
const dragThreshold = 4
const mapFilename = "map.bmp"

const rogue = true
## change ``rogue`` to true if you want font tiles :P

when rogue:
  const tileSize  = vec2i(8,12)
  const tileSizeLogical = vec2i(8,8)
  const tileImage = "pixelfont.png"
else:
  const tileSize  = vec2i(16)
  const tileSizeLogical = tileSize
  const tileImage = "tiles.gif"

import os

let tilePalette = loadTilePaletteFromFile(
  getAppDir() / "resources" / tileImage, tileSize, levels = 1)
tilePalette.parameter(GL_TEXTURE_MIN_FILTER, GL_NEAREST)
tilePalette.parameter(GL_TEXTURE_MAG_FILTER, GL_NEAREST)

# TODO make GL_R8UI work

proc noiseTextureRectangle(size: Vec2i): TextureRectangle =
  var randomTiles = newSeq[tuple[r,g,b,a: uint8]](mapwidth * mapwidth)
  for tile in randomTiles.mitems:
    tile.r = if rogue: rand_u8() else: 255
    tile.g = if rogue: rand_u8() else: 255
    tile.b = if rogue: rand_u8() else: 255
    tile.a = rand_u8()

  result = newTextureRectangle(size, internalFormat = GL_RGBA8)
  #result.setData(randomTiles)
  glTextureSubImage2D(
    texture = 3, level = 0,
    xoffset = 0, yoffset = 0,
    width = size.x, height = size.y,
    format = GL_RGBA, type = GL_UNSIGNED_BYTE,
    pixels = randomTiles[0].addr
  )

proc loadMapFromFile(): TextureRectangle =
  var surface = image.load(mapFilename)
  assert( not surface.isNil,
          s"can't load texture $mapFilename: ${fancygl.getError()}" )
  defer: freeSurface(surface)

  assert surface.format.BitsPerPixel == 8
  glCreateTextures(GL_TEXTURE_RECTANGLE, 1, result.handle.addr)
  glTextureStorage2D(result.handle, 1, GL_R8, surface.w, surface.h)
  glTextureSubImage2D(
    result.handle, 0, 0, 0,
    surface.w, surface.h,
    GL_RED, GL_UNSIGNED_BYTE, surface.pixels)

let tileSelectionMap = newTextureRectangle(vec2i(16), internalFormat = GL_RGBA8)

block:
  var selectionTiles = newSeq[Color](16 * 16)

  for i, tile in selectionTiles.mpairs:
    let x = i mod 16
    let y = 15 - i div 16
    tile.r = 255
    tile.g = 255
    tile.b = 255
    tile.a = uint8(x + y * 16)

  tileSelectionMap.setData(selectionTiles)

let map = noiseTextureRectangle(vec2i(mapwidth))
#let map = loadMapFromFile()

proc saveMap(): void {.noconv.} =
  map.saveToGrayscaleBmpFile(mapFilename)

#addQuitProc( saveMap )


var cameraPos = vec2f(mapwidth) * 0.5
let windowsize = window.size

proc pixelToWorldSpace(cameraPos: Vec2f; pos: Vec2i): Vec2f =
  (-vec2f(windowsize) * 0.5 + vec2f(pos.x.float32, float32(windowsize.y - pos.y))) / vec2f(tileSizeLogical * scaling) + cameraPos

proc gridTrianglesPosition*(size: Vec2i; offset: Vec2f) : seq[Vec4f] =
  result = newSeqOfCap[Vec4f](size.x * size.y * 6)

  let y = tileSize.y / tileSizeLogical.y

  for i in 0 ..< size.y:
    for j in 0 ..< size.x:
      let pos = vec2f(float32(j),float32(i)) + offset
      let
        a = vec4(pos + vec2f(0,0),    0, 1)
        b = vec4(pos + vec2f(1,0),    0, 1)
        c = vec4(pos + vec2f(0,y), -1, 1)
        d = vec4(pos + vec2f(1,y), -1, 1)

      result.add([a,d,c,a,b,d])

let gridSize     = vec2f(windowsize) / vec2f(tileSizeLogical * scaling)
let gridHalfSize = gridSize * 0.5

# one extra row and column for scrolling and one, because the offset is floored
let gridVertices    = arrayBuffer(gridTrianglesPosition(gridSize.vec2i + 3, floor(-gridHalfSize)))
let gridVerticesLen = gridVertices.len

let scale = vec2f(tileSizeLogical * 2 * scaling) / vec2f(windowsize)

proc drawTiles(highlightPos: Vec2i; map: TextureRectangle; cameraPos: Vec2f): void =

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = gridVerticesLen

    uniforms:
      scale
      cameraPos
      map
      tilePalette
      highlightPos

    attributes:
      pos      = gridVertices

    includes:
      """
      vec2 texCoords[6] = vec2[6](vec2(0,1),vec2(1,0),vec2(0,0),vec2(0,1),vec2(1,1),vec2(1,0));
      """
    vertexMain:
      """
      ivec2 gridPos = ivec2(pos.xy + floor(cameraPos));
      highlight = int(highlightPos == gridPos);

      tileId =  int(round(255 * texelFetch(map, gridPos).a));
      tintColor = texelFetch(map, gridPos).rgb;
      gl_Position = vec4((pos.xy - fract(cameraPos)) * scale, pos.zw);
      v_texCoord = texCoords[gl_VertexID % 6];
      """
    vertexOut:
      "out vec2 v_texCoord"
      "flat out int tileId"
      "flat out vec3 tintColor"
      "flat out int highlight"
    fragmentMain:
      """
      color = texture(tilePalette, vec3(v_texCoord, tileId));
      if( color.rgb == vec3(1,0,1) )
        discard;
      if(highlight == 0) {
        color.rgb *= tintColor;
      } else {
        color.rgb = fract(color.rgb * 1.2);
      }
      """


let crosshairVertices = arrayBuffer(
    [
      vec2f(-1,0), vec2f(1,0),
      vec2f(0,-1), vec2f(0,1)
    ]
)


proc drawCrosshair(): void =

  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = 12

    attributes:
      pos = crosshairVertices
    vertexMain:
      """
      gl_Position = vec4(pos,0,1);
      """
    fragmentMain:
      """
      color = vec4(1,1,1,1);
      """


var running = true
var frame = 0
#var gameTimer = newStopWatch(true)
var mouseLeftDrag = false
var mouseRightDrag = false
var mouseLeftDown, mouseRightDown = false
var dragLeftStartPos: Vec2i
var dragRightStartPos: Vec2i

setup()

var tileLeft : Color = (r:255'u8, g:255'u8, b:255'u8, a:128'u8)
var tileRight : Color =(r:255'u8, g:255'u8, b:255'u8, a:0'u8)
var tileSelection : bool = false

proc mouseClicked(evt: MouseButtonEventPtr): void =
  echo "mouseClicked"
  if tileSelection:
    let mousePos = pixelToWorldSpace(vec2f(8), vec2i(evt.x, evt.y))
    let gridPos = vec2i(mousePos.floor)
    let pixel: Color =
      (r: 255'u8, g: 255'u8, b: 255'u8,
            a: uint8(gridPos.x + (15 - gridPos.y) * 16))

    if evt.button == ButtonLeft:
      tileLeft = pixel
    elif evt.button == ButtonRight:
      tileRight = pixel
    else:
      return

    tileSelection = false
  else:
    let mousePos = pixelToWorldSpace(cameraPos, vec2i(evt.x, evt.y))
    let gridPos = vec2i(mousePos.floor)

    if fancygl.any( gridPos .< vec2i(0) ) or fancygl.any(gridPos .>= vec2i(mapwidth)):
      return

    var pixel : Color
    if evt.button == ButtonLeft:
      pixel = tileLeft
    elif evt.button == ButtonRight:
      pixel = tileRight
    else:
      return

    map.setPixel(gridPos, pixel)

proc mouseDragged(evt: MouseMotionEventPtr): void =
  if tileSelection:
    return

  if mouseLeftDown:
    let mousePos = pixelToWorldSpace(cameraPos, vec2i(evt.x, evt.y))
    let gridPos = vec2i(mousePos.floor)

    if fancygl.any( gridPos .< vec2i(0) ) or fancygl.any(gridPos .>= vec2i(mapwidth)):
      return

    map.setPixel(gridPos, tileLeft)

  if mouseRightDown:
    var movement : Vec2f
    movement.x =  evt.xrel / (tileSizeLogical.x * scaling)
    movement.y = -evt.yrel / (tileSizeLogical.y * scaling)
    cameraPos -= movement

while running:
  defer:
    frame += 1

  var evt = defaultEvent

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      running = false
      break

    elif evt.kind == KeyDown:
      case evt.key.keysym.scancode:
      of SDL_SCANCODE_ESCAPE:
        running = false
        break
      of SDL_SCANCODE_F10:
        window.screenshot
      of SDL_SCANCODE_SPACE:
        tileSelection = not tileSelection
      else:
        discard

    elif evt.kind == MouseButtonDown:
      if evt.button.button == ButtonLeft:
        mouseLeftDown = true
        dragLeftStartPos = vec2i(evt.button.x, evt.button.y)
      elif evt.button.button == ButtonRight:
        mouseRightDown = true
        dragRightStartPos = vec2i(evt.button.x, evt.button.y)

    elif evt.kind == MouseButtonUp:

      if evt.button.button == ButtonLeft:
        if not mouseLeftDrag:
          mouseClicked(evt.button)

        mouseLeftDrag = false
        mouseLeftDown = false

      if evt.button.button == ButtonRight:
        if not mouseRightDown:
          mouseClicked(evt.button)

        mouseRightDrag = false
        mouseRightDown = false

    elif evt.kind == MouseMotion:
      let mousePos = vec2i(evt.motion.x, evt.motion.y)

      if mouseLeftDown:
        let dist = abs(dragLeftStartPos - mousePos);
        if dist.x + dist.y > dragThreshold:
          mouseLeftDrag = true
          mouseDragged(evt.motion)

      if mouseRightDown:
        let dist = abs(dragRightStartPos - mousePos);
        if dist.x + dist.y > dragThreshold:
          mouseRightDrag = true
          mouseDragged(evt.motion)

    else:
      discard

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  var mousePos : Vec2i

  discard getMouseState(mousePos.x.addr, mousePos.y.addr)

  if tileSelection:
    drawTiles(pixelToWorldSpace(vec2f(8), mousePos).floor.vec2i, tileSelectionMap, vec2f(8))
  else:
    drawTiles(pixelToWorldSpace(cameraPos, mousePos).floor.vec2i, map, cameraPos)


  drawCrosshair()

  glSwapWindow(window)
