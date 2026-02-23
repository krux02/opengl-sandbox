import ../fancygl, sdl2/sdl_image as img, times

let (window, context) = defaultSetup()

proc setup(): void =
  glEnable(GL_DEPTH_TEST)
  #glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  glProvokingVertex(GL_FIRST_VERTEX_CONVENTION)

var rogue = false
## change ``rogue`` to true if you want font tiles :P ( toggle by pressing 1 )

const numTiles = 256

proc updateTilePaletteFromFile(self: Texture2DArray; filename: string, tileSize: Vec2i): void =

  var surface = img.load(filename)
  if surface.isNil:
    let message = s"can't load texture $filename: ${img.getError()}"
    echo message
    surface = createErrorSurface(message)
  defer: freeSurface(surface)

  var rect: Rect
  rect.x = 0
  rect.y = 0
  rect.w = tileSize.x
  rect.h = tileSize.y

  var layerSurface = createRGBSurface(0, tileSize.x, tileSize.y, 32, 0,0,0,0)
  defer: freeSurface(layerSurface)

  let rows = (surface.h div tileSize.y)
  let cols = (surface.w div tileSize.x)

  var i = 0
  for y in 0 ..< rows:
    for x in 0 ..< cols:
      rect.x = cint(x) * tileSize.x
      rect.y = cint(y) * tileSize.y
      discard blitSurface(surface, rect.addr, layerSurface, nil)
      self.subImage(layerSurface, layer = i)
      i += 1



const dragThreshold = 4

proc noiseTextureRectangle(size: Vec2i, rogue: bool): TextureRectangle =
  var randomTiles = newSeq[Color](size.x * size.y)
  for tile in randomTiles.mitems:
    tile.r = if rogue: rand_u8() else: 255
    tile.g = if rogue: rand_u8() else: 255
    tile.b = if rogue: rand_u8() else: 255
    tile.a = rand_u8()

  result = newTextureRectangle(size, internalFormat = GL_RGBA8)
  result.setData(randomTiles)

proc loadMapFromFile(mapFilename: string): TextureRectangle =
  var surface = img.load(mapFilename)
  assert( not surface.isNil, s"can't load texture $mapFilename: ${fancygl.getError()}" )
  defer: freeSurface(surface)

  glCreateTextures(GL_TEXTURE_RECTANGLE, 1, result.handle.addr)
  glTextureStorage2D(result.handle, 1, GL_RGBA8, surface.w, surface.h)
  glTextureSubImage2D(
    result.handle, 0, 0, 0,
    surface.w, surface.h,
    GL_RGBA, GL_UNSIGNED_BYTE, surface.pixels)

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

var cameraPos = vec2f(512)
let windowsize = window.size


proc gridTrianglesPosition*(size: Vec2i; offset: Vec2f, tileSize: Vec2i, tileSizeLogical: Vec2i) : seq[Vec4f] =
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

type
  TileMap = object
    tileSize: Vec2i
    tileSizeLogical: Vec2i
    scaling: int32
    tilePalette: Texture2DArray
    tilePalettePath: string
    tilePaletteModificationTime: times.Time
    gridSize, gridHalfSize: Vec2f
    gridVertices: ArrayBuffer[Vec4f]
    gridVerticesLen: int
    scale: Vec2f
    map: TextureRectangle
    mapSize: Vec2i

const mapwidth  = 1024

from os import getLastModificationTime, fileExists

proc newTileMap(tileSize: Vec2i, tileSizeLogical: Vec2i, scaling: int32, tilePalettePath: string, windowSize: Vec2i): TileMap =
  result.tileSize = tileSize
  result.tileSizeLogical = tileSizeLogical
  result.scaling = scaling
  result.tilePalette = newTexture2DArray(tileSize, numTiles, levels = 1)
  result.tilePalettePath = getResourcePath(tilePalettePath)
  echo result.tilePalettePath
  result.tilePaletteModificationTime = getLastModificationTime(result.tilePalettePath)
  result.tilePalette.updateTilePaletteFromFile(result.tilePalettePath, result.tileSize)
  result.tilePalette.parameter(GL_TEXTURE_MIN_FILTER, GL_NEAREST)
  result.tilePalette.parameter(GL_TEXTURE_MAG_FILTER, GL_NEAREST)
  result.gridSize = vec2f(windowSize) / vec2f(result.tileSizeLogical * result.scaling)
  result.gridHalfSize = result.gridSize * 0.5
  # one extra row and column for scrolling and one, because the offset is floored
  result.gridVertices = arrayBuffer(gridTrianglesPosition(
    result.gridSize.vec2i + 3, floor(-result.gridHalfSize),
    result.tileSize, result.tileSizeLogical))
  result.gridVerticesLen = result.gridVertices.len
  result.scale = vec2f(tileSizeLogical * 2 * scaling) / vec2f(windowSize)
#let map = loadMapFromFile()


proc resourceReloading(this: var TileMap): void =
  let newTileMapModificationTime = getLastModificationTime(this.tilePalettePath)
  if this.tilePaletteModificationTime < newTileMapModificationTime:
    this.tilePalette.updateTilePaletteFromFile(this.tilePalettePath, this.tileSize)

proc saveMap(this: TileMap; mapFilename: string): void {.noconv.} =
  # this.map.saveToGrayscaleBmpFile(mapFilename)
  this.map.savePng(mapFileName)
#addQuitProc( saveMap )

proc pixelToWorldSpace(this: TileMap; cameraPos: Vec2f; pos: Vec2i): Vec2f =
  let tileSizeLogical = this.tileSizeLogical
  let scaling = this.scaling
  (-vec2f(windowsize) * 0.5 + vec2f(pos.x.float32, float32(windowsize.y - pos.y))) / vec2f(tileSizeLogical * scaling) + cameraPos

proc drawTiles(this: TileMap, highlightPos: Vec2i; map: TextureRectangle, cameraPos: Vec2f, time: float32): void =

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = this.gridVerticesLen

    uniforms:
      scale = this.scale
      cameraPos
      map
      tilePalette = this.tilePalette
      highlightPos
      time

    attributes:
      pos      = this.gridVertices

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
      if(highlight == 0) {
        if( color.rgb == vec3(1,0,1) ) {
          discard;
        } else {
          color.rgb *= tintColor;
        }
      } else {
        if( color.rgb == vec3(1,0,1) ) {
          if(fract((gl_FragCoord.x + gl_FragCoord.y) * 0.125 + time * 2) > 0.5)
            color = vec4(1,1,0,1);
          else
            discard;
        } else {
          color.rgb = sqrt(color.rgb);
        }
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
var gameTimer = newStopWatch(true)
var mouseLeftDrag = false
var mouseRightDrag = false
var mouseLeftDown, mouseRightDown = false
var dragLeftStartPos: Vec2i
var dragRightStartPos: Vec2i

setup()


var tileLeft  : Color = Color(r:255'u8, g:255'u8, b:255'u8, a:128'u8)
var tileRight : Color = Color(r:255'u8, g:255'u8, b:255'u8, a:0'u8)
var tileSelection : bool = false

proc mouseClicked(tileMap: TileMap, evt: MouseButtonEventObj): void =
  echo "mouseClicked"
  let tileSizeLogical = tileMap.tileSizeLogical
  let scaling         = tileMap.scaling
  if tileSelection:
    let mousePos = tileMap.pixelToWorldSpace(vec2f(8), vec2i(evt.x, evt.y))
    let gridPos = vec2i(mousePos.floor)
    let pixel: Color =
      Color(r: 255'u8, g: 255'u8, b: 255'u8,
            a: uint8(gridPos.x + (15 - gridPos.y) * 16))

    if evt.button == ButtonLeft:
      tileLeft = pixel
    elif evt.button == ButtonRight:
      tileRight = pixel
    else:
      return

    tileSelection = false
  else:
    let mousePos = tileMap.pixelToWorldSpace(cameraPos, vec2i(evt.x, evt.y))
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

    tileMap.map.setPixel(gridPos, pixel)

proc mouseDragged(tileMap: TileMap, evt: MouseMotionEventObj): void =
  if tileSelection:
    return

  if mouseLeftDown:
    let mousePos = tileMap.pixelToWorldSpace(cameraPos, vec2i(evt.x, evt.y))
    let gridPos = vec2i(mousePos.floor)

    if fancygl.any( gridPos .< vec2i(0) ) or fancygl.any(gridPos .>= vec2i(mapwidth)):
      return

    tileMap.map.setPixel(gridPos, tileLeft)

  if mouseRightDown:
    var movement : Vec2f
    movement.x =  evt.xrel / (tileMap.tileSizeLogical.x * tileMap.scaling)
    movement.y = -evt.yrel / (tileMap.tileSizeLogical.y * tileMap.scaling)
    cameraPos -= movement


proc main*(window: Window): void =

  var tileMapA = newTileMap(vec2i(16), vec2i(16), 2, "tiles.gif", window.size)
  var tileMapB = newTileMap(vec2i(8,12), vec2i(8,4), 4, "pixelfont.png", window.size)

  let pathA = "tileMapA.png"
  let pathB = "tileMapB.png"

  let loading = true
  
  # tileMapA.mapSize = vec2i(mapwidth)
  if fileExists(pathA) and loading:
    tileMapA.map = loadMapFromFile(pathA)
  else:
    tileMapA.map = noiseTextureRectangle(vec2i(mapwidth), false)
  tileMapA.mapSize = vec2i(mapwidth)

  if fileExists(pathB) and loading:
    tileMapB.map = loadMapFromFile(pathB)
  else:
    tileMapB.map = noiseTextureRectangle(vec2i(mapwidth), true)  
  tileMapB.mapSize = vec2i(mapwidth)

  defer:
    saveMap(tileMapA, pathA)
    saveMap(tileMapB, pathB)
    
  while running:
    defer:
      frame += 1

    let tileMap = if rogue: tileMapB else: tileMapA

    for evt in events():
      if evt.kind == QUIT:
        running = false
        break

      elif evt.kind == KEY_DOWN:
        case evt.key.keysym.scancode:
        of SCANCODE_ESCAPE:
          running = false
          break
        of SCANCODE_F10:
          window.screenshot
        of SCANCODE_1:
          rogue = not rogue
        of SCANCODE_SPACE:
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
            tileMap.mouseClicked(evt.button)

          mouseLeftDrag = false
          mouseLeftDown = false

        if evt.button.button == ButtonRight:
          if not mouseRightDown:
            tileMap.mouseClicked(evt.button)

          mouseRightDrag = false
          mouseRightDown = false

      elif evt.kind == MouseMotion:
        let mousePos = vec2i(evt.motion.x, evt.motion.y)

        if mouseLeftDown:
          let dist = abs(dragLeftStartPos - mousePos);
          if dist.x + dist.y > dragThreshold:
            mouseLeftDrag = true
            tileMap.mouseDragged(evt.motion)

        if mouseRightDown:
          let dist = abs(dragRightStartPos - mousePos);
          if dist.x + dist.y > dragThreshold:
            mouseRightDrag = true
            tileMap.mouseDragged(evt.motion)

      else:
        discard

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

    var mousePos : Vec2i

    discard getMouseState(mousePos.x.addr, mousePos.y.addr)

    let time = gameTimer.time

    if tileSelection:
      tileMap.drawTiles(tileMap.pixelToWorldSpace(vec2f(8), mousePos).floor.vec2i, tileSelectionMap, vec2f(8), time)
    else:
      tileMap.drawTiles(tileMap.pixelToWorldSpace(cameraPos, mousePos).floor.vec2i, tileMap.map, cameraPos, time)

    #drawCrosshair()
    tileMapA.resourceReloading()
    tileMapB.resourceReloading()

    glSwapWindow(window)

when isMainModule:
  # let (window, context) = defaultSetup()
  main(window)

# Local Variables:
# compile-command: "cd examples; nim c -r retro_tiling.nim"
# End:
