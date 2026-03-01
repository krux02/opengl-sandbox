import ../fancygl, sdl2/sdl_image as img, times

from os import getLastModificationTime, fileExists

# this is supposded to take over the console example, but with actual rendering
proc setup(): void =
  glEnable(GL_DEPTH_TEST)
  #glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  glProvokingVertex(GL_FIRST_VERTEX_CONVENTION)

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

proc randomColor(): Color =
  result.r = rand_u8()
  result.g = rand_u8()
  result.b = rand_u8()
  result.a = rand_u8()

  

proc noiseTextureRectangle(size: Vec2i): TextureRectangle =
  var randomTiles = newSeq[Color](size.x * size.y)
  for tile in randomTiles.mitems:
    tile = randomColor()
    # tile.r = rand_u8()
    # tile.g = rand_u8()
    # tile.b = rand_u8()
    # tile.a = rand_u8()

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

var tileSelectionMap: TextureRectangle

var cameraPos = vec2f(0)

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

proc pixelToWorldSpace(this: TileMap; cameraPos: Vec2f; windowsize, pos: Vec2i): Vec2f =
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


var crosshairVertices: ArrayBuffer[Vec2f]

proc drawCrosshair(): void =

  if crosshairVertices.handle == 0:
    crosshairVertices = arrayBuffer([
      vec2f(-1,0), vec2f(1,0),
      vec2f(0,-1), vec2f(0,1)
    ])


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
var mouseLeftDown, mouseRightDown = false


var tileLeft  : Color = Color(r:255'u8, g:255'u8, b:255'u8, a:128'u8)
var tileRight : Color = Color(r:255'u8, g:255'u8, b:255'u8, a:0'u8)

proc mouseClicked(tileMap: TileMap, windowsize: Vec2i, evt: MouseButtonEventObj): void =
  let tileSizeLogical = tileMap.tileSizeLogical
  let scaling         = tileMap.scaling
  let mousePos = tileMap.pixelToWorldSpace(cameraPos, windowsize, vec2i(evt.x, evt.y))
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

proc main*(window: Window): void =  
  setup()
  
  tileSelectionMap = newTextureRectangle(vec2i(16), internalFormat = GL_RGBA8)

  var cursorPos: Vec2i
  cursorPos.y = mapwidth-1
  
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

  
  let windowsize = window.size

  var tileMap = newTileMap(vec2i(8,12), vec2i(8,12), 4, "pixelfont.png", window.size)

  let tileMapPath = "consoleLog.png"

  let loading = true
  var currentTextColor = randomColor()
  
  if fileExists(tileMapPath) and loading:
    tileMap.map = loadMapFromFile(tileMapPath)
  else:
    tileMap.map = noiseTextureRectangle(vec2i(mapwidth))
    
  tileMap.mapSize = vec2i(mapwidth)
  defer:
    saveMap(tileMap, tileMapPath)
    
  while running:
    defer:
      frame += 1

    for evt in events():
      case evt.kind:
      of QUIT:
        running = false
        break

      of KEY_DOWN:
        case evt.key.keysym.scancode:
        of SCANCODE_ESCAPE:
          running = false
          break
        of SCANCODE_F10:
          window.screenshot
        of SCANCODE_UP:
          cursorPos.y += 1
        of SCANCODE_DOWN:
          cursorPos.y -= 1
        of SCANCODE_LEFT:
          cursorPos.x -= 1
        of SCANCODE_RIGHT:
          cursorPos.x += 1
        of SCANCODE_RETURN:
          cursorPos.x = 0
          cursorPos.y -= 1
          currentTextColor = randomColor() # just for fun, no particular reason
        of SCANCODE_BACKSPACE:
          let color = Color(
            r: 255'u8,
            g: 255'u8,
            b: 255'u8, 
            a: 0
          )
          cursorPos.x -= 1
          if cursorPos.x >= 0:
            tileMap.map.setPixel(cursorPos, color)
          else:
            cursorPos.x = 0

        of SCANCODE_HOME:
          cursorPos.x = 0
          if (evt.key.keysym.mods and (uint16)KMOD_CTRL) != 0:
            cursorPos.y = 0
        of SCANCODE_END:
          cursorPos.x = tileMap.mapSize.x
          if (evt.key.keysym.mods and (uint16)KMOD_CTRL) != 0:
            cursorPos.y = tileMap.mapSize.y
        else:
          #echo (int)
          # tileMap.map.setPixel(cursorPos, (uint8)evt.key.keysym.sym)
          # cursorPos.x += 1
          # echo evt.key.keysym
          # echo evt.key
          discard

      of TextInput:
        let color = Color(
          r: currentTextColor.r,
          g: currentTextColor.g,
          b: currentTextColor.b,
          a: (uint8)evt.text.text[0]
        )
        var i = 0
        while evt.text.text[i] != (char)0:
          tileMap.map.setPixel(cursorPos, color)
          cursorPos.x += 1
          i += 1
        
      of TextEditing:
        echo evt.edit

      of MouseButtonUp:

        if evt.button.button == ButtonLeft:
          tileMap.mouseClicked(windowsize, evt.button)
        if evt.button.button == ButtonRight:
          tileMap.mouseClicked(windowsize, evt.button)

      of MouseMotion:
        let mousePos = vec2i(evt.motion.x, evt.motion.y)

      else:
        discard

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

    var mousePos : Vec2i

    discard getMouseState(mousePos.x.addr, mousePos.y.addr)

    let time = gameTimer.time

    cameraPos = vec2f(cursorPos) + vec2f(0.5f) # offset of 0.5 to center the middle of a tile

    tileMap.drawTiles(cursorPos, tileMap.map, cameraPos, time)

    # drawCrosshair()
    tileMap.resourceReloading()

    glSwapWindow(window)

when isMainModule:
  let (window, context) = defaultSetup()
  main(window)

# Local Variables:
# compile-command: "cd examples; nim c -r console2.nim"
# End:
