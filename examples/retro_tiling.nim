
import ../fancygl, sdl2/image


let (window, context) = defaultSetup()

proc `$`(v: Vec): string =
  fancygl.`$`(v)

proc setup(): void =
  glDisable(GL_DEPTH_TEST)
  #glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  glProvokingVertex(GL_FIRST_VERTEX_CONVENTION)

proc loadTilemapFromFile*(filename: string; tilesize: Vec2i): Texture2DArray =
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
  
  result = newTexture2DArray(tilesize, rows * cols)

  var i = 0
  for y in 0 ..< rows:
    for x in 0 ..< cols:
      rect.x = cint(x) * tilesize.x
      rect.y = cint(y) * tilesize.y
      blitSurface(surface, rect.addr, layerSurface, nil)
      result.subImage(layerSurface, layer = i)
      i += 1

const tilewidth = 16
const mapwidth  = 1024
const scaling   = 4

let tilemap = loadTilemapFromFile("resources/tiles.gif", vec2i(tilewidth))
tilemap.parameter(GL_TEXTURE_MIN_FILTER, GL_NEAREST)
tilemap.parameter(GL_TEXTURE_MAG_FILTER, GL_NEAREST)

let map = newTextureRectangle(vec2i(mapwidth), internalFormat = GL_R8)
# TODO make GL_R8UI work

let tileSelectionMap = newTextureRectangle(vec2i(16), internalFormat = GL_R8)

block:
  var randomTiles = newSeq[uint8](mapwidth * mapwidth)
  for tile in randomTiles.mitems:
    tile = rand_u8()
    
  map.setData(randomTiles)

  var selectionTiles = newSeq[uint8](16 * 16)
  for i, tile in selectionTiles.mpairs:
    let x = i mod 16
    let y = 15 - i div 16
    tile = uint8(x + y * 16)

  tileSelectionMap.setData(selectionTiles)
    
                           
var cameraPos = vec2f(0) # vec2f(mapwidth) * 0.5
let windowsize = window.size

proc pixelToWorldSpace(cameraPos: Vec2f; pos: Vec2i): Vec2f =
  (-vec2f(windowsize) * 0.5 + vec2f(pos.x.float32, float32(windowsize.y - pos.y))) / vec2f(tilewidth * scaling) + cameraPos

proc gridTrianglesPosition*(size: Vec2i; offset: Vec2f) : seq[Vec2f] =
  result = newSeqOfCap[Vec2f](size.x * size.y * 6)

  for i in 0 ..< size.y:
    for j in 0 ..< size.x:
      let pos = vec2f(float32(j),float32(i)) + offset
      let
        a = pos + vec2f(0,0)
        b = pos + vec2f(1,0)
        c = pos + vec2f(0,1)
        d = pos + vec2f(1,1)

      result.add([a,d,c,a,b,d])

let gridSize     = vec2f(windowsize) / vec2f(tilewidth * scaling)
let gridHalfSize = gridSize * 0.5

# one extra row and column for scrolling and one, because the offset is floored
let gridVertices    = arrayBuffer(gridTrianglesPosition(gridSize.vec2i + 3, floor(-gridHalfSize)))
let gridVerticesLen = gridVertices.len

let scale = vec2f(tilewidth * 2 * scaling) / vec2f(windowsize)

proc drawTiles(highlightPos: Vec2i; map: TextureRectangle; cameraPos: Vec2f): void =

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = gridVerticesLen

    uniforms:
      scale
      cameraPos
      map
      tilemap
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
      
      tileId =  int(round(255 * texelFetch(map, gridPos).r));
      gl_Position = vec4((pos - fract(cameraPos)) * scale, 0, 1);
      v_texCoord = texCoords[gl_VertexID % 6];
      """
    vertexOut:
      "out vec2 v_texCoord"
      "flat out int tileId"
      "flat out int highlight"
    fragmentMain:
      """
      color = texture(tilemap, vec3(v_texCoord, tileId));
      if( color.rgb == vec3(1,0,1) )
        discard;
      if(highlight != 0) {
        color.rgb = vec3(1) - color.gbr;
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
var mouseDrag = false
var mouseDown = false

setup()


var tileLeft : uint8 = 128
var tileRight : uint8 = 0

var tileSelection : bool = false

proc mouseClicked(evt: MouseButtonEventPtr): void =
  if tileSelection:
    let mousePos = pixelToWorldSpace(vec2f(8), vec2i(evt.x, evt.y))
    let gridPos = vec2i(mousePos.floor)
    let pixel: uint8 = uint8(gridPos.x + (15 - gridPos.y) * 16)

    if evt.button == ButtonLeft:
      tileLeft = pixel
    elif evt.button == ButtonRight:
      tileRight = pixel
    else:
      return
    
    
  else:
    let mousePos = pixelToWorldSpace(cameraPos, vec2i(evt.x, evt.y))
    let gridPos = vec2i(mousePos.floor)

    if fancygl.any( gridPos .< vec2i(0) ) or fancygl.any(gridPos .>= vec2i(mapwidth)):
      return

    var pixel : uint8
    if evt.button == ButtonLeft:
      pixel = tileLeft
    elif evt.button == ButtonRight:
      pixel = tileRight
    else:
      return

    map.setPixel(gridPos, pixel)

proc mouseDragged(evt: MouseMotionEventPtr): void =
  var movement : Vec2f
  movement.x =  evt.xrel / (tilewidth * scaling)
  movement.y = -evt.yrel / (tilewidth * scaling)
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
      mouseDown = true
        
    elif evt.kind == MouseButtonUp:
      if not mouseDrag:
        mouseClicked(evt.button)
      mouseDown = false
      mouseDrag = false
        
    elif evt.kind == MouseMotion and mouseDown:
      mouseDrag = true
      mouseDragged(evt.motion)
    else:
      discard

  glClear(GL_COLOR_BUFFER_BIT)

  var mousePos : Vec2i

  discard getMouseState(mousePos.x.addr, mousePos.y.addr)

  if tileSelection:
    drawTiles(pixelToWorldSpace(vec2f(8), mousePos).floor.vec2i, tileSelectionMap, vec2f(8))
  else:
    drawTiles(pixelToWorldSpace(cameraPos, mousePos).floor.vec2i, map, cameraPos)
  
  
  drawCrosshair()

  glSwapWindow(window)
  

  
