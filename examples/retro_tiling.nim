
import ../fancygl, sdl2/image


let (window, context) = defaultSetup()

glDisable(GL_DEPTH_TEST)

proc loadTilemapFromFile*(filename: string; tilesize: Vec2i): Texture2DArray =
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
const mapwidth  = 2048
      
let tilemap = loadTilemapFromFile("resources/tiles.gif", vec2i(tilewidth))
tilemap.parameter(GL_TEXTURE_MIN_FILTER, GL_NEAREST)
tilemap.parameter(GL_TEXTURE_MAG_FILTER, GL_NEAREST)

let map = newTexture2D(vec2i(mapwidth), GL_R8) # TODO make GL_R8UI work 
map.parameter(GL_TEXTURE_MIN_FILTER, GL_NEAREST)
map.parameter(GL_TEXTURE_MAG_FILTER, GL_NEAREST)

block:
  var randomTiles = newSeq[uint8](mapwidth * mapwidth)
  for tile in randomTiles.mitems:
    tile = rand_u8()
  map.setData(randomTiles)
                           
var cameraPos = vec2f(0)
let windowsize = window.size

proc screenToWorldSpace(x,y: int32): Vec2f =
  vec2f(x.float32, y.float32) / vec2f(tilewidth) + vec2f(windowsize) * 0.5 / vec2f(tilewidth) + cameraPos

proc gridTrianglesPosition*(size: Vec2i) : seq[Vec4f] =
  result = newSeqOfCap[Vec4f](size.x * size.y * 6)

  for i in 0 ..< (size.y - 1):
    for j in 0 ..< (size.x - 1):
      let y = float32(i)
      let x = float32(j)
      let
        a = vec4f(x    , y    , 0, 1)
        b = vec4f(x + 1, y    , 0, 1)
        c = vec4f(x    , y + 1, 0, 1)
        d = vec4f(x + 1, y + 1, 0, 1)

      result.add([c,a,d,a,b,d])

let gridVertices    = arrayBuffer(gridTrianglesPosition(windowsize div tilewidth + vec2i(2)))
let gridVerticesLen = gridVertices.len

let scale = vec2f(tilewidth * 2) / vec2f(windowsize)

proc drawTiles(): void =

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = gridVerticesLen

    uniforms:
      scale
      cameraPos
      map
      tilemap

    attributes:
      pos      = gridVertices

    includes:
      """
      vec2 texCoords[6] = vec2[6](vec2(0,0),vec2(0,1),vec2(1,0),vec2(0,1),vec2(1,1),vec2(1,0));
      """
    vertexMain:
      """
      
      tileId =  int(round(255 * texelFetch(map, ivec2(floor(pos.xy) + floor(cameraPos)), 0).r));
      gl_Position = vec4((pos.xy - fract(cameraPos)) * scale - 1, 0, 1);
      v_texCoord = texCoords[gl_VertexID % 6];
      """
    vertexOut:
      "out vec2 v_texCoord"
      "flat out int tileId"
    fragmentMain:
      """
      color = texture(tilemap, vec3(v_texCoord, tileId));
      if( color.rgb == vec3(1,0,1) )
        discard;
      """

var running = true
var frame = 0
#var gameTimer = newStopWatch(true)
var mouseDrag = false

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
      else:
        discard
    elif evt.kind == MouseButtonDown:
      if   evt.button.button == ButtonLeft:
        mouseDrag = true
      elif evt.button.button == ButtonRight:
        discard
    elif evt.kind == MouseButtonUp:
      if   evt.button.button == ButtonLeft:
        mouseDrag = false
      elif evt.button.button == ButtonRight:
        discard
        
    elif evt.kind == MouseMotion and mouseDrag:
      var movement : Vec2f
      movement.x =  evt.motion.xrel / tilewidth
      movement.y = -evt.motion.yrel / tilewidth
      cameraPos -= movement
    else:
      discard

  glClear(GL_COLOR_BUFFER_BIT)
  
  drawTiles()

  glSwapWindow(window)
  

  
