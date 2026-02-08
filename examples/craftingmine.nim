import ../fancygl

#let (window, context) = defaultSetup(vec2i(640,480))
let (window, context) = defaultSetup()

# totally not minecraft

glCullFace(GL_FRONT)

import glm/noise
import sdl2/sdl_image as img

var maxTextureSize: GlInt
glGetIntegerv(GL_MAX_TEXTURE_SIZE, maxTextureSize.addr)
echo "max texture size: ", maxTextureSize
discard setRelativeMouseMode(true)

var verticesSeq: seq[Vec4f]
var texCoordsSeq: seq[Vec3f]

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

let tilesTexture = newTexture2DArray(vec2i(32), 100, levels = 1)
tilesTexture.updateTilePaletteFromFile getResourcePath("craftingtiles.gif"), vec2i(32)
tilesTexture.parameter(GL_TEXTURE_MIN_FILTER, GL_NEAREST)
tilesTexture.parameter(GL_TEXTURE_MAG_FILTER, GL_NEAREST)

proc worldProc(pos: Vec3f): float =
  simplex(pos * 0.05 + 0.000123)

var verticesTmp: array[6, Vec4f]

const TileDataSize = 64
var tileData: array[TileDataSize, array[TileDataSize, array[TileDataSize, int8]]]

proc getTileData(i,j,k: int): int8 =
  if 0 <= i and i < TileDataSize and 0 <= j and j < TileDataSize and 0 <= k and k < TileDataSize:
    return tileData[i][j][k]
  else:
    return -1
                           

var idx = 0
for i in 0 ..< TileDataSize:
  for j in 0 ..< TileDataSize:
    for k in 0 ..< TileDataSize:
      let
        x = float32(i) + 0.5
        y = float32(j) + 0.5
        z = float32(k) + 0.5
        res = worldProc(vec3f(x,y,z))
      if worldProc(vec3f(x,y,z)) > 0:
        tileData[i][j][k] = 3
      else:
        tileData[i][j][k] = -1

for i in 1 ..< 63:
  for j in 1 ..< 63:
    for k in countdown(62,1):
      if tiledata[i][j][k] == 3:
        if tiledata[i][j][k+1] == -1:
          if k < 32:
            tiledata[i][j][k] = 7
          else:
            tiledata[i][j][k] = 0
        elif tiledata[i][j][k+1] == 0:
          tiledata[i][j][k] = 1
        elif tiledata[i][j][k+1] == 1:
          tiledata[i][j][k] = 13


let singleBoxVertices = arrayBuffer([
  vec4f(1, 0, 1, 1.0), vec4f(1, 1, 0, 1.0), vec4f(1, 0, 0, 1.0),
  vec4f(1, 1, 0, 1.0), vec4f(1, 0, 1, 1.0), vec4f(1, 1, 1, 1.0),
  vec4f(1, 1, 0, 1.0), vec4f(0, 1, 1, 1.0), vec4f(0, 1, 0, 1.0),
  vec4f(0, 1, 1, 1.0), vec4f(1, 1, 0, 1.0), vec4f(1, 1, 1, 1.0),
  vec4f(1, 0, 1, 1.0), vec4f(0, 0, 1, 1.0), vec4f(0, 1, 1, 1.0),
  vec4f(1, 0, 1, 1.0), vec4f(0, 1, 1, 1.0), vec4f(1, 1, 1, 1.0),
  vec4f(0, 1, 0, 1.0), vec4f(0, 0, 1, 1.0), vec4f(0, 0, 0, 1.0),
  vec4f(0, 0, 1, 1.0), vec4f(0, 1, 0, 1.0), vec4f(0, 1, 1, 1.0),
  vec4f(0, 0, 1, 1.0), vec4f(1, 0, 0, 1.0), vec4f(0, 0, 0, 1.0),
  vec4f(1, 0, 0, 1.0), vec4f(0, 0, 1, 1.0), vec4f(1, 0, 1, 1.0),
  vec4f(0, 0, 0, 1.0), vec4f(1, 0, 0, 1.0), vec4f(0, 1, 0, 1.0),
  vec4f(0, 1, 0, 1.0), vec4f(1, 0, 0, 1.0), vec4f(1, 1, 0, 1.0),
], label="singleBoxVertices")

let singleBoxTexCoords = arrayBuffer([
  vec2f(0,1), vec2f(1,0), vec2f(0,0),
  vec2f(1,0), vec2f(0,1), vec2f(1,1),
  vec2f(1,0), vec2f(0,1), vec2f(0,0),
  vec2f(0,1), vec2f(1,0), vec2f(1,1),
  vec2f(1,0), vec2f(0,0), vec2f(0,1),
  vec2f(1,0), vec2f(0,1), vec2f(1,1),
  vec2f(1,0), vec2f(0,1), vec2f(0,0),
  vec2f(0,1), vec2f(1,0), vec2f(1,1),
  vec2f(0,1), vec2f(1,0), vec2f(0,0),
  vec2f(1,0), vec2f(0,1), vec2f(1,1),
  vec2f(0,0), vec2f(1,0), vec2f(0,1),
  vec2f(0,1), vec2f(1,0), vec2f(1,1),
], label="singleBoxTexCoords")

var vertices: ArrayBuffer[Vec4f]
var texCoords: ArrayBuffer[Vec3f]

proc computeTileData() =
  verticesSeq.setLen 0
  texCoordsSeq.setLen 0
  for i in 1 ..< 63:
    for j in 1 ..< 63:
      for k in 1 ..< 63:
        let worldPos = vec3f(float32(i) + 0.5, float32(j) + 0.5, float32(k) + 0.5)
        var counter{.global.}: int  = 0
        inc counter
        let texZ = float32(tileData[i][j][k])
        if texZ >= 0:
          let
            x = float32(i) + 0.5
            y = float32(j) + 0.5
            z = float32(k) + 0.5
          if tileData[i+1][j][k] < 0:
            verticesSeq.add vec4f(x+0.5, y-0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y+0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y-0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y+0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y-0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y+0.5, z+0.5, 1.0)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(0,0,texZ)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(1,1,texZ)
          if tileData[i][j+1][k] < 0:
            verticesSeq.add vec4f(x+0.5, y+0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y+0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y+0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y+0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y+0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y+0.5, z+0.5, 1.0)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(0,0,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(1,1,texZ)
          if tileData[i][j][k+1] < 0:
            verticesSeq.add vec4f(x+0.5, y-0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y-0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y+0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y-0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y+0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y+0.5, z+0.5, 1.0)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(0,0,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(1,1,texZ)
          if tileData[i-1][j][k] < 0:
            verticesSeq.add vec4f(x-0.5, y+0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y-0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y-0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y-0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y+0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y+0.5, z+0.5, 1.0)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(0,0,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(1,1,texZ)
          if tileData[i][j-1][k] < 0:
            verticesSeq.add vec4f(x-0.5, y-0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y-0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y-0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y-0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y-0.5, z+0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y-0.5, z+0.5, 1.0)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(0,0,texZ)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(1,1,texZ)
          if tileData[i][j][k-1] < 0:
            verticesSeq.add vec4f(x-0.5, y-0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y-0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y+0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x-0.5, y+0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y-0.5, z-0.5, 1.0)
            verticesSeq.add vec4f(x+0.5, y+0.5, z-0.5, 1.0)
            texCoordsSeq.add vec3f(0,0,texZ)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(0,1,texZ)
            texCoordsSeq.add vec3f(1,0,texZ)
            texCoordsSeq.add vec3f(1,1,texZ)


  if vertices.handle > 0:
    delete vertices
  if texCoords.handle > 0:
    delete texCoords

  vertices = arrayBuffer(verticesSeq)
  texCoords = arrayBuffer(texCoordsSeq)

computeTileData()

let (waterVertices, waterVerticesLen) = (block:
  var dataSeq: seq[Vec4f]
  for i in 0 .. 64:
    let x = float32(i)
    dataSeq.add vec4f(x,0, 31.872,1)
    for j in 0 .. 64:
      let y = float32(j)
      dataSeq.add vec4f(x,y, 31.872,1)
      dataSeq.add vec4f(x+1,y, 31.872,1)
    dataSeq.add vec4f(x+1,64, 31.872,1)
  (arrayBuffer(dataSeq), dataSeq.len)
)

let cubeVertices = arrayBuffer([
  vec4f( 1, 1, 1, 1), vec4f( 0, 1, 1, 1), vec4f( 1, 0, 1, 1), vec4f( 0, 0, 1, 1),
  vec4f( 1, 1, 0, 1), vec4f( 0, 1, 0, 1), vec4f( 1, 0, 0, 1), vec4f( 0, 0, 0, 1)
])

let cubeLineIndices = elementArrayBuffer([
  0'i8,1, 2,3, 4,5, 6,7,
  0,2, 1,3, 4,6, 5,7,
  0,4, 1,5, 2,6, 3,7
])

let cubeLineIndicesLen = cubeLineIndices.len
var runGame: bool = true
let timer = newStopWatch(true)
let aspect = window.aspectRatio.float32
let projMat : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

type
  Player = object
    node: WorldNode
    activeTile: int8

var
  player: Player
  cameraControls = CameraControls(speed: 0.04'f32)
  onGround: bool


addEventWatch(cameraControlEventWatch, cameraControls.addr)
player.node.pos = vec4f(9.803473472595215, 32.1359748840332, 32.0, 1.0)
player.activeTile = -1

proc clampToTileDataSize(node: var WorldNode): void =
  node.pos = clamp(node.pos, vec4f(0.5f), vec4f(float(TileDataSize)-0.5f))
  

proc snapToGround(p: var Player): void =
  var p = vec4i(floor(player.node.pos))

  while p.z > 0 and getTileData(p.x, p.y, p.z-1) < 0:
    dec p.z
  while p.z < 64 and getTileData(p.x, p.y, p.z) >= 0:
    inc p.z

  var p1 = vec3i(floor(player.node.pos.xyz - vec3f(0.5, 0.5, 0)))
  var p2 = p1 + vec3i(1,0,0)
  var p3 = p1 + vec3i(0,1,0)
  var p4 = p1 + vec3i(1,1,0)

  while p1.z > 0 and getTileData(p1.x, p1.y, p1.z-1) < 0:
    dec p1.z
  while p1.z < 64 and getTileData(p1.x, p1.y, p1.z) >= 0:
    inc p1.z
  while p2.z > 0 and getTileData(p2.x, p2.y, p2.z-1) < 0:
    dec p2.z
  while p2.z < 64 and getTileData(p2.x, p2.y, p2.z) >= 0:
    inc p2.z
  while p3.z > 0 and getTileData(p3.x, p3.y, p3.z-1) < 0:
    dec p3.z
  while p3.z < 64 and getTileData(p3.x, p3.y, p3.z) >= 0:
    inc p3.z
  while p4.z > 0 and getTileData(p4.x, p4.y, p4.z-1) < 0:
    dec p4.z
  while p4.z < 64 and getTileData(p4.x, p4.y, p4.z) >= 0:
     inc p4.z

  var f = fract(player.node.pos.xy - vec2f(0.5))
  f = f*f*(3.0f-2.0f*f)
  let tmp1 = mix(float32(p1.z), float32(p2.z), f.x)
  let tmp2 = mix(float32(p3.z), float32(p4.z), f.x)
  let tmp3 = mix(tmp1, tmp2, f.y)

  player.node.pos.z = tmp3

type
  Ray = object
    pos: Vec3f
    dir: Vec3f

import options

proc raytracer( ray:Ray, top:bool, distance:float32): Option[Vec3i] =
  let distanceSq = distance * distance
  var srcPos = vec3f(ray.pos)

  for i in 0 ..< 3:
    if srcPos[i] == floor(srcPos[i]):
      srcPos[i] += 0.000001

  let t = ray.dir

  var pos = vec3i(floor(srcPos))
  let step = vec3i(sign(ray.dir))
  var tMax = vec3f(0)
  var tDelta = vec3f(0)

  tMax.x = if step.x == 1: (ceil(srcPos.x)-srcPos.x)/abs(t.x) else: (srcPos.x-floor(srcPos.x))/abs(t.x)
  tMax.y = if step.y == 1: (ceil(srcPos.y)-srcPos.y)/abs(t.y) else: (srcPos.y-floor(srcPos.y))/abs(t.y)
  tMax.z = if step.z == 1: (ceil(srcPos.z)-srcPos.z)/abs(t.z) else: (srcPos.z-floor(srcPos.z))/abs(t.z)

  tDelta.x = 1/abs(t.x)
  tDelta.y = 1/abs(t.y)
  tDelta.z = 1/abs(t.z)

  var h = getTileData(pos.x, pos.y, pos.z)
  var i = 0

  var axis = 0

  while h < 0:
    if tMax.x < tMax.y:
      if tMax.x < tMax.z:
        axis = 0
        pos.x += step.x
        tMax.x += tDelta.x
      else:
        axis = 2
        pos.z += step.z;
        tMax.z += tDelta.z;
    else:
      if tMax.y < tMax.z:
        axis = 1
        pos.y += step.y;
        tMax.y += tDelta.y;
      else:
        axis = 2
        pos.z += step.z;
        tMax.z += tDelta.z;


    # early exit for out of range
    if pos.x notin 0..63 or pos.y notin 0..63 or pos.z notin 0..63:
      return
    if distanceSq < length2((vec3f(pos) + 0.5f) - ray.pos):
      return

    h = getTileData(pos.x,pos.y,pos.z)
    i += 1

  if top:
    pos[axis] -= step[axis]
  if h >= 0:
    result = some(pos)

var flymode = false

# glDisable(GL_CLIP_DISTANCE0)
glEnable(GL_DEPTH_CLAMP)

proc drawCubeLines(mvp: Mat4f; color: Vec4f) =
  shadingDsl:
    primitiveMode = GL_LINES
    indices = cubeLineIndices
    numVertices = cubeLineIndicesLen
    uniforms:
      mvp
      lineColor = color
    attributes:
      a_vertex = cubeVertices
    vertexMain:
      """
      gl_Position = mvp * a_vertex;
      """
    fragmentMain:
      """
      color = lineColor;
      """


proc drawCube(mvp: Mat4f; tileId: float32; tint: Vec4f) =
  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = 36
    uniforms:
      mvp
      tileId
      tex = tilesTexture
      tint
    attributes:
      a_vertex = singleBoxVertices
      a_texCoord = singleBoxTexCoords
    vertexMain:
      """
      gl_Position = mvp * a_vertex;
      v_texCoord = a_texCoord;
      """
    vertexOut:
      "out vec2 v_texCoord"
    fragmentMain:
      """
      color = texture(tex, vec3(v_texCoord, tileId)) * tint;
      """

while runGame:
  var mouseClicked = false
  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_ESCAPE:
      runGame = false
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_F10:
      window.screenshot
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_SPACE:
      flymode = not flymode
    if evt.kind == MOUSE_BUTTON_DOWN:
      if evt.button.button == 1:
        mouseClicked = true

  update(player.node, cameraControls)
  if not flymode:
    clampToTileDataSize(player.node)
    player.snapToGround

  let time: float64 = timer.time
  var cameraNode = player.node;
  cameraNode.moveAbsolute(vec3f(0, 0, 1.7))
  let ray = Ray(pos: cameraNode.pos.xyz, dir: cameraNode.dirVec.xyz)


  let mouseTarget = raytracer(ray, player.activeTile >= 0 , 5)

  if mouseClicked and mouseTarget.isSome:
    if player.activeTile >= 0: # placement mode      
      let p = mouseTarget.get
      # echo "mouse target: ", mouseTarget, " tile: ",  tileData[p.x][p.y][p.z]
      tileData[p.x][p.y][p.z] = player.activeTile
      player.activeTile = -1
      computeTileData()
    else: # picking mode  
      let p = mouseTarget.get
      # echo "mouse target: ", mouseTarget, " tile: ",  tileData[p.x][p.y][p.z]
      player.activeTile = tileData[p.x][p.y][p.z]
      tileData[p.x][p.y][p.z] = -1
      computeTileData()

  let viewMat = cameraNode.viewMat()
  let mvp = projMat * viewMat# * modelMat


  let iPos = vec3i(floor(cameraNode.pos.xyz))
  if getTileData(iPos.x, iPos.y, iPos.z) >= 0:
    # camera is stuck in wall
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glDisable(GL_DEPTH_TEST)
    shadingDsl:
      uniforms:
        tex = tilesTexture
        texId = float32(tileData[iPos.x][iPos.y][iPos.z])
        windowsize = vec2f(window.size)
      fragmentMain:
        """
        color = texture(tex, vec3(gl_FragCoord.xy / windowsize, texId));
        // color = vec4(1,0,1,1);
        """
  else:
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
    glDisable(GL_BLEND)
    glEnable(GL_CULL_FACE)
    glEnable(GL_DEPTH_TEST)

    shadingDsl:
      primitiveMode = GL_TRIANGLES
      numVertices = verticesSeq.len * 3
      uniforms:
        mvp
        viewMat
        tex = tilesTexture
      attributes:
        a_vertex = vertices
        a_texCoord = texCoords
      vertexMain:
        """
        gl_Position = mvp * a_vertex;
        v_texCoord = a_texCoord;
        v_distance = length((viewMat * a_vertex).xyz);
        v_height = a_vertex.z;
        """
      vertexOut:
        "out vec3 v_texCoord"
        "out float v_distance"
        "out float v_height"
      fragmentMain:
        """
        color = texture(tex, v_texCoord);
        color *= min(1, (32.0 - v_distance) * 0.125);
        color *= clamp((v_height - 27) * 0.2, 0,1);
        """

    
    if player.activeTile >= 0:
      let pos = player.node.pos.xyz + vec3f(0,0,3)
      drawCube(mvp.translate(pos), float32(player.activeTile), vec4f(0.5f))

    glDisable(GL_CULL_FACE)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    shadingDsl:
      primitiveMode = GL_TRIANGLE_STRIP
      numVertices = waterVerticesLen
      uniforms:
        mvp
        viewMat
        tex = tilesTexture
      attributes:
        a_vertex = waterVertices
      vertexMain:
        """
        gl_Position = mvp * a_vertex;
        v_texCoord = vec3(a_vertex.xy, 10.0);
        v_distance = length((viewMat * a_vertex).xyz);
        gl_Position = mvp * a_vertex;
        v_distance = length(viewMat * a_vertex);
        """
      vertexOut:
        "out vec3 v_texCoord"
        "out float v_distance"
      fragmentMain:
        """
        color = texture(tex, v_texCoord);
        color *= min(1, (32.0 - v_distance) * 0.125);
        color.a = 0.5;
        """

    glDisable(GL_DEPTH_TEST)
    if mouseTarget.isSome:
      let pos = vec3f(mouseTarget.get)
      let cubeMVP = mvp.translate(vec3f(pos))
      if player.activeTile >= 0:
        let cubeTint = vec4f(0.5f)
        drawCube(cubeMVP, float32(player.activeTile), cubeTint)
        let lineColor = vec4f(1,1,0,0.5f)
        drawCubeLines(cubeMVP, lineColor)
      else:
        let lineColor = vec4f(1)
        drawCubeLines(cubeMVP, lineColor)
        
  glSwapWindow(window)

echo "done"
