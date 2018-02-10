import ../fancygl
import glm/noise

let (window, context) = defaultSetup(vec2i(640,480))

glPolygonOffset( 1, 1);
glEnable(GL_POLYGON_OFFSET_FILL)


var boxPositions = newSeq[Vec3f](0)

for x in -20 .. 20:
  for z in -20 .. 20:
    let pos2d = vec2f(x.float32, z.float32)
    let n = perlin(pos2d * 0.2) * 3
    boxPositions.add vec3f(x.float32, n, z.float32)



let faceVertices = arrayBuffer(boxVertices)
let numFaceVertices = boxVertices.len

let outlineVertices = arrayBuffer([
  vec4f(-1, 1, 1,1),
  vec4f( 1, 1, 1,1),

  vec4f(-1,-1, 1,1),
  vec4f( 1,-1, 1,1),

  vec4f(-1, 1,-1,1),
  vec4f( 1, 1,-1,1),

  vec4f(-1,-1,-1,1),
  vec4f( 1,-1,-1,1),


  vec4f( 1, 1, 1,1),
  vec4f( 1,-1, 1,1),

  vec4f(-1, 1, 1,1),
  vec4f(-1,-1, 1,1),

  vec4f( 1, 1,-1,1),
  vec4f( 1,-1,-1,1),

  vec4f(-1, 1,-1,1),
  vec4f(-1,-1,-1,1),


  vec4f( 1, 1, 1,1),
  vec4f( 1, 1,-1,1),

  vec4f(-1, 1, 1,1),
  vec4f(-1, 1,-1,1),

  vec4f( 1,-1, 1,1),
  vec4f( 1,-1,-1,1),

  vec4f(-1,-1, 1,1),
  vec4f(-1,-1,-1,1),
])

let subLineVertices = arrayBuffer([
  vec4f(-1, 0, 1, 1),
  vec4f( 1, 0, 1, 1),
  vec4f(-1, 0,-1, 1),
  vec4f( 1, 0,-1, 1),
  vec4f(-1, 0, 1, 1),
  vec4f(-1, 0,-1, 1),
  vec4f( 1, 0, 1, 1),
  vec4f( 1, 0,-1, 1),
])

var groundGridVertices: ArrayBuffer[Vec4f]

block initGroundGridVertices:
  var buffer: seq[Vec4f] = @[]
  for i in -20..20:
    buffer.add vec4f( float32(i), 0, -20.0'f32, 1)
    buffer.add vec4f( float32(i), 0,  20.0'f32, 1)
    buffer.add vec4f(-20.0'f32 , 0, float32(i), 1)
    buffer.add vec4f( 20.0'f32 , 0, float32(i), 1)

  groundGridVertices = arrayBuffer(buffer)

let groundSurfaceVertices = arrayBuffer([
  vec4f(-20, 0,-20, 1),
  vec4f( 20, 0,-20, 1),
  vec4f( 20, 0, 20, 1),

  vec4f(-20, 0,-20, 1),
  vec4f( 20, 0, 20, 1),
  vec4f(-20, 0, 20, 1)
])

let numOutlineVertices = outlineVertices.len

var runGame: bool = true

let timer = newStopWatch(true)


const GridSize = 32

var volumeGrid: array[GridSize*GridSize*GridSize, byte]

import glm/noise

proc flatIndex(x,y,z: int): int =
  let x: int = int(cast[uint](x) and uint(GridSize-1))
  let y: int = int(cast[uint](y) and uint(GridSize-1))
  let z: int = int(cast[uint](z) and uint(GridSize-1))

  return x + GridSize * y + GridSize * GridSize * z


proc flatIndex(pos: Vec3i): int =
  let x: int = int(cast[uint](pos.x) and uint(GridSize-1))
  let y: int = int(cast[uint](pos.y) and uint(GridSize-1))
  let z: int = int(cast[uint](pos.z) and uint(GridSize-1))

  return x + GridSize * y + GridSize * GridSize * z


# init volume grid
for z in 0 ..< GridSize:
  for y in 0 ..< GridSize:
    for x in 0 ..< GridSize:
      let pos3D = vec3f(x.float32 + 0.5, y.float32 + 0.5, z.float32 + 0.5) * 0.1f
      let n = if perlin(pos3D) > 0: 1 else: 0
      let i = flatIndex(x,y,z)
      volumeGrid[i] = byte(n)


type
  AADirection = enum
    PositiveX
    NegativeX
    PositiveY
    NegativeY
    PositiveZ
    NegativeZ

converter glenum2aadirection(arg: GLenum): AADirection =
  case arg
  of GL_TEXTURE_CUBE_MAP_POSITIVE_X:
    return PositiveX
  of GL_TEXTURE_CUBE_MAP_NEGATIVE_X:
    return NegativeX
  of GL_TEXTURE_CUBE_MAP_POSITIVE_Y:
    return PositiveY
  of GL_TEXTURE_CUBE_MAP_NEGATIVE_Y:
    return NegativeY
  of GL_TEXTURE_CUBE_MAP_POSITIVE_Z:
    return PositiveZ
  of GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
    return NegativeZ
  else:
    assert(false, "incompative enum for glenum2aadirection")



var surfaceVertices = newSeq[Vec4f](0)

proc addSurface(position: Vec3i; dir: AADirection): void =
  case dir
  of PositiveX:
    surfaceVertices.add vec4f(1,0,0,1)
    surfaceVertices.add vec4f(1,1,0,1)
    surfaceVertices.add vec4f(1,1,1,1)
    surfaceVertices.add vec4f(1,0,1,1)
  of PositiveY:
    surfaceVertices.add vec4f(0,1,0,1)
    surfaceVertices.add vec4f(0,1,1,1)
    surfaceVertices.add vec4f(1,1,1,1)
    surfaceVertices.add vec4f(1,1,0,1)
  of PositiveZ:
    surfaceVertices.add vec4f(0,0,1,1)
    surfaceVertices.add vec4f(1,0,1,1)
    surfaceVertices.add vec4f(1,1,1,1)
    surfaceVertices.add vec4f(0,1,1,1)
  of NegativeX:
    surfaceVertices.add vec4f(0,0,0,1)
    surfaceVertices.add vec4f(0,1,0,1)
    surfaceVertices.add vec4f(0,1,1,1)
    surfaceVertices.add vec4f(0,0,1,1)
  of NegativeY:
    surfaceVertices.add vec4f(0,0,0,1)
    surfaceVertices.add vec4f(1,0,1,1)
    surfaceVertices.add vec4f(0,0,1,1)
    surfaceVertices.add vec4f(1,0,0,1)
  of NegativeZ:
    surfaceVertices.add vec4f(0,0,0,1)
    surfaceVertices.add vec4f(1,1,0,1)
    surfaceVertices.add vec4f(1,0,0,1)
    surfaceVertices.add vec4f(0,1,0,1)


for z in 0 ..< GridSize:
  for y in 0 ..< GridSize:
    for x in 0 ..< GridSize:
      let i0 = volumeGrid[flatIndex(x  ,y  ,z  )] != 0
      let ix = volumeGrid[flatIndex(x-1,y  ,z  )] != 0
      let iy = volumeGrid[flatIndex(x  ,y-1,z  )] != 0
      let iz = volumeGrid[flatIndex(x  ,y  ,z-1)] != 0

      if i0:
        if not ix:
          addSurface(vec3i(x-1, y   , z   ), PositiveX)
        if not iy:
          addSurface(vec3i(x  , y-1 , z   ), PositiveY)
        if not iz:
          addSurface(vec3i(x  , y   , z-1 ), PositiveZ)
      else:
        if ix:
          addSurface(vec3i(x, y, z), NegativeX)
        if iy:
          addSurface(vec3i(x, y, z), NegativeY)
        if iz:
          addSurface(vec3i(x, y, z), NegativeZ)


var oblique = mat4f(1)
oblique[2].xy = vec2f(-0.5f, -0.5f)

let proj : Mat4f = ortho(-10.0f, 10.0f, -7.5f, 7.5f, -128.0f, 128.0f) * oblique

while runGame:

  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_ESCAPE:
      runGame = false
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_F10:
      window.screenshot

  #let time = (frame / 100) * Pi * 2
  let time = timer.time.float32

  let viewMat = mat4f(1)
    .translate(0,0,0)            # position camera at position 0,1,5
    .inverse                     # the camera matrix needs to be inverted

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = groundGridVertices.len
    uniforms:
      modelViewProj = proj * viewMat
    attributes:
      a_vertex = groundGridVertices
    vertexMain:
      """
      gl_Position = modelViewProj * a_vertex;
      """
    fragmentMain:
      """
      color = vec4(vec3(0.5), 1.0);
      """


  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = 6
    uniforms:
      modelViewProj = proj * viewMat
    attributes:
      a_vertex = groundSurfaceVertices
    vertexMain:
      """
      gl_Position = modelViewProj * a_vertex;
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = vec4(vec3(0.0), 1.0);
      """

  for position in boxPositions:
    let modelMat = mat4f(1).translate(position).scale(0.5f).translate(1,1,1)
    let modelViewProj = proj * modelMat * viewMat


    shadingDsl:
      primitiveMode = GL_LINES
      numVertices = numOutlineVertices
      uniforms:
        modelViewProj
      attributes:
        a_vertex = outlineVertices
      vertexMain:
        """
        gl_Position = modelViewProj * a_vertex;
        """
      fragmentMain:
        """
        color = vec4(1.0);
        """

    shadingDsl:
      primitiveMode = GL_LINES
      numVertices = subLineVertices.len
      uniforms:
        modelViewProj
      attributes:
        a_vertex = subLineVertices
      vertexMain:
        """
        gl_Position = modelViewProj * a_vertex;
        """
      fragmentMain:
        """
        color = vec4(vec3(0.5), 1);
        """


    shadingDsl:
      primitiveMode = GL_TRIANGLES
      numVertices = numFaceVertices
      uniforms:
        modelViewProj
      attributes:
        a_vertex = faceVertices
      vertexMain:
        """
        gl_Position = modelViewProj * a_vertex;
        """
      vertexOut:
        "out vec4 v_color"
      fragmentMain:
        """
        color = vec4(vec3(0.0), 1.0);
        """


  glSwapWindow(window)

echo "done"
