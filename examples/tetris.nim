import ../fancygl

import sequtils, algorithm

let (window, _) = defaultSetup()
let windowsize = window.size

let projection_mat : Mat4f = perspective(45'f32, windowsize.x / windowsize.y, 0.1, 100.0)

type
  SimpleMesh = object
    vertices,normals,colors: ArrayBuffer[Vec4f]
    indices: ElementArrayBuffer[int16]

var camera = newWorldNode()
camera.pos.xyz = vec3f(0,0,30)
camera.lookAt(vec3f(0.0001,0.0002,0.0001), vec3f(0,1,0))

let colorsArray = [
  vec4f(1,0,0,1),
  vec4f(0,1,0,1),
  vec4f(0,0,1,1),
  vec4f(1,1,0,1),
  vec4f(0,1,1,1),
  vec4f(1,0,1,1),
  vec4f(0.5)
]

#
#    ()
#  ()XX()
#
#  ()
#  ()xx()
#
#      ()
#  ()xx()
#
#  ()XX
#    ()()
#
#    XX()
#  ()()
#
#    XX()
#    ()()
#
#    ()
#    XX
#    ()
#    ()
#

const piecesArray = [
  [vec2i(-1, 0), vec2i( 1, 0), vec2i( 0, 1)],
  [vec2i(-1, 0), vec2i( 1, 0), vec2i(-1, 1)],
  [vec2i(-1, 0), vec2i( 1, 0), vec2i( 1, 1)],
  [vec2i(-1, 0), vec2i( 0,-1), vec2i( 1,-1)],
  [vec2i( 1, 0), vec2i( 0,-1), vec2i(-1,-1)],
  [vec2i( 1, 0), vec2i( 0,-1), vec2i( 1,-1)],
  [vec2i( 0, 1), vec2i( 0,-1), vec2i( 0,-2)]
]


# BUG: why does this not work with const?
let rotationsArray = [
  mat2i(vec2i( 1, 0), vec2i( 0, 1)),
  mat2i(vec2i( 0, 1), vec2i(-1, 0)),
  mat2i(vec2i(-1, 0), vec2i( 0,-1)),
  mat2i(vec2i( 0,-1), vec2i( 1, 0))
]

var icosphereMesh: SimpleMesh

block init:

  let isNumVerts = icosphereIndicesTriangles.len
  var unrolledVertices = newSeqOfCap[Vec4f](isNumVerts)
  var unrolledColors = newSeqOfCap[Vec4f](isNumVerts)
  var unrolledNormals = newSeqOfCap[Vec4f](isNumVerts)

  for i in countup(0, icosphereIndicesTriangles.len-1, 3):
    var normal : Vec4f
    for j in 0 ..< 3:
      let idx = icosphereIndicesTriangles[i+j]
      let v = icosphereVertices[idx]
      unrolledVertices.add v
      normal += v

    # averageing vertex positions of a face, to get face normals,
    # really only works for spherical meshes, where the xyz components
    # of the normal and the point, is equal.
    normal.w = 0
    normal = normalize(normal)
    unrolledNormals.add([normal,normal,normal])

    let color = vec4f(rand_f32(), rand_f32(), rand_f32(), 1'f32)
    unrolledColors.add([color,color,color])

  icosphereMesh.vertices = arrayBuffer(unrolledVertices)
  icosphereMesh.colors   = arrayBuffer(unrolledColors)
  icosphereMesh.normals  = arrayBuffer(unrolledNormals)
  icosphereMesh.indices  = elementArrayBuffer(iotaSeq[int16](unrolledVertices.len.int16))

  glDisable(GL_DEPTH_CLAMP)

let icosphereVerticesLen = icosphereMesh.vertices.len

var planeVertices = arrayBuffer([
  vec4f(0,0,0,1), vec4f( 1, 0,0,0), vec4f( 0, 1,0,0),
  vec4f(0,0,0,1), vec4f( 0, 1,0,0), vec4f(-1, 0,0,0),
  vec4f(0,0,0,1), vec4f(-1, 0,0,0), vec4f( 0,-1,0,0),
  vec4f(0,0,0,1), vec4f( 0,-1,0,0), vec4f( 1, 0,0,0)
])

var planeNode = newWorldNode()

var evt: Event = defaultEvent
var runGame: bool = true

var frame = 0

const NumRows = 24
const NumCols = 10

var fieldRows: array[NumRows, array[NumCols, int]]

proc fieldRead(pos: Vec2i): int =
  if 0 <= pos.x and 0 <= pos.y and pos.x < NumCols:
    if pos.y < NumRows:
      fieldRows[pos.y][pos.x]
    else:
      -1
  else:
    0x7fffffff

for y in 0 ..< NumRows:
  fieldRows[y].fill(-1)

for y in 0 ..< NumRows div 2:
  for x in 0 ..< NumCols:
    fieldRows[y][x] = max(rand(int32(colorsArray.len * 2)) - colorsArray.len, -1)

let positionsBuffer = newArrayBuffer[Vec4f](NumRows*NumCols)
let colorsBuffer    = newArrayBuffer[Vec4f](NumRows*NumCols)

var noiseArray: array[21, float32]
for x in noiseArray.mitems:
  x = (rand_f32()*2-1) * 0.01f;


const startBlockPos = vec2i(NumCols div 2, NumRows - 1)
const nextBlockPos  = vec2i(NumCols + 5, NumRows - 5)

var nextBlockType = 0
var nextBlockRot  = 0

var blockPos: Vec2i
var blockType: int
var blockRot: int

proc callNextBlock(): void =
  blockPos  = startBlockPos
  blockType = nextBlockType
  blockRot  = nextBlockRot

  nextBlockType = rand_i32() mod piecesArray.len
  nextBlockRot      = rand_i32() mod 3

callNextBlock()
callNextBlock()

iterator blockPositions(pos: Vec2i, rot,typ: int): Vec2i =
  yield pos
  for rawOffset in piecesArray[typ]:
    yield pos + rotationsArray[rot] * rawOffset

proc validBlockPos(pos: Vec2i, rot,typ: int): bool =
  for npos in blockPositions(pos,rot,typ):
    if fieldRead(npos) > -1:
      return false

  return true

while runGame:
  frame += 1

  ####################
  # Input Processing #
  ####################

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SDL_SCANCODE_ESCAPE:
        runGame = false
        break

      of SDL_SCANCODE_I, SDL_SCANCODE_UP:
        let offset = vec2i(0,1)
        if validBlockPos(blockPos + offset, blockRot, blockType):
          blockPos += offset
      of SDL_SCANCODE_J, SDL_SCANCODE_LEFT:
        let offset = vec2i(-1,0)
        if validBlockPos(blockPos + offset, blockRot, blockType):
          blockPos += offset
      of SDL_SCANCODE_K, SDL_SCANCODE_DOWN:
        let offset = vec2i(0,-1)
        if validBlockPos(blockPos + offset, blockRot, blockType):
          blockPos += offset
      of SDL_SCANCODE_L, SDL_SCANCODE_RIGHT:
        let offset = vec2i(1,0)
        if validBlockPos(blockPos + offset, blockRot, blockType):
          blockPos += offset

      of SDL_SCANCODE_O:
        let nBlockRot = (blockRot - 1) and 3
        if validBlockPos(blockPos, nBlockRot, blockType):
          blockRot = nBlockRot

      of SDL_SCANCODE_U:
        let nBlockRot = (blockRot + 1) and 3
        if validBlockPos(blockPos, nBlockRot, blockType):
          blockRot = nBlockRot

      of SDL_SCANCODE_SPACE:
        let offset = vec2i(0, -1)
        while validBlockPos(blockPos + offset, blockRot, blockType):
          blockPos += offset

        for pos in blockPositions(blockPos, blockRot, blockType):
          fieldRows[pos.y][pos.x] = blockType

        callNextBlock()

      of SDL_SCANCODE_F10:
        window.screenshot

      else:
        discard

  ####################
  # apply game logic #
  ####################

  # remove full lines

  for y in 0 ..< NumRows:
    while fieldRows[y].find(-1) == -1:
      fieldRows[y].fill(-1)
      for y2 in y+1 ..< NumRows:
        swap(fieldRows[y2-1], fieldRows[y2])

  ###############
  # Render Code #
  ###############

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  #proc renderShape(node: WorldNode, vertices, normals, texcoords: ArrayBuffe[Vec4f])

  iterator allBlockPositions(): tuple[pos:Vec2i; typ: int] =
    var counter = 0
    for y, row in fieldRows:
      for x, value in row:
        let tile = fieldRows[y][x]
        if tile > -1:
          yield((pos: vec2i(x,y), typ: tile))
          counter += 1

    for pos in blockPositions(blockPos, blockRot, blockType):
      yield((pos: pos, typ: blockType))
      counter += 1

    for pos in blockPositions(nextBlockPos, nextBlockRot, nextBlockType):
      yield((pos: pos, typ: nextBlockType))
      counter += 1

  var numPositions = 0
  positionsBuffer.mapWriteBlock:
    colorsBuffer.mapWriteBlock:

      for pos, typ in allBlockPositions():
        let posx = float32(pos.x - NumCols div 2)
        let posy = float32(pos.y - NumRows div 2)
        positionsBuffer[numPositions] = vec4f(posx, posy, 0, 1)
        colorsBuffer[numPositions] = colorsArray[typ]
        numPositions += 1

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices   = icosphereVerticesLen
    indices       = icosphereMesh.indices
    numInstances  = numPositions

    uniforms:
      proj = projection_mat
      modelView = camera.viewMat

    attributes:
      a_vertex   = icosphereMesh.vertices
      a_normal   = icosphereMesh.normals
      a_texCoord = icosphereMesh.colors

      instanceData:
        objectPos   = positionsBuffer
        objectColor = colorsBuffer


    vertexMain:
      """
      gl_Position = proj * modelView * vec4(a_vertex.xyz * 0.25 + objectPos.xyz, 1);
      v_normal = modelView * a_normal;
      v_Color = vec4(a_texCoord.x + a_texCoord.y) * objectColor;
      """
    vertexOut:
      "out vec4 v_normal"
      "out vec4 v_Color"

    fragmentMain:
      """
      // cheap fake lighting from camera direction
      color = v_Color * v_normal.z;
      """

  let modelViewProj = projection_mat * camera.viewMat * planeNode.modelMat

  # shapes with infinitely far away points, can't interpolate alon the vertices,
  # therefore so varyings don't work.
  # The matrix transformation of can be inverted in the fragment shader, so that that in this case
  # object space coordinates can be recontructed.

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = planeVertices.len
    uniforms:
      modelViewProj
      invModelViewProj = inverse(modelViewProj)
      invWindowSize    = vec2f(1 / float32(windowSize.x), 1 / float32(windowSize.y))

    attributes:
      a_vertex   = planeVertices

    vertexMain:
      """
      gl_Position = modelViewProj * a_vertex;
      """

    fragmentMain:
      """
      color = vec4(1,0,1,0);
      vec4 tmp = gl_FragCoord;

      // reconstructing normalized device coordinates from fragment depth, fragment position.
      vec4 ndc_pos;
      ndc_pos.xy = gl_FragCoord.xy * invWindowSize * 2 - 1;
      ndc_pos.z  = gl_FragCoord.z                  * 2 - 1;
      ndc_pos.w = 1;

      // coordinate in object space coordinate
      vec4 objPos = invModelViewProj * ndc_pos;
      // the projection part of this operation alternates the w component of the vector
      // in order to make xyz components meaningful, a normalization is required
      objPos /= objPos.w;

      // objPos.z is expected to be 0, fract on an almost 0 value would lead to weird patterns
      // an optimization would be to shrinkthe matrices, so that it isn't calculated anymore.
      color = vec4(fract(objPos.xy) * 0.1, 0, 1);
      """

  glSwapWindow(window)
