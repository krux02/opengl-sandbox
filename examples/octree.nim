import ../fancygl, sequtils, AntTweakBar

# not read yet te be part of the nimble build system

template addVarRW(bar: ptr TwBar; value: var float32, stuff: string= ""): void =
  discard TwAddVarRW(bar,astToStr(value), TW_TYPE_FLOAT, value.addr, stuff)

template addVarRW(bar: ptr TwBar; value: var bool, stuff: string = ""): void =
  discard TwAddVarRW(bar,astToStr(value), TW_TYPE_BOOL8, value.addr, stuff)

template addVarRW(bar: ptr TwBar; value: var Quatf, stuff: string = ""): void =
  discard TwAddVarRW(bar,astToStr(value), TW_TYPE_QUAT4F, value.addr, stuff)

iterator twFilteredSdl2Events(): Event =
  ## same as the default sdl2 event iterator, it just consumes events in AntTweakBar
  var event: Event
  while pollEvent(event.addr) != 0:
    if TwEventSDL(cast[pointer](event.addr), 2.cuchar, 0.cuchar) == 0:
      yield event

const enableChecks = false

proc sq(arg: float32): float32 =
  arg * arg

proc `<`[N: static[int],T](a,b: Vec[N,T]): Vec[N,bool] =
  for i in 0 ..< N:
    result.arr[i] = a.arr[i] < b.arr[i]

proc `<=`[N: static[int],T](a,b: Vec[N,T]): Vec[N,bool] =
  for i in 0 ..< N:
    result.arr[i] = a.arr[i] <= b.arr[i]

proc reset[T](arg: var seq[T]): void =
  if arg.isNil:
    arg.newSeq(0)
  else:
    arg.setLen(0)

proc `$`[N,T](arg: array[N,T]): string =
  result = "["
  for i, x in arg:
    if i != 0:
      result.add ", "
    result.add $x
  result.add "]"

type
  AABB = object
    min,max: Vec3f

  Sphere = object
    center: Vec3f
    radius: float32

  Tree = object
    data: seq[Value]
    nodes: seq[Node]
    aabb: AABB

  Value = object
    pos: Vec3f
    color: Color
    vel: Vec3f
    kind: IdMesh


  Node = object
    a,b: int32
    case isLeaf: bool
    of false:
      children: array[8, int32]
    else:
      discard




  IdMesh {.size: 4.} = enum
    IdCone,
    IdCylinder,
    IdIcosphere,
    IdSphere,
    IdBox,
    IdTetraeder,
    IdTorus

var vertices,normals,colors: ArrayBuffer[Vec4f]
var indices: ElementArrayBuffer[int16]

type
  SimpleMesh = object
    vertexOffset: int
    numVertices: int
    baseVertex: int

var meshes: array[IdMesh, SimpleMesh]

proc initMeshes(): void =
  const numSegments = 32

  var verticesSeq = newSeq[Vec4f](0)
  var normalsSeq  = newSeq[Vec4f](0)
  var colorsSeq   = newSeq[Vec4f](0)
  var indicesSeq  = newSeq[indices.T](0)

  proc insertMesh(id: IdMesh,
      newVertices, newNormals, newColors: openarray[Vec4f];
      newIndices: openarray[int16]): void =

    let offset = verticesSeq.len

    meshes[id].vertexOffset = indicesSeq.len
    meshes[id].numVertices = newIndices.len
    meshes[id].baseVertex = offset

    verticesSeq.add(newVertices)
    normalsSeq.add(newNormals)
    colorsSeq.add(newColors)
    indicesSeq.add(newIndices)
    # apply offset

  IdCone.insertMesh(
    coneVertices(numSegments),
    coneNormals(numSegments),
    coneColors(numSegments),
    coneIndices(numSegments))

  IdCylinder.insertMesh(
    cylinderVertices(numSegments),
    cylinderNormals(numSegments),
    cylinderColors(numSegments),
    cylinderIndices(numSegments))

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

  IdIcosphere.insertMesh(
    unrolledVertices,
    unrolledNormals,
    unrolledColors,
    iotaSeq[int16](unrolledVertices.len.int16))

  IdSphere.insertMesh(
    uvSphereVertices(numSegments, numSegments div 2),
    uvSphereNormals(numSegments, numSegments div 2),
    uvSphereColors(numSegments, numSegments div 2),
    uvSphereIndices(numSegments, numSegments div 2))

  IdBox.insertMesh(
    boxVertices,
    boxNormals,
    boxColors,
    iotaSeq[int16](boxVertices.len.int16))

  IdTetraeder.insertMesh(
    tetraederVertices,
    tetraederNormals,
    tetraederColors,
    iotaSeq[int16](tetraederVertices.len.int16))

  IdTorus.insertMesh(
    torusVertices(numSegments, numSegments div 2, 1, 0.5),
    torusNormals(numSegments, numSegments div 2),
    torusColors(numSegments, numSegments div 2),
    torusIndicesTriangles(numSegments, numSegments div 2).map(proc(x: int32): int16 = int16(x)))

  vertices = arrayBuffer(verticesSeq)
  normals = arrayBuffer(normalsSeq)
  colors = arrayBuffer(colorsSeq)
  indices = elementArrayBuffer(indicesSeq)

proc catchUpOrder[T](this: var Tree; data: var seq[T]): void =
  ## the tree changes the order of the nodes,
  ## catch Up order also applies that permutation to data that is associated with the tree data

  # inverse id
  let N = this.data.len
  var i = 0
  while i < N:
    let dst = this.data[i].id and 0xffff
    if dst < i:
      this.data[dst].id = uint32(i) and 0xffff
    else:
      this.data[dst].id = this.data[dst].id and 0xffff or uint32(i shl 16)
    this.data[i].id = this.data[i].id shr 16
  i += 1
  # reorder
  assert data.len == this.data.len, "Illegal Argument"
  i = 0
  var k = 0
  while i < N:
    while int(this.data[i].id) != int(i):
      let j = int(this.data[i].id)
      swap(this.data[i].id, this.data[j].id)
      swap(     data[i]   ,      data[j]   )
      k += 1
    i += 1
  assert k < i

proc mid(this: AABB): Vec3f = mix(this.min, this.max, 0.5f)

proc size(this: AABB): Vec3f =
  this.max - this.min

proc volume(this: AABB): float32 =
  let s = this.size
  s.x * s.y * s.z

proc incl(this: var AABB; point: Vec3f): void =
  this.min = min(this.min, point)
  this.max = max(this.max, point)

proc octreeChild(this: AABB, idx: 0..7): AABB =
  result = this
  let mid = this.mid
  for i in 0 .. 2:
    let bitmask = 1 shl i
    if (idx and bitmask) != 0:
      result.min.arr[i] = mid[i]
    else:
      result.max.arr[i] = mid[i]

when isMainModule and enableChecks:
  var tmp = AABB(min: vec3f(-1), max: vec3f(1))
  doAssert tmp.volume == 8
  for i in 0 ..< 8:
    let subBB = octreeChild(tmp, i)
    doAssert subBB.volume == 1

proc contains(this: AABB; value: Vec3f): bool =
  for i in 0 .. 2:
    if value.arr[i] < this.min.arr[i]:
      return false
    if value.arr[i] > this.max.arr[i]:
      return false
  return true

proc leafNodeBoxes(this: Tree; aabb: AABB; node: Node; dst: var seq[AABB]) =
  if node.isLeaf:
    dst.add aabb
  else:
    for i, childIdx in node.children:
      leafNodeBoxes(this, aabb.octreeChild(i), this.nodes[childIdx], dst)

proc leafNodeBoxes(this: Tree; dst: var seq[AABB]): void =
  dst.reset
  leafNodeBoxes(this, this.aabb, this.nodes[0], dst)

proc leafNodeBoxes(this: Tree): seq[AABB] =
  result.newSeq(0)
  leafNodeBoxes(this, this.aabb, this.nodes[0], result)

proc divide_by_mid(t: var Tree, a,b: int32, mid: Vec3f, dim: 0..2): int32 =
  var i = a
  var j = b-1

  while true:
    # find first element from the left that should be on the right
    while i < b and t.data[i].pos.arr[dim] < mid.arr[dim]:
      i += 1
    # find first ellement from the right thet shuld be on the left
    while j >= a and t.data[j].pos.arr[dim] >= mid.arr[dim]:
      j -= 1

    if i < j:
      swap(t.data[i], t.data[j])
    else:
      break

  result = i

  when enableChecks:
    assert a <= result
    assert result <= b

  var fail = false
  for k in a ..< b:
    let v1 = t.data[k].pos.arr[dim]
    let v2 = mid.arr[dim]
    if (v1 < v2) != (k < result):
      fail = true
  if fail:
    echo s"a $a res $result b $b"
    for k in a ..< b:
      let v1 = t.data[k].pos.arr[dim]
      let v2 = mid.arr[dim]
      echo k, " ", v1 < v2
    assert(false)

proc octreeSort(this: var Tree; boundingBox: AABB; a,b: int32): array[0..8, int32] =
  result[0] = a
  result[8] = b

  let mid = boundingBox.mid

  result[4] = this.divide_by_mid(result[0], result[8], mid, 2)

  result[2] = this.divide_by_mid(result[0], result[4], mid, 1)
  result[6] = this.divide_by_mid(result[4], result[8], mid, 1)

  result[1] = this.divide_by_mid(result[0], result[2], mid, 0)
  result[3] = this.divide_by_mid(result[2], result[4], mid, 0)
  result[5] = this.divide_by_mid(result[4], result[6], mid, 0)
  result[7] = this.divide_by_mid(result[6], result[8], mid, 0)

  when enableChecks:
    for i in 0 ..< 8:
      assert(result[i] <= result[i+1], $(@result))

    for i in 0 ..< 8:
      let childBB = boundingBox.octreeChild(i)
      for j in result[i] ..< result[i+1]:
        assert(childBB.contains(this.data[j].pos))

proc subtreeInit(t: var Tree; boundingBox: AABB; a,b: int32): int32 =
  result = int32(t.nodes.len)
  t.nodes.add Node()
  template newNode: untyped = t.nodes[result]

  newNode = Node(isLeaf: b - a <= 8, a: a, b: b)
  if not newNode.isLeaf:
    let arr = t.octreeSort(boundingBox, a, b)
    for i in 0 ..< 8:
      let childBB = boundingBox.octreeChild(i)
      let childIdx = t.subtreeInit(childBB, arr[i], arr[i+1])
      newNode.children[i] = childIdx

proc init(tree: var Tree): void =
  ## expects ``data`` to be filled, the rest will be initialized
  tree.aabb = AABB(min: vec3f(Inf), max: vec3f(-Inf))
  for d in tree.data:
    tree.aabb.incl(d.pos)

  tree.nodes.reset
  discard tree.subtreeInit(tree.aabb, 0, int32(tree.data.len))

proc squaredDist(box: AABB; pos: Vec3f): float32 =
  for i in 0 ..< 3:
    let v = pos.arr[i]
    let min = box.min.arr[i]
    let max = box.max.arr[i]

    if v < min:
      result += sq(min - v)
    if v > max:
      result += sq(v - max)

proc intersect(a: AABB; b: Sphere): bool =
  squaredDist(a, b.center) < b.radius * b.radius

proc intersect(a: Sphere; pos: Vec3f): bool =
  length2(a.center - pos) <= a.radius * a.radius

proc neighborSearch(tree: Tree; node: Node; aabb: AABB; query: Sphere; skipIndex: int; dst: var seq[(int32,int32)]): void =
  if node.b <= skipIndex:
    return

  if not intersect(aabb, query):
    return

  if node.isLeaf:
    for i in node.a ..< node.b:
      let data = tree.data[i]
      if intersect(query, data.pos):
        dst.add((int32(skipIndex), int32(i)))
  else:
    for i, idx in node.children:
      let childNode = tree.nodes[idx]
      let childAABB = aabb.octreeChild(i)
      neighborSearch(tree, childNode, childAABB,  query, skipIndex, dst)

proc neighborSearch(tree: Tree; radius: float32; dst: var seq[(int32,int32)]): void =
  dst.reset
  for i, value in tree.data:
    let query = Sphere(center: value.pos, radius: radius)
    neighborSearch(tree, tree.nodes[0], tree.aabb, query, i, dst)

proc randomSpiralPosition(): Vec3f =
  template rn: untyped =
    float32(randNormal())
  let x = rn * 2
  let spiralPos = vec3f(sin(x), cos(x), x * 0.35)
  let offset = vec3f(rn, rn, rn)
  return spiralPos * 200 + offset * 2

proc randomSpiralVelocity(): Vec3f =
  template rn: untyped =
    float32(randNormal())
  vec3f(rn, rn, rn)

proc spiralOctree(length: int): Tree =
  ## creates a new octree with data distributed on a spiral
  result.data = newSeq[Value](length)
  for i, d in result.data.mpairs:
    d.pos = randomSpiralPosition()
    d.vel = randomSpiralVelocity()
  result.init

var tree = spiralOctree(1000)

let (window, context) = defaultSetup()

if TwInit(TW_OPENGL_CORE, nil) == 0:
  quit("could not initialize AntTweakBar: " & $TwGetLastError())

discard TwWindowSize(window.size.x, window.size.y)

var obj_quat : Quatf
var scale = 1'f32

var searchRadius = 10'f32

var bar = TwNewBar("TwBar")

bar.addVarRW scale, " precision=5 step=0.001"
bar.addVarRW searchRadius, " precision=5 step=0.002"
bar.addVarRW obj_quat, " label='Object rotation' opened=true help='Change the object orientation.' "

initMeshes()

var treeDataBuffer = arrayBuffer(tree.data, GL_STREAM_DRAW)
let posBuffer      = treeDataBuffer.view(pos)

for value in tree.data.mitems:
  value.color.r = rand_u8()
  value.color.g = rand_u8()
  value.color.b = rand_u8()
  value.color.a = 255
  value.kind = IdMesh rand(IdMesh.high.int + 1)

var runGame: bool = true

let timer = newStopWatch(true)

let aspect = window.aspectRatio.float32
let proj : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

# AABB box rendering

let cubeVertices = arrayBuffer([
  vec4f( 1, 1, 1, 1),
  vec4f( 0, 1, 1, 1),
  vec4f( 1, 0, 1, 1),
  vec4f( 0, 0, 1, 1),
  vec4f( 1, 1, 0, 1),
  vec4f( 0, 1, 0, 1),
  vec4f( 1, 0, 0, 1),
  vec4f( 0, 0, 0, 1)
])

let cubeLineIndices = elementArrayBuffer([
  0'i8, 1,
  2, 3,
  4,5,
  6,7,
  0,2,
  1,3,
  4,6,
  5,7,
  0,4,
  1,5,
  2,6,
  3,7

#  0'i8,7,
#  1,6,
#  2,5,
#  3,4
])
let cubeLineIndicesLen = cubeLineIndices.len

var boxes : seq[AABB] = tree.leafNodeBoxes # just for the approximation of the length
var boxesBuffer = newArrayBuffer[AABB](boxes.len * 2, GL_STREAM_DRAW)

let boxesMinView = boxesBuffer.view(min)
let boxesMaxView = boxesBuffer.view(max)

proc drawBoxes(proj,modelView: Mat4f): void =
  tree.leafNodeBoxes(boxes)
  boxesBuffer.setData(boxes)

  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = cubeLineIndicesLen
    indices = cubeLineIndices
    numInstances = boxes.len
    uniforms:
      modelViewProj = proj * modelView
    attributes:
      a_vertex = cubeVertices
      a_max = boxesMaxView {.divisor: 1.}
      a_min = boxesMinView {.divisor: 1.}
    vertexMain:
      """
      vec4 pos_ws = vec4(mix(a_min, a_max, a_vertex.xyz), 1);
      gl_Position = modelViewProj * pos_ws;
      """
    fragmentMain:
      """
      color = vec4(0,0,1,1);
      """

var rotationX, rotationY: float32

var mouse1 = false
var mouse2 = false
var mouse3 = false


var neighborSearchResult: seq[(int32,int32)]
var neighborSearchResultBuffer = newElementArrayBuffer[int32](40000, GL_STREAM_DRAW)

proc drawNeighborhood(proj,modelView: Mat4f): void =
  tree.neighborSearch(searchRadius, neighborSearchResult)

  let sizeArg = GLsizeiptr(neighborSearchResult.len * sizeof(int32) * 2)
  let handleArg = neighborSearchResultBuffer.handle
  let dataArg = neighborSearchResult[0].addr
  glNamedBufferSubData(handleArg, 0, sizeArg, dataArg)

  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = neighborSearchResult.len * 2
    indices = neighborSearchResultBuffer
    uniforms:
      modelView
      proj
    attributes:
      a_vertex = posBuffer
    vertexMain:
      """
      gl_Position = proj * modelView * vec4(a_vertex, 1);
      """
    fragmentMain:
      """
      color = vec4(1,1,0,1);
      """

import gifh

var animation: GifAnimation
var remainingFrames = 0

while runGame:

  for evt in twFilteredSdl2Events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SCANCODE_ESCAPE:
        runGame = false
      of SCANCODE_F10:
        window.screenshot
      of SCANCODE_S:
        #animation = window.startGifAnimation(delay = 1, dither = false)
        remainingFrames = 100
      else:
        discard

    if evt.kind == MouseButtonDown:
      case evt.button.button
      of 1:
        mouse1 = true
      of 2:
        mouse2 = true
      of 3:
        mouse3 = true
      else:
        discard
    if evt.kind == MouseButtonUp:
      case evt.button.button
      of 1:
        mouse1 = false
      of 2:
        mouse2 = false
      of 3:
        mouse3 = false
      else:
        discard

    if evt.kind == MouseMotion:
      if mouse1:
        obj_quat *= quatf(vec3f(0,1,0), float32(evt.motion.xrel) * 0.001f)
        obj_quat *= quatf(vec3f(0,0,1), float32(evt.motion.yrel) * 0.001f)
      if mouse3:
        scale *= 1'f32 + float32(evt.motion.xrel) * 0.003f

  #let time = timer.time.float32


  let tmp = mat4f(obj_quat)

  let viewMat = tmp * mat4f(1)
    .translate(vec3f(0,-1,-5) * scale)
    .rotateX(rotationX)
    .rotateY(rotationY)
    .scale(0.01)

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  block:
    let s = sin(2 * Pi * 0.0001'f32)
    let c = cos(2 * Pi * 0.0001'f32)
    let mat = mat2(vec2(c,s), vec2(-s,c))
    for node in tree.data.mitems:
      node.pos.xy = mat * node.pos.xy

  tree.init
  treeDataBuffer.setData(tree.data)


  #tree.catchUpOrder(objectDataSeq)
  #objectDataBuffer.setData(objectDataSeq)

  drawNeighborhood(proj, viewMat)

  #[
  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices = tree.data.len
    uniforms:
      modelView = viewMat
      proj
    attributes:
      a_vertex = posBuffer
      a_color  = colorsBuffer
    vertexMain:
      """
      gl_Position = proj * modelView * vec4(a_vertex, 1);
      //gl_Position = a_vertex;
      v_color = a_color;
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = v_color;
      """
  ]#

  #for i, node in worldNodes:
  #  let mesh = meshes[i]
  let meshDeform = mat4f(1)
  var buffer = newArrayBuffer[tuple[offset: Vec3f, color: Color]](tree.data.len)
  defer:
    buffer.delete

  for meshId in IdMesh:
    ## TODO: render each object with a Mesh acconding to it's kind.
    ## important for flocking.

    let mesh = meshes[meshId]

    var i = 0
    mapWriteBlock(buffer):
      for value in tree.data:
        if value.kind == meshId:
          buffer[i].offset = value.pos
          buffer[i].color  = value.color
          i += 1

    shadingDsl:
      debug
      primitiveMode = GL_TRIANGLES
      numVertices = mesh.numVertices
      vertexOffset = mesh.vertexOffset
      baseVertex = mesh.baseVertex
      indices = indices
      numInstances = i

      uniforms:
        proj
        view = viewMat
        meshDeform

      attributes:
        a_vertex = vertices
        a_normal = normals
        a_color  = colors
        instancePos = buffer.view(offset) {.divisor: 1.}
        instanceColor = buffer.view(color) {.divisor: 1.}

      vertexMain:
        """
        gl_Position = proj * view * (meshDeform * a_vertex + vec4(instancePos,0));
        v_normal = normalize(view * a_normal);
        v_color = a_color * instanceColor;
        """
      vertexOut:
        "out vec4 v_normal"
        "out vec4 v_color"

      fragmentMain:
        """
        // cheap fake lighting from camera direction
        color = v_color * v_normal.z;
        """


  drawBoxes(proj, viewMat)
  discard TwDraw()
  glSwapWindow(window)

  if remainingFrames > 0:
    animation.frameGifAnimationGl()
    remainingFrames -= 1
    if remainingFrames == 0:
      animation.endGifAnimation()





#when isMainModule:
#  main()
