import ../fancygl

const enableChecks = false

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
    id : uint32  ## after ``init`` ``srcPositions[id] == pos`` should be true

  Node = object
    a,b: int32
    case isLeaf: bool
    of false:
      children: array[8, int32]
    else:
      discard


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

proc newOctree(data: openarray[Value]): Tree =
  result = Tree(data: @data)
  result.init

#[
  Tree = object
    data: seq[Value]
    nodes: seq[Node]
    aabb: AABB

  Value = object
    pos: Vec3f
    color: Vec3f

  NodeType = enum
    ntValueRange

  Node = object
    case isLeaf: bool
    of true:
      a,b: int
    else:
      children: array[8, int32]
]#


proc squaredDist(box: AABB; pos: Vec3f): float32 =
  for i in 0 ..< 3:
    let v = pos.arr[i]
    let min = box.min.arr[i]
    let max = box.max.arr[i]

    if v < min:
      result += (min - v) * (min - v)
    if v > max:
      result += (v - max) * (v - max)

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
  let x = float32(randNormal() * 2)
  let spiralPos = vec3f(sin(x), cos(x), x * 0.35)
  let offset = vec3f(vec3(randNormal(), randNormal(), randNormal()) * 0.01)
  return spiralPos + offset

proc spiralOctree(length: int): Tree =
  ## creates a new octree with data distributed on a spiral
  result.data = newSeq[Value](length)
  for i, d in result.data.mpairs:
    d.pos = randomSpiralPosition()
    d.id = uint32(i)
  result.init

proc randomColors(N: int): seq[Color] =
  result.newSeq(N)
  for color in result.mitems:
    color.r = rand_u8()
    color.g = rand_u8()
    color.b = rand_u8()
    color.a = 255

var tree = spiralOctree(1000)

let (window, context) = defaultSetup()

glPointSize(20)

var treeDataBuffer = arrayBuffer(tree.data, GL_STREAM_DRAW)

let posBuffer      = treeDataBuffer.view(pos)

var colorsData = randomColors(tree.data.len)
let colorsBuffer = arrayBuffer(colorsData)

var runGame: bool = true

let timer = newStopWatch(true)

let aspect = float32(window.size.x / window.size.y)
let proj : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

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
var scale = 1.0f


var neighborSearchResult: seq[(int32,int32)]
var neighborSearchResultBuffer = newElementArrayBuffer[int32](40000, GL_STREAM_DRAW)

proc drawNeighborhood(proj,modelView: Mat4f): void =
  tree.neighborSearch(0.1f, neighborSearchResult)

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

  for evt in events():
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
        animation = window.startGifAnimation(delay = 1, dither = false)
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
        rotationY += float32(evt.motion.xrel) * 0.001f
        rotationX += float32(evt.motion.yrel) * 0.001f
      if mouse3:
        scale += float32(evt.motion.xrel) * 0.001f

  #let time = timer.time.float32

  let viewMat = mat4f(1)
    .translate(0,1,5)            # position camera at position 0,1,5
    .rotateX(Pi * -0.05)         # look a bit down
    .inverse                     # the camera matrix needs to be inverted

  let modelMat = mat4f(1)
    .rotateX(rotationX)
    .rotateY(rotationY)    # rotate the triangle
    .scale(scale)                    # scale the triangle to be big enough on screen

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  block:
    let s = sin(2 * Pi * 0.001'f32)
    let c = cos(2 * Pi * 0.001'f32)
    let mat = mat2(vec2(c,s), vec2(-s,c))
    for node in tree.data.mitems:
      node.pos.xy = mat * node.pos.xy

  tree.init
  treeDataBuffer.setData(tree.data)
  tree.catchUpOrder(colorsData)
  colorsBuffer.setData(colorsData)

  drawNeighborhood(proj, viewMat * modelMat)


  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices = tree.data.len
    uniforms:
      modelView = viewMat * modelMat
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

  drawBoxes(proj, viewMat * modelMat)
  glSwapWindow(window)

  if remainingFrames > 0:
    animation.frameGifAnimationGl()
    remainingFrames -= 1
    if remainingFrames == 0:
      animation.endGifAnimation()





#when isMainModule:
#  main()
