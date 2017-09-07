import ../fancygl

proc `<`[N: static[int],T](a,b: Vec[N,T]): Vec[N,bool] =
  for i in 0 ..< N:
    result.arr[i] = a.arr[i] < b.arr[i]

proc `<=`[N: static[int],T](a,b: Vec[N,T]): Vec[N,bool] =
  for i in 0 ..< N:
    result.arr[i] = a.arr[i] <= b.arr[i]

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

  Tree = object
    data: seq[Value]
    nodes: seq[Node]
    aabb: AABB

  Value = object
    pos: Vec3f

  NodeType = enum
    ntValueRange

  Node = object
    case isLeaf: bool
    of true:
      a,b: int
    else:
      children: array[8, int32]

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


when isMainModule:
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

  if b - a < 8:
    newNode = Node(isLeaf: true, a: a, b: b)
  else:
    let arr = t.octreeSort(boundingBox, a, b)
    for i in 0 ..< 8:
      let childBB = boundingBox.octreeChild(i)
      let childIdx = t.subtreeInit(childBB, arr[i], arr[i+1])
      newNode.children[i] = childIdx


proc newOctree(data: openarray[Value]): Tree =
  result = Tree(data: @data)
  result.aabb = AABB(min: vec3f(Inf), max: vec3f(-Inf))
  for d in data:
    result.aabb.incl(d.pos)

  result.nodes.newSeq(0)
  discard result.subtreeInit(result.aabb, 0, int32(data.len))

proc randOctree(length: int): Tree =
  var data = newSeq[Value](length)
  for d in data.mitems:
    d.pos = Vec3f(arr: [rand_f32(), rand_f32(), rand_f32()])

  result = newOctree(data)


proc spiralOctree(length: int): Tree =
  ## creates a new octree with data distributed on a spiral
  var data = newSeq[Value](length)
  for d in data.mitems:
    let x = float32(randNormal() * 2)
    let spiralPos = vec3f(sin(x), cos(x), x * 0.35)
    let offset = vec3f(vec3(randNormal(), randNormal(), randNormal()) * 0.01)
    d.pos = spiralPos + offset

  result = newOctree(data)

let tree = spiralOctree(1000)

let (window, context) = defaultSetup()

glPointSize(5)

let posBuffer = arrayBuffer(tree.data).view(pos)

var colors = newSeq[Vec3f](1000)
for v in colors.mitems():
  v.x = rand_f32()
  v.y = rand_f32()
  v.z = rand_f32()

let colorsBuffer = arrayBuffer(colors)


var evt: Event
var runGame: bool = true

let timer = newStopWatch(true)

let aspect = float32(window.size.x / window.size.y)
let proj : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

var queryPos: Vec4f = vec4f(0,0,0,1)


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
])

var linePositions = newArrayBuffer[Vec4f](1000)

let boxes = tree.leafNodeBoxes
let boxesBuffer = arrayBuffer(boxes)# newArrayBuffer[AABB](1000)
let boxesMinView = boxesBuffer.view(min)
let boxesMaxView = boxesBuffer.view(max)

proc drawBoxes(proj,modelView: Mat4f, maxDepth: int): void =
  #let boxes = tree.leafNodeBoxes(#[maxDepth]#)
  #boxesBuffer.setData(boxes)

  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = 24
    indices = cubeLineIndices
    numInstances = boxes.len
    uniforms:
      modelView
      proj
    attributes:
      a_vertex = cubeVertices
      a_max = boxesMaxView {.divisor: 1.}
      a_min = boxesMinView {.divisor: 1.}
    vertexMain:
      """
      vec4 pos_ws = vec4(mix(a_min + 0.001, a_max - 0.001, a_vertex.xyz), 1);
      gl_Position = proj * modelView * pos_ws;
      """
    fragmentMain:
      """
      color = vec4(1);
      """

proc lines(mvp: Mat4f; vertices: openarray[Vec4f]): void =
  linePositions.setData(vertices)

  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = vertices.len
    uniforms:
      mvp
    attributes:
      a_vertex = linePositions
    vertexMain:
      """
      gl_Position = mvp * a_vertex;
      """
    fragmentMain:
      """
      color = vec4(1);
      """


var rotationX, rotationY: float32
var maxDepth = 0

while runGame:

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown:
      if evt.key.keysym.scancode == SDL_SCANCODE_ESCAPE:
        runGame = false
      if evt.key.keysym.scancode == SDL_SCANCODE_KP_PLUS:
        maxDepth += 1
      if evt.key.keysym.scancode == SDL_SCANCODE_KP_MINUS:
        maxDepth -= 1

    if evt.kind == MouseMotion:
      queryPos.x =     (evt.motion.x / window.size.x) * 2 - 1
      queryPos.y = 1 - (evt.motion.y / window.size.y) * 2

      rotationY += float32(evt.motion.xrel) * 0.001f
      rotationX += float32(evt.motion.yrel) * 0.001f

  let time = timer.time.float32

  let viewMat = mat4f(1)
    .translate(0,1,5)            # position camera at position 0,1,5
    .rotateX(Pi * -0.05)         # look a bit down
    .inverse                     # the camera matrix needs to be inverted

  let modelMat = mat4f(1)
    .rotateY(rotationY)    # rotate the triangle
    .rotateX(rotationX)
    .scale(1.5f)                    # scale the triangle to be big enough on screen

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  var selected = 0
  var selected_dist = float32(Inf)

  for i, node in tree.data:
    let dist = length(node.pos.xyz - queryPos.xyz)
    if dist < selected_dist:
      selected = i
      selected_dist = dist

  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices = 1000
    uniforms:
      modelView = viewMat * modelMat
      proj
      select    = int32(selected)
    attributes:
      a_vertex = posBuffer
      a_color  = colorsBuffer
    vertexMain:
      """
      gl_Position = proj * modelView * vec4(a_vertex, 1);
      //gl_Position = a_vertex;
      if (select == gl_VertexID) {
        v_color = vec4(1);
      } else {
        v_color = vec4(a_color, 1);
      }
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = v_color;
      """

  #[
  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = 24
    indices = cubeLineIndices
    uniforms:
      modelView = viewMat * modelMat
      proj
      select    = int32(selected)
    attributes:
      a_vertex = cubeVertices
    vertexMain:
      """
      gl_Position = proj * modelView * a_vertex;
      """
    fragmentMain:
      """
      color = vec4(1);
      """
  ]#

  drawBoxes(proj, viewMat * modelMat, maxDepth)


  let mvp = proj * viewMat * modelMat

  var a,b,c,d,e: Vec4f = queryPos
  a.x = -1
  b.x =  1
  c.y = -1
  d.y =  1
  e.xy = tree.data[selected].pos.xy

  #[
  lines(mvp, [
    vec4f( 1, 1, queryPos.z, 1), vec4f(-1, 1, queryPos.z, 1),
    vec4f(-1, 1, queryPos.z, 1), vec4f(-1,-1, queryPos.z, 1),
    vec4f(-1,-1, queryPos.z, 1), vec4f( 1,-1, queryPos.z, 1),
    vec4f( 1,-1, queryPos.z, 1), vec4f( 1, 1, queryPos.z, 1),
    a,b,c,d, queryPos, e, e, vec4f(tree.data[selected].pos, 1), vec4f(tree.data[selected].pos, 1), queryPos
  ])
  ]#

  glSwapWindow(window)



#when isMainModule:
#  main()
