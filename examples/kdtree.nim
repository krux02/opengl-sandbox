import glm
import ../fancygl

type
  KdTree = object
    data: seq[KdNode]
    rootIdx: int

  KdNode = object
    pos: Vec3f
    left,right: int32

  Box = object
    min,max: Vec3f

  DimIndex = 0..2

proc split(box: Box; value: float32; dim: DimIndex): tuple[left,right: Box] =
  if value < box.min[dim] or box.max[dim] < value:
    echo s"illigal split ${box.min[dim]} < $value < ${box.max[dim]}"

  result.left  = box
  result.right = box

  result.left.max[dim] = value
  result.right.min[dim] = value


proc leafNodeBoxes(this: KdTree, node: KdNode, boundingBox: Box, dim: DimIndex; depth: int; maxDepth: int, dest: var seq[Box]): void =

  if depth == maxDepth:
    dest.add boundingBox
    return

  let value = node.pos[dim]
  if value < boundingBox.min[dim] or boundingBox.max[dim] < value:
    echo s"illigal split ${boundingBox.min[dim]} < $value < ${boundingBox.max[dim]}"
    dest.add boundingBox
    return

  let (leftBox,rightBox) = boundingBox.split(value, dim)
  if node.left >= 0:
    leafNodeBoxes(this, this.data[node.left], leftBox, (dim + 1) mod 3, depth + 1, maxDepth, dest):
  else:
    dest.add leftBox

  if node.right >= 0:
    leafNodeBoxes(this, this.data[node.right], rightBox, (dim + 1) mod 3, depth + 1, maxDepth, dest):
  else:
    dest.add rightBox

proc leafNodeBoxes(this: KdTree; maxDepth: int = -1): seq[Box] =
  let maxDepth = if maxDepth < 0: high(int) else: maxDepth
  var boundingBox = Box(min: vec3f(Inf), max: vec3f(-Inf))
  for node in this.data:
    boundingBox.min = min(boundingBox.min, node.pos)
    boundingBox.max = max(boundingBox.max, node.pos)

  result.newSeq(0)
  leafNodeBoxes(this, this.data[this.rootIdx], boundingBox, 0, 0, maxDepth, result)


proc divide_by_mean(t: var KdTree, a,b: int, dim: DimIndex): int =
  var mean: float32
  for i in a ..< b:
    mean += t.data[i].pos.arr[dim]
  mean /= float32(b - a)

  var a = a
  var b = b-1

  while a < b:
    swap(t.data[a], t.data[b])
    while t.data[a].pos.arr[dim] < mean:
      a += 1
    while t.data[b].pos.arr[dim] >= mean:
      b -= 1

  return a

proc subtree_sort(t: var KdTree; a,b: int, dim: DimIndex): int =
  if a == b:
    return -1


  result = t.divide_by_mean(a, b, dim)

  if 0 <= result:
    t.data[result].left  = subtree_sort(t, a,   result, (dim + 1) mod 3).int32
    t.data[result].right = subtree_sort(t, result+1, b, (dim + 1) mod 3).int32

proc init(t: var KdTree): void =
  t.rootIdx = t.subtree_sort(0, t.data.len, 0)

proc nearestIntern(this: KdTree; node_idx: int; pos: Vec3f, dim: int; best_idx: var int, best_dist: var float32, visited: var int): void =
  if node_idx == -1:
    return

  template node: KdNode = this.data[node_idx]

  let d = length(node.pos - pos)
  let dx = node.pos.arr[dim] - pos[dim]

  visited += 1

  if best_idx == -1 or d < best_dist:
    best_dist = d
    best_idx = node_idx

  if best_dist == 0:
    return #(perfect match)

  let dim2 = (dim + 1) mod 3

  let first  = if 0 < dx: node.left else: node.right
  let second = if 0 < dx: node.right else: node.left

  this.nearestIntern(first,  pos, dim2, best_idx, best_dist, visited)
  if (dx * dx >= best_dist):
    return
  this.nearestIntern(second, pos, dim2, best_idx, best_dist, visited)


proc nearest(this: KdTree, node: KdNode): tuple[best_idx: int, best_dist: float32, visited: int] =
  result.best_idx = -1
  this.nearestIntern(this.rootIdx, node.pos, 0, result.best_idx, result.best_dist, result.visited)

proc nearestLinear(this: KdTree, pos: Vec3f): tuple[best_idx: int, best_dist: float32] =

  result.best_dist = Inf
  for i, node in this.data:
    let dist = length(node.pos - pos)
    if dist < result.best_dist:
      result.best_idx = i
      result.best_dist = dist

import random
proc rand1(): float32 =
  float32(random.random(2.0f) - 1.0f)

proc rand_pos(): Vec3f = Vec3f(arr: [rand1(), rand1(), rand1()])

proc rand_node(): KdNode = KdNode(pos: rand_pos())

proc rand_tree(N: int): KdTree =
  result.data = newSeq[KdNode](N)
  for i in 0 ..< N:
    result.data[i] = rand_node()
  result.init

proc testKdTree(): void =
  echo "test KdTree"
  let million = rand_tree(100)

  var sum: int = 0
  let test_runs = 1000
  var error_count = 0
  for i in 1 .. test_runs: # number of test runs
    var testNode = rand_node()
    let (foundA, distA, visited) = million.nearest(testNode)
    let (foundB, distB) = million.nearestLinear(testNode.pos)
    if foundA != foundB:
      echo s"$foundA should be $foundB"
      echo s"found dist: $distA should be $distB"
      error_count += 1
    sum += visited
  echo s"$error_count errors of $test_runs runs"
  echo s"visited $sum nodes for $test_runs random findings (${sum/test_runs} per lookup)"

testKdTree()


let (window, context) = defaultSetup()

glPointSize(5)



let tree = rand_tree(1000)

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

let boxesBuffer = newArrayBuffer[Box](1000)
let boxesMinView = boxesBuffer.view(min)
let boxesMaxView = boxesBuffer.view(max)

proc drawBoxes(proj,modelView: Mat4f, maxDepth: int): void =
  let boxes = tree.leafNodeBoxes(maxDepth)
  boxesBuffer.setData(boxes)

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

  for evt in events():
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
    .scale(3)                    # scale the triangle to be big enough on screen

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
