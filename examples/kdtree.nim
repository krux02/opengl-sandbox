import glm

type
  KdTree = object
    data: seq[KdNode]
    rootIdx: int

  KdNode = object
    x: Vec3f
    left,right: int32

  Box = object
    min,max: Vec3f

proc split(box: Box; value: float32; dim: int): tuple[l,r: Box] =
  assert( box.min[dim] <= value )
  assert( value <= box.max[dim] )

  var l = box
  var r = box

  l.max[dim] = value
  r.min[dim] = value


iterator leafNodeBoxes(this: KdTree, node: KdNode, boundingBox: Box, dim: int): Box {.closure.} =
  let (letfBox,rightBox) = boundingBox.split(node.x[dim], dim)
  if node.left >= 0:
    for box in leafNodeBoxes(this, this.data[node.left], leftBox):
      yield box
  if node.right >= 0:
    for box in leafNodeBoxes(this, this.data[node.right]. rightBox):
      yield box

iterator leafNodeBoxes(this: KdTree): Box {.closure.} =

  var boundingBox: Box(min: vec3f(Inf), max: vec3f(-Inf))
  for node in this.data:
    boundingBox.min = min(node.x, boundingBox.min)
    boundingBox.max = max(node.x, boundingBox.max)

  for box in leafNodeBoxes(this, this.data[this.rootIdx], boundingBox):
    yield box


proc divide_by_mean(t: var KdTree, a,b: int, dim: int): int =
  var mean: float32
  for i in a ..< b:
    mean += t.data[i].x.arr[dim]
  mean /= float32(b - a)

  var a = a
  var b = b-1

  while a < b:
    swap(t.data[a], t.data[b])
    while t.data[a].x.arr[dim] < mean:
      a += 1
    while t.data[b].x.arr[dim] >= mean:
      b -= 1

  return a

proc subtree_sort(t: var KdTree; a,b: int, i: int): int =
  if a == b:
    return -1


  result = t.divide_by_mean(a, b, i)

  if 0 <= result:
    t.data[result].left  = subtree_sort(t, a,   result, (i + 1) mod 3).int32
    t.data[result].right = subtree_sort(t, result+1, b, (i + 1) mod 3).int32

proc init(t: var KdTree): void =
  t.rootIdx = t.subtree_sort(0, t.data.len, 0)

proc nearestIntern(this: KdTree; node_idx: int; pos: Vec4f, dim: int; best_idx: var int, best_dist: var float32, visited: var int): void =
  if node_idx == -1:
    return

  template node: KdNode = this.data[node_idx]

  let d = length(root.x - node.x)
  let dx = root.x.arr[dim] - node.x[dim]
  let dx2 = dx * dx

  visited += 1

  if best_idx == -1 or d < best_dist:
    best_dist = d
    best_idx = node_idx

  if best_dist == 0:
    return #(perfect match)

  let dim2 = (dim + 1) mod 3

  this.nearestIntern(if 0 < dx: root.left else: root.right, pos, dim2, best_idx, best_dist, visited)
  if (dx2 >= best_dist):
    return
  this.nearestIntern(if 0 < dx: root.right else: root.left, pos, dim2, best_idx, best_dist, visited)


proc nearest(this: KdTree, node: KdNode): tuple[best_idx: int, best_dist: float32, visited: int] =
  result.best_idx = -1
  this.nearestIntern(this.rootIdx, node.pos, 0, result.best_idx, result.best_dist, result.visited)

proc nearestLinear(this: KdTree, node: KdNode): tuple[best_idx: int, best_dist: float32] =

  result.best_dist = Inf
  for i, node in this.data:
    let dist = length(node.x - node.x)
    if dist < result.best_dist:
      result.best_idx = i
      result.best_dist = dist

const N = 10000

import random
proc rand1(): float32 =
  float32(random.random(1.0f))

proc rand_node(): KdNode {.inline.} =
  result.x.arr = [rand1(), rand1(), rand1()]

proc main(): void =


  block:

    var wp = KdTree(data: @[
      KdNode(x: vec3f(2,3,1)),
      KdNode(x: vec3f(5,4,2)),
      KdNode(x: vec3f(9,6,3)),
      KdNode(x: vec3f(4,7,4)),
      KdNode(x: vec3f(8,1,5)),
      KdNode(x: vec3f(7,2,6))
    ])
    wp.init
    var testNode = KdNode(x: vec3f(2,3,0))
    let (found_idx, best_dist, visited) = wp.nearest(testNode);
    let found = wp.data[found_idx]

    echo ">> WP tree\nsearching for ", testNode.x
    echo "found ", found.x, " dist ", sqrt(best_dist)
    echo "seen ", visited, " nodes\n"

  var million = KdTree(data: newSeq[KdNode](N))
  for i in 0 ..< N:
    million.data[i] = rand_node()
  million.init

  block:
    var testNode = rand_node()
    let (found_idx, best_dist, visited) = million.nearest(testNode);
    let found = million.data[found_idx]

    echo ">> Million tree"
    echo "searching for ", testNode.x
    echo "found ", found.x, " dist ", sqrt(best_dist)
    echo "seen ", visited


  block:
    #[ search many random points in million tree to see average behavior
       tree size vs avg nodes visited:
       10    ~  7
       100   ~ 16.5
       1000    ~ 25.5
       10000     ~ 32.8
       100000    ~ 38.3
       1000000   ~ 42.6
       10000000  ~ 46.7        ]#

    var sum: int = 0
    let test_runs = 1000
    for i in 1 .. test_runs: # number of test runs
      var testNode = rand_node()
      let (foundA, distA, visited) = million.nearest(testNode)
      let (foundB, distB) = million.nearestLinear(testNode)
      doAssert(foundA == foundB, $foundA & " == " & $foundB & " distA: " & $distA & " distB: " & $distB)
      sum += visited

    echo "Million tree"
    echo "visited ", sum ," nodes for ", test_runs,
         " random findings (", sum / test_runs, " per lookup)\n"

main()
quit(0)

import ../fancygl

let (window, context) = defaultSetup()

glPointSize(5)

var tmp = newSeq[Vec4f](1000)

for v in tmp.mitems():
  v.x = rand_f32()
  v.y = rand_f32()
  v.z = rand_f32()
  v.w = 1.0f

let colors   = arrayBuffer(tmp)

for v in tmp.mitems():
  v.x = rand_f32() * 2 - 1
  v.y = rand_f32() * 2 - 1
  v.z = rand_f32() * 2 - 1
  v.w = 1.0f

var vertices: ArrayBuffer[Vec4f] = arraybuffer(tmp)


var evt: Event
var runGame: bool = true

let timer = newStopWatch(true)

let aspect = float32(window.size.x / window.size.y)
let proj : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

var queryPos: Vec4f = vec4f(0,0,0,1)


let cubeVertices = arrayBuffer([
  vec4f( 1, 1, 1, 1),
  vec4f(-1, 1, 1, 1),
  vec4f( 1,-1, 1, 1),
  vec4f(-1,-1, 1, 1),
  vec4f( 1, 1,-1, 1),
  vec4f(-1, 1,-1, 1),
  vec4f( 1,-1,-1, 1),
  vec4f(-1,-1,-1, 1)
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

while runGame:

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown and evt.key.keysym.scancode == SDL_SCANCODE_ESCAPE:
      runGame = false
    if evt.kind == MouseMotion:
      queryPos.x =     (evt.motion.x / window.size.x) * 2 - 1
      queryPos.y = 1 - (evt.motion.y / window.size.y) * 2

  let time = timer.time.float32

  let viewMat = mat4f(1)
    .translate(0,1,5)            # position camera at position 0,1,5
    .rotateX(Pi * -0.05)         # look a bit down
    .inverse                     # the camera matrix needs to be inverted

  let modelMat = mat4f(1)
    #.rotateY(time)               # rotate the triangle
    .scale(3)                    # scale the triangle to be big enough on screen

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  var selected = 0
  var selected_dist = float32(Inf)

  for i, v in tmp:
    let dist = length(v.xyz - queryPos.xyz)
    if dist < selected_dist:
      selected = i
      selected_dist = dist

  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices = tmp.len
    uniforms:
      modelView = viewMat * modelMat
      proj
      select    = int32(selected)
    attributes:
      a_vertex = vertices
      a_color  = colors
    vertexMain:
      """
      gl_Position = proj * modelView * a_vertex;
      //gl_Position = a_vertex;
      if (select == gl_VertexID) {
        v_color = vec4(1);
      } else {
        v_color = a_color;
      }
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = v_color;
      """

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

  let mvp = proj * viewMat * modelMat

  var a,b,c,d, e, f: Vec4f = queryPos
  a.x = -1
  b.x =  1
  c.y = -1
  d.y =  1
  e.xy = tmp[selected].xy

  lines(mvp, [
    vec4f( 1, 1, queryPos.z, 1), vec4f(-1, 1, queryPos.z, 1),
    vec4f(-1, 1, queryPos.z, 1), vec4f(-1,-1, queryPos.z, 1),
    vec4f(-1,-1, queryPos.z, 1), vec4f( 1,-1, queryPos.z, 1),
    vec4f( 1,-1, queryPos.z, 1), vec4f( 1, 1, queryPos.z, 1),
    a,b,c,d, queryPos, e, e, tmp[selected], tmp[selected], queryPos
  ])

  glSwapWindow(window)



#when isMainModule:
#  main()
