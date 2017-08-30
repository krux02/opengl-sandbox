import glm

type
  KdNode = object
    x: Vec3f
    left,right: ptr KdNode

proc dist(a,b: ptr KdNode): float32 {.inline.} =
  length(a.x - b.x)

proc swap(x, y: ptr KdNode): void =
  swap(x[], y[])

proc `+`[T](a: ptr T, i: int): ptr T =
  cast[ptr T](cast[uint](a) + uint(sizeof(T) * i))

proc `-`[T](a: ptr T, i: int): ptr T =
  cast[ptr T](cast[uint](a) - uint(sizeof(T) * i))

proc `-`[T](a,b: ptr T): int =
  cast[int](cast[uint](a) - cast[uint](b)) div sizeof(T)

proc `++`[T](arg: var ptr T): void =
  arg = cast[ptr T](cast[uint](arg) + uint(sizeof(T)))

proc find_median(data: var openarray[KdNode]; a,b: int, idx: int): int =
  if b <= a:
    return -1;

  if a + 1 == b:
    return a;

  var start = a
  var endd = b

  var p = -1
  var store = -1
  let md: int = start + (endd - start) div 2;
  var pivot: float32

  while true:
    pivot = data[md].x.arr[idx]

    swap(data[md], data[endd - 1])

    p = start
    store = p

    while p < endd:
      if data[p].x.arr[idx] < pivot:
        if p != store:
          swap(data[p], data[store]);
        store += 1
      p += 1

    swap(data[store], data[endd - 1])

    # median has duplicate values
    if data[store].x.arr[idx] == data[md].x.arr[idx]:
      return md;

    if store > md:
      endd = store;
    else:
      start = store;

proc make_tree(data: var openarray[KdNode]; a,b: int, i: int): int =
  if a == b:
    return -1

  result = find_median(data, a, b, i)

  if 0 <= result:
    let idxL = make_tree(data, a,   result, (i + 1) mod 3)
    let idxR = make_tree(data, result+1, b, (i + 1) mod 3)
    data[result].left  = if idxL < 0: nil else: data[idxL].addr
    data[result].right = if idxR < 0: nil else: data[idxR].addr

proc nearest(root, nd: ptr KdNode): tuple[best: ptr KdNode, best_dist: float32, visited: int] =
  var visited = 0
  proc nearest(root, nd: ptr KdNode, i: int; best: var ptr KdNode, best_dist: var float32): void =
    if root == nil:
      return

    let d = dist(root, nd)
    let dx = root.x.arr[i] - nd.x[i]
    let dx2 = dx * dx

    visited += 1

    if best == nil or d < best_dist:
      best_dist = d
      best = root


    if best_dist == 0:
       return #(perfect match)

    let j = (i + 1) mod 3

    nearest(if 0 < dx: root.left else: root.right, nd, j, best, best_dist)
    if (dx2 >= best_dist):
      return
    nearest(if 0 < dx: root.right else: root.left, nd, j, best, best_dist)

  nearest(root, nd, 0, result.best, result.best_dist)
  result.visited = visited

const N = 1000000

import random
proc rand1(): float32 =
  float32(random.random(1.0f))

proc rand_node(): KdNode {.inline.} =
  result.x.arr = [rand1(), rand1(), rand1()]


#define rand1() (rand() / (double)RAND_MAX)
#define rand_pt(v) { v.x[0] = rand1(); v.x[1] = rand1(); v.x[2] = rand1(); }

proc main(): void =
  var wp : seq[KdNode] = @[
    KdNode(x: vec3f(2,3,1)),
    KdNode(x: vec3f(5,4,2)),
    KdNode(x: vec3f(9,6,3)),
    KdNode(x: vec3f(4,7,4)),
    KdNode(x: vec3f(8,1,5)),
    KdNode(x: vec3f(7,2,6))
  ]


  block:
    var testNode = KdNode(x: vec3f(2,3,0))
    let root = wp[make_tree(wp, 0, wp.len, 0)].addr
    let (found, best_dist, visited) = nearest(root, testNode.addr);

    echo ">> WP tree\nsearching for ", testNode.x
    echo "found ", found.x, " dist ", sqrt(best_dist)
    echo "seen ", visited, " nodes\n"

  var million = newSeq[KdNode](N)
  for i in 0 ..< N:
    million[i] = rand_node()
  let root = million[make_tree(million, 0, N, 0)].addr;

  block:
    var testNode = rand_node()
    let (found, best_dist, visited) = nearest(root, testNode.addr);

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
      let (found, best_dest, visited) = nearest(root, testNode.addr)
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
