import ../fancygl, sequtils, AntTweakBar

# not read yet te be part of the nimble build system

template addVarRW(bar: TwBar; value: var float32, stuff: string= ""): void =
  discard TwAddVarRW(bar,astToStr(value), TW_TYPE_FLOAT, value.addr, stuff)

template addVarRW(bar: TwBar; value: var bool, stuff: string = ""): void =
  discard TwAddVarRW(bar,astToStr(value), TW_TYPE_BOOL8, value.addr, stuff)

template addVarRW(bar: TwBar; value: var Quatf, stuff: string = ""): void =
  discard TwAddVarRW(bar,astToStr(value), TW_TYPE_QUAT4F, value.addr, stuff)

var mouseModeToggle = true

const enableChecks = false

proc sq(arg: float32): float32 =
  arg * arg

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

  Sphere = object
    center: Vec3f
    radius: float32

  Tree = object
    data: seq[Value]
    nodes: seq[Node]
    aabb: AABB

  Value = object
    position: Vec3f
    color: Color
    velocity: Vec3f
    kind: IdMesh
    acceleration: Vec3f
    align: float32

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


var separationFactor           = 1.5f
var alignFactor                = 1.0f
var cohesionFactor             = 1.0f
var desiredseparation          = 25.0f
var neighbordist               = 50.0f
var maxspeed                   = 2.0f
var maxforce                   = 0.03f
var enableBoxDrawing           = false
var enableNeighbourhoodDrawing = false
var enableDrawForces           = false
var stepSimulation             = false

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
    boxVerticesCenterAtZero,
    boxNormals,
    boxColors,
    iotaSeq[int16](boxVerticesCenterAtZero.len.int16))

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
  dst.setLen 0
  leafNodeBoxes(this, this.aabb, this.nodes[0], dst)

proc leafNodeBoxes(this: Tree): seq[AABB] =
  result.newSeq(0)
  leafNodeBoxes(this, this.aabb, this.nodes[0], result)

proc divide_by_mid(t: var Tree, a,b: int32, mid: Vec3f, dim: 0..2): int32 =
  var i = a
  var j = b-1

  while true:
    # find first element from the left that should be on the right
    while i < b and t.data[i].position.arr[dim] < mid.arr[dim]:
      i += 1
    # find first ellement from the right thet shuld be on the left
    while j >= a and t.data[j].position.arr[dim] >= mid.arr[dim]:
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
    let v1 = t.data[k].position.arr[dim]
    let v2 = mid.arr[dim]
    if (v1 < v2) != (k < result):
      fail = true
  if fail:
    echo s"a $a res $result b $b"
    for k in a ..< b:
      let v1 = t.data[k].position.arr[dim]
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
        assert(childBB.contains(this.data[j].position))

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
    tree.aabb.incl(d.position)

  tree.nodes.reset
  discard tree.subtreeInit(tree.aabb, 0, int32(tree.data.len))

proc squaredDist(box: AABB; position: Vec3f): float32 =
  for i in 0 ..< 3:
    let v = position.arr[i]
    let min = box.min.arr[i]
    let max = box.max.arr[i]

    if v < min:
      result += sq(min - v)
    if v > max:
      result += sq(v - max)

proc intersect(a: AABB; b: Sphere): bool =
  squaredDist(a, b.center) < b.radius * b.radius

proc intersect(a: Sphere; position: Vec3f): bool =
  length2(a.center - position) <= a.radius * a.radius

proc neighborSearch(tree: Tree; node: Node; aabb: AABB; query: Sphere; skipIndex: int; dst: var seq[(int32,int32)]): void =
  if node.b <= skipIndex:
    return

  if not intersect(aabb, query):
    return

  if node.isLeaf:
    for i in node.a ..< node.b:
      let data = tree.data[i]
      if intersect(query, data.position):
        dst.add((int32(skipIndex), int32(i)))
  else:
    for i, idx in node.children:
      let childNode = tree.nodes[idx]
      let childAABB = aabb.octreeChild(i)
      neighborSearch(tree, childNode, childAABB,  query, skipIndex, dst)

proc neighborSearch(tree: Tree; radius: float32; dst: var seq[(int32,int32)]): void =
  dst.reset
  for i, value in tree.data:
    let query = Sphere(center: value.position, radius: radius)
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
    d.position = randomSpiralPosition()
    d.velocity = randomSpiralVelocity()
  result.init

var tree = spiralOctree(1000)

let (window, context) = defaultSetup()
discard setRelativeMouseMode(mouseModeToggle)


if TwInit(TW_OPENGL_CORE, nil) == 0:
  quit("could not initialize AntTweakBar: " & $TwGetLastError())

discard TwWindowSize(window.size.x, window.size.y)

var searchRadius = 10'f32

var bar = TwNewBar("TwBar")

bar.addVarRW searchRadius, " precision=5 step=0.002"
bar.addVarRW separationFactor
bar.addVarRW alignFactor
bar.addVarRW cohesionFactor
bar.addVarRW desiredseparation
bar.addVarRW neighbordist
bar.addVarRW maxspeed
bar.addVarRW maxforce, " step = 0.01"
bar.addVarRW enableBoxDrawing
bar.addVarRW enableNeighbourhoodDrawing
bar.addVarRW enableDrawForces
bar.addVarRW stepSimulation

proc limit(arg: Vec3f; maxlength: float32): Vec3f =
  let len2 = arg.length2
  if len2 > maxlength * maxlength:
    arg / len2
  else:
    arg

proc dist(a,b: Vec3f): float32 =
  length(a - b)

######################
# Flocking Behiviour #
######################

proc flock(tree: var Tree; drawForcesData: var seq[tuple[position: Vec3f; color: Color]]): void =
  var neighborSearchResult: seq[(int32,int32)] # TODO does it need init
  # Separation
  # Method checks for nearby boids and steers away
  tree.neighborSearch(desiredseparation, neighborSearchResult)
  var separation = newSeq[tuple[steer: Vec3f; count: int32]](tree.data.len)

  for i,j in neighborSearchResult.items:
    var diff = tree.data[i].position - tree.data[j].position

    let length2 = diff.length2

    if length2 > 0:
      diff *= 1 / length2

      separation[i].steer += diff
      separation[i].count += 1
      separation[j].steer -= diff
      separation[j].count += 1

  for i, it in separation.mpairs:
    assert it.steer == it.steer

    if it.count > 0:
      it.steer /= float32(it.count)

    assert it.steer == it.steer

  for i, it in separation.mpairs:
    #template velocity(): untyped = tree.data[i].velocity
    if it.steer.length2 > 0:
      it.steer *= maxspeed / it.steer.length
      it.steer -= tree.data[i].velocity
      it.steer = it.steer.limit(maxforce)

    assert it.steer == it.steer

  tree.neighborSearch(neighbordist, neighborSearchResult)

  # Alignment
  # For every nearby boid in the system, calculate the average velocity

  var align = newSeq[tuple[steer: Vec3f; count: int32; sum: Vec3f]](tree.data.len)

  for i,j in neighborSearchResult.items:
    let position_i = tree.data[i].position
    let v_i = tree.data[i].velocity
    let position_j = tree.data[j].position
    let v_j = tree.data[j].velocity

    align[i].sum   += v_j
    align[i].count += 1
    align[j].sum   += v_i
    align[j].count += 1

  for i, it in align.mpairs:
    if it.count > 0 and it.sum.length2 > 0:
      #it.sum /= float32(it.count)
      let tmp = normalize(it.sum) * maxspeed
      let velocity = tree.data[i].velocity
      it.steer = limit(tmp - velocity, maxforce)
    assert it.steer == it.steer

  # Cohesion
  # For the average position (i.e. center) of all nearby boids, calculate steering vector towards that position

  var cohesion = newSeq[tuple[steer: Vec3f; count: int32; sum: Vec3f]](tree.data.len)

  for i,j in neighborSearchResult.items:
    cohesion[i].sum += tree.data[j].position
    cohesion[i].count += 1
    cohesion[j].sum += tree.data[i].position
    cohesion[j].count += 1

  for i, it in cohesion.mpairs:
    if it.count > 0:
      assert it.sum == it.sum
      # seek
      let target = it.sum / float32(it.count)
      # A vector pointing from the position to the target
      var desired = target - tree.data[i].position
      assert desired == desired

      # Scale to maximum speed
      if desired.length2 > 0:
        desired *= maxspeed / desired.length
        # Steering = Desired minus Velocity
        it.steer = limit(desired - tree.data[i].velocity, maxforce)

    assert it.steer == it.steer

  for i, tup in separation:
    tree.data[i].acceleration += tup.steer * separationFactor
  for i, tup in align:
    tree.data[i].acceleration += tup.steer * alignFactor
  for i, tup in cohesion:
    tree.data[i].acceleration += tup.steer * cohesionFactor

  if enableDrawForces:
    # var data = newSeqOfCap[tuple[position: Vec3f; color: Color]](tree.data.len * 2 * 3)
    drawForcesData.setLen(0)

    for i, it in tree.data:
      let center = it.position
      let colors = [
        Color(r: 255, g: 0, b: 0),
        Color(r: 0, g: 255, b: 0),
        Color(r: 0, g: 0, b: 255)
      ]

      drawForcesData.add( (position: center, color: colors[0]) )
      drawForcesData.add( (position: center + separation[i].steer, color: colors[0]) )
      drawForcesData.add( (position: center, color: colors[1]) )
      drawForcesData.add( (position: center + align[i].steer, color: colors[1]) )
      drawForcesData.add( (position: center, color: colors[2]) )
      drawForcesData.add( (position: center + cohesion[i].steer, color: colors[2]) )

  # apply flocking forces to velocity and velocity to position
  for node in tree.data.mitems:
    # Update velocity
    node.velocity += node.acceleration

    # Limit speed
    node.velocity = limit(node.velocity, maxspeed);
    node.position += node.velocity;
    # Reset accelertion to 0 each cycle
    node.acceleration = vec3f(0)

  # when the positions update the tree needs to be rebuild
  tree.init

##########################
# End Flocking Behiviour #
##########################

initMeshes()

var treeDataBuffer = arrayBuffer(tree.data, GL_STREAM_DRAW)
let posBuffer      = treeDataBuffer.view(position)

for value in tree.data.mitems:
  value.color.r = rand_u8()
  value.color.g = rand_u8()
  value.color.b = rand_u8()
  value.color.a = 255
  value.kind = IdMesh rand(IdMesh.high.int + 1)

var runGame: bool = true

let timer = newStopWatch(true)

let aspect = window.aspectRatio.float32
let proj : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 10000.0)

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
var boxesBuffer = createArrayBuffer[AABB](boxes.len * 2, GL_STREAM_DRAW)

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
      vec4 position_ws = vec4(mix(a_min, a_max, a_vertex.xyz), 1);
      gl_Position = modelViewProj * position_ws;
      """
    fragmentMain:
      """
      color = vec4(0,0,1,1);
      """

# proc drawForces(proj, modelView: Mat4f; drawForcesBuffer: ArrayBuffer[tuple[position: Vec3f; color: Color]]): void =
proc drawForces(proj, modelView: Mat4f; a_vertex: ArrayBufferView[Vec3f], a_color: ArrayBufferView[Color], numVertices: int): void =
  glDisable(GL_DEPTH_TEST)
  shadingDsl:
    primitiveMode = GL_LINES
    numVertices = numVertices
    uniforms:
      modelViewProj = proj * modelView
    attributes:
      a_vertex# = drawForcesBuffer.view(position)
      a_color# = drawForcesBuffer.view(color)
    vertexMain:
      """
      gl_Position = modelViewProj * vec4(a_vertex, 1);
      v_color = a_color;
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = v_color;
      """
  glEnable(GL_DEPTH_TEST)
      
var camera = newWorldNode()
var cameraControls: CameraControls
cameraControls.speed = 300
addEventWatch(cameraControlEventWatch, cameraControls.addr)

var neighborSearchResult: seq[(int32,int32)]
var neighborSearchResultBuffer = createElementArrayBuffer[int32](40000, GL_STREAM_DRAW)

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

proc drawMeshInstanced(proj, modelView: Mat4f, mesh: SimpleMesh, positions: ArrayBufferView[Vec3f], instanceColors: ArrayBufferView[Color], numInstances: int): void =
  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = mesh.numVertices
    vertexOffset = mesh.vertexOffset
    baseVertex = mesh.baseVertex
    indices = indices
    numInstances = numInstances

    uniforms:
      proj
      view = modelView

    attributes:
      a_vertex = vertices
      a_normal = normals
      a_color  = colors
      instancePos = positions {.divisor: 1.}
      instanceColor = instanceColors {.divisor: 1.}

    vertexMain:
      """
      gl_Position = proj * view * (a_vertex + vec4(instancePos,0));
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

#var animation: GifAnimation
var remainingFrames = 0
var remainingSimulationSteps = 0

var time, lastTime: float64

var drawForcesBuffer: ArrayBuffer[tuple[position: Vec3f; color: Color]]
var drawForcesData: seq[tuple[position: Vec3f; color: Color]]

var newRenderFunction = false

while runGame:

  discard showCursor(if mouseModeToggle: 1 else: 0)
  var evt: Event
  while pollEvent(evt.addr) != 0:
    if not mouseModeToggle and TwEventSDL(cast[pointer](evt.addr), 2.cuchar, 0.cuchar) != 0:
      continue

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
        discard
        #animation = window.startGifAnimation(delay = 1, dither = false)
        #remainingFrames = 100
      of SCANCODE_1:
        enableBoxDrawing           = not enableBoxDrawing
      of SCANCODE_2:
        enableNeighbourhoodDrawing = not enableNeighbourhoodDrawing
      of SCANCODE_3:
        enableDrawForces           = not enableDrawForces
      of SCANCODE_4:
        stepSimulation             = not stepSimulation
      of SCANCODE_5:
        newRenderFunction = not newRenderFunction
      of SCANCODE_SPACE:
        if stepSimulation:
          remainingSimulationSteps += 1
      else:
        discard

    if evt.kind == MOUSE_BUTTON_DOWN:
      case evt.button.button
      of 3:
        mouseModeToggle = not mouseModeToggle
        discard setRelativeMouseMode(mouseModeToggle)
      else:
        discard

  lastTime = time
  time = timer.time
  let deltaTime = float32(time-lastTime)

  if mouseModeToggle:
    camera.update(cameraControls, deltaTime)
    
  if stepSimulation:
    while remainingSimulationSteps > 0:
      remainingSimulationSteps -= 1
      tree.flock(drawForcesData)
  else:
    tree.flock(drawForcesData)

  treeDataBuffer.setData(tree.data)

  #tree.catchUpOrder(objectDataSeq)
  #objectDataBuffer.setData(objectDataSeq)

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
  let modelView = camera.viewMat

  if enableNeighbourhoodDrawing:
    drawNeighborhood(proj, modelView)

  var buffer = createArrayBuffer[tuple[offset: Vec3f, color: Color]](tree.data.len)
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
          buffer[i].offset = value.position
          buffer[i].color  = value.color
          i += 1

    drawMeshInstanced(proj, modelView, mesh, buffer.view(offset), buffer.view(color), i)

  if enableBoxDrawing:
    drawBoxes(proj, modelView)

  if enableDrawForces and len(drawForcesData) > 0:
    #drawForces(proj, modelView, drawForcesBuffer)
    if drawForcesBuffer.handle == 0:
      drawForcesBuffer = arrayBuffer(drawForcesData)
    else:
      drawForcesBuffer.setData(drawForcesData)
      
    drawForces(proj, modelView, drawForcesBuffer.view(position), drawForcesBuffer.view(color), len(drawForcesBuffer))

  discard TwDraw()
  glSwapWindow(window)

  #if remainingFrames > 0:
  #  animation.frameGifAnimationGl()
  #  remainingFrames -= 1
  #  if remainingFrames == 0:
  #    animation.endGifAnimation()


#when isMainModule:
#  main()

# Local Variables:
# compile-command: "cd examples; nim c -r octree.nim"
# End:
