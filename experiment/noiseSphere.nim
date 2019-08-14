import renderMacro
import glm/noise
import sequtils

let (window, context) = defaultSetup()
let windowsize = window.size

var timer = newStopWatch(true)

# disable debug notifications
glDebugMessageControl(GL_DONT_CARE,GL_DONT_CARE,GL_DEBUG_SEVERITY_NOTIFICATION, 0, nil, false)
let projection_mat : Mat4f = perspective(45'f32, window.aspectRatio, 0.1, 100.0)

type
  IdMesh = enum
    IdIcosphere,
    IdSphere,
type
  SimpleVertexType = tuple[vertex,normal,color: Vec4f]

genMeshType(SimpleMesh, SimpleVertexType)

var meshes: array[IdMesh, SimpleMesh]
var renderAll: bool = true

proc addSubdivided(dst: var seq[Vec4f]; recursionDepth: int; vertices: openarray[Vec4f]): void =
  if recursionDepth == 0:
    dst.add vertices
  else:
    doAssert vertices.len == 3
    let v1 = vertices[0]
    let v2 = 0.5f * vertices[0] + 0.5f * vertices[1]
    let v3 = vertices[1]
    let v4 = 0.5f * vertices[1] + 0.5f * vertices[2]
    let v5 = vertices[2]
    let v6 = 0.5f * vertices[2] + 0.5f * vertices[0]

    dst.addSubdivided(recursionDepth - 1, [v1,v2,v6])
    dst.addSubdivided(recursionDepth - 1, [v2,v3,v4])
    dst.addSubdivided(recursionDepth - 1, [v4,v5,v6])
    dst.addSubdivided(recursionDepth - 1, [v2,v4,v6])

proc init() =
  #var vertices,normals,colors: ArrayBuffer[Vec4f]
  #var indices: ElementArrayBuffer[int32]

  const numSegments = 32

  var verticesSeq = newSeq[SimpleVertexType](0)
  var indicesSeq  = newSeq[int32](0)

  proc insertMesh(id: IdMesh,
      newVertices, newNormals, newColors: openarray[Vec4f];
      newIndices: openarray[int32]): void =

    var indices: VertexIndexBuffer
    ## This is like ElementArrayBuffer, but it does not store at
    ## compile time the type of the elements.  The type is stored as a
    ## member in the field `typ`.

    indices.typ         = GL_UNSIGNED_INT
    indices.baseIndex   = indicesSeq.len
    indices.baseVertex  = verticesSeq.len
    indices.numVertices = newIndices.len
    indices.mode        = GL_TRIANGLES

    meshes[id].vertexIndices = indices

    for vertex in zip(newVertices, newNormals, newColors):
      verticesSeq.add(vertex)

    indicesSeq.add(newIndices)

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
    iotaSeq[int32](unrolledVertices.len.int32))

  echo "icosphere inserted ", timer.time

  unrolledVertices.setLen 0
  unrolledColors.setLen 0
  unrolledNormals.setLen 0

  var sphereIndices: seq[int32] = newSeq[int32]()

  var icosphereColors = newSeq[Vec4f](icosphereVertices.len)

  for color in icosphereColors.mitems:
    color = vec4f(rand_f32(), rand_f32(), rand_f32(), 1'f32)


  for i in countup(0, icosphereIndicesTriangles.len-1, 3):
    let idx1 = icosphereIndicesTriangles[i+0]
    let idx2 = icosphereIndicesTriangles[i+1]
    let idx3 = icosphereIndicesTriangles[i+2]

    let v1 = icosphereVertices[idx1]
    let v2 = icosphereVertices[idx2]
    let v3 = icosphereVertices[idx3]

    let n1 = vec4f(normalize(v1.xyz), 0)
    let n2 = vec4f(normalize(v2.xyz), 0)
    let n3 = vec4f(normalize(v3.xyz), 0)

    let color1 = icosphereColors[idx1]
    let color2 = icosphereColors[idx2]
    let color3 = icosphereColors[idx3]

    const numSubdivisions = 6

    unrolledVertices.addSubdivided(numSubdivisions, [v1,v2,v3])
    unrolledNormals.addSubdivided(numSubdivisions, [n1,n2,n3])
    unrolledColors.addSubdivided(numSubdivisions, [color1, color2, color3])

  echo "mesh subdivided ", timer.time

  for i, vertex in unrolledVertices.mpairs:
    vertex.xyz /= length(vertex.xyz)
    unrolledNormals[i].xyz = vertex.xyz

  echo "mesh normalized ", timer.time

  for i in countup(0, unrolledVertices.len-1, 3):
    let v1 = unrolledVertices[i+0]
    let v2 = unrolledVertices[i+1]
    let v3 = unrolledVertices[i+2]

    let normal = vec4f(normalize(cross(xyz(v2 - v1), xyz(v3-v1))), 0)

    unrolledNormals[i + 0] = normal
    unrolledNormals[i + 1] = normal
    unrolledNormals[i + 2] = normal

  IdSphere.insertMesh(
    unrolledVertices,
    unrolledNormals,
    unrolledColors,
    iotaSeq[int32](unrolledVertices.len.int32)
  )

  echo "sphere added ", timer.time

  let verticesBuffer = arrayBuffer(verticesSeq)

  let vertices = verticesBuffer.view(vertex)
  let normals = verticesBuffer.view(normal)
  let colors = verticesBuffer.view(color)
  let indices = elementArrayBuffer(indicesSeq)

  for mesh in meshes.mitems:
    mesh.vertexIndices.handle  = indices.handle
    mesh.buffers.vertex = vertices
    mesh.buffers.normal = normals
    mesh.buffers.color  = colors

  echo "init done: ", timer.time

init()

# for each mesh create one node in the world to Draw it there
var worldNode: WorldNode = newWorldNode()

var camera = newWorldNode(0,0,9)
camera.lookAt(vec3f(0.0, 1, 0.0))

var runGame: bool = true
var frame = 0
var currentMeshId : IdMesh = IdSphere
var dragMode: int

var mouseState: Vec2i

var offset = vec3f(1.123, 2.456, 3.567)

while runGame:
  frame += 1
  let time = timer.time.float32
  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KEY_DOWN:
      case evt.key.keysym.scancode
      of SCANCODE_ESCAPE:
        runGame = false
        break

      of SCANCODE_F10:
        window.screenshot

      of SCANCODE_KP_PLUS:
        currentMeshId = IdMesh((ord(currentMeshId) + 1) mod (ord(high(IdMesh)) + 1))

      of SCANCODE_KP_MINUS:
        currentMeshId = IdMesh((ord(currentMeshId) + ord(high(IdMesh))) mod (ord(high(IdMesh)) + 1))

      of SCANCODE_SPACE:
        renderAll = not renderAll
      else:
        discard

    if evt.kind in {MouseButtonDown, MouseButtonUp}:
      if evt.kind == MouseButtonDown:
        discard showCursor(0)
        discard setRelativeMouseMode(true)
        discard getMouseState(addr mouseState.x, addr mouseState.y)

        if evt.button.button == ButtonLeft:
          dragMode = dragMode or 0x1
        if evt.button.button == ButtonRight:
          dragMode = dragMode or 0x2
        if evt.button.button == ButtonMiddle:
          dragMode = dragMode or 0x4
      if evt.kind == MouseButtonUp:
        discard setRelativeMouseMode(false)
        warpMouseInWindow(nil, mouseState.x, mouseState.y)
        discard showCursor(1)

        if evt.button.button == ButtonLeft:
          dragMode = dragMode and (not 0x1)
        if evt.button.button == ButtonRight:
          dragMode = dragMode and (not 0x2)
        if evt.button.button == ButtonMiddle:
          dragMode = dragMode and (not 0x4)

    if evt.kind == MouseMotion:
      let motion = vec2f(evt.motion.xrel.float32, evt.motion.yrel.float32)
      if dragMode == 0x1:
        worldNode.turnAbsoluteX(motion.y *  0.002)
        worldNode.turnAbsoluteY(motion.x *  0.002)
      if dragMode == 0x2:
        offset.xy += motion.xy * 0.001
      if dragMode == 0x4:
        discard

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  let mesh = meshes[currentMeshId]

  let node = worldNode
  let proj = projection_mat
  let modelView = camera.viewMat * node.modelMat

  mesh.renderDebug do (v, gl):
    let pos = v.vertex.xyz
    var sum: float32
    for i in 0 ..< 5:
      sum = sum + simplex(pos * pow(2.0f, float32(i)) + offset) * pow(0.5f, float32(i))
    let height = max(sum, 0)

    let color =
      if height == 0:
        vec4f(0,0,1,1)
      elif height >= 0.75:
        vec4f(1,1,1,1)
      else:
        v.color

    let vertexTransformed = vec4f(pos * 5 + pos * height, 1)
    let worldpos = vertexTransformed

    gl.Position = proj * modelView * vertexTransformed
    # this normal is incorrect because it does not take the transformation of the vertex into concideration
    let normal_cs = modelView * v.normal

    ## rasterize

    let worldpos_dx = dFdx(worldpos.xyz)
    let worldpos_dy = dFdx(worldpos.xyz)
    # this normal is incorrect, because it creates a lot of NaN values
    let normal = normalize(cross(worldpos_dx, worldpos_dy));
    # mixed together the result is interesting
    result.color = mix(
      normal_cs.z * color,
      max(normal.z,0) * color,
      0.5
    )

  # shapes with infinitely far away points, can't interpolate alon the vertices,
  # therefore so varyings don't work.
  # The matrix transformation of can be inverted in the fragment shader, so that that in this case
  # object space coordinates can be recontructed.

  glSwapWindow(window)
