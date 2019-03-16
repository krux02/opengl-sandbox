import renderMacro

import sequtils

let (window, context) = defaultSetup()
let windowsize = window.size

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

block init:
  #var vertices,normals,colors: ArrayBuffer[Vec4f]
  #var indices: ElementArrayBuffer[int16]

  const numSegments = 32

  var verticesSeq = newSeq[SimpleVertexType](0)
  var indicesSeq  = newSeq[int16](0)

  proc insertMesh(id: IdMesh,
      newVertices, newNormals, newColors: openarray[Vec4f];
      newIndices: openarray[int16]): void =

    var indices: VertexIndexBuffer
    ## This is like ElementArrayBuffer, but it does not store at
    ## compile time the type of the elements.  The type is stored as a
    ## member in the field `typ`.

    indices.typ         = GL_UNSIGNED_SHORT
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
    iotaSeq[int16](unrolledVertices.len.int16))

  unrolledVertices.setLen 0
  unrolledColors.setLen 0
  unrolledNormals.setLen 0

  for v in icosphereVertices:
    unrolledVertices.add v
    unrolledNormals.add vec4f(normalize(v.xyz), 0)
    unrolledColors.add vec4f(rand_f32(), rand_f32(), rand_f32(), 1'f32)

  var sphereIndices: seq[int16] = newSeq[int16](icosphereIndicesTriangles.len)
  for i, idx in icosphereIndicesTriangles:
    sphereIndices[i] = int16(idx)

  IdSphere.insertMesh(
    unrolledVertices,
    unrolledNormals,
    unrolledColors,
    sphereIndices
  )


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

# for each mesh create one node in the world to Draw it there
var worldNode: WorldNode = newWorldNode()

var camera = newWorldNode(0,9,4)
camera.lookAt(vec3f(0.1,0.2,1))

var runGame: bool = true
var frame = 0
var noiseArray: array[21, float32]

for x in noiseArray.mitems:
  x = (rand_f32()*2-1) * 0.01f;

var timer = newStopWatch(true)
var currentMeshId : IdMesh

while runGame:
  frame += 1

  let time = timer.time.float32

  # just some meaningless numbers to make the shapes rotate
  worldNode.turnRelativeX(noiseArray[6])
  worldNode.turnRelativeY(noiseArray[7])
  worldNode.turnRelativeZ(noiseArray[8])

  # The plane on the ground is rotating the camera is still.  It
  # really provides the illusion the camera would rotate around the
  # shapes though

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

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  let mesh = meshes[currentMeshId]

  let node = worldNode
  let proj = projection_mat
  let modelView = camera.viewMat * node.modelMat
  mesh.render do (v, gl):
    gl.Position = proj * modelView * v.vertex
    let normal_cs = modelView * v.normal
    ## rasterize
    result.color = normal_cs.z * v.color

  # shapes with infinitely far away points, can't interpolate alon the vertices,
  # therefore so varyings don't work.
  # The matrix transformation of can be inverted in the fragment shader, so that that in this case
  # object space coordinates can be recontructed.

  glSwapWindow(window)
