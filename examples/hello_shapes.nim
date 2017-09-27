import ../fancygl

import sequtils

let (window, context) = defaultSetup()
let windowsize = window.size

let projection_mat : Mat4f = perspective(45'f32, windowsize.x / windowsize.y, 0.1, 100.0)


type
  IdMesh = enum
    IdCone,
    IdCylinder,
    IdIcosphere,
    IdSphere,
    IdBox,
    IdTetraeder,
    IdTorus

# for each mesh create one node in the world to Draw it there
var worldNodes : array[IdMesh, WorldNode] = [
  newWorldNode(-3, 3, 1),
  newWorldNode( 3,-3, 1),
  newWorldNode(-3,-3, 1),
  newWorldNode( 3, 3, 1),
  newWorldNode( 0, 0, 1),
  newWorldNode( 0,-6, 1),
  newWorldNode(-6, 0, 1)
]

var camera = newWorldNode(0,9,4)
camera.lookAt(vec3f(0.1,0.2,1))

var vertices,normals,colors: ArrayBuffer[Vec4f]
var indices: ElementArrayBuffer[int16]

type
  SimpleMesh = object
    vertexOffset: int
    numVertices: int
    baseVertex: int

var meshes: array[IdMesh, SimpleMesh]

block init:
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

var planeVertices = arrayBuffer([
  vec4f(0,0,0,1), vec4f( 1, 0,0,0), vec4f( 0, 1,0,0),
  vec4f(0,0,0,1), vec4f( 0, 1,0,0), vec4f(-1, 0,0,0),
  vec4f(0,0,0,1), vec4f(-1, 0,0,0), vec4f( 0,-1,0,0),
  vec4f(0,0,0,1), vec4f( 0,-1,0,0), vec4f( 1, 0,0,0)
])

var planeNode = newWorldNode()

var runGame: bool = true
var frame = 0

var noiseArray: array[21, float32]

for x in noiseArray.mitems:
  x = (rand_f32()*2-1) * 0.01f;

while runGame:
  frame += 1

  # just some meaningless numbers to make the shapes rotate
  worldNodes[IdCone].turnRelativeZ(noiseArray[0])
  worldNodes[IdCone].turnRelativeX(noiseArray[1])
  worldNodes[IdCone].turnRelativeY(noiseArray[2])

  worldNodes[Idcylinder].turnRelativeX(noiseArray[3])
  worldNodes[Idcylinder].turnRelativeY(noiseArray[4])
  worldNodes[Idcylinder].turnRelativeZ(noiseArray[5])

  worldNodes[Idicosphere].turnRelativeX(noiseArray[6])
  worldNodes[Idicosphere].turnRelativeY(noiseArray[7])
  worldNodes[Idicosphere].turnRelativeZ(noiseArray[8])

  worldNodes[Idsphere].turnRelativeX(noiseArray[9])
  worldNodes[Idsphere].turnRelativeY(noiseArray[10])
  worldNodes[Idsphere].turnRelativeZ(noiseArray[11])

  worldNodes[Idbox].turnRelativeX(noiseArray[12])
  worldNodes[Idbox].turnRelativeY(noiseArray[13])
  worldNodes[Idbox].turnRelativeZ(noiseArray[14])

  worldNodes[Idtetraeder].turnRelativeX(noiseArray[15])
  worldNodes[Idtetraeder].turnRelativeY(noiseArray[16])
  worldNodes[Idtetraeder].turnRelativeZ(noiseArray[17])

  worldNodes[Idtorus].turnRelativeX(noiseArray[18])
  worldNodes[Idtorus].turnRelativeY(noiseArray[19])
  worldNodes[Idtorus].turnRelativeZ(noiseArray[20])

  # the plane on the ground is rotating the camera is still.  It
  # really provides the illusion the camera would rotate around the
  # shapes though
  planeNode.turnAbsoluteZ(0.0001)

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

      else:
        discard

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  let magic = int32(frame mod 2)

  for i, node in worldNodes:
    let mesh = meshes[i]

    shadingDsl:
      debug
      primitiveMode = GL_TRIANGLES
      numVertices = mesh.numVertices
      vertexOffset = mesh.vertexOffset
      baseVertex = mesh.baseVertex
      indices = indices

      uniforms:
        proj = projection_mat
        modelView = camera.viewMat * node.modelMat
        magic

      attributes:
        a_vertex = vertices
        a_normal = normals
        a_color  = colors

      vertexMain:
        """
        gl_Position = proj * modelView * a_vertex;
        v_vertex = a_vertex;
        v_normal = modelView * a_normal;
        v_color = a_color;
        """
      vertexOut:
        "out vec4 v_vertex"
        "out vec4 v_normal"
        "out vec4 v_color"

      fragmentMain:
        """
        // cheap fake lighting from camera direction
        color = v_color * v_normal.z;
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
      vec2 texcoord = objPos.xy;
      vec2 texcoord_dx = fwidth(texcoord);
      //vec2 texcoord_dy = dFdy(texcoord);
      color = vec4(0,0,0,1);
      // antialiasing
      for(int i = 0; i <= 4; ++i) {
        vec2 offset = texcoord_dx * 0.5 * (float(i-2) / 2.0);
        color.rg += fract(texcoord + offset) / 5;
      }

      """

  glSwapWindow(window)
