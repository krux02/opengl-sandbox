import ../fancygl

import sequtils

let (window, context) = defaultSetup()
let windowsize = window.size

let projection_mat : Mat4f = perspective(45'f32, windowsize.x / windowsize.y, 0.1, 100.0)

type
  SimpleMesh = object
    vertices,normals,colors: ArrayBuffer[Vec4f]
    indices: ElementArrayBuffer[int16]

var coneNode = newWorldNode()
coneNode.pos.xyz = vec3f(-3, 3,1)

var cylinderNode = newWorldNode()
cylinderNode.pos.xyz = vec3f(3,-3,1)

var icosphereNode = newWorldNode()
icosphereNode.pos.xyz = vec3f(-3,-3,1)

var sphereNode = newWorldNode()
sphereNode.pos.xyz = vec3f(3,3,1)

var boxNode = newWorldNode()
boxNode.pos.xyz = vec3f(0,0,1)

var tetraederNode = newWorldNode()
tetraederNode.pos.xyz = vec3f(0,-6,1)

var torusNode = newWorldNode()
torusNode.pos.xyz = vec3f(-6,0,1)

var camera = newWorldNode()
camera.pos.xyz = vec3f(0,9,4)
camera.lookAt(vec3f(0,0,1))

const numSegments = 32

var coneMesh, cylinderMesh, icosphereMesh, sphereMesh, boxMesh, tetraederMesh, torusMesh: SimpleMesh

block init:
  proc texCoord2Color(x: Vec2f): Vec4f = vec4f(x,0,1)
  coneMesh.vertices  = arrayBuffer(coneVertices(numSegments))
  coneMesh.normals   = arrayBuffer(coneNormals(numSegments))
  coneMesh.colors    = arrayBuffer(coneTexCoords(numSegments).map(texCoord2Color))
  coneMesh.indices   = elementArrayBuffer(coneIndices(numSegments))

  cylinderMesh.vertices  = arrayBuffer(cylinderVertices(numSegments))
  cylinderMesh.normals   = arrayBuffer(cylinderNormals(numSegments))
  cylinderMesh.colors    = arrayBuffer(cylinderTexCoords(numSegments).map(texCoord2Color))
  cylinderMesh.indices   = elementArrayBuffer(cylinderIndices(numSegments))

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

  icosphereMesh.vertices = arrayBuffer(unrolledVertices)
  icosphereMesh.colors   = arrayBuffer(unrolledColors)
  icosphereMesh.normals  = arrayBuffer(unrolledNormals)
  icosphereMesh.indices  = elementArrayBuffer(iotaSeq[int16](unrolledVertices.len.int16))

  sphereMesh.vertices = arrayBuffer(uvSphereVertices(numSegments, numSegments div 2))
  sphereMesh.colors   = arrayBuffer(uvSphereTexCoords(numSegments, numSegments div 2).map(texCoord2Color))
  sphereMesh.normals  = arrayBuffer(uvSphereNormals(numSegments, numSegments div 2))
  sphereMesh.indices  = elementArrayBuffer(uvSphereIndices(numSegments, numSegments div 2))

  boxMesh.vertices = arrayBuffer(boxVertices)
  boxMesh.colors = arrayBuffer(boxColors)
  boxMesh.normals = arrayBuffer(boxNormals)
  boxMesh.indices = elementArrayBuffer(iotaSeq[int16](boxVertices.len.int16))

  tetraederMesh.vertices = arrayBuffer(tetraederVertices)
  tetraederMesh.colors = arrayBuffer(tetraederColors)
  tetraederMesh.normals = arrayBuffer(tetraederNormals)
  tetraederMesh.indices = elementArrayBuffer(iotaSeq[int16](tetraederVertices.len.int16))

  torusMesh.vertices = arrayBuffer(torusVertices(numSegments, numSegments div 2, 1, 0.5))
  torusMesh.colors   = arrayBuffer(torusTexCoords(numSegments, numSegments div 2).map(texCoord2Color))
  torusMesh.normals  = arrayBuffer(torusNormals(numSegments, numSegments div 2))
  torusMesh.indices  = elementArrayBuffer(torusIndicesTriangles(numSegments, numSegments div 2).map(proc(x: int32): int16 = int16(x)))

  glDisable(GL_DEPTH_CLAMP)

let icosphereVerticesLen = icosphereMesh.vertices.len

var planeVertices = arrayBuffer([
  vec4f(0,0,0,1), vec4f( 1, 0,0,0), vec4f( 0, 1,0,0),
  vec4f(0,0,0,1), vec4f( 0, 1,0,0), vec4f(-1, 0,0,0),
  vec4f(0,0,0,1), vec4f(-1, 0,0,0), vec4f( 0,-1,0,0),
  vec4f(0,0,0,1), vec4f( 0,-1,0,0), vec4f( 1, 0,0,0)
])

var planeNode = newWorldNode()

var evt: Event = defaultEvent
var runGame: bool = true

var frame = 0


var noiseArray: array[21, float32]

for x in noiseArray.mitems:
  x = (rand_f32()*2-1) * 0.01f;

while runGame:
  frame += 1

  # just some meaningless numbers to make the shapes rotate
  coneNode.turnRelativeZ(noiseArray[0])
  coneNode.turnRelativeX(noiseArray[1])
  coneNode.turnRelativeY(noiseArray[2])

  cylinderNode.turnRelativeX(noiseArray[3])
  cylinderNode.turnRelativeY(noiseArray[4])
  cylinderNode.turnRelativeZ(noiseArray[5])

  icosphereNode.turnRelativeX(noiseArray[6])
  icosphereNode.turnRelativeY(noiseArray[7])
  icosphereNode.turnRelativeZ(noiseArray[8])

  sphereNode.turnRelativeX(noiseArray[9])
  sphereNode.turnRelativeY(noiseArray[10])
  sphereNode.turnRelativeZ(noiseArray[11])

  boxNode.turnRelativeX(noiseArray[12])
  boxNode.turnRelativeY(noiseArray[13])
  boxNode.turnRelativeZ(noiseArray[14])

  tetraederNode.turnRelativeX(noiseArray[15])
  tetraederNode.turnRelativeY(noiseArray[16])
  tetraederNode.turnRelativeZ(noiseArray[17])

  torusNode.turnRelativeX(noiseArray[18])
  torusNode.turnRelativeY(noiseArray[19])
  torusNode.turnRelativeZ(noiseArray[20])

  # the plane on the ground is rotating the camera is still.
  # I really provides the illusion the camera would rotate
  # around the shapes though
  planeNode.turnAbsoluteZ(0.0001)

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SDL_SCANCODE_ESCAPE:
        runGame = false
        break

      of SDL_SCANCODE_F10:
        window.screenshot

      else:
        discard

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  let magic = int32(frame mod 2)

  #proc renderShape(node: WorldNode, vertices, normals, texcoords: ArrayBuffe[Vec4f])

  for i, node in [coneNode, cylinderNode, icosphereNode, sphereNode, boxNode, tetraederNode, torusNode]:
    let mesh   = [coneMesh, cylinderMesh, icosphereMesh, sphereMesh, boxMesh, tetraederMesh, torusMesh][i]

    shadingDsl:
      primitiveMode = GL_TRIANGLES
      numVertices = mesh.indices.len

      indices = mesh.indices

      uniforms:
        proj = projection_mat
        modelView = camera.viewMat * node.modelMat
        magic

      attributes:
        a_vertex   = mesh.vertices
        a_normal   = mesh.normals
        a_texCoord = mesh.colors

      vertexMain:
        """
        gl_Position = proj * modelView * a_vertex;
        v_normal = modelView * a_normal;
        v_Color = a_texCoord;
        """
      vertexOut:
        "out vec4 v_normal"
        "out vec4 v_Color"

      fragmentMain:
        """
        // cheap fake lighting from camera direction
        color = v_Color * v_normal.z;
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
      color = vec4(fract(objPos.xy), 0, 1);
      """

  glSwapWindow(window)
