import ../fancygl

import sequtils
import math
import strformat

proc `*`(a: Mat4f; b: seq[Vec4f]): seq[Vec4f] =
  result.newSeq(b.len)
  for i in 0 ..< len(b):
    result[i] = a * b[i]

type
  IdMesh = enum
    IdPyramid,
    IdCone,
    IdCylinder,
    IdIcosphere,
    IdSphere,
    IdBox,
    IdTetraeder,
    IdTorus

proc newWorldNode(x,y,z: float32): WorldNode =
  result = newWorldNode()
  result.pos.xyz = vec3f(x,y,z)

# for each mesh create one node in the world to Draw it there
var worldNodes : array[IdMesh, WorldNode] = [
  newWorldNode(-6,-6, 1),
  newWorldNode(-3, 3, 1),
  newWorldNode( 3,-3, 1),
  newWorldNode(-3,-3, 1),
  newWorldNode( 3, 3, 1),
  newWorldNode( 0, 0, 15),
  newWorldNode( 0,-6, 1),
  newWorldNode(-6, 0, 1),
]

var vertices,normals,colors: ArrayBuffer[Vec4f]
var indices: ElementArrayBuffer[int16]

type
  SimpleMesh = object
    vertexOffset: int
    numVertices: int
    baseVertex: int

var meshes: array[IdMesh, SimpleMesh]

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

  let m = mat4f(1).translate(0,0,1)

  IdPyramid.insertMesh(
    m * coneVertices(4),
    m * coneNormals(4),
    coneColors(4),
    coneIndices(4))

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
    torusVertices(numSegments, numSegments div 2, 1, 0.1),
    torusNormals(numSegments, numSegments div 2),
    torusColors(numSegments, numSegments div 2),
    torusIndicesTriangles(numSegments, numSegments div 2).map(proc(x: int32): int16 = int16(x)))

  vertices = arrayBuffer(verticesSeq)
  normals = arrayBuffer(normalsSeq)
  colors = arrayBuffer(colorsSeq)
  indices = elementArrayBuffer(indicesSeq)

proc renderMesh(id: IdMesh, projMat, viewMat, modelMat: Mat4f): void =
  let mesh = meshes[id]

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = mesh.numVertices
    vertexOffset = mesh.vertexOffset
    baseVertex = mesh.baseVertex
    indices = indices

    uniforms:
      proj = projMat
      modelView = viewMat * modelMat

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


type
  BulletState = object
    pos {.align(16).}: Vec3f
    vel {.align(16).}: Vec3f
    alive: bool

var playerBullets: array[20, BulletState]

proc renderFloor(projMat, viewMat: Mat4f, viewport: Vec4f, floorVertices: ArrayBuffer[Vec4f]): void =
  # shapes with infinitely far away points, can't interpolate alon the vertices,
  # therefore so varyings don't work.
  # The matrix transformation of can be inverted in the fragment shader, so that that in this case
  # object space coordinates can be recontructed.
  let modelViewProj = projMat * viewMat

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = 12
    uniforms:
      modelViewProj
      invModelViewProj = inverse(modelViewProj)
      invWindowSize    = vec2f(1 / viewport.z, 1 / viewport.w)
      viewport

    attributes:
      a_vertex   = floorVertices

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
      ndc_pos.xy = (gl_FragCoord.xy - viewport.xy) * invWindowSize * 2 - 1;
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

proc main*(window: Window): void =
  let windowSize = window.size
  let scale = float32(1 / 540)
  let projMat: Mat4f = frustum(-540'f32 * scale, 540'f32 * scale, -560'f32 * scale, 1360'f32 * scale, 1, 100)
  let invProjMat = inverse(projMat)

  var viewport: Vec4f
  block:
    let w = float32(windowSize.y) * 0.5625f
    viewport.x = float32(windowSize.x) * 0.5f - w * 0.5
    viewport.y = 0
    viewport.z = w
    viewport.w = float32(windowSize.y)
    glViewport(int32(viewport.x), int32(viewport.y), int32(viewport.z), int32(viewport.w))

  initMeshes()
  
  var playerNode = newWorldNode(0, 0, 0)
  var cameraNode = newWorldNode(0, -1.5, 9)
  cameraNode.lookAt(newWorldNode(0, 0, 0))

  const gunPosition = vec3f(0, -0.5, 1.5)

  var runGame: bool = true
  var frame = 0

  var joy: Joystick

  var gameTimer = newStopWatch(true)

  var floorVertices = arrayBuffer([
    vec4f(0,0,0,1), vec4f( 1, 0,0,0), vec4f( 0, 1,0,0),
    vec4f(0,0,0,1), vec4f( 0, 1,0,0), vec4f(-1, 0,0,0),
    vec4f(0,0,0,1), vec4f(-1, 0,0,0), vec4f( 0,-1,0,0),
    vec4f(0,0,0,1), vec4f( 0,-1,0,0), vec4f( 1, 0,0,0)
  ])
  
  while runGame:
    frame += 1

    # input processing

    var axisInput: Vec2f
    axisInput.x = -float32(joystickGetAxis(joy, 0)) / 32767
    axisInput.y = -float32(joystickGetAxis(joy, 1)) / 32767

    let state = getKeyboardState(nil);
    if state[SCANCODE_E] != 0:
      axisInput.y += 1
    if state[SCANCODE_D] != 0:
      axisInput.y -= 1
    if state[SCANCODE_S] != 0:
      axisInput.x -= 1
    if state[SCANCODE_F] != 0:
      axisInput.x += 1

    if axisInput.length2 > 1:
      # clamp range
      axisInput = axisInput.normalize

    cameraNode.pos.x = 0
    cameraNode.pos.y = max(playerNode.pos.y - 1.5, cameraNode.pos.y)

    var mousePos: Vec2i
    let mouseState = getMouseState(mousePos.x.addr, mousePos.y.addr)
    let relativeMousePos = ((vec2f(mousePos) - viewport.xy) / viewport.zw * 2 - 1) * vec2f(1,-1)

    let p0 = cameraNode.pos
    var p1 = cameraNode.modelMat * invProjMat * vec4f(relativeMousePos, -1, 1)
    p1 /= p1.w # normalize extended coordinates
    # project the mouse on the ground
    let mousePosWs = mix(p0, p1, -p0.z / (p1.z - p0.z))
    var tmpNode = newWorldNode(mousePosWs)
    let playerAngle    = arctan2(mousePosWs.y - playerNode.pos.y, mousePosWs.x - playerNode.pos.x)
    playerNode.dir = quat(vec3f(0,0,1), playerAngle)
    let playerModelMat = playerNode.modelMat

    for evt in events():
      case evt.kind
      of QUIT:
        runGame = false
        break
      of KEY_DOWN:
        case evt.key.keysym.scancode
        of SCANCODE_ESCAPE:
          runGame = false
          break

        of SCANCODE_F10:
          window.screenshot

        else:
          discard
      of MouseButtonDown:
        if evt.button.button == 1:
          for bullet in playerBullets.mitems:
            if not bullet.alive:
              bullet.alive = true
              bullet.pos = xyz(playerModelMat * vec4f(gunPosition,1))
              bullet.vel = xyz(playerModelMat * vec4f(20,0,0,0))
              break

      of JOY_DEVICE_ADDED:
        joy = joystickOpen(evt.jdevice.which)
      else:
        discard

    # update everything

    let viewMat = cameraNode.viewMat

    playerNode.pos.xy += axisInput * 0.05

    for bullet in playerBullets.mitems:
      if bullet.alive:
        var pos = vec4f(bullet.pos, 1) # world space
        pos = projMat * (viewMat * pos) # clip space
        # bullet not visible anymore
        if abs(pos.x / pos.w) > 1 or abs(pos.y / pos.w) > 1:
          bullet.alive = false


        bullet.pos += bullet.vel / 60'f32


    # render everything

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

    for id, node in worldNodes:
      renderMesh(id, projMat, viewMat, node.modelMat)

    renderMesh(IdTorus, projMat, viewMat, tmpNode.modelMat)


    renderMesh(IdCone, projMat, viewMat, playerModelMat.scale(1.5,1.5,2))
    renderMesh(IdSphere,  projMat, viewMat, playerModelMat.translate(0,0,2).scale(0.5))
    renderMesh(IdBox, projMat, viewMat, playerModelMat.translate(gunPosition).scale(1.0, 0.1, 0.1))

    for bullet in playerBullets:
      if bullet.alive:
        renderMesh(IdSphere, projMat, viewMat, mat4f(1).translate(bullet.pos).scale(0.1))

    renderFloor(projMat, viewMat, viewport, floorVertices)
    renderText(fmt"axisInput: {axisInput.x:5.2f} {axisInput.y:5.2f}", vec2i(22,44))
    let value = tmpNode.pos - playerNode.pos
    renderText(fmt"mouseInput: {value.x:5.1f} {value.y:5.1f}", vec2i(22,88))

    glSwapWindow(window)

when isMainModule:
  let (window, _) = defaultSetup()
  main(window)
    
# Local Variables:
# compile-command: "cd examples; nim c -r shooty.nim"
# End:
