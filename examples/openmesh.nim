import ../fancygl, memfiles, OpenMesh, hashes, tables, mersenne

var mt = newMersenneTwister(0)

proc randomColor() : Color =
  result.r = cast[uint8](mt.getNum)
  result.g = cast[uint8](mt.getNum)
  result.b = cast[uint8](mt.getNum)
  result.a = 255.uint8

proc hash(v: Vec3f): Hash =
  hash(v.arr)

createMeshType(MyMeshType):
  #debug
  type
    VertexData = object
      point:         Vec3f
      normal:        Vec3f
      boundary:      bool

    FaceData = object
      color: Color
      state: int8
      stateNext: int8

    HalfedgeData = object
      texCoord:      Vec2f

    EdgeData = object
      someValue : int32

proc connect(face: MyMeshType.FaceRef; halfedge: MyMeshType.HalfedgeRef) =
  let mesh = face.mesh
  assert mesh == halfedge.mesh
  halfedge.connectivity.face_handle = face.handle
  face.connectivity.halfedge_handle = halfedge.handle

proc connect(face: MyMeshType.FaceRef; he1, he2, he3: MyMeshType.HalfedgeRef) =
  he1.connectivity.face_handle = face.handle
  he2.connectivity.face_handle = face.handle
  he3.connectivity.face_handle = face.handle

  he1.connectivity.next_halfedge_handle = he2.handle
  he2.connectivity.next_halfedge_handle = he3.handle
  he3.connectivity.next_halfedge_handle = he1.handle

  he1.connectivity.prev_halfedge_handle = he3.handle
  he2.connectivity.prev_halfedge_handle = he1.handle
  he3.connectivity.prev_halfedge_handle = he2.handle

  face.connectivity.halfedge_handle = he1.handle

proc extrude(face: MyMeshType.FaceRef): void =
  let he1 = face.goHalfedge
  let he2 = he1.goNext
  let he3 = he2.goNext

  let vert1 = he1.goToVertex
  let vert2 = he2.goToVertex
  let vert3 = he3.goToVertex

  let vec1 = vert2.propPoint - vert1.propPoint
  let vec2 = vert3.propPoint - vert2.propPoint
  let vec3 = vert1.propPoint - vert3.propPoint

  let averageLength = (length(vec1) + length(vec2) + length(vec3)) / 3.0'f32
  let center = (vert1.propPoint + vert2.propPoint + vert3.propPoint) / 3.0'f32
  var n = normalize(cross(-vec1, vec2))

  let mesh = face.mesh
  let newVert = mesh[].addVertex()
  newVert.propPoint() = center + n * averageLength

  let newEdge1A = mesh[].addEdge.goHalfEdge()
  let newEdge1B = newEdge1A.goOpp
  let newEdge2A = mesh[].addEdge.goHalfEdge()
  let newEdge2B = newEdge2A.goOpp
  let newEdge3A = mesh[].addEdge.goHalfEdge()
  let newEdge3B = newEdge3A.goOpp

  newEdge1A.connectivity.vertex_handle = newVert.handle
  newEdge1B.connectivity.vertex_handle = vert1.handle
  newEdge2A.connectivity.vertex_handle = newVert.handle
  newEdge2B.connectivity.vertex_handle = vert2.handle
  newEdge3A.connectivity.vertex_handle = newVert.handle
  newEdge3B.connectivity.vertex_handle = vert3.handle

  let face1 = face
  face1.propColor() = randomColor()
  connect(face1, he1, newEdge1A, newEdge3B)
  let face2 = mesh[].addFace()
  face2.propColor() = randomColor()
  connect(face2, he2, newEdge2A, newEdge1B)
  let face3 = mesh[].addFace()
  face3.propColor() = randomColor()
  connect(face3, he3, newEdge3A, newEdge2B)


################################################################################
######################### prepare render vertex array ##########################
################################################################################

proc updateRenderBuffers(mymesh: var MyMeshType,
  renderPositionBuffer: var ArrayBuffer[Vec3f],
  renderNormalBuffer:   var ArrayBuffer[Vec3f],
  renderTexCoordBuffer: var ArrayBuffer[Vec2f],
  renderColorBuffer:    var ArrayBuffer[Color],
  faceColors:           var Texture1D): void =

  var
    renderPositionSeq = newSeq[renderPositionBuffer.T](0)
    renderNormalSeq   = newSeq[renderNormalBuffer.T](0)
    renderTexCoordSeq = newSeq[renderTexCoordBuffer.T](0)
    renderColorSeq    = newSeq[renderColorBuffer.T](0)

  for face in mymesh.faceRefs:
    for halfedge in face.circulateInHalfedges:
      let vertex = halfedge.goToVertex

      renderPositionSeq.add vertex.propPoint
      renderNormalSeq.add   vertex.propNormal
      renderTexCoordSeq.add halfedge.propTexCoord
      renderColorSeq.add    face.propColor

  renderPositionBuffer.setData(renderPositionSeq)
  renderNormalBuffer.setData(renderNormalSeq)
  renderTexCoordBuffer.setData(renderTexCoordSeq)
  renderColorBuffer.setData(renderColorSeq)
  faceColors.setData(mymesh.faceProps.color)

################################################################################
#################################### Main ######################################
################################################################################

proc `$`(arg: Color): string =
  result.add "("
  result.addInt int64(arg.r)
  result.add ", "
  result.addInt int64(arg.g)
  result.add ", "
  result.addInt int64(arg.b)
  result.add ")"


proc main() =
  let (window, context) = defaultSetup()
  let 
    WindowSize = vec2f(window.size)
    Near = 0.1f
    Far = 1000.0f
    halfHeight = Near
    halfWidth  = Near * (WindowSize.x / WindowSize.y)
    projection = frustum(-halfWidth, halfWidth, -halfHeight, halfHeight, Near, Far)
  

  var file = memfiles.open(getResourcePath("mrfixit.iqm"))
  defer:
    close(file)

  let header = cast[ptr iqmheader](file.mem)
  echo "version:   ", header.version

  #let texts = header.getTexts
  #var textTextures = newSeq[TextureRectangle](texts.len)
  #var textWidths = newSeq[cint](texts.len)

  template text(offset : int32) : cstring =
    cast[cstring](cast[uint](file.mem) + header.ofs_text.uint + offset.uint)

  let meshData = header.getMeshData
  let triangles = header.getTriangles
  #let indices = header.getIndices
  #let adjacencies = header.getAdjacencies
  let meshes = header.getMeshes

  #let numVertices = header.num_vertexes.int

  var meshTextures = newSeq[Texture2D](meshes.len)
  for i, mesh in meshes:
    meshTextures[i] = loadTexture2DFromFile( getResourcePath($text(mesh.material)) )

  echo "=========================================================================="

  ################################################################################
  ############################ transform file to mesh ############################
  ################################################################################

  var mymesh: MyMeshType
  # halfedges at the same edge need to be stored together

  var vertexPairs = initTable[tuple[v1,v2:int], HalfedgeHandle]()
  var vertexTable = initTable[Vec3f, VertexHandle]()

  proc lazyHalfedge(vertex1,vertex2: MyMeshType.VertexRef): MyMeshType.HalfedgeRef =
    #lookup in order
    let v1  = vertex1.handle.int
    let v2  = vertex2.handle.int
    let e = (v1: v1, v2: v2)
    if vertexPairs.haskey(e):
      result.mesh = mymesh.addr
      result.handle = vertexPairs[e]
      # key e can now be taken out of the table again, because
      # it is not expected to be drawn again
    else:
      # new halfedge pair needs to be created
      let
        edge = mymesh.addEdge
        halfedge0 = edge.goHalfedge
        halfedge1 = edge.goHalfedge.goOpp

      result = halfedge0
      # insert in inverse order
      vertexPairs[(v1: v2, v2: v1)] = halfedge1.handle

  proc lazyVertex(position: Vec3f): MyMeshType.VertexRef =
    if vertexTable.haskey(position):
      result.mesh = mymesh.addr
      result.handle = vertexTable[position]
    else:
      result = mymesh.addVertex
      result.propPoint() = position
      vertexTable[position] = result.handle

  echo "triangles: ", triangles.len
  for triIndex, tri in triangles:
    let
      faceHandle = FaceHandle(triIndex)

      pos0 = meshData.position[tri.vertex[0]]
      pos1 = meshData.position[tri.vertex[1]]
      pos2 = meshData.position[tri.vertex[2]]

      v0 = lazyVertex(pos0)
      v1 = lazyVertex(pos1)
      v2 = lazyVertex(pos2)

      halfedge0 = lazyHalfedge(v0, v1)
      halfedge1 = lazyHalfedge(v1, v2)
      halfedge2 = lazyHalfedge(v2, v0)

    #echo halfedge0.handle, " ", halfedge1.handle, " ", halfedge2.handle

    v0.connectivity.out_halfedge_handle = halfedge0.handle
    v1.connectivity.out_halfedge_handle = halfedge1.handle
    v2.connectivity.out_halfedge_handle = halfedge2.handle


    halfedge0.connectivity() = Halfedge(
             face_handle:   faceHandle,
             vertex_handle: v1.handle,
             next_halfedge_handle: halfedge1.handle,
             prev_halfedge_handle: halfedge2.handle)

    halfedge1.connectivity() = Halfedge(
             face_handle:   faceHandle,
             vertex_handle: v2.handle,
             next_halfedge_handle: halfedge2.handle,
             prev_halfedge_handle: halfedge0.handle)

    halfedge2.connectivity() = Halfedge(
             face_handle:   faceHandle,
             vertex_handle: v0.handle,
             next_halfedge_handle: halfedge0.handle,
             prev_halfedge_handle: halfedge1.handle)

    # todo add the correct mesh at boundary
    let face = mymesh.addFace
    face.connectivity.halfedge_handle = halfedge0.handle

  for face in mymesh.faceRefs:
    face.propColor() = Color(r:11,g:11,b:11)


  echo "numVertices: ", mymesh.vertices.len
  echo "numEdges:    ", mymesh.edges.len
  echo "numFaces:    ", mymesh.faces.len

  var brokenHalfedges = 0

  for i,vertex in mymesh.vertices:
    if vertex.out_halfedge_handle.isInvalid:
      echo "invalid halfedge handle in vertex at index ", i
  for i, edge in mymesh.edges:
    for j, halfedge in edge:
      if halfedge.face_handle.isInvalid or
         halfedge.vertex_handle.is_invalid or
         halfedge.next_halfedge_handle.is_invalid or
         halfedge.prev_halfedge_handle.is_invalid:
        brokenHalfedges += 1

  for i, face in mymesh.faces:
    if face.halfedge_handle.isInvalid:
      echo "invalid halfedge handle in face at index ", i
  echo "brokenHalfedges: ", brokenHalfedges




  # fixing stuff for boundary halfedges
  for he in mymesh.halfedgeRefs:
    if he.goToVertex.handle.isInvalid:
      he.connectivity.vertex_handle = he.goOpp.goPrev.goToVertex.handle



  for he in mymesh.halfedgeRefs:
    if he.isBoundary:
      he.goToVertex.propboundary() = true
      he.goFromVertex.propBoundary() = true

  var boundaryVertices: seq[MyMeshType.VertexRef]
  for vertex in mymesh.vertexRefs:
    if vertex.propBoundary():
      boundaryVertices.add vertex

  echo boundaryVertices

  for itA in boundaryVertices:
    var distance:float32 = Inf
    var closest: MyMeshType.VertexRef
    for itB in boundaryVertices:
      if itA.handle != itB.handle:
        let newDist = length(itA.propPoint - itB.propPoint)
        if newDist < distance:
          distance = newDist
          closest = itB
    echo int32(itA.handle), " <-> ", int32(closest.handle), " disdance: ", distance



  var indices: seq[int32]
  for face in mymesh.faceRefs:
    indices.setLen(0)
    var hasInvalid = false
    for it in face.circulateOutHalfedges:
      let handle1 = it.goToVertex.handle
      let handle2 = it.goNext.handle

      if not handle1.isValid or not handle2.isValid:
        face.propColor() = randomColor()
        # face.propColor.r = 255
        # face.propColor.g = 0
        # face.propColor.b = 0
        hasInvalid = true

      indices.add int32(handle1)
      indices.add int32(handle2)

    if hasInvalid:
      for it in face.circulateVertices:
        stdout.write int32(it.handle)
        stdout.write " "
      stdout.write("\n")
      echo indices

  # multiply everything wth 2 to have some slack for later faces
  var renderPositionBuffer = createArrayBuffer[Vec3f](mymesh.faces.len * 3 * 2, GL_DYNAMIC_DRAW)
  var renderNormalBuffer   = createArrayBuffer[Vec3f](mymesh.faces.len * 3 * 2, GL_DYNAMIC_DRAW)
  var renderTexCoordBuffer = createArrayBuffer[Vec2f](mymesh.faces.len * 3 * 2, GL_DYNAMIC_DRAW)
  var renderColorBuffer    = createArrayBuffer[Color](mymesh.faces.len * 3 * 2, GL_DYNAMIC_DRAW)

  var faceColors: Texture1D = newTexture1D(mymesh.faces.len * 2, GL_RGBA8)


  updateRenderBuffers(mymesh, renderPositionBuffer, renderNormalBuffer, renderTexCoordBuffer, renderColorBuffer, faceColors)

  ################################################################################
  ################################### other data #################################
  ################################################################################

  let planeVertices = arrayBuffer([
    vec4f(0,0,0,1), vec4f( 1, 0,0,0), vec4f( 0, 1,0,0),
    vec4f(0,0,0,1), vec4f( 0, 1,0,0), vec4f(-1, 0,0,0),
    vec4f(0,0,0,1), vec4f(-1, 0,0,0), vec4f( 0,-1,0,0),
    vec4f(0,0,0,1), vec4f( 0,-1,0,0), vec4f( 1, 0,0,0)
  ])

  ################################################################################
  ################################### main loop ##################################
  ################################################################################

  var mousePos: Vec2f

  let
    startPerfCount = getPerformanceCounter()
    invPerfFreq = 1.0 / float64(getPerformanceFrequency())

  var
    rotation = vec2f(0)
    offset = vec2f(0)
    dragMode : int

  var runGame = true

  var spreadBase: Color

  while runGame:
    let time = invPerfFreq * float64(getPerformanceCounter() - startPerfCount)

    #######################
    #### handle events ####
    #######################

    for evt in events():
      if evt.kind == QUIT:
        runGame = false
      elif evt.kind == KeyDown and evt.key.keysym.scancode == SCANCODE_ESCAPE:
        runGame = false

      if evt.kind in {MouseButtonDown, MouseButtonUp}:
        if evt.kind == MouseButtonDown:
          if evt.button.button == ButtonLeft:
            dragMode = dragMode or 0x1
          if evt.button.button == ButtonRight:
            dragMode = dragMode or 0x2
          if evt.button.button == ButtonMiddle:
            dragMode = dragMode or 0x4
        if evt.kind == MouseButtonUp:
          if evt.button.button == ButtonLeft:
            dragMode = dragMode and (not 0x1)
          if evt.button.button == ButtonRight:
            dragMode = dragMode and (not 0x2)
          if evt.button.button == ButtonMiddle:
            dragMode = dragMode and (not 0x4)

      if evt.kind == MouseMotion:
        mousePos.x = float32(evt.motion.x)
        mousePos.y = WindowSize.y - float32(evt.motion.y)
        let motion = vec2f(float32(evt.motion.xrel), float32(evt.motion.yrel))
        if dragMode == 0x1:
          rotation = rotation + motion / 100
        if dragMode == 0x2:
          offset = offset + motion / 100

      if evt.kind == MouseButtonDown:
        let color : Color = readPixel(mousePos.x.int, mousePos.y.int)

        block searchFace:
          for face in mymesh.faceRefs:
            if color == face.propColor:
              echo "clicked face: ", face.handle
              if evt.button.button == 1:
                discard
                spreadBase = randomColor()
                face.propState() = 2
              else:
                extrude(face)
              break searchFace

    for face in mymesh.faceRefs:
      if face.propState == 0:
        block b:
          for face2 in face.circulateFaces:
            if face2.handle.isValid and face2.propState ==  2:
              face.propStateNext() =  2
              break b
          face.propStateNext() = 0
      if face.propState == 1:
        face.propStateNext() = 0
      if face.propState == 2:
        face.propStateNext() = 1
        face.propColor() = spreadBase#  randomColor()
        spreadBase.b += 1
        if spreadBase.b == 0:
          spreadBase.g += 1
          if spreadBase.g == 0:
            spreadBase.r += 1

    swap(mymesh.faceProps.state, mymesh.faceProps.stateNext)

    updateRenderBuffers(mymesh, renderPositionBuffer,renderNormalBuffer, renderTexCoordBuffer, renderColorBuffer, faceColors)

    #####################
    #### render mesh ####
    #####################

    glClearColor(135/255, 206/255, 235/255, 1.0)
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)


    var view_mat = mat4d(1)

    view_mat = view_mat.translate( vec3d(0, -1.5f, -17) + vec3d(0, offset.y, offset.x) )
    view_mat = view_mat.translate( vec3d(0, 0, 3) )
    view_mat = view_mat.rotate( rotation.y-0.5f, vec3d(1,0,0))
    view_mat = view_mat.rotate( rotation.x, vec3d(0,0,1))

    view_mat = view_mat.translate( vec3d(0, 0, -3) )

    shadingDsl:
      numVertices = mymesh.faces.len * 3
      primitiveMode = GL_TRIANGLES

      uniforms:
        modelview = view_mat.mat4f
        projection
        faceColors
      attributes:
        a_position = renderPositionBuffer
        a_color    = renderColorBuffer
      vertexMain:
        """
        gl_Position = projection * modelview * vec4(a_position, 1);
        v_color = a_color;
        """
      vertexOut:
        "out vec4 v_color"
      fragmentMain:
        """
        color = texelFetch(faceColors, gl_PrimitiveID, 0);
        """

    shadingDsl:
      primitiveMode = GL_TRIANGLES
      numVertices = planeVertices.len
      uniforms:
        modelViewProj = projection * mat4f(view_mat)

      attributes:
        a_vertex   = planeVertices

      vertexMain:
        """
        gl_Position = modelViewProj * a_vertex;
        """

      fragmentMain:
        """
        color = vec4(0.486, 0.988, 0, 1);
        """


    window.glSwapWindow()
main()

# Local Variables:
# compile-command: "cd examples; nim c -r openmesh.nim"
# End:
