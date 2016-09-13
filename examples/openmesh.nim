import ../fancygl, sdl2, opengl, glm, memfiles, OpenMesh, math, hashes, tables, mersenne

var mt = newMersenneTwister(0)

proc randomColor() : Color =
  result.r = mt.getNum.uint8
  result.g = mt.getNum.uint8
  result.b = mt.getNum.uint8
  result.a = 255.uint8
  
proc xyz(v: var Vec4f): var Vec3f =
  return cast[ptr Vec3f](v.addr)[]

proc xyz(v: var Vec3f): var Vec3f =
  return cast[ptr Vec3f](v.addr)[]
  
proc `+=`(lhs: var Vec4f; rhs: Vec4f) =
  lhs = lhs + rhs

proc `+=`(lhs: var Vec3f; rhs: Vec3f) =
  lhs = lhs + rhs  

proc `-`(v: Vec4f): Vec4f =
  v * -1
  
proc `-`(v: Vec3f): Vec3f =
  v * -1
  
proc `*`(scalar: float32; v: Vec4f): Vec4f =
  v * scalar

proc `*`(scalar: float32; v: Vec3f): Vec3f =
  v * scalar
  
proc norm(v: Vec4f): auto = sqrt(dot(v,v))
proc norm(v: Vec3f): auto = sqrt(dot(v,v))

proc hash(v: Vec3f): Hash =
  hash(cast[array[3, float32]](v))

proc `==`(v1, v2: Vec3f): bool =
  for i in 0 ..< 3:
    if v1[i] != v2[i]:
      return false
  true
  
createMeshType(MyMeshType):
  type
    VertexData = object
      point:         Vec3f
      normal:        Vec3f
      #tangent:       Vec4f
      #blendindexes:  Vec4u8
      #blendweights:  Vec4u8

    FaceData = object
      color: Color
      state: int8
      stateNext: int8

    HalfedgeData = object
      texCoord:      Vec2f

    EdgeData = object
      someValue : int32

proc addVertex(mesh: var MyMeshType): MyMeshType_VertexRef =
  let vertex = Vertex(out_halfedge_handle: HalfedgeHandle(-1))
  mesh.vertices.add vertex
  var tmp1: Vec3f
  mesh.vertexProperties.point.add tmp1
  var tmp2: Vec3f
  mesh.vertexProperties.normal.add tmp2
  result.mesh = mesh.addr
  result.handle = VertexHandle(mesh.vertices.high)

proc addEdge(mesh: var MyMeshType): MyMeshType_EdgeRef =
  let
    halfedge = Halfedge(
        face_handle:          FaceHandle(-1),
        vertex_handle:        VertexHandle(-1),
        next_halfedge_handle: HalfedgeHandle(-1),
        prev_halfedge_handle: HalfedgeHandle(-1))
    edge: Edge = [halfedge,halfedge]
  mesh.edges.add edge
  
  var tmp1: int32
  mesh.edgeProperties.someValue.add tmp1
  var tmp2: Vec2f
  mesh.halfedgeProperties.texCoord.add tmp2
  mesh.halfedgeProperties.texCoord.add tmp2
  result.mesh = mesh.addr
  result.handle = EdgeHandle(mesh.edges.high)
  
proc addFace(mesh: var MyMeshType): MyMeshType_FaceRef =
  let face = Face(halfedge_handle: HalfedgeHandle(-1))
  
  mesh.faces.add face
  var tmp1: Color
  mesh.faceProperties.color.add tmp1
  var tmp2: int8
  mesh.faceProperties.state.add tmp2
  var tmp3: int8
  mesh.faceProperties.stateNext.add tmp3

  result.mesh = mesh.addr
  result.handle = FaceHandle(mesh.faces.high)


################################################################################
######################### prepare render vertex array ##########################
################################################################################

proc updateRenderBuffers(mymesh: var MyMeshType,
  renderPositionBuffer: var ArrayBuffer[Vec3f],
  renderNormalBuffer:   var ArrayBuffer[Vec3f],
  renderTexCoordBuffer: var ArrayBuffer[Vec2f],
  renderColorBuffer:    var ArrayBuffer[Color]): void =
  
  # renderPositionBuffer = createArrayBuffer[Vec3f](mymesh.faces.len * 3, GL_STATIC_DRAW)
  # renderNormalBuffer   = createArrayBuffer[Vec3f](mymesh.faces.len * 3, GL_STATIC_DRAW)
  # renderTexCoordBuffer = createArrayBuffer[Vec2f](mymesh.faces.len * 3, GL_STATIC_DRAW)
  # renderColorBuffer    = createArrayBuffer[Color](mymesh.faces.len * 3, GL_STATIC_DRAW)

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

  #for point in mymesh.vertexProperties.point:
  #  echo point
    
  #for a_position in renderPositionSeq:
    #echo a_position
    #echo mat4f(projection) * vec4f(a_position, 1);

  # var faceColors: Texture1D = texture1D(mymesh.faces.len, GL_RGBA8)
  # faceColors.setData(mymesh.faceProperties.color)
  # var indices = indicesSeq.elementArrayBuffer
  # faceColors.setData(mymesh.faceProperties.color)

################################################################################
#################################### Main ######################################
################################################################################
  
  
const
  WindowSize = vec2i(1024, 768)
  Near = 0.1
  Far = 1000
  halfHeight = Near
  halfWidth  = Near * (WindowSize.x / WindowSize.y)
  projection = frustum(-halfWidth, halfWidth, -halfHeight, halfHeight, Near, Far)
  
proc main() =
  let (window, context) = defaultSetup(vec2f(WindowSize))

  var file = memfiles.open("mrfixit.iqm")
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

  #mymesh.vertexProperties.texcoord.newSeq(numVertices)
  #mymesh.vertexProperties.normal.newSeq(numVertices)
  #mymesh.vertexProperties.tangent.newSeq(numVertices)
  #mymesh.vertexProperties.blendindexes.newSeq(numVertices)
  #mymesh.vertexProperties.blendweights.newSeq(numVertices)

  #for i in 0 ..< numVertices:
    #mymesh.vertexProperties.point[i]         = meshData.position[i] * vec3f(-1,1,1)
    #mymesh.vertexProperties.texcoord[i]      = meshData.texcoord[i]
    #mymesh.vertexProperties.normal[i]        = meshData.normal[i]
    #mymesh.vertexProperties.tangent[i]       = meshData.tangent[i]
    #mymesh.vertexProperties.blendindexes[i]  = meshData.blendindexes[i]
    #mymesh.vertexProperties.blendweights[i]  = meshData.blendweights[i]
  
  var meshTextures = newSeq[Texture2D](meshes.len)
  for i, mesh in meshes:
    meshTextures[i] = loadTexture2DFromFile( $text(mesh.material) )

  echo "=========================================================================="
  
  ################################################################################
  ############################ transform file to mesh ############################
  ################################################################################

  var mymesh: MyMeshType
  mymesh.new

  # halfedges at the same edge need to be stored together
  
  var vertexPairs = initTable[tuple[v1,v2:int], HalfedgeHandle]()
  var vertexTable = initTable[Vec3f, VertexHandle]()
  
  proc lazyHalfedge(vertex1,vertex2: MyMeshType_VertexRef): MyMeshType_HalfedgeRef =
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

  proc lazyVertex(position: Vec3f): MyMeshType_VertexRef =
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

    #echo halfedgeHandle0, " ", mymesh.get(halfedgeHandle0)
    #echo halfedgeHandle1, " ", mymesh.get(halfedgeHandle1)
    #echo halfedgeHandle2, " ", mymesh.get(halfedgeHandle2)
    
  for face in mymesh.faceRefs:
    face.propColor() = randomColor()
    

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

  var renderPositionBuffer = createArrayBuffer[Vec3f](mymesh.faces.len * 3, GL_DYNAMIC_DRAW)
  var renderNormalBuffer   = createArrayBuffer[Vec3f](mymesh.faces.len * 3, GL_DYNAMIC_DRAW)
  var renderTexCoordBuffer = createArrayBuffer[Vec2f](mymesh.faces.len * 3, GL_DYNAMIC_DRAW)
  var renderColorBuffer    = createArrayBuffer[Color](mymesh.faces.len * 3, GL_DYNAMIC_DRAW)

  updateRenderBuffers(mymesh, renderPositionBuffer,renderNormalBuffer, renderTexCoordBuffer, renderColorBuffer)

  ################################################################################
  ################################### main loop ##################################
  ################################################################################

  var mousePos: Vec2i

  let
    startPerfCount = getPerformanceCounter()
    invPerfFreq = 1.0 / float64(getPerformanceFrequency())
    
  var 
    rotation = vec2f(0)
    offset = vec2f(0)
    dragMode : int
  
  var runGame = true
  
  while runGame:
    let time = invPerfFreq * float64(getPerformanceCounter() - startPerfCount)

    #######################
    #### handle events ####
    #######################  

    var evt : sdl2.Event
    while pollEvent(evt):
      if evt.kind == QuitEvent:
        runGame = false
      elif evt.kind == KeyDown and evt.key.keysym.scancode == SDL_SCANCODE_ESCAPE:
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
        mousePos.x = evt.motion.x
        mousePos.y = WindowSize.y - evt.motion.y
        let motion = vec2f(evt.motion.xrel.float64, evt.motion.yrel.float64)
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
              for neighbor in face.circulateFaces:
                neighbor.propColor() = randomColor()
              face.propState() = 2
              break searchFace
          echo "could not find color: ", color
    
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
        face.propColor() = randomColor()
      
    swap(mymesh.faceProperties.state, mymesh.faceProperties.stateNext)
    
    updateRenderBuffers(mymesh, renderPositionBuffer,renderNormalBuffer, renderTexCoordBuffer, renderColorBuffer)
    
    #####################
    #### render mesh ####
    #####################

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)          

    
    var view_mat = I4d
    
    view_mat = view_mat.translate( vec3d(0, -1.5f, -17) + vec3d(0, offset.y, offset.x) )
    view_mat = view_mat.translate( vec3d(0, 0, 3) )
    view_mat = view_mat.rotate( vec3d(1,0,0), rotation.y-0.5f )
    view_mat = view_mat.rotate( vec3d(0,0,1), rotation.x )
    
    view_mat = view_mat.translate( vec3d(0, 0, -3) )

    shadingDsl(GL_TRIANGLES):
      debugResult
      numVertices = GLsizei(mymesh.faces.len * 3)
    
      uniforms:
        modelview = view_mat.mat4f
        projection = projection.mat4f
      attributes:
        a_position = renderPositionBuffer
        a_normal   = renderNormalBuffer
        a_texCoord = renderTexCoordBuffer
        a_color    = renderColorBuffer
        #a_position = mymesh.vertexproperties.point
        #a_texcoord = mymesh.vertexproperties.texcoord
        #a_normal_os = meshData.normal
        #a_tangent_os = meshData.tangent
        #a_blendindexes = meshData.blendindexes
        #a_blendweights = meshData.blendweights
      vertexMain:
        """
        gl_Position = projection * modelview * vec4(a_position, 1);
        v_color = a_color;
        """
      vertexOut:
        "out vec4 v_color"
      fragmentMain:
        """
        //color = texelFetch(faceColors, gl_PrimitiveID, 0);
        color = v_color;
        //color = vec4(1);

        //color /= max(max(color.x, color.y), color.z);
        //if(!gl_FrontFacing) {
        //  color *= 0.5;
        //}

        """
          
    window.glSwapWindow()
main()
