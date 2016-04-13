import memfiles, glm, ../fancygl, sdl2, sdl2/ttf , opengl, strutils, math

include iqm

const WindowSize = vec2i(1024, 768)

proc iqmFormatString(format : cuint) : string =
  case format
  of IQM_BYTE: "byte"
  of IQM_UBYTE: "ubyte"
  of IQM_SHORT: "short"
  of IQM_USHORT: "ushort"
  of IQM_INT: "int"
  of IQM_UINT: "uint"
  of IQM_HALF: "half"
  of IQM_FLOAT: "float"
  of IQM_DOUBLE: "double"
  else: "INVALID IQM FORMAT TAG: " & $format


proc iqmTypeString(t : cuint) : string =
  case t
  of IQM_POSITION:     "position"
  of IQM_TEXCOORD:     "texcoord"
  of IQM_NORMAL:       "normal"
  of IQM_TANGENT:      "tangent"
  of IQM_BLENDINDEXES: "blendindexes"
  of IQM_BLENDWEIGHTS: "blendweights"
  of IQM_COLOR:        "color"
  of IQM_CUSTOM:       "custom"
  else:                "INVALID IQM TYPE TAG: " & $t


type
  MeshData = object
    position : ArrayBuffer[Vec3f]
    texcoord : ArrayBuffer[Vec2f]
    normal   : ArrayBuffer[Vec3f]
    tangent  : ArrayBuffer[Vec4f]
    blendindexes : ArrayBuffer[Vec4[uint8]]
    blendweights : ArrayBuffer[Vec4[uint8]]

  Mesh = object
    data : ptr MeshData
    firstVertex : int
    numVertices : int

proc memptr[T](file:MemFile, offset: cuint) : ptr T = cast[ptr T](cast[int](file.mem) + offset.int)
proc memptr[T](file:MemFile, offset: cuint, num_elements: cuint) : DataView[T] =
  dataView[T]( cast[pointer](cast[int](file.mem) + offset.int), num_elements.int )

proc grouped[T](t : var seq[T]; groupSize : int) : seq[DataView[T]] =
  result.newSeq(t.len div groupSize + (if t.len mod groupSize == 0: 0 else: 1))
  for i in 0 .. < result.len:
    result[i] = dataView[T]( t[i * groupSize].addr.pointer, min(groupSize, t.len - i * groupSize) )

proc jointPose(ij : iqmjoint) : JointPose =
  for i in 0 .. < 3:
    result.translate[i] = ij.translate[i]
  for i in 0 .. < 4:
    result.rotate[i]    = ij.rotate[i]
  for i in 0 .. < 3:
    result.scale[i]     = ij.scale[i]


proc matrix(joint : iqmjoint) : Mat4f =
  var jp : JointPose
  jp.translate = joint.translate.Vec3f
  jp.rotate    = joint.rotate.Quatf
  jp.scale     = joint.scale.Vec3f
  jp.poseMatrix

proc main() =
  discard sdl2.init(INIT_EVERYTHING)
  discard ttfinit()

  #this is a comment

  let window = createWindow("SDL/OpenGL Skeleton", 100, 100, WindowSize.x, WindowSize.y, SDL_WINDOW_OPENGL) # SDL_WINDOW_MOUSE_CAPTURE
  # let context = window.glCreateContext()
  discard window.glCreateContext()
  # Initialize OpenGL
  loadExtensions()

  let quadTexCoords = @[
    vec2f(0,0),
    vec2f(0,1),
    vec2f(1,0),
    vec2f(1,1)
  ].arrayBuffer

  let textHeight = 16
  let font = ttf.openFont("/usr/share/fonts/truetype/inconsolata/Inconsolata.otf", textHeight.cint)

  var file = memfiles.open("mrfixit.iqm")
  defer:
    close(file)

  let hdr = memptr[iqmheader](file, 0)
  echo "version:   ", hdr.version

  var texts = newSeq[cstring](0)
  let textData = memptr[char](file, hdr.ofs_text, hdr.num_text)
  var i = 0
  while i < textData.len:
    texts.add(cast[cstring](textData[i].addr))
    while textData[i] != '\0':
      i += 1
    i += 1

  var textTextures = newSeq[TextureRectangle](texts.len)
  var textWidths = newSeq[cint](texts.len)
  i = 0
  for text in texts:
    echo "text: ", text
    if text[0] != '\0':
      let fg : sdl2.Color = (255.uint8, 255.uint8, 255.uint8, 255.uint8)
      let bg : sdl2.Color = (0.uint8, 0.uint8, 0.uint8, 255.uint8)
      let surface = font.renderTextShaded(text, fg, bg)
      defer: freeSurface(surface)

      textTextures[i] = surface.textureRectangle
      textWidths[i] = surface.w

    else:
      textWidths[i] = -1

    i += 1

  echo "texts.len: ", texts.len
  echo "texts:     ", texts.mkString

  let vertexArrays = memptr[iqmvertexarray](file, hdr.ofs_vertexarrays, hdr.num_vertexarrays)

  template text(offset : cuint) : cstring =
    cast[cstring](cast[uint](file.mem) + hdr.ofs_text.uint + offset.uint)


  var meshData : MeshData

  echo "num vertex arrays: ", vertexArrays.len
  for va in vertexArrays:

    if va.`type` == IQM_POSITION and va.format == IQM_FLOAT and va.size == 3:
      echo "got positions"
      meshData.position = memptr[Vec3f](file, va.offset, hdr.num_vertexes).arrayBuffer

    if va.`type` == IQM_TEXCOORD and va.format == IQM_FLOAT and va.size == 2:
      echo "got texcoords"
      meshData.texcoord = memptr[Vec2f](file, va.offset, hdr.num_vertexes ).arrayBuffer

    if va.`type` == IQM_NORMAL and va.format == IQM_FLOAT and va.size == 3:
      echo "got normals"
      meshData.normal = memptr[Vec3f](file, va.offset, hdr.num_vertexes ).arrayBuffer

    if va.`type` == IQM_TANGENT and va.format == IQM_FLOAT and va.size == 4:
      echo "got tangents"
      meshData.tangent = memptr[Vec4f](file, va.offset, hdr.num_vertexes ).arrayBuffer

    if va.`type` == IQM_BLENDINDEXES and va.format == IQM_UBYTE and va.size == 4:
      echo "got blend indices"
      meshData.blendindexes = memptr[Vec4[uint8]](file, va.offset, hdr.num_vertexes ).arrayBuffer

    if va.`type` == IQM_BLENDWEIGHTS and va.format == IQM_UBYTE and va.size == 4:
      echo "got blend weights"
      meshData.blendweights = memptr[Vec4[uint8]](file, va.offset, hdr.num_vertexes ).arrayBuffer

  #end for

  echo "=========================================================================="
  let triangles = memptr[iqmtriangle](file, hdr.ofs_triangles, hdr.num_triangles)
  echo "triangles: ", triangles.len
  for tri in triangles.take(10):
    echo tri.vertex[0], ", ", tri.vertex[1], ", ", tri.vertex[2]

  let indices = memptr[uint32](file, hdr.ofs_triangles, hdr.num_triangles * 3).elementArrayBuffer

  echo "=========================================================================="
  let adjacencies = memptr[iqmadjacency](file, hdr.ofs_adjacency, hdr.num_triangles)
  echo "adjacencies: ", adjacencies.len
  for adj in adjacencies.take(10):
    echo adj.triangle[0], ", ", adj.triangle[1], ", ", adj.triangle[2]

  echo "=========================================================================="
  let meshes = memptr[iqmmesh](file, hdr.ofs_meshes, hdr.num_meshes)
  echo "meshes: ", meshes.len

  var meshTextures = newSeq[Texture2D](meshes.len)

  for i, mesh in meshes:
    echo "got iqm mesh:"
    echo "  name:           ", text(mesh.name)
    echo "  material:       ", text(mesh.material)
    echo "  first_vertex:   ", mesh.first_vertex
    echo "  num_vertexes:   ", mesh.num_vertexes
    echo "  first_triangle: ", mesh.first_triangle
    echo "  num_triangles:  ", mesh.num_triangles
    meshTextures[i] = loadTexture2DFromFile( $text(mesh.material) )


  echo "=========================================================================="
  let joints = memptr[iqmjoint](file, hdr.ofs_joints, hdr.num_joints)
  echo "joints: ", joints.len
  for joint in joints.take(10):
    echo "name:      ", text(joint.name)
    echo "parent:    ", joint.parent
    echo "translate: ", joint.translate.Vec3f
    echo "rotate:    ", joint.rotate.Quatf
    echo "scale:     ", joint.scale.Vec3f

  var jointNameIndices = newSeq[int](joints.len)
  for i, joint in joints:
    let jointName = text(joint.name)
    var j = 0
    while jointName != texts[j]:
      j += 1
    jointNameIndices[i] = j

  var jointMatrices = newSeq[Mat4f](joints.len)
  for i in 0 .. < joints.len:
    var joint = joints[i]
    jointMatrices[i] = joint.matrix
    while joint.parent >= 0:
      joint = joints[joint.parent]
      jointMatrices[i] = joint.matrix * jointMatrices[i]

  var outframe = newSeq[Mat4f](joints.len)
  var outframe_texture = textureRectangle( vec2i(4, joints.len.int32), GL_RGBA32F )

  echo "=========================================================================="

  let poses = memptr[iqmpose](file, hdr.ofs_poses, hdr.num_poses)
  echo "poses: ", poses.len
  for pose in poses.take(10):
    echo "parent:        ", pose.parent
    echo "mask:          ", pose.mask.int.toHex(8)
    echo "channeloffset: ", pose.channeloffset.mkString()
    echo "channelscale:  ", pose.channelscale.mkString()

  echo "=========================================================================="

  let anims = memptr[iqmanim](file, hdr.ofs_anims, hdr.num_anims)
  echo "anims: ", anims.len
  for anim in anims.take(10):
    echo "  name:        ", text(anim.name)
    echo "  first_frame: ", anim.first_frame
    echo "  num_frames:  ", anim.num_frames
    echo "  framerate:   ", anim.framerate
    echo "  flags:       ", anim.flags.int.toHex(8)

  echo "=========================================================================="

  ########################
  #### load base pose ####
  ########################

  var
    baseframe = newSeq[Mat4f](hdr.num_joints.int)
    inversebaseframe = newSeq[Mat4f](hdr.num_joints.int)

  for i, joint in joints:
    baseframe[i] = joint.jointPose.poseMatrix
    inversebaseframe[i] = baseframe[i].inverse
    if joint.parent >= 0:
      baseframe[i] = baseframe[joint.parent.int] * baseframe[i]
      inversebaseframe[i] = inversebaseframe[i] * inversebaseframe[joint.parent.int]

  #############################
  #### load iqm animations ####
  #############################

  assert(hdr.num_poses == hdr.num_joints)

  #  lilswap((uint32_t*)&buf[hdr.ofs_poses], hdr.num_poses*sizeof(iqmpose)/sizeof(uint32_t));
  #  lilswap((uint32_t*)&buf[hdr.ofs_anims], hdr.num_anims*sizeof(iqmanim)/sizeof(uint32_t));
  #  lilswap((uint16_t*)&buf[hdr.ofs_frames], hdr.num_frames*hdr.num_framechannels);
  #  //numanims = hdr.num_anims;
  #  //numframes = hdr.num_frames;

  #  auto anims_ptr = (iqmanim *)&buf[hdr.ofs_anims];
  #  anims.assign(anims_ptr, anims_ptr + hdr.num_anims);
  #  auto poses_ptr = (iqmpose*)&buf[hdr.ofs_poses];
  #  poses.assign(poses_ptr, poses_ptr + hdr.num_poses);

  # let str = texts[hdr.ofs_text.int]
  var
    frames_data = newSeq[Mat4f](hdr.num_frames * hdr.num_poses)
    frames = frames_data.grouped(hdr.num_joints.int)

  let framedata_view = memptr[uint16](file, hdr.ofs_frames, 10000)
  var framedata_idx = 0

  for i in 0 .. < hdr.num_frames.int:
    for j, p in poses:
      var pose : JointPose

      for k in 0 .. < 10:
        var raw = 0.0f
        if (p.mask.int and (1 shl k)) != 0:
           raw = framedata_view[framedata_idx].float32
           framedata_idx += 1

        let
          offset = p.channeloffset[k]
          scale  = p.channelscale[k]

        pose[k] = raw * scale + offset

      let m = pose.poseMatrix
      if p.parent >= 0:
        frames[i][j] = baseframe[p.parent.int] * m * inversebaseframe[j.int]
      else:
        frames[i][j] = m * inversebaseframe[j.int]

  let
    boxColors   = fancygl.boxColors.arrayBuffer

    boneVerticesArray = [
      vec3f(0,0,0), vec3f(+0.1f, 0.1f, +0.1f), vec3f(+0.1f, 0.1f, -0.1f),
      vec3f(0,0,0), vec3f(+0.1f, 0.1f, -0.1f), vec3f(-0.1f, 0.1f, -0.1f),
      vec3f(0,0,0), vec3f(-0.1f, 0.1f, -0.1f), vec3f(-0.1f, 0.1f, +0.1f),
      vec3f(0,0,0), vec3f(-0.1f, 0.1f, +0.1f), vec3f(+0.1f, 0.1f, +0.1f),

      vec3f(0,1,0), vec3f(+0.1f, 0.1f, -0.1f), vec3f(+0.1f, 0.1f, +0.1f),
      vec3f(0,1,0), vec3f(-0.1f, 0.1f, -0.1f), vec3f(+0.1f, 0.1f, -0.1f),
      vec3f(0,1,0), vec3f(-0.1f, 0.1f, +0.1f), vec3f(-0.1f, 0.1f, -0.1f),
      vec3f(0,1,0), vec3f(+0.1f, 0.1f, +0.1f), vec3f(-0.1f, 0.1f, +0.1f)
    ]
    boneVertices = boneVerticesArray.arrayBuffer
    boneNormals = (block:
      var normals = newSeq[Vec3f](boneVerticesArray.len)
      for i in countup(0, boneVerticesArray.len-1, 3):
        let
          v1 = boneVerticesArray[i + 0]
          v2 = boneVerticesArray[i + 2]
          v3 = boneVerticesArray[i + 1]
          normal = cross(v2-v1, v3-v1).normalize

        normals[i + 0] = normal
        normals[i + 1] = normal
        normals[i + 2] = normal

      normals.arrayBuffer
    )

  var
    runGame = true
    time = 0.0f
    projection_mat = perspective(45.0, WindowSize.x / WindowSize.y, 0.1, 100.0)

    offset = vec2d(0)
    rotation = vec2d(0)
    boneScale = vec2d(1,1)
    dragMode = 0

    renderMesh = true
    renderBones = true
    renderBoneNames = true

  glEnable(GL_DEPTH_TEST)
  #glEnable(GL_CULL_FACE)
  #glCullFace(GL_FRONT)

  while runGame:
    #######################
    #### handle events ####
    #######################

    var evt = sdl2.defaultEvent
    while pollEvent(evt):

      if evt.kind == QuitEvent:
        runGame = false
        break

      if evt.kind == KeyDown:
        let keyboardEvent = cast[KeyboardEventPtr](addr(evt))

        case keyboardEvent.keysym.scancode
        of SDL_SCANCODE_ESCAPE:
          runGame = false
        of SDL_SCANCODE_1:
          renderMesh = not renderMesh
        of SDL_SCANCODE_2:
          renderBones = not renderBones
        of SDL_SCANCODE_3:
          renderBoneNames = not renderBoneNames
        else:
          discard

      if evt.kind in {MouseButtonDown, MouseButtonUp}:
        let buttonEvent = cast[MouseButtonEventPtr](evt.addr)
        if evt.kind == MouseButtonDown:
          if buttonEvent.button == ButtonLeft:
            dragMode = dragMode or 0x1
          if buttonEvent.button == ButtonRight:
            dragMode = dragMode or 0x2
          if buttonEvent.button == ButtonMiddle:
            dragMode = dragMode or 0x4
        if evt.kind == MouseButtonUp:
          if buttonEvent.button == ButtonLeft:
            dragMode = dragMode and (not 0x1)
          if buttonEvent.button == ButtonRight:
            dragMode = dragMode and (not 0x2)
          if buttonEvent.button == ButtonMiddle:
            dragMode = dragMode and (not 0x4)

      if evt.kind == MouseMotion:
        let
          motionEvent = cast[MouseMotionEventPtr](evt.addr)
          motion = vec2d(motionEvent.xrel.float64, motionEvent.yrel.float64)
        if dragMode == 0x1:
          rotation = rotation + motion * 0.1
        if dragMode == 0x2:
          offset = offset + motion * 0.1
        if dragMode == 0x4:
          boneScale.x = boneScale.x * pow(2.0, motion.x / 100)
          boneScale.y = boneScale.y * pow(2.0, motion.y / 100)

    ##################
    #### simulate ####
    ##################

    time = getTicks().float32 / 1000.0

    var view_mat = I4d
    view_mat = view_mat.translate( vec3d(0, -1.5f, -17) + vec3d(0, offset.y, offset.x) )
    view_mat = view_mat.rotate( vec3d(1,0,0), -0.5f )
    view_mat = view_mat.rotate( vec3d(0,0,1), rotation.x )

    ################
    #### render ####
    ################

    #  ###########  #
    # ## animate ## #
    #  ###########  #

    let
      current_frame = time
      frame1 = frames[current_frame.floor.int mod hdr.num_frames.int]
      frame2 = frames[(current_frame.floor.int + 1) mod hdr.num_frames.int]
      frameoffset = current_frame - current_frame.floor

    for i in 0 .. < outframe.len:
      let mat = mix( frame1[i], frame2[i], frameoffset )
      outframe[i] = if joints[i].parent >= 0: outframe[joints[i].parent] * mat else: mat

    # write outframe into a texture that can be read from the shader
    outframeTexture.subImage(outframe)

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

    #  ###############
    # ## render Mesh ##
    #  ###############

    if renderMesh:
      # TODO render different textures for each mesh

      shadingDsl(GL_TRIANGLES):
        numVertices = GLsizei(triangles.len * 3)

        uniforms:
          modelview = view_mat
          projection = projection_mat
          outframeTexture
          material = meshTextures[0]
          time

        attributes:
          indices
          a_position = meshData.position
          a_texcoord = meshData.texcoord
          a_normal_os = meshData.normal
          a_tangent_os = meshData.tangent
          a_blendindexes = meshData.blendindexes
          a_blendweights = meshData.blendweights

        vertexMain:
          """
          mat4 mat = mat4(0.0);
          for(int i = 0; i < 4; ++i) {
            int blendIndex = int(a_blendindexes[i]);
            float blendWeight = a_blendweights[i];

            mat4 tmp;
            for(int j = 0; j < 4; ++j) {
              tmp[j] = texelFetch(outframeTexture, ivec2(j, blendIndex));
            }
            mat += tmp * blendWeight;
          }


          gl_Position = projection * modelview * mat * vec4(a_position, 1);
          v_texcoord = a_texcoord;
          v_normal_cs  = modelview * vec4(a_normal_os, 0);
          v_tangent_cs = modelview * a_tangent_os;
          """

        vertexOut:
          "out vec2 v_texcoord"
          "out vec4 v_normal_cs"
          "out vec4 v_tangent_cs"

        fragmentMain:
          """
          color = texture(material, v_texcoord) * v_normal_cs.z;
          //color.rgb = v_normal_cs.xyz;
          """


    #  ################  #
    # ## render bones ## #
    #  ################  #

    glClear(GL_DEPTH_BUFFER_BIT)

    if renderBones:
      for i, joint in joints:
        let model_mat = outframe[i] * jointMatrices[i]

        shadingDsl(GL_TRIANGLES):
          numVertices = GLsizei(triangles.len * 3)

          uniforms:
            modelview = view_mat.mat4f * model_mat
            projection = projection_mat
            boneScale = boneScale.vec2f
            time

          attributes:

            a_position_os = boneVertices
            a_normal_os   = boneNormals
            a_color    = boxColors

          vertexMain:
            """
            gl_Position = projection * modelview * vec4(a_position_os * boneScale.xyx, 1);
            v_normal_cs  = modelview * vec4(a_normal_os, 0);
            v_color      = a_color;
            """

          vertexOut:
            "out vec4 v_normal_cs"
            "out vec3 v_color"

          fragmentMain:
            """
            color.rgb = v_color * v_normal_cs.z;
            """

    glClear(GL_DEPTH_BUFFER_BIT)

    #  #####################
    # ## render bone names ##
    #  #####################

    if renderBoneNames:
      for i, _ in joints:
        let textIndex = jointNameIndices[i]
        let model_mat = outframe[i].mat4d * jointMatrices[i].mat4d;

        var pos = projection_mat * view_mat * model_mat[3]

        # culling of bone names behind the camera
        if pos.w <= 0:
          continue

        pos /= pos.w

        let rectPos = floor(vec2f(pos.xy) * vec2f(WindowSize) * 0.5f)

        shadingDsl(GL_TRIANGLE_STRIP):
          numVertices = 4

          uniforms:
            rectPos
            depth = pos.z
            rectSize = vec2f(textWidths[textIndex].float32, textHeight.float32)
            viewSize = vec2f(WindowSize)
            tex = textTextures[textIndex]

          attributes:
            a_texcoord = quadTexCoords

          vertexMain:
            """
            gl_Position = vec4( (rectPos + a_texcoord * rectSize) / (viewSize * 0.5f), depth, 1);
            v_texcoord = a_texcoord * rectSize;
            v_texcoord.y = rectSize.y - v_texcoord.y;
            """

          vertexOut:
            "out vec2 v_texcoord"

          fragmentMain:
            """
            vec2 texcoord = gl_FragCoord.xy - rectPos;
            color = texture(tex, v_texcoord);
            //color.xy = v_texcoord;
            """

    window.glSwapWindow()

main()






