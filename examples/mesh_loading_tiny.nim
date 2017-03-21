import memfiles, glm, ../fancygl, sdl2, sdl2/ttf , opengl, strutils, sequtils, math, AntTweakBar

const WindowSize = vec2i(1024, 768)

proc `$`(v: Vec): string = glm.`$`(v)

proc main() =
  let (window, context) = defaultSetup(WindowSize)

  defer: sdl2.quit()
  discard ttfinit()

  if TwInit(TW_OPENGL_CORE, nil) == 0:
    echo "could not initialize AntTweakBar: ", TwGetLastError()
  defer: discard TwTerminate()

  let quadTexCoords = arrayBuffer([
    vec2f(0,0),
    vec2f(0,1),
    vec2f(1,0),
    vec2f(1,1)
  ], label = "quadTexCoords")

  let textHeight = 16
  var font = ttf.openFont("/usr/share/fonts/truetype/inconsolata/Inconsolata.otf", textHeight.cint)
  if font.isNil:
    font = ttf.openFont("/usr/share/fonts/TTF/Inconsolata-Regular.ttf", textHeight.cint)
  if font.isNil:
    panic "from example: could not load font: ", sdl2.getError(),
        "\nfrom example: sorry system font locations are hard coded into the program, change that to fix this problem"

  var file = memfiles.open("resources/mrfixit.iqm")
  defer:
    close(file)

  let header = cast[ptr iqmheader](file.mem)
  echo "version:   ", header.version

  let texts = header.getTexts

  let textTextures = textureArray(texts.mapIt($it))
  let (textSize, _) = textTextures.size

  let textPositions =
    newArrayBuffer[Vec4f](texts.len, GL_DYNAMIC_DRAW, "textPositions")

  let textPixelPositions =
    newArrayBuffer[Vec2i](texts.len, GL_DYNAMIC_DRAW, "textPixelPositions")

  #[
  block:
    var i = 0
    for text in texts:
      if text[0] != '\0':
        let fg : sdl2.Color = (255.uint8, 255.uint8, 255.uint8, 255.uint8)
        let bg : sdl2.Color = (0.uint8, 0.uint8, 0.uint8, 255.uint8)
        let surface = font.renderTextShaded(text, fg, bg)
        defer: freeSurface(surface)

        surface.flipY

        textTextures[i] = surface.textureRectangle
        textWidths[i] = surface.w

      else:
        textWidths[i] = -1

      i += 1
  ]#

  template text(offset : int32) : cstring =
    cast[cstring](cast[uint](file.mem) + header.ofs_text.uint + offset.uint)

  let meshData = header.getMeshData.arrayBufferMeshData
  let triangles = header.getTriangles
  let indices = header.getIndices.elementArrayBuffer
  let adjacencies = header.getAdjacencies
  let meshes = header.getMeshes
  let joints = header.getJoints
  let poses = header.getPoses
  let (baseframe, inversebaseframe) = joints.calcBaseframe

  echo "texts.len: ", texts.len

  var jointNameIndices = newSeq[float32](joints.len)
  for i, joint in joints:
    let jointName = text(joint.name)
    var j = 0
    while jointName != texts[j]:
      j += 1
    jointNameIndices[i] = float32(j)
  let jointNameIndicesBuffer =
    arrayBuffer(jointNameIndices, label ="jointNameIndices")

  var jointMatrices = newSeq[Mat4f](joints.len)
  for i in 0 .. < joints.len:
    var joint = joints[i]
    jointMatrices[i] = joint.matrix
    while joint.parent >= 0:
      joint = joints[joint.parent]
      jointMatrices[i] = joint.matrix * jointMatrices[i]

  var outframe         = newSeq[Mat4f](joints.len)
  var outframe_texture = newTextureRectangle( vec2i(4, joints.len.int32), GL_RGBA32F )

  echo "=========================================================================="

  echo "triangles: ", triangles.len
  for tri in triangles.take(4):
    echo tri.vertex[0], ", ", tri.vertex[1], ", ", tri.vertex[2]

  echo "=========================================================================="

  echo "adjacencies: ", adjacencies.len
  for adj in adjacencies.take(4):
    echo adj.triangle[0], ", ", adj.triangle[1], ", ", adj.triangle[2]

  echo "=========================================================================="

  echo "meshes: ", meshes.len

  var meshTextures = newSeq[Texture2D](meshes.len)

  for i, mesh in meshes:
    echo "got iqm mesh:"
    echo "  name:           ", text(mesh.name)
    echo "  material:       ", text(mesh.material)
    meshTextures[i] = loadTexture2DFromFile( "resources/" & $text(mesh.material) )

  echo "=========================================================================="


  echo "joints: ", joints.len
  for joint in joints.take(3):
    echo "name:      ", text(joint.name)
    echo "parent:    ", joint.parent
    echo "translate: ", joint.translate
    echo "rotate:    ", joint.rotate
    echo "scale:     ", joint.scale

  echo "=========================================================================="

  echo "poses: ", poses.len
  for pose in poses.take(3):
    echo "parent:        ", pose.parent
    echo "mask:          ", pose.mask.int.toHex(8)
    echo "channeloffset: ", pose.channeloffset.mkString()
    echo "channelscale:  ", pose.channelscale.mkString()

  echo "=========================================================================="

  let anims = header.getAnims
  echo "anims: ", anims.len
  for anim in anims.take(10):
    echo "  name:        ", text(anim.name)
    echo "  first_frame: ", anim.first_frame
    echo "  num_frames:  ", anim.num_frames
    echo "  framerate:   ", anim.framerate
    echo "  flags:       ", anim.flags.int.toHex(8)

  echo "=========================================================================="

  assert(header.num_poses == header.num_joints)

  var
    frames_data = newSeq[Mat4f](header.num_frames * header.num_poses)
    frames = frames_data.grouped(header.num_joints.int)

  let framedata_view = header.getFrames
  var framedata_idx  = 0

  for i in 0 .. < framedata_view.len:
    for j, p in poses:
      var rawPose : array[10, float32]
      for k in 0 .. high(rawPose):
        var raw = 0.0f
        if (p.mask.int and (1 shl k)) != 0:
           raw = framedata_view[framedata_idx].float32
           framedata_idx += 1

        let
          offset = p.channeloffset[k]
          scale  = p.channelscale[k]

        rawPose[k] = raw * scale + offset

      let m = jointPose(rawPose).matrix
      if p.parent >= 0:
        frames[i][j] = baseframe[p.parent.int] * m * inversebaseframe[j.int]
      else:
        frames[i][j] = m * inversebaseframe[j.int]

  let
    boxColors   = arraybuffer(fancygl.boxColors)

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
    boneVertices = arrayBuffer(boneVerticesArray, label = "boneVerticesArray")
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

      arrayBuffer(normals, label = "normals")
    )

  var
    runGame = true
    simulationTimer = newStopWatch(true)
    fpsTimer        = newStopWatch(true)
    hor = WindowSize.x / WindowSize.y
    ver = WindowSize.y / WindowSize.y
    projection_mat : Mat4d

    offset = vec2d(0)
    rotation = vec2d(0)
    boneScale = vec2d(1,1)
    dragMode = 0

    renderMesh = true
    renderBones = false
    renderBoneNames = false
    renderNormalMap = false

  ################################
  #### create AntTweakBar gui ####
  ################################


  discard TwWindowSize(WindowSize.x, WindowSize.y)

  var obj_quat : Quatf

  var scale: float32 = 1

  var bar = TwNewBar("TwBar")
  discard TwAddVarRW(bar, "scale", TW_TYPE_FLOAT, scale.addr, " precision=3 step=0.01")
  discard TwAddVarRW(bar, "renderBoneNames", TW_TYPE_BOOL8, renderBoneNames.addr, "")
  discard TwAddVarRW(bar, "renderBones", TW_TYPE_BOOL8, renderBones.addr, "")
  discard TwAddVarRW(bar, "renderMesh", TW_TYPE_BOOL8, renderMesh.addr, "")
  discard TwAddVarRW(bar, "renderNormalMap", TW_TYPE_BOOL8, renderNormalMap.addr, "")
  discard TwAddVarRW(bar, "objRotation", TW_TYPE_QUAT4F, obj_quat.addr, " label='Object rotation' opened=true help='Change the object orientation.' ");

  glEnable(GL_DEPTH_TEST)
  glEnable(GL_CULL_FACE)
  glCullFace(GL_FRONT)

  while runGame:
    #######################
    #### handle events ####
    #######################

    var evt = sdl2.defaultEvent
    while pollEvent(evt):
      let handled = TwEventSDL(cast[pointer](evt.addr), 2.cuchar, 0.cuchar) != 0
      if handled:
        continue


      if evt.kind == QuitEvent:
        runGame = false
        break

      if evt.kind == KeyDown:

        case evt.key.keysym.scancode
        of SDL_SCANCODE_ESCAPE:
          runGame = false
        of SDL_SCANCODE_1:
          renderMesh = not renderMesh
        of SDL_SCANCODE_2:
          renderBones = not renderBones
        of SDL_SCANCODE_3:
          renderBoneNames = not renderBoneNames
        of SDL_SCANCODE_4:
          renderNormalMap = not renderNormalMap
        of SDL_SCANCODE_F10:
          window.screenshot
        else:
          discard

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
        let motion = vec2d(evt.motion.xrel.float64, evt.motion.yrel.float64)
        if dragMode == 0x1:
          rotation = rotation + motion / 100
        if dragMode == 0x2:
          offset = offset + motion / 100
        if dragMode == 0x4:
          boneScale.x = boneScale.x * pow(2.0, motion.x / 100)
          boneScale.y = boneScale.y * pow(2.0, motion.y / 100)

    ##################
    #### simulate ####
    ##################

    projection_mat = frustum(-hor * scale, hor * scale, -ver * scale, ver * scale, 1, 100)

    var view_mat = mat4d()

    view_mat = view_mat.translate( vec3d(0, -1.5f, -17) + vec3d(0, offset.y, offset.x) )
    view_mat = view_mat.translate( vec3d(0, 0, 3) )

    view_mat = view_mat.rotate( vec3d(1,0,0), rotation.y-0.5f )
    view_mat = view_mat.rotate( vec3d(0,0,1), rotation.x )
    view_mat = view_mat * obj_quat.mat4(vec4f(0,0,0,1)).mat4d

    view_mat = view_mat.translate( vec3d(0, 0, -3) )

    ################
    #### render ####
    ################

    let frameTime = simulationTimer.time


    #  ###########  #
    # ## animate ## #
    #  ###########  #

    let
      animFrame = frameTime * 15
      frame1 = frames[animFrame.floor.int mod header.num_frames.int]
      frame2 = frames[(animFrame.floor.int + 1) mod header.num_frames.int]
      frameoffset = animFrame - animFrame.floor

    for i in 0 .. < outframe.len:
      let mat = mix( frame1[i], frame2[i], frameoffset )
      outframe[i] = if joints[i].parent >= 0: outframe[joints[i].parent] * mat else: mat

    # write outframe into a texture that can be read from the shader
    outframeTexture.subImage(outframe)

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

    #  ###############
    # ## render Mesh ##
    #  ###############

    if renderNormalMap:
      for i, mesh in meshes:
        shadingDsl:
          primitiveMode = GL_POINTS
          numVertices = mesh.num_vertexes
          vertexOffset = mesh.first_vertex

          uniforms:
            modelview = mat4f(view_mat)
            projection = mat4f(projection_mat)
            outframeTexture

          attributes:
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

              for(int j = 0; j < 4; ++j) {
                mat[j] += blendWeight * texelFetch(outframeTexture, ivec2(j, blendIndex));
              }
            }
            mat = modelview * mat;


            v_pos_cs = mat * vec4(a_position, 1);
            v_normal_cs  = normalize(mat * vec4(a_normal_os, 0));
            v_tangent_cs = normalize(mat * vec4(a_tangent_os.xyz, 0));
            v_cotangent_cs = normalize(vec4(cross(v_normal_cs.xyz, v_tangent_cs.xyz),0));

            v_pos_cs  = mat * vec4(a_position, 1);
            """

          vertexOut:
            "out vec4 v_pos_cs"
            "out vec4 v_normal_cs"
            "out vec4 v_tangent_cs"
            "out vec4 v_cotangent_cs"

          geometryMain:
            "layout(line_strip, max_vertices=6) out"
            """
            vec4 center_ndc = projection * v_pos_cs[0];
            float scale = center_ndc.w * 0.05;
            vec4 pos_r      = projection * (v_pos_cs[0] + v_normal_cs[0] * scale);
            vec4 pos_g      = projection * (v_pos_cs[0] + v_tangent_cs[0] * scale);
            vec4 pos_b      = projection * (v_pos_cs[0] + v_cotangent_cs[0] * scale);

            g_color = vec4(1, 0, 0, 1);
            gl_Position = pos_r;
            EmitVertex();
            gl_Position = center_ndc;
            EmitVertex();

            g_color = vec4(0, 1, 0, 1);
            gl_Position = pos_g;
            EmitVertex();
            gl_Position = center_ndc;
            EmitVertex();

            g_color = vec4(0, 0, 1, 1);
            gl_Position = pos_b;
            EmitVertex();
            gl_Position = center_ndc;
            EmitVertex();

            """
          geometryOut:
            "flat out vec4 g_color"
          fragmentMain:
            """
            color = g_color;
            """

    if renderMesh:
      for i, mesh in meshes:
        shadingDsl:
          primitiveMode = GL_TRIANGLES
          numVertices = mesh.num_triangles * 3
          vertexOffset = mesh.first_triangle * 3
          indices = indices

          uniforms:
            modelview = mat4f(view_mat)
            projection = mat4f(projection_mat)
            outframeTexture
            material = meshTextures[i]
            time = float32(frameTime)
            renderNormalMap

          attributes:
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

              for(int j = 0; j < 4; ++j) {
                mat[j] += blendWeight * texelFetch(outframeTexture, ivec2(j, blendIndex));
              }
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
            if(renderNormalMap) {
              color.rgb = v_normal_cs.xyz;
            } else {
              color = texture(material, v_texcoord) * v_normal_cs.z;
            }
            """


    #  ################  #
    # ## render bones ## #
    #  ################  #

    glClear(GL_DEPTH_BUFFER_BIT)

    if renderBones:
      for i, joint in joints:
        let model_mat = outframe[i] * jointMatrices[i]

        shadingDsl:
          primitiveMode = GL_TRIANGLES
          numVertices = triangles.len * 3

          uniforms:
            modelview = view_mat.mat4f * model_mat
            projection = projection_mat.mat4f
            boneScale = boneScale.vec2f
            time = float32(frameTime)

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
            "out vec4 v_color"


          fragmentMain:
            """
            color.rgb = v_color.rgb * v_normal_cs.z;
            color.a = v_color.a;
            """

    glClear(GL_DEPTH_BUFFER_BIT)

    #  #####################
    # ## render bone names ##
    #  #####################

    let normalizedRectSize = vec2f(textSize) / vec2f(WindowSize) * 2.0f

    textPositions.mapWriteBlock:
      textPixelPositions.mapWriteBlock:
        for i, _ in joints:
          let model_mat = outframe[i].mat4d * jointMatrices[i].mat4d
          var pos = projection_mat * view_mat * model_mat[3]
          pos /= pos.w
          textPositions[i] = vec4f(pos)
          textPixelPositions[i] = vec2i(vec2f(pos.xy + 1) * 0.5f * vec2f(WindowSize))


    if renderBoneNames:
      shadingDsl:
          primitiveMode = GL_TRIANGLE_STRIP
          numVertices = 4
          numInstances = joints.len

          uniforms:
            textTextures
            normalizedRectSize

            windowSize = vec2f(WindowSize)

          attributes:
            a_texcoord = quadTexCoords

            instanceData:
              a_position = textPositions
              a_textIndex = jointNameIndicesBuffer

          vertexMain:
            """
            gl_Position = a_position + vec4(normalizedRectSize * a_texcoord, 0, 0);
            rectPosPixels = ivec2((a_position.xy + 1) * 0.5 * windowSize);
            textIndex = int(a_textIndex);
            """

          vertexOut:
            "flat out ivec2 rectPosPixels"
            "flat out int textIndex"

          fragmentMain:
            """
            ivec2 texcoord = ivec2(gl_FragCoord.xy) - rectPosPixels;
            color = texelFetch(textTextures, ivec3(texcoord, textIndex), 0);
            """

      #[
      for i, _ in joints:
        let textIndex = jointNameIndices[i]

        echo s"i: $i textIndex: $textIndex"
        let model_mat = outframe[i].mat4d * jointMatrices[i].mat4d;
        var pos = projection_mat * view_mat * model_mat[3]

        # culling of bone names behind the camera
        if pos.w <= 0:
          continue

        pos /= pos.w

        shadingDsl:
          primitiveMode = GL_TRIANGLE_STRIP
          numVertices = 4

          uniforms:
            position = vec4f(pos)
            textTextures
            textIndex = int32(textIndex)
            normalizedRectSize

            windowSize = vec2f(WindowSize)

          attributes:
            a_texcoord = quadTexCoords

          vertexMain:
            """
            gl_Position = position + vec4(normalizedRectSize * a_texcoord, 0, 0);
            rectPosPixels = ivec2((position.xy + 1) * 0.5 * windowSize);
            """

          vertexOut:
            "flat out ivec2 rectPosPixels"


          fragmentMain:
            """
            ivec2 texcoord = ivec2(gl_FragCoord.xy) - rectPosPixels;
            color = texelFetch(textTextures, ivec3(texcoord, textIndex), 0);
            """

      ]#



    discard TwDraw()
    window.glSwapWindow()

main()
