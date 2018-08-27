import ../fancygl

#var windowsize = vec2i(640,480)
let (window, context) = defaultSetup(windowsize = vec2i(640,480))

var hm = newHeightMap(128,64)
hm.DiamondSquare(64)

let windowsize = window.size
var viewport = vec4f(0,0,window.size.vec2f)

let
  crateTexture = loadTexture2DFromFile(getResourcePath("crate.png"))
  starTexture = loadTexture2DFromFile(getResourcePath("star_symmetric_gray.png"))

  hmVertices = hm.vertices.arrayBuffer(GL_STATIC_DRAW)
  hmNormals = hm.normals.arrayBuffer(GL_STATIC_DRAW)
  hmTexCoords = hm.texCoords.arrayBuffer(GL_STATIC_DRAW)
  hmIndices = hm.indicesTriangles.elementArrayBuffer(GL_STATIC_DRAW)

  sphereVertices = uvSphereVertices(32,16).arrayBuffer
  sphereNormals = uvSphereNormals(32,16).arrayBuffer
  #sphereTexCoords = uvSphereTexCoords(32,16).arrayBuffer
  sphereIndices = uvSphereIndices(32,16).elementArrayBuffer

var hideNormals, hideDeferredShading, flatShading, wireframe: bool

## TODO: extend framebuffer makro with glsl types, and handle costructor arguments (size)

declareFramebuffer(FirstFramebuffer):
  depth = newDepthTexture2D(windowsize)
  color = newTexture2D(windowsize, GL_RGBA8)
  normal = newTexture2D(windowsize, GL_RGBA16F)

let fb1 = newFirstFramebuffer()

let projMat : Mat4f = perspective(45.0f, float32(windowsize.x / windowsize.y), 0.1f, 1000.0f)

const numLights = 500

var
  runGame = true
  mousePos = vec2f(0)

  gameTimer  = newStopWatch(true)
  fpsTimer   = newStopWatch(true)
  frame          = 0
  fpsFrameMarker = 0

  camera = newWorldNode()
  cameraControls : CameraControls

  lightPositions = createArrayBuffer[Vec3f](numLights, GL_DYNAMIC_DRAW, "lightPositions")
  lightColors = createArrayBuffer[Vec3f](numLights, GL_DYNAMIC_DRAW, "lightColors")

camera.pos.z   = hm[0,0] + 10
cameraControls.speed = 0.4'f32

addEventWatch(cameraControlEventWatch, cameraControls.addr)

for color in lightColors.mitems:
  color = vec3f(rand_f32(), rand_f32(), rand_f32())

var
  effectOrigin = camera.pos.xy
  effectTimer  = newStopWatch(true, 100)

proc showNormals(mvp: Mat4f, positions: ArrayBuffer[Vec4f], normals: ArrayBuffer[Vec4f], length:float32 = 1, color:Vec4f = vec4f(1)) =

  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices = normals.len

    uniforms:
      mvp
      normalColor = color
      scale = length

    attributes:
      pos = positions
      normal = normals

    vertexMain:
      """
      v_normal = normal;
      v_pos = pos;
      """

    vertexOut:
      "out vec4 v_normal"
      "out vec4 v_pos"

    geometryMain:
      "layout(line_strip, max_vertices=2) out"
      """
      gl_Position = mvp * v_pos[0];
      EmitVertex();
      gl_Position = mvp * (v_pos[0] + v_normal[0] * scale);
      EmitVertex();
      """

    fragmentMain:
      """
      color = normalColor;
      """

proc render() =

  let time = gameTimer.time.float32

  let viewMat = camera.viewMat()

  let lightDir_cs = view_mat * vec4f(vec3f(0.577f),0)

  # Clear color and depth buffers
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT)

  if wireframe:
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )
  else:
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )

  blockBindFramebuffer(fb1):
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT)


    glEnable(GL_CULL_FACE)
    glCullFace(GL_BACK)
    glDepthFunc(GL_LEQUAL)

    var baseOffset = vec4f(0,0,0,0)
    baseOffset.xy = (round(camera.pos.xy / hm.size.vec2f) - vec2f(1)) * hm.size.vec2f

    shadingDsl:
      primitiveMode = GL_TRIANGLES
      numVertices = hmindices.len
      indices = hmIndices
      numInstances = 4

      uniforms:
        modelview = viewMat
        projection = projMat
        time
        crateTexture
        baseOffset
        lightDir_cs
        effectOrigin
        effectLength = effectTimer.time.float32
        instanceSize = vec2f(hm.w.float32, hm.h.float32)
        flatShading

      attributes:
        pos = hmVertices
        texcoord = hmTexCoords
        normal = hmNormals

      vertexMain:
        """
        vec4 offset = vec4(gl_InstanceID % 2, gl_InstanceID / 2, 0, 0);
        offset.xy *= instanceSize;
        vec4 pos_ws = pos + offset + baseOffset;

        float effectParameter = effectLength * 5.0 - length(pos_ws.xy - effectOrigin) * 0.2;
        float effect = cos(clamp(effectParameter, -3.14, 3.14)) + 1;

        pos_ws.z += effect * 10.0 / (effectLength + 1);

        v_texcoord = texcoord * instanceSize;
        v_pos_cs = modelview * pos_ws;
        v_pos_ws = pos_ws;
        v_normal_cs = modelview * normal;
        v_normal_ws = normal;
        """

      vertexOut:
        "out vec2 v_texcoord"
        "out vec4 v_pos_cs"
        "out vec4 v_pos_ws"
        "out vec4 v_normal_cs"
        "out vec4 v_normal_ws"

      geometryMain:
        "layout(triangle_strip, max_vertices=3) out"
        """
        if( flatShading ) {
          g_normal_cs.xyz = normalize(cross(v_pos_cs[1].xyz - v_pos_cs[0].xyz, v_pos_cs[2].xyz - v_pos_cs[0].xyz));
        }

        for( int i=0; i < 3; i++) {
          gl_Position = projection * v_pos_cs[i];
          g_texcoord = v_texcoord[i];
          g_normal_ws = v_normal_ws[i];
          if( !flatShading ) {
            g_normal_cs = v_normal_cs[i];
          }
          EmitVertex();
        }
        """


      geometryOut:
        "out vec2 g_texcoord"
        "out vec4 g_normal_cs"
        "out vec4 g_normal_ws"

      fragmentMain:
        """
        color = texture(crateTexture, g_texcoord);
        normal = g_normal_cs;
        """
    #end shadingDsl
  #end bindFramebuffer(fb1)

  glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )


  var
    mvp = projMat * viewMat
    inverse_mvp = mvp

  inverse_mvp = inverse(inverse_mvp)

  if hideDeferredShading:

    shadingDsl:
      primitiveMode = GL_TRIANGLES
      numVertices = 3

      uniforms:
        tex = fb1.color
        depth = fb1.depth
        norm = fb1.normal
        time
        viewport
        texSize = fb1.color.size.vec2f
        inverse_mvp = inverse_mvp
        border = 0.5f * viewport.zw

      fragmentMain:
        """
        vec2 texcoord = (texCoord * viewport.zw ) / texSize;
        gl_FragDepth = texture(depth, texcoord).x;
        vec4 fragCoord = gl_FragCoord / gl_FragCoord.w;
        vec4 ndc_pos = vec4( ( vec3(gl_FragCoord.xy / viewport.zw, gl_FragDepth) ) * 2 - 1, 1 );
        vec4 worldpos = inverse_mvp * ndc_pos;
        worldpos /= worldpos.w;

        int tile = int(gl_FragCoord.x < border.x) + int(gl_FragCoord.y < border.y) * 2;

        if( gl_FragDepth != 1 ) {
          switch(tile) {
          case 0:
            //color = worldpos - floor(worldpos);
            color = fract(worldpos);
            break;
          case 1:
            color = texture(tex, texcoord);
            break;
          case 2:
            //vec3 worldpos_dx = dFdx(worldpos.xyz);
            //vec3 worldpos_dy = dFdx(worldpos.xyz);
            //color.rgb = normalize(cross(worldpos_dx, worldpos_dy));
            color = texture(norm, texcoord).z * texture(tex, texcoord);
            break;
          case 3:
            color = texture(norm, texcoord);
            break;
          }
        }
        color.a = 1.0;
        """

  else:

    fb1.handle.bindRead

    glBlitFramebuffer(
      0,0, windowSize.x.int32, windowSize.y.int32,
      0,0, windowSize.x.int32, windowSize.y.int32,
      GL_DEPTH_BUFFER_BIT,
      GL_NEAREST.GLenum
    )

    glBindFramebuffer(GL_READ_FRAMEBUFFER, 0)
    glDepthMask(false)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
    glCullFace(GL_FRONT)
    glDepthFunc(GL_GEQUAL)

    mapWriteBlock(lightPositions):
      let poslen = numLights
      for i in 0 ..< poslen:
        let
          distance = time * 30
          r = float32(i)
          alpha = distance / r
          x = cos(alpha).float32 * r
          y = sin(alpha).float32 * r
          z = hm[x,y] + 1.5f

        lightPositions[i] = vec3f(x, y, z)

    #### render lights ####
    shadingDsl:
      primitiveMode = GL_TRIANGLES
      numVertices = sphereIndices.len
      indices = sphereIndices
      numInstances = numLights

      uniforms:
        normalMat = viewMat
        mvp       = mvp
        inverse_mvp = inverse_mvp

        scale = 5
        lightDir_cs
        viewport

        color_tex = fb1.color
        depth_tex = fb1.depth
        normal_tex = fb1.normal

        #offset = vec3f(0)
        #col    = vec3f(1)

      attributes:
        pos = sphereVertices
        normal = sphereNormals
        #texCoord = sphereTexCoords
        offset = lightPositions {.divisor: 1.}
        col = lightColors       {.divisor: 1.}
      vertexMain:
        """
        gl_Position = mvp * vec4(pos.xyz * scale + offset, 1);
        v_normal = normalMat * normal;
        v_col = col;
        //v_texCoord = texCoord;
        v_lightPos = offset;
        """

      vertexOut:
        "out vec4 v_normal"
        "out vec3 v_col"
        "out vec3 v_lightPos"

      fragmentMain:
        """
        vec2 texcoord = (gl_FragCoord.xy - viewport.xy) / viewport.zw;

        float depth = texture(depth_tex, texcoord).x * 2 - 1;
        vec3 diffuse = texture(color_tex, texcoord).rgb * 2;
        vec3 normal = texture(normal_tex, texcoord).xyz;

        vec4 worldpos = inverse_mvp * vec4( texcoord * 2 - vec2(1), depth, 1 );
        worldpos /= worldpos.w;

        vec3 light_dir = v_lightPos - worldpos.xyz;
        float dist = length(light_dir);
        light_dir /= dist;
        light_dir = (normalMat * vec4(light_dir,0)).xyz;
        float factor1 = 1 - dist / scale;

        if( factor1 <= 0 ) {
          discard;
        }


        float factor2 = dot(normal, light_dir);

        color.rgb = v_col * diffuse * (factor1 * factor2);
        color.a = 1.0;
        """

    glDepthMask(false)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
    glCullFace(GL_BACK)
    glDisable(GL_DEPTH_TEST)

    shadingDsl:
      primitiveMode = GL_POINTS
      numVertices = numLights

      uniforms:
        star_tex = starTexture
        depth_tex = fb1.depth

        projection_mat = projMat
        view_mat       = viewMat

        scale = 3
        viewport

      attributes:
        a_pos = lightPositions
        a_color = lightColors

      vertexMain:
        """
        v_pos = a_pos;
        v_color = a_color;
        """

      vertexOut:
        "out vec3 v_pos"
        "out vec3 v_color"

      geometryMain:
        "layout(triangle_strip, max_vertices=4) out"
        """
        vec4 center_cs = view_mat * vec4(v_pos[0], 1);
        vec4 center_ndc = projection_mat * center_cs;
        center_ndc /= center_ndc.w;

        float scene_depth = texture(depth_tex, (center_ndc.xy + vec2(1))* 0.5).x * 2 - 1;
        float vertex_depth = center_ndc.z;

        if( abs(center_ndc.x) < 1 && abs(center_ndc.y) < 1 && scene_depth > vertex_depth) {

          g_color = v_color[0];

          gl_Position = projection_mat * (center_cs + vec4(-scale,-scale,0,0));
          texCoord = vec2( 0, 0);
          EmitVertex();

          gl_Position = projection_mat * (center_cs + vec4( scale,-scale,0,0));
          texCoord = vec2( 1, 0);
          EmitVertex();

          gl_Position = projection_mat * (center_cs + vec4(-scale, scale,0,0));
          texCoord = vec2( 0, 1);
          EmitVertex();

          gl_Position = projection_mat * (center_cs + vec4( scale, scale,0,0));
          texCoord = vec2( 1, 1);
          EmitVertex();
        }
        """

      geometryOut:
        "out vec2 texCoord"
        "out vec3 g_color"

      fragmentMain:
        """
        color = texture(star_tex, texCoord);
        color.rgb *= g_color;
        """

  glDepthMask(true)
  glDisable(GL_BLEND)
  glCullFace(GL_BACK)
  glEnable(GL_DEPTH_TEST)
  glDepthFunc(GL_LESS)

  if not hideNormals:
    showNormals(projMat * viewMat, sphereVertices, sphereNormals, 0.3f)

  glSwapWindow(window)


proc mainLoopFunc(): void =

  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KEY_DOWN:

      case evt.key.keysym.scancode
      of SCANCODE_ESCAPE:
        runGame = false
        break

      of SCANCODE_SPACE:
        effectOrigin = camera.pos.xy
        effectTimer.reset

      of SCANCODE_PAUSE:
        gameTimer.toggle

      of SCANCODE_1:
        hideDeferredShading = not hideDeferredShading

      of SCANCODE_2:
        hideNormals = not hideNormals

      of SCANCODE_3:
        flatShading = not flatShading

      of SCANCODE_4:
        wireframe = not wireframe

      of SCANCODE_F10:
        window.screenshot

      else:
        discard

    if evt.kind == MOUSE_BUTTON_DOWN:
      var toggle {. global .} = false
      if evt.button.button == 3:
        toggle = not toggle
        discard setRelativeMouseMode(toggle)

    if evt.kind == MouseMotion:
      mousePos.x = evt.motion.x.float32
      mousePos.y = 960 - evt.motion.y.float32

  if fpsTimer.time >= 1:
    echo "FPS: ", frame - fpsFrameMarker
    fpsFrameMarker = frame
    fpsTimer.reset

  update(camera, cameraControls)

  render()
  frame += 1

while runGame:
  mainLoopFunc()
