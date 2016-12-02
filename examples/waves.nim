import sequtils, strutils, ../fancygl

var windowsize = vec2i(1024,768)
let (window, context) = defaultSetup(windowsize)

discard setRelativeMouseMode(Bool32(true))
var hm = newHeightMap(128,128)
hm.DiamondSquare(64)

let
  crateTexture   = loadTexture2DFromFile("crate.png")

  hmVertices  = arrayBuffer(hm.vertices, GL_STATIC_DRAW)
  hmNormals   = arrayBuffer(hm.normals, GL_STATIC_DRAW)
  hmTexCoords = arrayBuffer(hm.texCoords, GL_STATIC_DRAW)
  hmIndices   = elementArrayBuffer(hm.indicesTriangles, GL_STATIC_DRAW)

var heightsTexture = texture2D(hm.size, GL_R32F)
heightsTexture.parameter(GL_TEXTURE_WRAP_S, GL_REPEAT)
heightsTexture.parameter(GL_TEXTURE_WRAP_T, GL_REPEAT)
heightsTexture.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)
heightsTexture.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
heightsTexture.setData(hm.data)

var viewport = vec4f(0,0,vec2f(windowsize))

  
let projection_mat = perspective(45.0, windowsize.x / windowsize.y, 0.1, 1000.0)

var
  mousePos = vec2f(0)
  frameCounter = 0

  movement = vec3d(0,0,0)
  rotation = vec2d(PI/2,0)
  position = vec3d(0,0, hm[0,0] + 10 )
  
  runGame         = true
  
  gameTimer       = newStopWatch(true)
  fpsTimer        = newStopWatch(true)
  fpsFrameCounter = 0

proc render() =
  let time = gameTimer.time.float32
  var view_mat = I4d

  view_mat = view_mat.translate( position )
  view_mat = view_mat.rotate( vec3d(0,0,1), rotation.y )
  view_mat = view_mat.rotate( vec3d(1,0,0), rotation.x )

  let movement_ws = (view_mat * vec4d(movement, 0)).xyz
  position = position + movement_ws

  view_mat = view_mat.inverse

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT)

  glEnable(GL_CULL_FACE)
  glCullFace(GL_BACK)
  glDepthFunc(GL_LEQUAL)

  var baseOffset : Vec2f
  baseOffset.x = (floor(position.x / hm.w.float) - 1) * hm.w.float
  baseOffset.y = (floor(position.y / hm.h.float) - 1) * hm.h.float

  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices = hmindices.len
    numInstances = 9

    uniforms:
      modelview = view_mat.mat4f
      projection = projection_mat.mat4f
      time
      crateTexture
      baseOffset
      instanceSize = vec2f(hm.w.float32, hm.h.float32)
      border = 0.5f * viewport.zw
      lightRadius = 64.0f
      heightsTexture

    attributes:
      indices = hmIndices
      pos_os = hmVertices
      texcoord = hmTexCoords

    includes:
      """
      float getHeight(vec2 pos) {
        vec2 p1 = pos + vec2(time,  time);
        vec2 p2 = pos + vec2(time, -time);

        //float h1 = texelFetch(heightsTexture, ivec2(mod(p1, instanceSize)), 0).r;
        //float h2 = texelFetch(heightsTexture, ivec2(mod(p2, instanceSize)), 0).r;
        float h1 = textureLod(heightsTexture, p1 / instanceSize, 0).r;
        float h2 = textureLod(heightsTexture, p2 / instanceSize, 0).r;
        
        return h1 + h2;
      }
      """
    vertexMain:
      """
      vec2 offset = instanceSize * vec2(gl_InstanceID % 3, gl_InstanceID / 3);
      vec2 flat_pos_ws = pos_os.xy + offset + baseOffset;
      v_pos_ws = vec4(flat_pos_ws, getHeight(flat_pos_ws), 1);

      vec2 p = v_pos_ws.xy;
      float a = getHeight(p + vec2(-1,0)) - getHeight(p + vec2(1,0));
      float b = getHeight(p + vec2(0,-1)) - getHeight(p + vec2(0,1));

      v_normal_ws = normalize(vec4(a,b,2,0));

      v_texcoord = texcoord * instanceSize;
      v_pos_cs = modelview * v_pos_ws;
      v_normal_cs = modelview * v_normal_ws;

      gl_Position = projection * v_pos_cs;
      """

    vertexOut:
      "out vec2 v_texcoord"
      "out vec4 v_pos_cs"
      "out vec4 v_pos_ws"
      "out vec4 v_normal_cs"
      "out vec4 v_normal_ws"


    #[
    geometryMain:
      "layout(line_strip, max_vertices=8) out"
      
      """
      vec4 center = v_pos_cs[0] + v_pos_cs[1] + v_pos_cs[2];

      vec4 v1 = v_pos_cs[1] - v_pos_cs[0];
      vec4 v2 = v_pos_cs[2] - v_pos_cs[0];
      vec4 normal = vec4(cross(v1.xyz,v2.xyz),0);

      /*
      g_color = abs(normal);
      gl_Position = projection * center;
      EmitVertex();
      gl_Position = projection * (center + normal);
      EmitVertex();
      EndPrimitive();
      */

      normal = v_normal_cs[0];
      center = v_pos_cs[0];

      g_color = abs(normal);
      gl_Position = projection * center;
      EmitVertex();
      gl_Position = projection * (center + normal);
      EmitVertex();
      EndPrimitive();

      normal = v_normal_cs[1];
      center = v_pos_cs[1];

      g_color = abs(normal);
      gl_Position = projection * center;
      EmitVertex();
      gl_Position = projection * (center + normal);
      EmitVertex();
      EndPrimitive();

      normal = v_normal_cs[2];
      center = v_pos_cs[2];

      g_color = abs(normal);
      gl_Position = projection * center;
      EmitVertex();
      gl_Position = projection * (center + normal);
      EmitVertex();
      EndPrimitive();
      
      """
    geometryOut:
      "out vec2 g_texcoord"
      "out vec3 g_pos_cs"
      "out vec3 g_pos_ws"
      "out vec3 g_normal_cs"
      "out vec3 g_normal_ws"
      "out vec4 g_color"
    ]#
    fragmentMain:
      """
      //color = v_normal_ws;
      float lightFactor1 = max(1 - length(v_pos_cs.xyz) / lightRadius, 0);
      float lightFactor2 = max(v_normal_cs.z, 0);
      color =  mix(texture(crateTexture, v_texcoord) * lightFactor1 * lightFactor2, v_normal_cs, 0.25);
      //color = vec4(v_normal_ws, 1);
      //color = vec4(fract(v_pos_ws), 1);
      //color = texture(crateTexture, v_texcoord);
      //color = v_normal_cs.z * texture(crateTexture, v_texcoord);
          
      """

  #end shadingDsl

  glSwapWindow(window)
#end render

proc mainLoopFunc(): void =
  var evt: Event  = defaultEvent
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown:

      case evt.key.keysym.scancode
      of SDL_SCANCODE_ESCAPE:
        runGame = false
        break

      of SDL_SCANCODE_PAUSE:
        gameTimer.toggle
        
      of SDL_SCANCODE_F10:
        screenshot(window, "defered_shading")

      else:
        discard
        
    if evt.kind == MouseButtonDown:
      var toggle {. global .} = true
      if evt.button.button == 3:
        toggle = not toggle
        discard setRelativeMouseMode(Bool32(toggle))
        

    if evt.kind == MouseMotion:
      mousePos.x = evt.motion.x.float32
      mousePos.y = 960 - evt.motion.y.float32
      rotation.x = clamp( rotation.x - evt.motion.yrel.float / 128.0 , 0, PI )
      rotation.y = rotation.y - evt.motion.xrel.float / 128.0


  var state = getKeyboardState()

  movement.z = (state[SDL_SCANCODE_D.int].float - state[SDL_SCANCODE_E.int].float) * 0.4
  movement.x = (state[SDL_SCANCODE_F.int].float - state[SDL_SCANCODE_S.int].float) * 0.4

  if fpsTimer.time >= 1:
    echo "FPS: ", fpsFrameCounter
    fpsFrameCounter = 0
    fpsTimer.reset

  render()
  fpsFrameCounter += 1
  frameCounter += 1


while runGame:
  mainLoopFunc()
