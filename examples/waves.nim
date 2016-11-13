import math, random, sequtils, strutils, ../fancygl

var windowsize = vec2f(800,600)
let (window, context) = defaultSetup(windowsize)

discard setRelativeMouseMode(Bool32(true))
var hm = createFlatMap(128,64)

let
  crateTexture   = loadTexture2DFromFile("crate.png")

  hmVertices  = arrayBuffer(hm.vertices, GL_STATIC_DRAW)
  hmNormals   = arrayBuffer(hm.normals, GL_STATIC_DRAW)
  hmTexCoords = arrayBuffer(hm.texCoords, GL_STATIC_DRAW)
  hmIndices   = elementArrayBuffer(hm.indices, GL_STATIC_DRAW)

hm.DiamondSquare(64)
var heightsTexture = texture2D(hm.size, GL_R32F)
heightsTexture.parameter(GL_TEXTURE_WRAP_S, GL_REPEAT)
heightsTexture.parameter(GL_TEXTURE_WRAP_T, GL_REPEAT)
heightsTexture.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)
heightsTexture.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
heightsTexture.setData(hm.data)

var viewport = vec4f(0,0,windowsize)

  
let projection_mat = perspective(45.0, windowsize.x / windowsize.y, 0.1, 1000.0)

var
  mousePos = vec2f(0)
  simulationTime = 0.0
  frameCounter = 0

  movement = vec3d(0,0,0)
  rotation = vec2d(PI/2,0)
  position = vec3d(0,0, hm[0,0] + 10 )
  
  runGame         = true
  
  gameTimer       = newStopWatch(true)
  fpsTimer        = newStopWatch(true)
  fpsFrameCounter = 0

proc render() =
  let time = simulationTime
  var view_mat = I4d

  view_mat = view_mat.translate( position )
  view_mat = view_mat.rotate( vec3d(0,0,1), rotation.y )
  view_mat = view_mat.rotate( vec3d(1,0,0), rotation.x )

  let movement_ws = (view_mat * vec4d(movement, 0)).xyz
  position = position + movement_ws

  view_mat = view_mat.inverse

  let lightDir_cs = (view_mat * vec3d(0.577).vec4d(0)).xyz.vec3f

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT)

  glEnable(GL_CULL_FACE)
  glCullFace(GL_BACK)
  glDepthFunc(GL_LEQUAL)

  var baseOffset = vec3f(0,0,0)
  baseOffset.x = (round(position.x / hm.w.float) - 1) * hm.w.float
  baseOffset.y = (round(position.y / hm.h.float) - 1) * hm.h.float

  shadingDsl(GL_TRIANGLES):
    numVertices = hmindices.len.GLsizei
    numInstances = 4

    uniforms:
      modelview = view_mat.mat4f
      projection = projection_mat.mat4f
      time
      crateTexture
      baseOffset
      lightDir_cs
      instanceSize = vec2f(hm.w.float32, hm.h.float32)
      border = 0.5f * viewport.zw
      lightRadius = 32.0f
      heightsTexture

    attributes:
      indices = hmIndices
      pos = hmVertices
      texcoord = hmTexCoords

    includes:
      """
      float getHeight(vec2 pos) {
        float h = texelFetch(heightsTexture, ivec2(mod(pos, instanceSize)), 0).r;
        return h + sin(length(pos * 0.1) + time * 2.2 ) * 7;
      }
      """
    vertexMain:
      """
      vec3 offset = vec3(instanceSize, 1) * vec3(gl_InstanceID % 2, gl_InstanceID / 2, 0);
      vec3 pos_ws = pos + offset + baseOffset;

      float height = getHeight(pos_ws.xy);

      pos_ws.z = height;

      vec2 p = pos_ws.xy;
      float a = getHeight(p + vec2(1,0)) - getHeight(p + vec2(-1,0));
      float b = getHeight(p + vec2(0,1)) - getHeight(p + vec2(0,-1));
      vec3 normal = normalize(vec3(-a,-b,2));

      v_texcoord = texcoord * instanceSize;
      v_pos_cs = (modelview * vec4(pos_ws, 1)).xyz;
      v_pos_ws = pos_ws;
      v_normal_cs = (modelview * vec4(normal,0)).xyz;
      v_normal_ws = normal;

      gl_Position = projection * vec4(v_pos_cs, 1);
      """

    vertexOut:
      "out vec2 v_texcoord"
      "out vec3 v_pos_cs"
      "out vec3 v_pos_ws"
      "out vec3 v_normal_cs"
      "out vec3 v_normal_ws"

    geometryMain:
      "layout(line_strip, max_vertices=2) out"
      
      """
      vec4 center = vec4(v_pos_cs[0] + v_pos_cs[1] + v_pos_cs[2], 3);

      vec3 v1 = (v_pos_cs[1] - v_pos_cs[0]).xyz;
      vec3 v2 = (v_pos_cs[2] - v_pos_cs[0]).xyz;
      vec4 normal = vec4(cross(v1,v2),0);

      gl_Position = projection * center;
      g_color = abs(normal);
      EmitVertex();
      gl_Position = projection * (center + normal);
      g_color = abs(normal);
      EmitVertex();
      """
    geometryOut:
      "out vec2 g_texcoord"
      "out vec3 g_pos_cs"
      "out vec3 g_pos_ws"
      "out vec3 g_normal_cs"
      "out vec3 g_normal_ws"
      "out vec4 g_color"
    fragmentMain:
      """
      color = g_color;
      //float lightFactor = 1 - length(v_pos_cs.xy) / lightRadius;
      //color = v_normal_ws.z * texture(crateTexture, v_texcoord) * lightFactor;
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

  simulationTime = gameTimer.time

  if fpsTimer.time >= 1:
    echo "FPS: ", fpsFrameCounter
    fpsFrameCounter = 0
    fpsTimer.reset

  render()
  fpsFrameCounter += 1
  frameCounter += 1


while runGame:
  mainLoopFunc()
