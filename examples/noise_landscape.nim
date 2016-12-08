const glslnoise = """
vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 mod289(vec4 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 permute(vec4 x) {
     return mod289(((x*34.0)+1.0)*x);
}

vec4 taylorInvSqrt(vec4 r) {
  return 1.79284291400159 - 0.85373472095314 * r;
}

float snoise(vec3 v) { 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //   x0 = x0 - 0.0 + 0.0 * C.xxx;
  //   x1 = x0 - i1  + 1.0 * C.xxx;
  //   x2 = x0 - i2  + 2.0 * C.xxx;
  //   x3 = x0 - 1.0 + 3.0 * C.xxx;
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

// Permutations
  i = mod289(i); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients: 7x7 points over a square, mapped onto an octahedron.
// The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
  float n_ = 0.142857142857; // 1.0/7.0
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
}

float calcHeight(vec2 pos) {
  return snoise(vec3(pos * 0.123, time * 0.0123)) * 5.0;
}
"""


import ../fancygl

const
  cubemapWidth = 64'i32
  # this is just for some test conversion
  saveSkybox   = false
  # tweak this parameter to be able to see further
  gridTiles = 128

let (window, context) = defaultSetup()

let windowsize = window.size

proc reverse[T](arg: seq[T]): seq[T] =
  result = newSeq[T](arg.len)
  for i, x in arg:
    result[arg.high - i] = x

let verts = arrayBuffer(gridVerticesXMajor(vec2i(gridTiles + 1)))
let triangleStripIndices = elementArrayBuffer(gridIndicesTriangleStrip(vec2i(gridTiles + 1)))
let quadIndices          = elementArrayBuffer(gridIndicesQuads(vec2i(gridTiles + 1)))

for vert in verts.mitems:
  vert.xy -= vec2f(gridTiles div 2)

let skyTexture = loadTexture2DFromFile("resources/panorama.jpg")

skyTexture.parameter(GL_TEXTURE_WRAP_S, GL_REPEAT)
skyTexture.parameter(GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)

let
  sphereVertices  = arrayBuffer(uvSphereVertices(128,64))
  sphereNormals   = arrayBuffer(uvSphereNormals(128,64))
  sphereTexCoords = arrayBuffer(uvSphereTexCoords(128,64))
  sphereIndices   = elementArrayBuffer(uvSphereIndices(128,64))

let
  torusVertices = arrayBuffer(torusVertices(48,12, 1.0f, 0.1f))
  torusNormals  = arrayBuffer(torusNormals(48,12))
  torusIndices  = elementArrayBuffer(torusIndicesTriangleStrip(48,12))

let triangleStripIndicesLen = triangleStripIndices.len
let quadIndicesLen = quadIndices.len
let sphereIndicesLen = sphereIndices.len
let torusIndicesLen = torusIndices.len

var
  running = true
  skybox  = true

var projMat = perspective(45'f32, windowsize.x / windowsize.y, 0.9, 1000.0)

var
  gameTimer = newStopWatch(true)
  time: float32 = 0

discard setRelativeMouseMode(Bool32(true))

let layers = newTexture1D(512, GL_RGBA8)

block:
  var data = newSeq[Vec4u8](512)
  for color in data.mitems:
    color.x = rand_u8()
    color.y = rand_u8()
    color.z = rand_u8()
    color.w = 255
    
  layers.setData data
  layers.generateMipmap

proc rotMat2f(angle: float32): Mat2f =
  let s = sin(angle)
  let c = cos(angle)

  result[0,0] =  c
  result[0,1] =  s
  result[1,0] = -s
  result[1,1] =  c

proc setup(): void =
  glEnable(GL_CULL_FACE)
  glEnable(GL_BLEND)
  glEnable(GL_DEPTH_CLAMP)
  glDepthFunc(GL_LEQUAL)
  glViewport(0,0,window.size.x, window.size.y)
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

proc drawSky(viewMat: Mat4f): void =
  
  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices   = sphereIndicesLen
    indices = sphereIndices

    uniforms:
      mvp = projMat * viewMat
      skyTexture

    attributes:
      normal = sphereNormals
      texCoord = sphereTexCoords

    vertexMain:
      """
      gl_Position = mvp * -normal;
      v_texCoord = vec2(1) - texCoord;
      """
    vertexOut:
      "out vec2 v_texCoord"
    fragmentMain:
      """
      color = texture(skyTexture, v_texCoord);
      color.a = 1.0;
      """

proc drawSphere(modelViewMat: Mat4f): void =
  shadingDsl:
    primitiveMode = GL_TRIANGLES
    numVertices   = sphereIndicesLen
    indices       = sphereIndices

    uniforms:
      mvp = projMat * modelViewMat
      modelViewMat
      skyTexture

    attributes:
      position = sphereVertices
      normal = sphereNormals
      texCoord = sphereTexCoords

    vertexMain:
      """
      gl_Position = mvp * position;
      v_normal_cs   = modelViewMat * normal;
      v_texCoord = texCoord;
      """
    vertexOut:
      "out vec2 v_texCoord"
      "out vec4 v_normal_cs"
    fragmentMain:
      """
      color = texture(skyTexture, v_texCoord) * max(dot(v_normal_cs, normalize(vec4(1,2,3,0))), 0);
      color.a = 1;
      """
#[
when false:
  proc drawTorus(modelViewMat: Mat4f): void =
    shadingDsl:
      primitiveMode = GL_TRIANGLE_STRIP
      numVertices   = torusIndicesLen
      indices       = torusIndices

      uniforms:
        mvp = projMat * modelViewMat
        modelViewMat
        skyTexture

      attributes:
        position = torusVertices 
        normal   = torusNormals  

      vertexMain:
        """
        gl_Position = mvp * position;
        v_normal_cs   = modelViewMat * normal;
        v_texCoord = texCoord;
        """
      vertexOut:
        "out vec2 v_texCoord"
        "out vec4 v_normal_cs"
      fragmentMain:
        """
        color = texture(skyTexture, v_texCoord) * max(dot(v_normal_cs, normalize(vec4(1,2,3,0))), 0);
        color.a = 1;
        """
]#

let quadVertices = arrayBuffer([vec4f(-1,-1,0,1), vec4f(1, -1, 0 ,1), vec4f(-1,1,0,1), vec4f(1,1,0,1)])
      
proc drawPlane(modelViewMat: Mat4f): void =
  shadingDsl:
    primitiveMode = GL_TRIANGLE_STRIP
    numVertices = 4

    uniforms:
      mvp = projMat * modelViewMat

    attributes:
      position = quadVertices

    vertexMain:
      """
      gl_Position = mvp * position;
      v_texCoord = position.xy * 0.5 + 0.5;
      """
    vertexOut:
      "out vec2 v_texCoord"
    fragmentMain:
      """
      color= vec4(v_texCoord,0,1);
      """
  
      
proc drawHeightMap(viewMat: Mat4f): void =
  let mvp = projMat * viewMat
  let viewInverted = inverse(viewMat)

  let offset = floor(viewInverted[3].xy)
  let camdir = -viewInverted[2].xy
  let angle = round((arctan2(camdir.y, camdir.x) - radians(90f)) / radians(90f)) * radians(90f)
  let rotMat = rotMat2f(angle)
  
  when true:
    shadingDsl:
      primitiveMode = GL_TRIANGLE_STRIP
      numVertices = triangleStripIndicesLen
      indices = triangleStripIndices

      uniforms:
        mvp
        viewMat
        time
        layers
        offset
        rotMat
        gridTiles

      attributes:
        pos = verts
      includes:
        glslNoise
      vertexMain:
        """
        pos2d_ws = rotMat * pos.xy + offset;
        vec4 vertexPos = vec4(  rotMat * pos.xy + offset,calcHeight(pos2d_ws),1);
        gl_Position = mvp * vertexPos;
        distance = length(viewMat * vertexPos);
        """
      vertexOut:
        "out vec2 pos2d_ws"
        "out float distance"
      fragmentMain:
        """
        float height = calcHeight(pos2d_ws);
        float detail = snoise(vec3(pos2d_ws * 12.3, 0));
        color = texture(layers,height * 0.01 + detail * 0.003);
        color.a = 1 - (distance - float(gridTiles) * 0.5 + 32) / 32.0;
        """
  else:
    shadingDsl:
      primitiveMode = GL_LINES_ADJACENCY
      numVertices = quadIndicesLen
      indices = quadIndices

      uniforms:
        mvp
        viewMat
        time
        layers
        offset
        rotMat
        gridTiles

      attributes:
        pos = verts
      includes:
        glslNoise
      vertexMain:
        """
        v_pos2d_ws = rotMat * pos.xy + offset;
        v_height = calcHeight(v_pos2d_ws);
        vec4 vertexPos = vec4(  rotMat * pos.xy + offset,v_height,1);
        gl_Position = mvp * vertexPos;
        v_distance = length(viewMat * vertexPos);
        """
      vertexOut:
        "out float v_height"
        "out vec2 v_pos2d_ws"
        "out float v_distance"
      geometryMain:
        "layout(triangle_strip, max_vertices=4) out"
        """
        int indicesB[4] = int[4](0,1,2,3);
        int indicesA[4] = int[4](2,0,3,1);

        int indices[4] = abs(v_height[0] - v_height[3]) < abs(v_height[1] - v_height[2]) ? indicesA : indicesB;

        for(int i = 0; i < 4; ++i) {
          gl_Position = gl_in[indices[i]].gl_Position;
          pos2d_ws = v_pos2d_ws[indices[i]];
          distance = v_distance[indices[i]];
          EmitVertex();
        }
        """
      geometryOut:
        "out vec2 pos2d_ws"
        "out float distance"
      fragmentMain:
        """
        float height = calcHeight(pos2d_ws);
        float detail = snoise(vec3(pos2d_ws * 12.3, 0));
        color = texture(layers,height * 0.01 + detail * 0.003);
        color.a = 1 - (distance - float(gridTiles) * 0.5 + 32.0) / 32.0;
        """

when saveSkybox:
  declareFramebuffer(SkyboxFramebuffer):
    depth = newDepthRenderBuffer(vec2i(cubemapWidth))
    color = newTexture2D(vec2i(cubemapWidth))

  let skyboxRt = newSkyboxFramebuffer()

  blockBindFramebuffer(skyboxRt):
    let c = cubemapWidth

    #glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
    glViewport(0,0, c, c)

    let proj: Mat4f = frustum[float32](-1,1,-1,1,1,1000)
    var tempCam = newWorldNode()

    #let cubemap = newCubemapTexture()
    #glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_CUBE_MAP_POSITIVE_X, cubemap.handle, 0)

    drawSky(tempCam.viewMat)
    skyboxRt.color.saveBMP("skyboxtest-1.bmp")

    tempCam.turnRelativeX(radians(90.0))
    drawSky(tempCam.viewMat)
    skyboxRt.color.saveBMP("skyboxtest-2.bmp")

    tempCam.turnAbsoluteZ(radians(90.0))
    drawSky(tempCam.viewMat)
    skyboxRt.color.saveBMP("skyboxtest-3.bmp")

    tempCam.turnAbsoluteZ(radians(90.0))
    drawSky(tempCam.viewMat)
    skyboxRt.color.saveBMP("skyboxtest-4.bmp")

    tempCam.turnAbsoluteZ(radians(90.0))
    drawSky(tempCam.viewMat)
    skyboxRt.color.saveBMP("skyboxtest-5.bmp")

    tempCam.turnAbsoluteZ(radians(90.0))
    tempCam.turnRelativeX(radians(90.0))
    drawSky(tempCam.viewMat)
    skyboxRt.color.saveBMP("skyboxtest-6.bmp")

proc toggleWireframe() : void =
  var wireframe {.global.} = false
  wireframe = not wireframe
  glPolygonMode( GL_FRONT_AND_BACK, if wireframe: GL_LINE else: GL_FILL );


var frame = 0
var camera = newWorldNode()

camera.turnRelativeX(radians(45.0f))
camera.moveRelative(vec3f(0,0,20))

var object1 = newWorldNode()
object1.moveAbsolute(vec3f(10,10,10))
object1.turnRelativeX(rand_f32() * 10)
object1.turnRelativeY(rand_f32() * 10)
object1.turnRelativeZ(rand_f32() * 10)
var object2 = newWorldNode()
object2.moveAbsolute(vec3f(3,2,9))
object2.turnRelativeX(rand_f32() * 10)
object2.turnRelativeY(rand_f32() * 10)
object2.turnRelativeZ(rand_f32() * 10)

proc drawScene(viewMat: Mat4f): void =
  drawSky(viewMat)
  drawHeightMap(viewMat)
  #drawSphere(viewMat * object1.modelmat)
  #drawSphere(viewMat * object2.modelmat)
  drawPlane(viewMat * object1.modelmat)
  drawPlane(viewMat * object2.modelmat)

proc drawPortal(viewMat: Mat4f, src,dst: WorldNode) =
  let viewPrime = viewMat * src.modelmat * dst.viewmat
  let objPos_cs = viewMat * src.pos
  var a = projMat * (objPos_cs + vec4f(-1,-1,0,0))
  var b = projMat * (objPos_cs + vec4f( 1, 1,0,0))
  
  a /= a.w
  b /= b.w
  var x = vec2i((a.xy * 0.5 + 0.5) * vec2f(window.size))
  var y = vec2i((b.xy * 0.5 + 0.5) * vec2f(window.size))
  x = clamp(x, vec2i(0), window.size)
  y = clamp(y, vec2i(0), window.size) 
  let s = y - x

  if s.x > 0 and s.y > 0:
    glEnable(GL_SCISSOR_TEST)
    glScissor(x.x, x.y,  s.x, s.y)
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
    drawScene(viewPrime)
    glDisable(GL_SCISSOR_TEST)

  
  
setup()

while running:
  defer:
    frame += 1

  # handle events
  
  var evt = defaultEvent
  var rotation, movement : Vec3f

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      running = false
      break
    elif evt.kind == KeyDown:
      case evt.key.keysym.scancode:
      of SDL_SCANCODE_ESCAPE:
        running = false
        break
      of SDL_SCANCODE_F10:
        window.screenshot
      of SDL_SCANCODE_W:
        toggleWireframe()
      of SDL_SCANCODE_R:
        skybox = not skybox
      else:
        discard
        
    elif evt.kind == MouseMotion:
      rotation.x = rotation.x - evt.motion.yrel.float / 128.0
      rotation.y = rotation.y - evt.motion.xrel.float / 128.0

    else:
      discard

  var state = getKeyboardState()

  movement.z = (state[SDL_SCANCODE_D.int].float - state[SDL_SCANCODE_E.int].float) * 0.4
  movement.x = (state[SDL_SCANCODE_F.int].float - state[SDL_SCANCODE_S.int].float) * 0.4

  camera.moveRelative(movement)
  camera.turnRelativeX(rotation.x)
  camera.turnAbsoluteZ(rotation.y)

  time = gameTimer.time.float32

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  let view = camera.viewMat

  drawScene(view)
  
  drawPortal(view, object1, object2)
  drawPortal(view, object2, object1)
  
  #[
  
  
  let cameraBackup = camera
  defer:
    camera = cameraBackup
    glDisable(GL_SCISSOR_TEST)

  for i in 0 .. GLint(5):
    
    camera.moveRelative(vec3f(0,0,-3))
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
    drawScene(camera.viewMat)
  ]#
  
  glSwapWindow(window)

  

  
