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


import ../fancygl, algorithm, os


const
  cubemapWidth = 64'i32
  # this is just for some test conversion
  saveSkybox   = false
  # tweak this parameter to be able to see further
  gridTiles = 128

let (window, context) = defaultSetup()

let windowsize = window.size

let verts                = arrayBuffer(gridVerticesXMajor(vec2i(gridTiles + 1)))
let triangleStripIndices = elementArrayBuffer(gridIndicesTriangleStrip(vec2i(gridTiles + 1)))
let quadIndices          = elementArrayBuffer(gridIndicesQuads(vec2i(gridTiles + 1)))
let circleVertices       = arrayBuffer(circleVertices(48))

for vert in verts.mitems:
  vert.xy -= vec2f(gridTiles div 2)

let skyTexture = loadTexture2DFromFile(getAppDir() / "resources/panorama.jpg")

skyTexture.parameter(GL_TEXTURE_WRAP_S, GL_REPEAT)
skyTexture.parameter(GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)

let
  sphereVertices  = arrayBuffer(uvSphereVertices(128,64))
  sphereNormals   = arrayBuffer(uvSphereNormals(128,64))
  sphereTexCoords = arrayBuffer(uvSphereTexCoords(128,64))
  sphereIndices   = elementArrayBuffer(uvSphereIndices(128,64))

let
  torusVertices  = arrayBuffer(torusVertices(48,12, 1.0f, 0.1f))
  torusNormals   = arrayBuffer(torusNormals(48,12))
  torusTexCoords = arrayBuffer(torusTexCoords(48,12))
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
  glPointSize(16)
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

proc drawPortalWindow(modelViewMat: Mat4f): void =
  shadingDsl:
    primitiveMode = GL_TRIANGLE_FAN
    numVertices   = 50

    uniforms:
      mvp          = projMat * modelViewMat
      modelViewMat

    attributes:
      position = circleVertices

    vertexMain:
      """
      gl_Position = mvp * position;
      v_texCoord  = position.xy * 0.5 + 0.5;
      """
    vertexOut:
      "out vec2 v_texCoord"
    fragmentMain:
      """
      //color.rg = v_texCoord;
      //color.zw = vec2(0,1);
      color = vec4(0,0,0,1);
      """

var frame = 0

proc drawTorus(modelViewMat: Mat4f, clipPlane_ws: Vec4f): void =
  shadingDsl:
    primitiveMode = GL_TRIANGLE_STRIP
    numVertices   = torusIndicesLen
    indices       = torusIndices

    uniforms:
      projMat
      modelViewMat
      skyTexture
      invProj = projMat.inverse

    attributes:
      position = torusVertices
      normal   = torusNormals
      texCoord = torusTexCoords

    vertexMain:
      """
      v_pos_cs = modelViewMat * position;
      gl_Position = projMat * v_pos_cs;
      v_normal_cs   = modelViewMat * normal;
      v_texCoord = texCoord;
      """
    vertexOut:
      "out vec2 v_texCoord"
      "out vec4 v_normal_cs"
      "out vec4 v_pos_cs"
    fragmentMain:
      """
      color = v_normal_cs;
      color.rgb = reflect(normalize(v_pos_cs.xyz), v_normal_cs.xyz);
      color.a = 1;
      """


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

proc drawHeightMap(viewMat: Mat4f, clipPlane_ws: Vec4f): void =
  let mvp = projMat * viewMat
  let viewInverted = inverse(viewMat)

  let offset = floor(viewInverted[3].xy)
  let camdir = -viewInverted[2].xy
  let angle = round((arctan2(camdir.y, camdir.x) - radians(90f)) / radians(90f)) * radians(90f)
  let rotMat = rotMat2f(angle)

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
      clipPlane_ws

    attributes:
      pos = verts
    includes:
      glslNoise
    vertexMain:
      """
      pos2d_ws = rotMat * pos.xy + offset;
      vec4 vertexPos = vec4(  rotMat * pos.xy + offset,calcHeight(pos2d_ws),1);

      gl_ClipDistance[0] = dot(clipPlane_ws, vertexPos);

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
  if wireframe:
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE)
    glDisable(GL_BLEND)
    glDisable(GL_CULL_FACE)
  else:
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )
    glEnable(GL_BLEND)
    glEnable(GL_CULL_FACE)

type
  Portal = object
    node : WorldNode
    target: ptr Portal

proc clipPlane(this: WorldNode): Vec4f =
  result = this.modelmat[2]
  result.w = -dot(result, this.pos)

proc connect(p0,p1: var Portal): void =
  p0.target = p1.addr
  p1.target = p0.addr

proc tunnelTest*(this: Portal; a,b : Vec4f): bool =
  ## tests whether the line between ``a`` and ``b`` connects through the portal
  ## ``a`` and ``b`` are both in world space coordinates and their w component is expected to be 1

  let viewmat = this.node.viewmat

  # everything is transformed into the coordinate system of the Portal
  let posA_os = viewmat * a
  let posB_os = viewmat * b

  let z1 = posA_os.z
  let z2 = posB_os.z

  # test if we intersect the xy plane
  if (z1 < 0 and 0 <= z2) or (z2 < 0 and 0 <= z1):
    # z is always 0 after this transformation, so it is left out
    # the result is the intersection from the xy plane and the line between a and b in object space
    let pos0_os = (posA_os.xy * z2 - posB_os.xy * z1) / (z2 - z1)
    # test if we are further away than 1 from the origin:
    return dot(pos0_os, pos0_os) <= 1

proc transformNode*(this: Portal; node: WorldNode): WorldNode =
  let mat = this.target.node.modelmat * this.node.viewmat * node.modelmat
  result.pos = mat[3]
  result.dir = quat(mat)


var camera = newWorldNode()

camera.turnRelativeX(radians(45.0f))
camera.moveRelative(vec3f(0,0,20))


var portals: array[2, Portal]

portals[0].node = newWorldNode()
portals[0].node.moveAbsolute(vec3f(10,10,10))
portals[0].node.turnRelativeX(rand_f32() * 10)
portals[0].node.turnRelativeY(rand_f32() * 10)
portals[0].node.turnRelativeZ(rand_f32() * 10)

portals[1].node = newWorldNode()
portals[1].node.moveAbsolute(vec3f(3,2,9))
portals[1].node.turnRelativeX(rand_f32() * 10)
portals[1].node.turnRelativeY(rand_f32() * 10)
portals[1].node.turnRelativeZ(rand_f32() * 10)

connect(portals[0],portals[1])

proc drawScene(viewMat: Mat4f, clipPlane_ws: Vec4f): void =
  drawSky(viewMat)
  drawHeightMap(viewMat, clipPlane_ws)

  for portal in portals:
    drawTorus(viewMat * portal.node.modelmat, clipPlane_ws)
    drawTorus(viewMat * portal.node.modelmat, clipPlane_ws)

proc drawPortal(viewMat: Mat4f, src,dst: WorldNode) =

  glStencilMask(system.high(uint32))
  glEnable(GL_STENCIL_TEST)
  glDisable(GL_CULL_FACE)

  glStencilFunc(GL_ALWAYS, 1, 1)
  glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE)

  drawPortalWindow(viewMat * src.modelmat)

  glEnable(GL_CULL_FACE)

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
    glEnable(GL_STENCIL_TEST)
    glStencilFunc(GL_EQUAL, 1, 1)
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP)

    glEnable(GL_SCISSOR_TEST)
    glScissor(x.x, x.y,  s.x, s.y)

    glEnable(GL_CLIP_PLANE0)
    glClear(GL_DEPTH_BUFFER_BIT)
    drawScene(viewPrime, dst.clipPlane)
    glDisable(GL_CLIP_PLANE0)

    glDisable(GL_SCISSOR_TEST)
    glDisable(GL_STENCIL_TEST)

  glDisable(GL_STENCIL_TEST)

setup()

while running:
  defer:
    frame += 1

  # handle events

  var rotation, movement : Vec3f

  for evt in events():
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

  let oldCamera = camera

  camera.moveRelative(movement)
  camera.turnRelativeX(rotation.x)
  camera.turnAbsoluteZ(rotation.y)

  camera.dir = normalize(camera.dir)

  for i, portal in portals:
    if portal.tunnelTest(oldCamera.pos, camera.pos):
      camera = portal.transformNode(camera)

  time = gameTimer.time.float32

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT)

  let view = camera.viewMat
  let camdir_ws = camera.modelmat[2]

  drawScene(view, portals[0].node.clipPlane)

  if dot(portals[0].node.pos, camdir_ws) < dot(portals[1].node.pos, camdir_ws):
    drawPortal(view, portals[0].node, portals[0].target.node)
    drawPortal(view, portals[1].node, portals[1].target.node)
  else:
    drawPortal(view, portals[1].node, portals[1].target.node)
    drawPortal(view, portals[0].node, portals[0].target.node)

  glSwapWindow(window)
