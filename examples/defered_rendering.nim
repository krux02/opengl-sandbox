import math, sequtils, strutils, sdl2, opengl, ../fancygl, glm

proc nextRnd(): float32 =
  random(1.0).float32 - 0.5f

proc lerp(start, stop, amt: float32) : float32 =
  (1 - amt) * start + amt * stop

type HeightMap = object
  w,h: int
  dataseq: seq[float32]

proc `[]`*(hm: var HeightMap, x,y: int): float32 {. inline .} =
  let
    nx = x and (hm.w - 1)
    ny = y and (hm.h - 1)

  hm.dataseq[nx + hm.w * ny]

proc `[]=`*(hm: var HeightMap, x,y: int; value: float32)  {. inline .} =
  let
    nx = x and (hm.w - 1)
    ny = y and (hm.h - 1)

  hm.dataseq[nx + hm.w * ny] = value

proc `[]`*(hm: HeightMap, x,y: float32): float32 {. inline .} =
  let
    bx = x.floor.int
    by = y.floor.int

    nx1 = bx and (hm.w - 1)
    nx2 = (bx + 1) and (hm.w - 1)
    ny1 = by and (hm.h - 1)
    ny2 = (by + 1) and (hm.h - 1)

    d1 = hm.dataseq[nx1 + hm.w * ny1]
    d2 = hm.dataseq[nx2 + hm.w * ny1]
    d3 = hm.dataseq[nx1 + hm.w * ny2]
    d4 = hm.dataseq[nx2 + hm.w * ny2]

    rx = x - x.floor
    ry = y - y.floor

  lerp( lerp(d1,d2, rx), lerp(d3,d4, rx), ry )

proc `[]`*(hm: HeightMap, pos: Vec2f) : float32 {. inline .} =
  hm[pos.x, pos.y]

proc vertices(hm: var HeightMap) : seq[Vec3f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      result.add vec3f(x.float32,y.float32,hm[x,y])

proc indices(hm: var HeightMap) : seq[int32] =
  result.newSeq(hm.w * hm.h * 6)
  result.setLen(0)

  for y in 0 ..< hm.h:
    for x in 0 ..< hm.w:
      let
        i1 = int32(x     + (hm.w + 1) * y)
        i2 = int32(x + 1 + (hm.w + 1) * y)
        i3 = int32(x     + (hm.w + 1) * (y + 1))
        i4 = int32(x + 1 + (hm.w + 1) * (y + 1))

        h1 = hm[x+0,y+0]
        h2 = hm[x+1,y+0]
        h3 = hm[x+0,y+1]
        h4 = hm[x+1,y+1]

      if abs(h1 - h4) < abs(h2 - h3):
        result.add([i3,i1,i4,i4,i1,i2])
      else:
        result.add([i1,i2,i3,i3,i2,i4])



proc texCoords(hm: var HeightMap) : seq[Vec2f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  let
    wf = hm.w.float32
    hf = hm.h.float32

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      let
        xf = x.float32
        yf = y.float32

      result.add vec2f(xf / wf, yf / hf)

proc minMax(hm: HeightMap): (float,float) =
  result[0] = Inf
  result[1] = NegInf

  for v in hm.dataseq:
    result[0] = min(result[0], v)
    result[1] = max(result[1], v)

proc linMap(v,min,max, newMin, newMax: float32): float32 =
  (v - min) * (newMax - newMin) / (max - min) + newMin

proc clamp(v, minv, maxv: float32): float32 =
  min(max(v,minv), maxv)

proc printMap(hm: var HeightMap): void =
  let (min,max) = hm.minMax
  if min == max:
    return

  const chars = " .:;+X# .:;+X#"

  for y in 0 ..< hm.h:
    for x in 0 ..< hm.w:
      let idx = hm[x,y].linMap(min,max, 0, chars.len-1).int
      #stdout.write( idx )
      stdout.write( chars[idx] )
    stdout.writeLine("")
  echo ""

proc DiamondSquare(hm: var HeightMap, startfactor: float32): void =
  let
    w = hm.w
    h = hm.h

  var
    stepSize = w
    factor = startfactor

  proc squares(hm: var HeightMap): void =
    for i in countup(0, w-1, stepSize):
      for j in countup(0, h-1, stepSize):
        var sum =  hm[i,j]
        sum += hm[i + stepSize, j]
        sum += hm[i, j + stepSize]
        sum += hm[i + stepSize, j + stepSize]

        let x = i + stepSize div 2
        let y = j + stepSize div 2
        hm[x,y] = sum * 0.25f + nextRnd() * factor

  proc diamonds(hm: var HeightMap): void =
    for i in countup(0, w-1, stepSize):
      for j in countup(0, h-1, stepSize):
        if ((i+j) div stepSize) mod 2 == 1:
          var sum = 0.0f
          var count = 0.0f
          if i != 0:
            sum += hm[i-stepSize, j]
            count += 1
          if i < w-1:
            sum += hm[i+stepSize, j]
            count += 1
          if j != 0:
            sum += hm[i, j-stepSize]
            count += 1
          if j < h-1:
            sum += hm[i, j+stepSize]
            count += 1

          hm[i,j] = (sum / count) + nextRnd() * factor


  while stepSize > 0:
    squares(hm)
    stepSize = stepSize div 2
    if stepSize == 0:
      break

    diamonds(hm)
    factor *= 0.5f

proc createFlatMap(width,height: int): HeightMap =
  result.w = width
  result.h = height
  result.dataseq = newSeq[float32](width*height)


var hm = createFlatMap(64,64)

hm.DiamondSquare(64)
#hm.printMap

discard sdl2.init(INIT_EVERYTHING)

var windowsize = vec2f(1024,768)
var viewport = vec4f(0,0,1024,768)

let window = createWindow("SDL/OpenGL Skeleton", 100, 100, windowsize.x.cint, windowsize.y.cint, SDL_WINDOW_OPENGL) # SDL_WINDOW_MOUSE_CAPTURE
let context = window.glCreateContext()

# Initialize OpenGL
loadExtensions()

let
  crateTexture = loadTexture2DFromFile("crate.png")

  hmVertices = hm.vertices.arrayBuffer
  hmTexCoords = hm.texCoords.arrayBuffer
  hmIndices = hm.indices.elementArrayBuffer

  sphereVertices = uvSphereVertices(32,16).arrayBuffer
  sphereNormals = uvSphereNormals(32,16).arrayBuffer
  sphereIndices = uvSphereIndices(32,16).elementArrayBuffer
  sphereTexCoords = uvSphereTexCoords(32,16).arrayBuffer

  #sphereVertices  = cylinderVertices(32, 0).arrayBuffer
  #sphereNormals   = cylinderNormals(32, 0).arrayBuffer
  #sphereTexCoords = cylinderTexCoords(32).arrayBuffer
  #sphereIndices   = cylinderIndices(32).elementArrayBuffer

  screenSpaceTriangleVerts = @[
    vec4f(-1,-1,1,1), vec4f(3,-1,1,1), vec4f(-1,3,1,1)
  ].arrayBuffer

  screenSpaceTriangleTexcoords = @[
    vec2f(0,0), vec2f(2,0), vec2f(0,2)
  ].arrayBuffer

var hideHeightmap, hideObjects, hideNormals, hideDifferedShading: bool

declareFramebuffer(Fb1FramebufferType):
  depth = newTexture(windowsize)
  color = newTexture(windowsize)
  normal = newTexture(windowsize)

if 0 != glSetSwapInterval(-1):
  echo "glSetSwapInterval -1 not supported"
  echo sdl2.getError()
  if 0 != glSetSwapInterval(1):
    echo "but glSetSwapInterval 1 is ok"
  else:
    echo "even 1 is not ok"
    echo sdl2.getError()

glClearColor(0.0, 0.0, 0.0, 1.0)                  # Set background color to black and opaque
glClearDepth(1.0)                                 # Set background depth to farthest
glEnable(GL_DEPTH_TEST)                           # Enable depth testing for z-culling                          # Set the type of depth-test
glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST) # Nice perspective corrections


let projection_mat = perspective(45.0, windowsize.x / windowsize.y, 0.1, 100.0)

var
  mouseX, mouseY: int32
  simulationTime = 0.0
  frameCounter = 0

  movement = vec3d(0,0,0)
  rotation = vec2d(PI/2,0)
  position = vec3d(0,0, hm[0,0] + 10 )

  positions = newSeq[Vec3f](20).arrayBuffer
  colors = newSeq[Vec3f](50).arrayBuffer

mapWriteBufferBlock(colors):
  let maximum = colors.len - 1
  for i in 0 .. maximum:
    mappedBuffer[i] = vec3f(random(1.0).float32, random(1.0).float32, random(1.0).float32)

var
  effectOrigin = position.xy.vec2f
  effectStartTime = -100.0f


proc showNormals(mvp: Mat4d, positions: ArrayBuffer[Vec3f], normals: ArrayBuffer[Vec3f], length:float32 = 1, color:Vec3f = vec3f(1)) =

  shadingDsl(GL_POINTS):
    numVertices = normals.len.GLsizei

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
      "out vec3 v_normal"
      "out vec3 v_pos"

    geometryMain:
      "layout(line_strip, max_vertices=2) out"
      """
      gl_Position = mvp * vec4(v_pos[0], 1);
      EmitVertex();
      gl_Position = mvp * vec4(v_pos[0] + v_normal[0] * scale, 1);
      EmitVertex();
      """

    fragmentMain:
      """
      color.rgb = normalColor;
      """




proc render() =

  let time = simulationTime

  var view_mat = I4()

  view_mat = view_mat.translate( position )
  view_mat = view_mat.rotate( vec3d(0,0,1), rotation.y )
  view_mat = view_mat.rotate( vec3d(1,0,0), rotation.x )


  let movement_ws = (view_mat * vec4d(movement, 0)).xyz
  position = position + movement_ws

  view_mat = view_mat.inverse

  let lightDir_cs = (view_mat * vec3d(0.577).vec4d(0)).xyz.vec3f

  # Clear color and depth buffers
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT)

  bindFramebuffer(fb1, Fb1FramebufferType):
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT)


    glEnable(GL_CULL_FACE)
    glCullFace(GL_BACK)
    glDepthFunc(GL_LEQUAL)

    var baseOffset = vec3f(0,0,0)
    baseOffset.x = floor(position.x / 64) * 64
    baseOffset.y = floor(position.y / 64) * 64

    shadingDsl(GL_TRIANGLES):
      numVertices = hmindices.len.GLsizei
      numInstances = 64

      uniforms:
        modelview = view_mat
        projection = projection_mat
        time
        crateTexture
        baseOffset
        lightDir_cs
        effectOrigin
        effectStartTime

      attributes:
        indices = hmIndices
        pos = hmVertices
        texcoord = hmTexCoords

      vertexMain:
        """
        vec3 offset = vec3(gl_InstanceID % 8 - 4, gl_InstanceID / 8 - 4, 0) * 64.0 + baseOffset;
        vec3 pos_ws = pos + offset;

        float effectLength = time - effectStartTime;
        float effectParameter = effectLength * 5.0 - length(pos_ws.xy - effectOrigin) * 0.2;
        float effect = cos(clamp(effectParameter, -3.14, 3.14)) + 1;

        pos_ws.z += effect * 10.0 / (effectLength + 1);

        v_texcoord = texcoord * 64.0;
        v_eyepos = (modelview * vec4(pos_ws, 1)).xyz;
        """

      vertexOut:
        "out vec2 v_texcoord"
        "out vec3 v_eyepos"

      geometryMain:
        "layout(triangle_strip, max_vertices=3) out"
        """
        g_normal = normalize(cross(v_eyepos[1] - v_eyepos[0], v_eyepos[2] - v_eyepos[0]));

        for( int i=0; i < 3; i++) {
          gl_Position = projection * vec4(v_eyepos[i], 1);
          g_texcoord = v_texcoord[i];
          EmitVertex();
        }
        """


      geometryOut:
        "out vec2 g_texcoord"
        "out vec3 g_normal"

      fragmentMain:
        """
        color = texture(crateTexture, g_texcoord);
        normal.rgb = (g_normal + vec3(1))/2;
        """

  fb1.color.generateMipmap

  mapWriteBufferBlock(positions):
    let poslen = positions.len
    for i in 0 .. < poslen:
      let
        distance = time * 10
        r = float32((i+1) / poslen) * 32
        alpha = distance / r
        x = cos(alpha).float32 * r + 32
        y = sin(alpha).float32 * r + 32
        z = -33.0f
        #z = hm[x,y] + 1.5f

      mappedBuffer[i] = vec3f(x, y, z)

  glDepthMask(false)
  glEnable(GL_BLEND)
  glBlendFunc(GL_SRC_ALPHA, GL_ONE)

  var
    mvp = project_map * view_mat
    inverse_mvp = mvp

  inverse_mvp = inverse(inverse_mvp)

  if hideDifferedShading:
    shadingDsl(GL_TRIANGLES):
      numVertices = 3

      uniforms:
        tex = fb1.color
        depth = fb1.depth
        norm = fb1.normal
        time
        viewport
        texSize = fb1.color.size
        inverse_mvp

      attributes:
        pos = screenSpaceTriangleVerts
        texcoord = screenSpaceTriangleTexcoords

      vertexMain:
        """
        gl_Position = pos;
        v_texcoord = texcoord;
        """

      vertexOut:
        "out vec2 v_texcoord"

      fragmentMain:
        """
        vec2 texcoord = (v_texcoord * viewport.zw ) / texSize;
        gl_FragDepth = texture(depth, texcoord).x;
        vec4 worldpos = inverse_mvp * vec4( (gl_FragCoord.xy / viewport.zw) * 2 - vec2(1), gl_FragDepth * 2 - 1, 1 );
        worldpos /= worldpos.w;
        // if((( int(gl_FragCoord.x) / 32) % 2 + ( int(gl_FragCoord.y) / 32) % 2) % 2 == 0) {

        if( gl_FragCoord.x > border.x ) {
          if( gl_FragDepth != 1 ) {
            color = worldpos - floor(worldpos);
          }
        } else {
          if( gl_FragCoord.y > border.y ) {
            color = texture(tex, texcoord);
          } else {
            color = texture(norm, texcoord);
          }
        }
        """

    glDepthMask(false)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
  else:
    #### render lights ####
    shadingDsl(GL_TRIANGLES):
      numVertices = sphereIndices.len.GLsizei
      numInstances = positions.len.GLsizei

      uniforms:
        normalMat = view_mat
        mvp
        inverse_mvp

        scale = 3
        lightDir_cs
        viewport

        color_tex = fb1.color
        depth_tex = fb1.depth
        normal_tex = fb1.normal

      attributes:
        indices = sphereIndices
        pos = sphereVertices
        normal = sphereNormals
        texCoord = sphereTexCoords

        instanceData:
          offset = positions
          col = colors

      vertexMain:
        """
        gl_Position = mvp * vec4(pos * scale + offset, 1);
        v_normal = (normalMat * vec4(normal,0)).xyz;
        v_col = col;
        v_texCoord = texCoord;
        """

      vertexOut:
        "out vec3 v_normal"
        "out vec3 v_col"
        "out vec2 v_texCoord"
        "out vec3 v_lightPos"

      fragmentMain:
        """
        vec2 texcoord = v_texCoord;

        float depth = texture(depth_tex, texcoord).x * 2 - 1;
        vec3 color = texture(color_tex, texcoord).rgb;
        vec3 normal = texture(normal_tex, texcoord).xyz;

        vec4 worldpos = inverse_mvp * vec4( (gl_FragCoord.xy / viewport.zw) * 2 - vec2(1), depth, 1 );
        worldpos /= worldpos.w;

        vec3 light_dir = v_lightPos - worldpos.xyz;
        float dist = length(light_dir);
        light_dir /= dist;
        float factor1 = max((scale - dist) / scale, 0);
        float factor2 = dot(normal, light_dir);

        color.rgb = factor1 * factor2 * v_col;
        """

  if not hideNormals:
    showNormals(projection_mat * view_mat, sphereVertices, sphereNormals, 0.3f)

  glSwapWindow(window)

var
  evt = sdl2.defaultEvent
  runGame = true
  gamePaused = false
  simulationTimeOffset = 0.0
  fpsFrameCounter = 0
  fpsFrameCounterStartTime = 0.0

#captureMouse(SDL_True)

while runGame:
  let time = float64( getTicks() ) / 1000.0

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown:
      let keyboardEvent = cast[KeyboardEventPtr](addr(evt))

      case keyboardEvent.keysym.scancode
      of SDL_SCANCODE_ESCAPE:
        runGame = false
        break

      of SDL_SCANCODE_SPACE:
        effectOrigin = position.xy.vec2f
        effectStartTime = simulationTime

      of SDL_SCANCODE_PAUSE:
        if gamePaused:
          gamePaused = false
          simulationTimeOffset = time - simulationTime
        else:
          gamePaused = true

      of SDL_SCANCODE_1:
        hideObjects = not hideObjects

      of SDL_SCANCODE_2:
        hideNormals = not hideNormals

      of SDL_SCANCODE_3:
        hideHeightmap = not hideHeightmap

      of SDL_SCANCODE_4
        hideDeferredShading = not hideDeferredShading

      else:
        discard

    if evt.kind == MouseMotion:
      let mouseEvent = cast[MouseMotionEventPtr](addr(evt))
      mouseX = mouseEvent.x
      mouseY = mouseEvent.y
      rotation.x = clamp( rotation.x - mouseEvent.yrel.float / 128.0 , 0, PI )
      rotation.y = rotation.y - mouseEvent.xrel.float / 128.0


  var state = getKeyboardState()

  movement.z = (state[SDL_SCANCODE_D.int].float - state[SDL_SCANCODE_E.int].float) * 0.15
  movement.x = (state[SDL_SCANCODE_F.int].float - state[SDL_SCANCODE_S.int].float) * 0.15

  if not gamePaused:
    simulationTime = time - simulationTimeOffset

  if time - fpsFrameCounterStartTime >= 1:
    echo "FPS: ", fpsFrameCounter
    fpsFrameCounter = 0
    fpsFrameCounterStartTime = time

  render()
  fpsFrameCounter += 1
  frameCounter += 1
