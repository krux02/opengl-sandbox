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

  for y in 0 ..< hm.h:
    for x in 0 ..< hm.w:
      result.add vec3f(x.float32,y.float32,hm[x,y])

proc indices(hm: var HeightMap) : seq[int32] =
  result.newSeq(hm.w * hm.h * 6)
  result.setLen(0)

  for y in 0 ..< hm.h-1:
    for x in 0 ..< hm.w-1:
      let
        i1 = int32(x     + hm.w * y)
        i2 = int32(x + 1 + hm.w * y)
        i3 = int32(x     + hm.w * y + hm.w)
        i4 = int32(x + 1 + hm.w * y + hm.w)
      result.add([i1,i2,i3,i3,i2,i4])

proc texCoords(hm: var HeightMap) : seq[Vec2f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  let
    wf = hm.w.float32
    hf = hm.h.float32

  for y in 0 ..< hm.h:
    for x in 0 ..< hm.w:
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


proc uvSphereVertices(segments, rings: int): seq[Vec3f] =
  result.newSeq(segments * rings)
  result.setLen(0)

  for j in 0 .. < segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32

    for i in 0 .. < rings:
      let
        alpha = (i / (rings-1)) * PI
        h = cos(alpha).float32
        r = sin(alpha).float32

      result.add( vec3f(x * r, y * r, h) )


proc uvSphereNormals(segments, rings: int): seq[Vec3f] =
  uvSphereVertices(segments, rings)

proc uvSphereIndices(segments, rings: int): seq[int16] =
  result.newSeq(segments * rings * 6)
  result.setLen(0)

  for segment in 0 ..< segments - 1:
    for ring in 0 ..< rings - 1:
      let
        i1 = int16( ring +     segment * rings )
        i2 = int16( ring + 1 + segment * rings )
        i3 = int16( ring +     segment * rings + rings )
        i4 = int16( ring + 1 + segment * rings + rings )
      result.add([i1,i2,i3,i3,i2,i4])

  for ring in 0 ..< rings - 1:
    let
      i1 = int16( ring +     segments * rings - rings )
      i2 = int16( ring + 1 + segments * rings - rings )
      i3 = int16( ring +     0 )
      i4 = int16( ring + 1 + 0 )

    result.add([i1,i2,i3,i3,i2,i4])

var hm = createFlatMap(64,64)

hm.DiamondSquare(64)
hm.printMap

discard sdl2.init(INIT_EVERYTHING)

var windowsize = vec2f(640,480)
var viewport = vec4f(0,0,640,480)

let window = createWindow("SDL/OpenGL Skeleton", 100, 100, windowsize.x.cint, windowsize.y.cint, SDL_WINDOW_OPENGL or SDL_WINDOW_MOUSE_FOCUS)
let context = window.glCreateContext()

# Initialize OpenGL
loadExtensions()

let
  crateTexture = loadAndBindTexture2DFromFile("crate.png")


  hmVertices = hm.vertices.arrayBuffer
  hmTexCoords = hm.texCoords.arrayBuffer
  hmIndices = hm.indices.elementArrayBuffer

  sphereVertices = uvSphereVertices(32,16).arrayBuffer
  sphereNormals = uvSphereNormals(32,16).arrayBuffer
  sphereIndices = uvSphereIndices(32,16).elementArrayBuffer

declareFramebuffer(Fb1FramebufferType):
  depth = newRenderbuffer(windowsize)
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
  position = vec3d(32,32, hm[32,32] + 10 )

  positions = newSeq[Vec3f](20)
  colors = newSeq[Vec3f](50)

for i in 0 .. < colors.len:
  colors[i] = vec3f(random(1.0).float32, random(1.0).float32, random(1.0).float32)

let colorsBuffer = colors.arrayBuffer

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT) # Clear color and depth buffers

  let time = simulationTime

  var view_mat = I4()

  view_mat = view_mat.translate( position )
  view_mat = view_mat.rotate( vec3d(0,0,1), rotation.y )
  view_mat = view_mat.rotate( vec3d(1,0,0), rotation.x )


  let movement_ws = (view_mat * vec4d(movement, 0)).xyz
  position = position + movement_ws

  view_mat = view_mat.inverse
  #bindFramebuffer(fb1, Fb1FramebufferType):

  glEnable(GL_CULL_FACE)
  glCullFace(GL_BACK)
  glDepthFunc(GL_LEQUAL)

  shadingDsl(GL_TRIANGLES):
    numVertices = hmindices.len.GLsizei

    uniforms:
      modelview = view_mat
      projection = projection_mat
      time
      crateTexture

    attributes:
      indices = hmIndices
      pos = hmVertices
      texcoord = hmTexCoords

    vertexMain:
      """
      gl_Position = projection * modelview * vec4(pos, 1);
      v_texcoord = texcoord * 64.0;
      v_eyepos = (modelview * vec4(pos, 1)).xyz;
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
      //normal.rgb = g_normal;
      //normal.rgb = (g_normal.xyz + vec3(1))/2;
      """



  for i in 0 .. < positions.len:
    let
      distance = time * 10
      r = float32((i+1) / positions.len) * 32
      alpha = distance / r
      x = cos(alpha).float32 * r + 32
      y = sin(alpha).float32 * r + 32
      z = -33.0f
      #z = hm[x,y] + 1.5f

    positions[i] = vec3f(x, y, z)


  glEnable(GL_STENCIL_TEST)

  for i in 1..2:

    if i == 1:
      glDisable(GL_CULL_FACE)
      #glCullFace(GL_FRONT)
      glDepthFunc(GL_LEQUAL)
      #


      glColorMask(false, false, false, false)
      glDepthMask(false)
      glStencilOpSeparate(GL_FRONT,  GL_KEEP,  GL_KEEP, GL_INCR_WRAP)
      glStencilOpSeparate(GL_BACK, GL_KEEP, GL_KEEP, GL_DECR_WRAP)
      #glStencilFunc(GL_EQUAL,  0, 0xff)
    else:
      glEnable(GL_CULL_FACE)
      glCullFace(GL_FRONT)
      glDepthFunc(GL_GREATER)

      glStencilOp(GL_KEEP,  GL_KEEP, GL_KEEP)

      glColorMask(true, true, true, true)
      glDepthMask(true)

      glStencilFunc(GL_GEQUAL, 1, 0xff)


    shadingDsl(GL_TRIANGLES):
      numVertices = sphereIndices.len.GLsizei
      numInstances = positions.len.GLsizei

      uniforms:
        normalMat = view_mat
        mvp = projection_mat * view_mat
        scale = 3
        crateTexture

      attributes:
        indices = sphereIndices
        pos = sphereVertices

        instanceData:
          offset = positions
          col = colorsBuffer

      vertexMain:
        """
        gl_Position = mvp * vec4(pos * scale + offset, 1);
        v_normal = normalMat * vec4(pos,0);
        v_col = col;
        """

      vertexOut:
        "out vec4 v_normal"
        "out vec3 v_col"

      fragmentMain:
        """
        //color.rgb = (v_normal.xyz + vec3(1))/2;
        color.rgb = v_normal.xyz;
        //color.rgb = v_col;
        """

  glDisable(GL_STENCIL_TEST)
  glColorMask(true, true, true, true)
  glDepthMask(true)

  glSwapWindow(window)

var
  evt = sdl2.defaultEvent
  runGame = true
  gamePaused = false
  simulationTimeOffset = 0.0
  fpsFrameCounter = 0
  fpsFrameCounterStartTime = 0.0

while runGame:
  let time = float64( getTicks() ) / 1000.0

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown:
      let keyboardEvent = cast[KeyboardEventPtr](addr(evt))

      if keyboardEvent.keysym.scancode == SDL_SCANCODE_ESCAPE:
        runGame = false
        break

      if keyboardEvent.keysym.scancode == SDL_SCANCODE_PAUSE:
        if gamePaused:
          gamePaused = false
          simulationTimeOffset = time - simulationTime
        else:
          gamePaused = true

    if evt.kind == MouseMotion:
      let mouseEvent = cast[MouseMotionEventPtr](addr(evt))
      mouseX = mouseEvent.x
      mouseY = mouseEvent.y
      rotation.x = clamp( rotation.x - mouseEvent.yrel.float / 128.0 , 0, PI )
      rotation.y = rotation.y - mouseEvent.xrel.float / 128.0


  var state = getKeyboardState()

  movement.z = (state[SDL_SCANCODE_D.int].float - state[SDL_SCANCODE_E.int].float) * 0.05
  movement.x = (state[SDL_SCANCODE_F.int].float - state[SDL_SCANCODE_S.int].float) * 0.05

  if not gamePaused:
    simulationTime = time - simulationTimeOffset

  if time - fpsFrameCounterStartTime >= 1:
    #echo "FPS: ", fpsFrameCounter
    fpsFrameCounter = 0
    fpsFrameCounterStartTime = time

  render()
  fpsFrameCounter += 1
  frameCounter += 1
