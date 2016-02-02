import math, sequtils, strutils, sdl2, opengl, ../fancygl, glm


proc nextRnd(): float32 =
  random(1.0).float32 - 0.5f

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

var hm = createFlatMap(64,64)

hm.DiamondSquare(64)
hm.printMap

discard sdl2.init(INIT_EVERYTHING)

var windowsize = vec2f(640,480)
var viewport = vec4f(0,0,640,480)

let window = createWindow("SDL/OpenGL Skeleton", 100, 100, windowsize.x.cint, windowsize.y.cint, SDL_WINDOW_OPENGL)
let context = window.glCreateContext()

# Initialize OpenGL
loadExtensions()

let
  crateTexture = loadAndBindTexture2DFromFile("crate.png")

  hmVertices = hm.vertices.arrayBuffer
  hmTexCoords = hm.texCoords.arrayBuffer
  hmIndices = hm.indices.elementArrayBuffer

  vertex = @[
    vec3f(+1, +1, -1), vec3f(-1, +1, -1), vec3f(-1, +1, +1),
    vec3f(+1, +1, +1), vec3f(+1, +1, -1), vec3f(-1, +1, +1),
    vec3f(+1, -1, +1), vec3f(-1, -1, +1), vec3f(-1, -1, -1),
    vec3f(+1, -1, -1), vec3f(+1, -1, +1), vec3f(-1, -1, -1),
    vec3f(+1, +1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, +1),
    vec3f(+1, -1, +1), vec3f(+1, +1, +1), vec3f(-1, -1, +1),
    vec3f(+1, -1, -1), vec3f(-1, -1, -1), vec3f(-1, +1, -1),
    vec3f(+1, +1, -1), vec3f(+1, -1, -1), vec3f(-1, +1, -1),
    vec3f(-1, +1, +1), vec3f(-1, +1, -1), vec3f(-1, -1, -1),
    vec3f(-1, -1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, -1),
    vec3f(+1, +1, -1), vec3f(+1, +1, +1), vec3f(+1, -1, +1),
    vec3f(+1, -1, -1), vec3f(+1, +1, -1), vec3f(+1, -1, +1)
  ].arrayBuffer

  normal = @[
    vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
    vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
    vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
    vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
    vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
    vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
    vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
    vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
    vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
    vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
    vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0),
    vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0)
  ].arrayBuffer

  color = @[
    vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
    vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
    vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
    vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
    vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
    vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
    vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
    vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
    vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
    vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
    vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0),
    vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0)
  ].arrayBuffer

  texcoord = @[
    vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),
    vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(1, 0), vec2f(0, 0),
    vec2f(0, 1), vec2f(1, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(1, 1), vec2f(0, 1),
    vec2f(0, 0), vec2f(1, 0), vec2f(0, 1)
  ].arrayBuffer


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
glEnable(GL_DEPTH_TEST)                           # Enable depth testing for z-culling
glDepthFunc(GL_LEQUAL)                            # Set the type of depth-test
glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST) # Nice perspective corrections

#glEnable(GL_CULL_FACE)
#glCullFace(GL_BACK)

let projection_mat = perspective(45.0, windowsize.x / windowsize.y, 0.1, 100.0)

var
  mouseX, mouseY: int32
  simulationTime = 0.0
  frameCounter = 0

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  let time = simulationTime

  var modelview_mat = I4()
  modelview_mat = modelview_mat.rotate( vec3d(1,0,0), -1.0 )
  modelview_mat = modelview_mat.translate( vec3d(sin(time)*32 - 32, cos(time)*32, -7) )

  #modelview_mat = modelview_mat.rotate( vec3[float](0,1,0), time )
  #modelview_mat = modelview_mat.rotate( vec3[float](1,0,0), time )

  shadingDsl(GL_TRIANGLES, hmindices.len.GLsizei):
    uniforms:
      modelview = modelview_mat
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
      v_texcoord = texcoord;
      """

    vertexOut:
      "out vec2 v_texcoord"

    fragmentMain:
      """
      color = texture(crateTexture, v_texcoord);
      """

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


  if not gamePaused:
    simulationTime = time - simulationTimeOffset

  if time - fpsFrameCounterStartTime >= 1:
    echo "FPS: ", fpsFrameCounter
    fpsFrameCounter = 0
    fpsFrameCounterStartTime = time

  render()
  fpsFrameCounter += 1
  frameCounter += 1
