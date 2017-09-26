import ../fancygl

let (window, context) = defaultSetup(vec2i(320,240))


let vertices = arrayBuffer([
  vec4f(-1,-1, 0, 1),
  vec4f( 1,-1, 0, 1),
  vec4f( 0, 1, 0, 1)
])

let colors   = arrayBuffer([
  vec4f( 1, 0,0,1),
  vec4f(0, 1,0,1),
  vec4f(0,0,1,1)
])

let indices = elementArrayBuffer([
  0'u16, 1, 2
])

var runGame: bool = true

let timer = newStopWatch(true)

let aspect = float32(window.size.x / window.size.y)
let proj : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

var frame = 0


import gifh

type
  GifAnimation = object
    writer: gifh.Writer
    w,h, delay: int32
    bitDepth: int32
    dither: bool
    buffer: seq[uint32]

proc startGifAnimation(window: WindowPtr, delay: int32; bitDepth: int32 = 8; dither: bool = false): GifAnimation =
  let (w,h) = window.getSize
  result.buffer = newSeq[uint32](w*h)
  result.w = w
  result.h = h
  result.delay = delay
  result.bitDepth = bitDepth
  result.dither = dither
  discard result.writer.begin(window.title & ".gif", w, h, delay, bitDepth, dither)


proc frameGifAnimation(this: var GifAnimation): void =
  glReadPixels(0,0,this.w,this.h,GL_RGBA, GL_UNSIGNED_BYTE, this.buffer[0].addr)
  discard this.writer.writeFrame(this.buffer[0].addr, this.w, this.h, this.delay, this.bitDepth, this.dither)

proc endGifAnimation(this: var GifAnimation): void =
  discard this.writer.gifEnd()
  this.buffer = nil

var animation = window.startGifAnimation(delay = 1, dither = false)

while runGame:
  frame += 1

  for evt in events():
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == KeyDown and evt.key.keysym.scancode == SDL_SCANCODE_ESCAPE:
      runGame = false

  let time = (frame / 100) * Pi * 2 # timer.time.float32

  let viewMat = mat4f(1)
    .translate(0,1,5)            # position camera at position 0,1,5
    .rotateX(Pi * -0.05)         # look a bit down
    .inverse                     # the camera matrix needs to be inverted

  let modelMat = mat4f(1)
    .rotateY(time)               # rotate the triangle
    .scale(3)                    # scale the triangle to be big enough on screen

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  shadingDsl:
    debug
    primitiveMode = GL_TRIANGLES
    numVertices = 3
    indices = indices
    uniforms:
      modelView = viewMat * modelMat
      proj
    attributes:
      a_vertex = vertices
      a_color  = colors
    vertexMain:
      """
      gl_Position = proj * modelView * a_vertex;
      v_color = a_color;
      """
    vertexOut:
      "out vec4 v_color"
    fragmentMain:
      """
      color = v_color;
      """

  glSwapWindow(window)

  if frame < 100:
    animation.frameGifAnimation
  elif frame == 100:
    animation.endGifAnimation
    runGame = false


echo "done"
