import ../fancygl, libarne, glm, glm/noise

let windowsize = vec2i(512,512)
let (window, context) = defaultSetup(windowsize)

const textureSize = 512
var pixels = newSeq[Vec4u8](textureSize * textureSize)

proc linmap(v, smin, smax, dmin, dmax: Vec) : Vec =
  (v - smin) / (smax-smin) * (dmax-dmin) + dmin

proc linmap[N,T](v: Vec[N,T]; smin, smax, dmin, dmax: T) : Vec[N,T] =
  (v - smin) * ((dmax-dmin) / (smax-smin)) + dmin

proc linmap(v, smin, smax, dmin, dmax: SomeReal) : SomeReal =
  (v - smin) * ((dmax-dmin) / (smax-smin)) + dmin

for i, pixel in pixels.mpairs:
  let x = linmap(float64(i mod textureSize) + 0.5, 0, textureSize, 0, 10)
  let y = linmap(float64(i div textureSize) + 0.5, 0, textureSize, 0, 10)
  let noise = cellular(vec2(x,y)).linmap(-1,1, 0, 255)
  pixel = vec4(noise.x.uint8, noise.y.uint8, 0, 0)

    
let texture1 = createEmptyTexture2D(vec2i(textureSize), GL_RGBA8)
texture1.setData(pixels)
texture1.generateMipmap

glDisable(GL_DEPTH_TEST)

var gameTimer = newStopWatch(true)

proc render(): void =
  glClear(GL_COLOR_BUFFER_BIT)
  
  shadingDsl(GL_TRIANGLES):
    debugResult
    numVertices = 3

    uniforms:
      tex = texture1
      time = gameTimer.time.float32

    fragmentMain:
      """
      vec2 tex = texture(tex, texCoord).rg;
      if(int(time) % 2  == 1)
        color = vec4(tex.r);
      else
        color = vec4(tex.g);
      //color = vec4(gl_FragCoord.x < gl_FragCoord.y);
      """

  glSwapWindow(window)
  

var runGame   = true
  
while runGame:

  var evt = defaultEvent
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    #if evt.kind == WindowEvent:
    #  #if evt.window.event == WindowEvent_Resized:
    #  #  windowsize.x = evt.window.data1.float32
    #  #  windowsize.y = evt.window.data2.float32
    #  #  etViewportAndProjection()
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SDL_SCANCODE_ESCAPE:
        runGame = false
      of SDL_SCANCODE_PAUSE:
        gameTimer.toggle
      of SDL_SCANCODE_F10:
        window.screenshot("noise")
      else:
        discard
       
  render()

  
  







