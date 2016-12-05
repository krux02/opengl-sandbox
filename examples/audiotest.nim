import sdl2, sdl2/audio, ../fancygl, fftw3

###############################################################################
# WARNING thistest is incomplete and has not yet been updatad for a long time #
###############################################################################

discard sdl2.init(INIT_EVERYTHING)

var wav_spec: AudioSpec
var wav_length: uint32
var wav_buffer: ptr uint8

if loadWAV("song.wav", wav_spec.addr, wav_buffer.addr, wav_length.addr) == nil:
  stderr.write "Could not open test.wav: ", sdl2.getError(), "\n"

echo wav_spec

echo cast[int](wav_buffer.pointer)
echo wav_buffer[]

var puffer = dataView[int16](wav_buffer.pointer, int(wav_length div 2))

echo puffer.len
# echo puffer.data

proc `*`(str: string, i: int): string =
  result = ""
  for _ in 1 .. i:
    result.add str


var
  streamA = newSeq[float64](puffer.len)
  streamB = newSeq[float64](puffer.len)

for i in 0 ..< (puffer.len div 2):
  let
    x = puffer[i*2].float64
    y = puffer[i*2+1].float64
    z = float64(high(int16))

  streamA[i] = x / z
  streamB[i] = y / z
  # echo " "*int(30+streamA[i]*60), "x"

echo wav_spec

const N = 44100 div 60
var input : array[N, float64]
var output : array[2*N, float64]

let plan = fftw_plan_dft_r2c_1d(N, input[0].addr,
  cast[ptr fftw_complex](output[0].addr), FFTW_ESTIMATE)

const WIDTH = 640
const HEIGHT = 480

var
  window = sdl2.createWindow("title", 0, 0, WIDTH, HEIGHT, 0)
  renderer = sdl2.createRenderer(window, -1, sdl2.RENDERER_PRESENTVSYNC)

for offset in 0 .. (wav_length div N):
  renderer.clear()
  for i in 0 ..< N:
    input[i] = streamA[int(offset)*N + i]

  fftw_execute(plan)

  for i in 0..< N:
      let x = output[i] * WIDTH / 12 + WIDTH / 2
      let y = output[i+1] * HEIGHT / 12 + HEIGHT / 2
      sdl2.drawPoint(renderer, cint(x), cint(y))

  sdl2.present(renderer)



# block b:
#   while true:
#     var event:sdl2.Event
#     while sdl2.pollEvent(event):
#       if event.kind == sdl2.QuitEvent:
#         break b
