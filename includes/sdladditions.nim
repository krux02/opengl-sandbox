# sdl additions
import sdl2, glm

proc size*(window: WindowPtr): Vec2i =
  var x,y: cint
  getSize(window, x, y)
  Vec2i(arr: [x.int32, y.int32])

proc `size=`*(window: WindowPtr; size: Vec2i): void =
  setSize(window, size.x, size.y)

proc position*(window: WindowPtr): Vec2i =
  var x,y: cint
  getPosition(window, x, y)
  Vec2i(arr: [x.int32, y.int32])

proc `position=`*(window: WindowPtr; pos: Vec2i): void =
  setPosition(window, pos.x, pos.y)


proc title*(window: WindowPtr): string =
  result = $getTitle(window)

proc `title=`*(window: WindowPtr; title: string): void =
  setTitle(window, title)

proc size*(surface: SurfacePtr): Vec2i =
  vec2i(surface.w, surface.h)

proc rel*(evt: MouseMotionEventObj): Vec2i =
  result.x = evt.xrel
  result.y = evt.yrel

proc pos*(evt: MouseMotionEventObj): Vec2i =
  result.x = evt.x
  result.y = evt.y

proc pos*(evt: MouseButtonEventObj): Vec2i =
  result.x = evt.x
  result.y = evt.y

proc pos*(evt: MouseWheelEventObj): Vec2i =
  result.x = evt.x
  result.y = evt.y

proc rel*(evt: MouseMotionEventPtr): Vec2i =
  result.x = evt.xrel
  result.y = evt.yrel

proc pos*(evt: MouseMotionEventPtr): Vec2i =
  result.x = evt.x
  result.y = evt.y

proc pos*(evt: MouseButtonEventPtr): Vec2i =
  result.x = evt.x
  result.y = evt.y

proc pos*(evt: MouseWheelEventPtr): Vec2i =
  result.x = evt.x
  result.y = evt.y


proc flipY*(surface: SurfacePtr): void =
  assert(not surface.isNil, "surface may not be nil")
  # handy conversion, because in Opengl the origin of texture coordinates is bottom left and not top left

  let
    h = surface.h
    pitch = surface.pitch
    pixels = surface.pixels

  let mem = alloc(pitch)
  defer:
    dealloc(mem)

  for y in 0 ..< surface.h div 2:
    let p1 = cast[pointer](cast[uint](pixels) + uint(y * pitch))
    let p2 = cast[pointer](cast[uint](pixels) + uint((h-y-1) * pitch))

    mem.copyMem(p1, pitch)
    p1.copyMem(p2, pitch)
    p2.copyMem(mem, pitch)

proc screenshot*(window : sdl2.WindowPtr; basename : string) : bool {.discardable.} =
  var
    (w,h) = window.getSize
    data = newSeq[uint32](w * h)

  glReadPixels(0,0,w,h,GL_RGBA, GL_UNSIGNED_BYTE, data[0].addr)

  #for y in 0 .. < h div 2:
  #  for x in 0 .. < w:
  #    swap(data[y*w+x], data[(h-y-1)*w+x])

  let surface = createRGBSurfaceFrom(data[0].addr,w,h,32,w*4,
                                     0x0000ffu32,0x00ff00u32,0xff0000u32,0xff000000u32)

  surface.flipY

  if surface.isNil:
    echo "Could not create SDL_Surface from pixel data: ", sdl2.getError()
    return false

  defer: surface.freeSurface

  os.createDir "screenshots"

  var i = 0
  template filename : string = getAppDir() / "screenshots" / (basename & "_" & intToStr(i,4) & ".png")
  while os.fileExists(filename):
    i += 1
    if i >= 10000:
      echo "too many screenshots with the same base name: ", basename

  if surface.savePNG(filename) == 0:
    echo sdl2.getError()
    return false

  true

proc screenshot*(window : sdl2.WindowPtr) : bool {.discardable.} =
  window.screenshot(window.title)

proc newSurface(texture: Texture2D): SurfacePtr =
  var
    size = texture.size
    stride = size.x * 4
    data = newSeq[uint32](size.x * size.y)

  glGetTextureImage(texture.handle, 0, GL_RGBA, GL_UNSIGNED_BYTE, GLsizei(data.len * 4), pointer(data[0].addr))

  result =
    createRGBSurfaceFrom(data[0].addr, size.x, size.y, 32, stride,
              0x0000ffu32,0x00ff00u32,0xff0000u32,0xff000000u32)

  if result.isNil:
    panic("Could not create SDL_Surface from pixel data: ", sdl2.getError())

proc saveBMP*(texture: Texture2D; filename: string): void =
  let surface = newSurface(texture)
  defer: surface.freeSurface
  surface.saveBMP(filename)

proc savePNG*(texture: Texture2D; filename: string): void =
  let surface = newSurface(texture)
  defer: surface.freeSurface
  discard surface.savePNG(filename)
