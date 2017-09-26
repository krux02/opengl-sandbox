# sdl additions
import glm

proc size*(window: Window): Vec2i =
  getWindowSize(window, result.x.addr, result.y.addr)

proc `size=`*(window: Window; size: Vec2i): void =
  setWindowSize(window, size.x, size.y)

proc position*(window: Window): Vec2i =
  getWindowPosition(window, result.x.addr, result.y.addr)

proc `position=`*(window: Window; pos: Vec2i): void =
  setWindowPosition(window, pos.x, pos.y)

proc title*(window: Window): string =
  result = $getWindowTitle(window)

proc `title=`*(window: Window; title: string): void =
  setWindowTitle(window, title)

proc size*(surface: Surface): Vec2i =
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


proc flipY*(surface: Surface): void =
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

    copyMem(mem, p1, pitch)
    copyMem(p1,  p2, pitch)
    copyMem(p2, mem, pitch)

proc screenshot*(window : sdl.Window; basename : string) : bool {.discardable.} =
  var
    size = window.size
    w = size.x
    h = size.y
    data = newSeq[uint32](size.x * size.y)

  glReadPixels(0,0,w,h,GL_RGBA, GL_UNSIGNED_BYTE, data[0].addr)
  let surface = createRGBSurfaceFrom(data[0].addr,w,h,32,w*4,
                                     0x0000ffu32,0x00ff00u32,0xff0000u32,0xff000000u32)

  surface.flipY

  if surface.isNil:
    echo "Could not create SDL_Surface from pixel data: ", sdl.getError()
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
    echo sdl.getError()
    return false

  true

proc screenshot*(window : sdl.Window) : bool {.discardable.} =
  window.screenshot(window.title)

proc newSurface(texture: Texture2D): Surface =
  var
    size = texture.size
    stride = size.x * 4
    data = newSeq[uint32](size.x * size.y)

  glGetTextureImage(texture.handle, 0, GL_RGBA, GL_UNSIGNED_BYTE, GLsizei(data.len * 4), pointer(data[0].addr))

  result =
    createRGBSurfaceFrom(data[0].addr, size.x, size.y, 32, stride,
              0x0000ffu32,0x00ff00u32,0xff0000u32,0xff000000u32)

  if result.isNil:
    panic("Could not create SDL_Surface from pixel data: ", sdl.getError())

proc saveBMP*(texture: Texture2D; filename: string): void =
  let surface = newSurface(texture)
  defer: surface.freeSurface
  discard surface.saveBMP(filename)

proc savePNG*(texture: Texture2D; filename: string): void =
  let surface = newSurface(texture)
  defer: surface.freeSurface
  discard surface.savePNG(filename)

iterator events*(): sdl.Event =
  var event: sdl.Event
  while pollEvent(event.addr) != 0:
    yield event
