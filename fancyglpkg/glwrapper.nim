# included from fancygl.nim

type
  UncheckedArray {.unchecked.} [t] = array[0,t]

  DataView*[T] = object
    data: ptr UncheckedArray[T]
    size: int

  ReadView*[T] = object
    data: ptr UncheckedArray[T]
    size: int

  WriteView*[T] = object
    data: ptr UncheckedArray[T]
    size: int

proc isNil[T](view: DataView[T] | ReadView[T] | WriteView[T]): bool {.inline.} =
  view.data.isNil

proc dataView*[T](data: pointer, size: int) : DataView[T] {.inline.} =
  DataView[T](data: cast[ptr UncheckedArray[T]](data), size: size)

proc len*(mb : ReadView | WriteView | DataView) : int {.inline.} =
  mb.size

proc `[]`*[T](mb : ReadView[T], index: int) : T {.inline.} =
  mb.data[index]

proc `[]=`*[T](mb : WriteView[T], index: int, val: T) : void {.inline.} =
  mb.data[index] = val

proc `[]`*[T](mb : DataView[T]; index: int) : var T {.inline.} =
  mb.data[index]

proc `[]=`*[T](mb : DataView[T], index: int, val: T) : void {.inline.} =
  mb.data[index] = val

iterator items*[T](view: ReadView[T]) : T {.inline.} =
  let p = view.data
  var i = 0
  while i < view.size:
    yield p[i]
    inc(i)

iterator pairs*[T](view: ReadView[T]): tuple[key: int, val: T] {.inline.} =
  let p = view.data
  var i = 0
  while i < view.size:
    yield (i, p[i])
    inc(i)

iterator items*[T](view: DataView[T]) : T {.inline.} =
  let p = view.data
  var i = 0
  while i < view.size:
    yield p[i]
    inc(i)

iterator pairs*[T](view: DataView[T]): tuple[key: int, val: T] {.inline.} =
  let p = view.data
  var i = 0
  while i < view.size:
    yield (i, p[i])
    inc(i)

iterator mitems*[T](view: DataView[T]) : var T {.inline.} =
  let p = view.data
  var i = 0
  while i < view.size:
    yield p[i]
    inc(i)

iterator mpairs*[T](view: DataView[T]): tuple[key: int, val: var T] {.inline.} =
  let p = view.data
  var i = 0
  while i < view.size:
    yield (i, p[i])
    inc(i)

iterator mitems*[T](wv: WriteView[T]) : var T {.inline.} =
  let p = wv.data
  var i = 0
  while i < wv.size:
    yield p[i]
    inc(i)

iterator mpairs*[T](wv: WriteView[T]): tuple[key: int, val: var T] {.inline.} =
  let p = wv.data
  var i = 0
  while i < wv.size:
    yield (i, p[i])
    inc(i)


proc take*[T](view: DataView[T], num: int) : DataView[T] {.inline.} =
  result.data = view.data
  result.size = max(min(num, view.size), 0)

#### program type ####

type
  Program* = object
    handle*: GLuint

  Shader*  = object
    handle*: GLuint

  Location* = object
    ## Location for a uniform or attribute from a shader program.
    ## Can be -1 for uniforms/attributes that are optimized out
    index*: GLint

  Binding* = object
    ## index of a buffer attached to a vertex array objact
    index*: GLuint

proc isNil*(program: Program): bool =
  program.handle == 0

proc isNil*(shader: Shader): bool =
  shader.handle == 0

proc isValid*(location: Location): bool =
  location.index >= 0

#### Uniform ####

## Matrix types

# float32

proc uniform*(program: Program; location: Location, mat: Mat4f) =
  glProgramUniformMatrix4fv(program.handle, location.index, 1, false, cast[ptr float32](mat.unsafeAddr))

proc uniform*(program: Program; location: Location, mat: Mat3f) =
  glProgramUniformMatrix3fv(program.handle, location.index, 1, false, cast[ptr float32](mat.unsafeAddr))

proc uniform*(program: Program; location: Location, mat: Mat2f) =
  glProgramUniformMatrix2fv(program.handle, location.index, 1, false, cast[ptr float32](mat.unsafeAddr))

when defined(Mat2x3f):
  proc uniform*(program: Program; location: Location, mat: Mat2x3f) =
    glProgramUniformMatrix2x3fv(program.handle, location.index, 1, false, cast[ptr float32](mat.unsafeAddr))

when defined(Mat3x2f):
  proc uniform*(program: Program; location: Location, mat: Mat3x2f) =
    glProgramUniformMatrix3x2fv(program.handle, location.index, 1, false, cast[ptr float32](mat.unsafeAddr))

when defined(Mat2x4f):
  proc uniform*(program: Program; location: Location, mat: Mat2x4f) =
    glProgramUniformMatrix2x4fv(program.handle, location.index, 1, false, cast[ptr float32](mat.unsafeAddr))

when defined(Mat4x2f):
  proc uniform*(program: Program; location: Location, mat: Mat4x2f) =
    glProgramUniformMatrix4x2fv(program.handle, location.index, 1, false, cast[ptr float32](mat.unsafeAddr))

when defined(Mat3x4f):
  proc uniform*(program: Program; location: Location, mat: Mat3x4f) =
    glProgramUniformMatrix3x4fv(program.handle, location.index, 1, false, cast[ptr float32](mat.unsafeAddr))

when defined(Mat4x3f):
  proc uniform*(program: Program; location: Location, mat: Mat4x3f) =
    glProgramUniformMatrix4x3fv(program.handle, location.index, 1, false, cast[ptr float32](mat.unsafeAddr))

# float64

proc uniform*(program: Program; location: Location; mat: Mat4d) =
  glProgramUniformMatrix4dv(program.handle, location.index, 1, false, cast[ptr float64](mat.unsafeAddr))

proc uniform*(program: Program; location: Location, mat: Mat3d) =
  glProgramUniformMatrix3dv(program.handle, location.index, 1, false, cast[ptr float64](mat.unsafeAddr))

proc uniform*(program: Program; location: Location, mat: Mat2d) =
  glProgramUniformMatrix2dv(program.handle, location.index, 1, false, cast[ptr float64](mat.unsafeAddr))

when defined(Mat2x3d):
  proc uniform*(program: Program; location: Location, mat: Mat2x3d) =
    glProgramUniformMatrix2x3dv(program.handle, location.index, 1, false, cast[ptr float64](mat.unsafeAddr))

when defined(Mat3x2d):
  proc uniform*(program: Program; location: Location, mat: Mat3x2d) =
    glProgramUniformMatrix3x2dv(program.handle, location.index, 1, false, cast[ptr float64](mat.unsafeAddr))

when defined(Mat2x4d):
  proc uniform*(program: Program; location: Location, mat: Mat2x4d) =
    glProgramUniformMatrix2x4dv(program.handle, location.index, 1, false, cast[ptr float64](mat.unsafeAddr))

when defined(Mat4x2d):
  proc uniform*(program: Program; location: Location, mat: Mat4x2d) =
    glProgramUniformMatrix4x2dv(program.handle, location.index, 1, false, cast[ptr float64](mat.unsafeAddr))

when defined(Mat3x4d):
  proc uniform*(program: Program; location: Location, mat: Mat3x4d) =
    glProgramUniformMatrix3x4dv(program.handle, location.index, 1, false, cast[ptr float64](mat.unsafeAddr))

when defined(Mat4x3d):
  proc uniform*(program: Program; location: Location, mat: Mat4x3d) =
    glProgramUniformMatrix4x3dv(program.handle, location.index, 1, false, cast[ptr float64](mat.unsafeAddr))


#[
# uint32

proc uniform(program: Program; location: Location, mat: Mat4ui) =
  glProgramUniformMatrix4uiv(program.handle, location.index, 1, false, cast[ptr uint32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat3ui) =
  glProgramUniformMatrix3uiv(program.handle, location.index, 1, false, cast[ptr uint32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat2ui) =
  glProgramUniformMatrix2uiv(program.handle, location.index, 1, false, cast[ptr uint32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat2x3ui) =
  glProgramUniformMatrix2x3uiv(program.handle, location.index, 1, false, cast[ptr uint32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat3x2ui) =
  glProgramUniformMatrix3x2uiv(program.handle, location.index, 1, false, cast[ptr uint32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat2x4ui) =
  glProgramUniformMatrix2x4uiv(program.handle, location.index, 1, false, cast[ptr uint32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat4x2ui) =
  glProgramUniformMatrix4x2uiv(program.handle, location.index, 1, false, cast[ptr uint32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat3x4ui) =
  glProgramUniformMatrix3x4uiv(program.handle, location.index, 1, false, cast[ptr uint32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat4x3ui) =
  glProgramUniformMatrix4x3uiv(program.handle, location.index, 1, false, cast[ptr uint32](mat.unsafeAddr))

# int32

proc uniform(program: Program; location: Location, mat: Mat4i) =
  glProgramUniformMatrix4iv(program.handle, location.index, 1, false, cast[ptr int32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat3i) =
  glProgramUniformMatrix3iv(program.handle, location.index, 1, false, cast[ptr int32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat2i) =
  glProgramUniformMatrix2iv(program.handle, location.index, 1, false, cast[ptr int32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat2x3i) =
  glProgramUniformMatrix2x3iv(program.handle, location.index, 1, false, cast[ptr int32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat3x2i) =
  glProgramUniformMatrix3x2iv(program.handle, location.index, 1, false, cast[ptr int32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat2x4i) =
  glProgramUniformMatrix2x4iv(program.handle, location.index, 1, false, cast[ptr int32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat4x2i) =
  glProgramUniformMatrix4x2iv(program.handle, location.index, 1, false, cast[ptr int32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat3x4i) =
  glProgramUniformMatrix3x4iv(program.handle, location.index, 1, false, cast[ptr int32](mat.unsafeAddr))

proc uniform(program: Program; location: Location, mat: Mat4x3i) =
  glProgramUniformMatrix4x3iv(program.handle, location.index, 1, false, cast[ptr int32](mat.unsafeAddr))

]#

# Vector types

proc uniform*(program: Program; location: Location, value: Vec2f) =
  glProgramUniform2f(program.handle, location.index, value[0], value[1])

proc uniform*(program: Program; location: Location, value: Vec3f) =
  glProgramUniform3f(program.handle, location.index, value[0], value[1], value[2])

proc uniform*(program: Program; location: Location, value: Vec4f) =
  glProgramUniform4f(program.handle, location.index, value[0], value[1], value[2], value[3])


proc uniform*(program: Program; location: Location, value: Vec2d) =
  glProgramUniform2d(program.handle, location.index, value[0], value[1])

proc uniform*(program: Program; location: Location, value: Vec3d) =
  glProgramUniform3d(program.handle, location.index, value[0], value[1], value[2])

proc uniform*(program: Program; location: Location, value: Vec4d) =
  glProgramUniform4d(program.handle, location.index, value[0], value[1], value[2], value[3])


proc uniform*(program: Program; location: Location, value: Vec2i) =
  glProgramUniform2i(program.handle, location.index, value[0], value[1])

proc uniform*(program: Program; location: Location, value: Vec3i) =
  glProgramUniform3i(program.handle, location.index, value[0], value[1], value[2])

proc uniform*(program: Program; location: Location, value: Vec4i) =
  glProgramUniform4i(program.handle, location.index, value[0], value[1], value[2], value[3])


when defined(Vec2ui):
  proc uniform*(program: Program; location: Location, value: Vec2ui) =
    glProgramUniform2ui(program.handle, location.index, value[0], value[1])

when defined(Vec3ui):
  proc uniform*(program: Program; location: Location, value: Vec3ui) =
    glProgramUniform3ui(program.handle, location.index, value[0], value[1], value[2])

when defined(Vec4ui):
  proc uniform*(program: Program; location: Location, value: Vec4ui) =
    glProgramUniform4ui(program.handle, location.index, value[0], value[1], value[2], value[3])


proc uniform*(program: Program; location: Location, value: Vec2b) =
  glProgramUniform2i(program.handle, location.index, value[0].GLint, value[1].GLint)

proc uniform*(program: Program; location: Location, value: Vec3b) =
  glProgramUniform3i(program.handle, location.index, value[0].GLint, value[1].GLint, value[2].GLint)

proc uniform*(program: Program; location: Location, value: Vec4b) =
  glProgramUniform4i(program.handle, location.index, value[0].GLint, value[1].GLint, value[2].GLint, value[3].GLint)

# scalar types

proc uniform*(program: Program; location: Location, value: float32) =
  glProgramUniform1f(program.handle, location.index, value)

proc uniform*(program: Program; location: Location, value: float64) =
  glProgramUniform1d(program.handle, location.index, value)

proc uniform*(program: Program; location: Location, value: int32) =
  glProgramUniform1i(program.handle, location.index, value)

proc uniform*(program: Program; location: Location, value: bool) =
  glProgramUniform1i(program.handle, location.index, value.GLint)

proc label*(arg: Program): string =
  const bufsize = 255
  result = newString(bufsize)
  var length: GLsizei
  glGetObjectLabel(GL_PROGRAM, arg.handle, bufsize, length.addr, result[0].addr)
  result.setLen(length)

proc `label=`*(arg: Program; label: string): void =
    ## does nothing when label is nil (allows nil checks on other places)
    if not isNil label:
      glObjectLabel(GL_PROGRAM, arg.handle, GLsizei(label.len), label[0].unsafeAddr)

proc label*(arg: Shader): string =
  const bufsize = 255
  result = newString(bufsize)
  var length: GLsizei
  glGetObjectLabel(GL_SHADER, arg.handle, bufsize, length.addr, result[0].addr)
  result.setLen(length)

proc `label=`*(arg: Shader; label: string): void =
    ## does nothing when label is nil (allows nil checks on other places)
    if not isNil label:
      glObjectLabel(GL_SHADER, arg.handle, GLsizei(label.len), label[0].unsafeAddr)

#### Vertex Array Object ####

type VertexArrayObject* = object
    handle*: GLuint

proc label*(arg: VertexArrayObject): string =
  const bufsize = 255
  result = newString(bufsize)
  var length: GLsizei
  glGetObjectLabel(GL_PROGRAM, arg.handle, bufsize, length.addr, result[0].addr)
  result.setLen(length)

proc `label=`*(arg: VertexArrayObject; label: string): void =
    ## does nothing when label is nil (allows nil checks on other places)
    if not isNil label:
      glObjectLabel(GL_PROGRAM, arg.handle, GLsizei(label.len), label[0].unsafeAddr)

proc newVertexArrayObject*(label: string = nil) : VertexArrayObject =
  glCreateVertexArrays(1, result.handle.addr)
  result.label = label

proc bindIt*(vao: VertexArrayObject): void =
  glBindVertexArray(vao.handle)

proc delete*(vao: VertexArrayObject) =
  glDeleteVertexArrays(1, vao.handle.unsafeAddr)

proc divisor*(vao: VertexArrayObject; binding: Binding; divisor: GLuint) : void =
  glVertexArrayBindingDivisor(vao.handle, binding.index, divisor)

proc enableAttrib*(vao: VertexArrayObject, location: Location) : void =
  if location.index >= 0:
    glEnableVertexArrayAttrib(vao.handle, location.index.GLuint)

template blockBind*(vao: VertexArrayObject, blk: untyped) : untyped =
  vao.bindIt
  blk
  nil_vao.bindIt

################################################################################
################################ Array  Buffers ################################
################################################################################

type
  ArrayBuffer*[T]        = object
    handle*: GLuint
  ElementArrayBuffer*[T] = object
    handle*: GLuint
  UniformBuffer*[T]      = object
    handle*: GLuint

type SeqLikeBuffer[T] = ArrayBuffer[T] | ElementArrayBuffer[T]
type AnyBuffer[T] = ArrayBuffer[T] | ElementArrayBuffer[T] | UniformBuffer[T]


proc new*[T](arrayBuffer: var ArrayBuffer[T] ) : void =
  glCreateBuffers(1, arrayBuffer.handle.addr)

proc new*[T](arrayBuffer: var ElementArrayBuffer[T] ) : void =
  glCreatebuffers(1, arrayBuffer.handle.addr)

proc new*[T](arrayBuffer: var UniformBuffer[T] ) : void =
  glCreateBuffers(1, arrayBuffer.handle.addr)

proc bindIt*(vao: VertexArrayObject; indices: ElementArrayBuffer): void =
  glVertexArrayElementBuffer(vao.handle, indices.handle)

proc newArrayBuffer*[T](length: int, usage: GLenum = GL_STATIC_DRAW, label: string = nil): ArrayBuffer[T] =
  result.new
  glNamedBufferData(result.handle, length * GLsizeiptr(sizeof(T)), nil, usage)
  if not label.isNil:
    result.label = label

proc newElementArrayBuffer*[T](length: int, usage: GLenum = GL_STATIC_DRAW): ElementArrayBuffer[T] =
  result.new
  glNamedBufferData(result.handle, length * GLsizeiptr(sizeof(T)), nil, usage)

proc newUniformBuffer*[T](usage: GLenum = GL_STATIC_DRAW): UniformBuffer[T] =
  result.new
  glNamedBufferData(result.handle, GLsizeiptr(sizeof(T)), nil, usage)

proc glGetInteger(name: GLenum): GLint =
  glGetIntegerv(name, result.addr)

proc currentArrayBuffer*[T](): ArrayBuffer[T] {. deprecated .} =
  result.handle = GLuint(glGetInteger(GL_ARRAY_BUFFER_BINDING))

proc currentElementArrayBuffer*[T](): ElementArrayBuffer[T] {. deprecated .} =
  result.handle = GLuint(glGetInteger(GL_ELEMENT_ARRAY_BUFFER_BINDING))

proc currentUniformBuffer*[T](): UniformBuffer[T] {. deprecated .} =
  result.handle = GLuint(glGetInteger(GL_UNIFORM_BUFFER_BINDING))

proc label*(arg: AnyBuffer): string =
  const bufsize = 255
  result = newString(bufsize)
  var length: GLsizei
  glGetObjectLabel(GL_BUFFER, arg.handle, bufsize, length.addr, result[0].addr)
  result.setLen(length)

proc `label=`*(arg: AnyBuffer; label: string): void =
    ## does nothing when label is nil (allows nil checks on other places)
    if not isNil label:
      glObjectLabel(GL_BUFFER, arg.handle, GLsizei(label.len), label[0].unsafeAddr)

proc bindingKind*[T](buffer: ArrayBuffer[T]) : GLenum {. inline .} =
  GL_ARRAY_BUFFER_BINDING

proc bindingKind*[T](buffer: ElementArrayBuffer[T]) : GLenum {. inline .} =
  GL_ELEMENT_ARRAY_BUFFER_BINDING

proc bindingKind*[T](buffer: UniformBuffer[T]) : GLenum {. inline .} =
  GL_UNIFORM_BUFFER_BINDING

proc bufferKind*[T](buffer: ArrayBuffer[T]) : GLenum {. inline .} =
  GL_ARRAY_BUFFER

proc bufferKind*[T](buffer: ElementArrayBuffer[T]) : GLenum {. inline .} =
  GL_ELEMENT_ARRAY_BUFFER

proc bufferKind*[T](buffer: UniformBuffer[T]) : GLenum {. inline .} =
  GL_UNIFORM_BUFFER

proc bindIt*[T](buffer: AnyBuffer[T]) =
  glBindBuffer(buffer.bufferKind, buffer.handle)

template blockBind*[T](buffer : AnyBuffer[T], blk:untyped) =
  let buf = buffer
  var outer : GLint
  glGetIntegerv(buf.bindingKind, outer.addr)
  buf.bindIt
  blk
  glBindBuffer(buf.bufferKind, GLuint(outer))

proc bufferData*[T](buffer: SeqLikeBuffer[T], data: openarray[T], usage: GLenum) =
  if buffer.handle.int > 0:
    glNamedBufferData(buffer.handle, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)

proc bufferData*[T](buffer: SeqLikeBuffer[T], dataview: DataView[T], usage: GLenum) =
  if buffer.handle.int > 0:
    glNamedBufferData( buffer.handle, GLsizeiptr(dataview.len * sizeof(T)), dataview.data, usage)

proc bufferData*[T](buffer: UniformBuffer[T], data: T, usage: GLenum) =
  if buffer.handle.int > 0:
    glNamedBufferData(buffer.handle, GLsizeiptr(sizeof(T)), unsafeAddr(data), usage)

proc setData*[T](buffer: SeqLikeBuffer[T], data: openarray[T]) =
  if buffer.handle.int > 0:
    glNamedBufferSubData(buffer.handle, 0, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]))

proc setData*[T](buffer: UniformBuffer[T], data: T) =
  if buffer.handle.int > 0:
    glNamedBufferSubData(buffer.handle, 0, GLsizeiptr(sizeof(T)), unsafeAddr(data))


proc len*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]) : int =
  var size: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_SIZE, size.addr)
  return size.int div sizeof(T).int

proc access*[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_ACCESS, tmp.addr)
  return tmp.GLenum

proc accessFlags*[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_ACCESS_FLAGS, tmp.addr)
  return tmp.GLenum

proc immutableStorage*[T](buffer: ArrayBuffer[T]) : bool =
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_IMMUTABLE_STORAGE, tmp.addr)
  return tmp != GL_FALSE

proc mapped*[T](buffer: ArrayBuffer[T]) : bool =
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_MAPPED, tmp.addr)

  return tmp != GL_FALSE

proc mapLength*[T](buffer: ArrayBuffer[T]) : int =
  var tmp: pointer
  glGetNamedBufferPointerv(buffer.handle, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc mapOffset*[T](buffer: ArrayBuffer[T]) : int =
  var tmp: pointer
  glGetNamedBufferPointerv(buffer.handle, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc byteSize*[T](buffer: ArrayBuffer[T]) : int =
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_SIZE, tmp.addr)
  return int(tmp)

proc storageFlags*[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_STORAGE_FLAGS, tmp.addr)
  return tmp.GLenum

proc usage*[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_USAGE, tmp.addr)
  return tmp.GLenum

iterator items*[T](buffer: SeqLikeBuffer[T]) : T =
  let mappedBuffer = buffer.mapRead
  defer:
    discard buffer.unmap
  for item in mappedBuffer.items:
    yield item

iterator pairs*[T](buffer: SeqLikeBuffer[T]) : tuple[key: int, val: T] =
  let mappedBuffer = buffer.mapRead
  defer:
    discard buffer.unmap
  for i, item in mappedBuffer.pairs:
    yield (i, item)

iterator mitems*[T](buffer: SeqLikeBuffer[T]) : var T =
  let mappedBuffer = buffer.mapReadWrite
  defer:
    discard buffer.unmap
  for item in mappedBuffer.mitems:
    yield item

iterator mpairs*[T](buffer: SeqLikeBuffer[T]) : tuple[key: int, val: var T] =
  let mappedBuffer = buffer.mapReadWrite
  defer:
    discard buffer.unmap
  for i, item in mappedBuffer.mpairs:
    yield (i, item)

iterator witems*[T](buffer: SeqLikeBuffer[T]) : var T =
  let mappedBuffer = buffer.mapWrite
  defer:
    discard buffer.unmap
  for item in mappedBuffer.mitems:
    yield item

iterator wpairs*[T](buffer: SeqLikeBuffer[T]) : tuple[key: int, val: var T] =
  let mappedBuffer = buffer.mapWrite
  defer:
    discard buffer.unmap
  for i, item in mappedBuffer.mpairs:
    yield (i, item)

proc unmap*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): bool =
  glUnmapNamedBuffer(buffer.handle) != GL_FALSE.GLboolean

proc mapRead*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): ReadView[T] =
  result.data = cast[ptr UncheckedArray[T]](glMapNamedBuffer(buffer.handle, GL_READ_ONLY))
  result.size = buffer.len

proc mapWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): WriteView[T] =
  result.data = cast[ptr UncheckedArray[T]](glMapNamedBuffer(buffer.handle, GL_WRITE_ONLY))
  result.size = buffer.len

proc mapReadWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): DataView[T] =
  result.data = cast[ptr UncheckedArray[T]](glMapNamedBuffer(buffer.handle, GL_READ_WRITE))
  result.size = buffer.len

macro mapReadBlock*(buffer: ArrayBuffer, blck: untyped) : untyped =
  buffer.expectKind nnkSym
  let ident = ident(buffer.repr)
  result = quote do:
    block:
      let `ident` {. inject .} = `buffer`.mapRead
      defer:
        discard `buffer`.unmap
      `blck`

macro mapWriteBlock*(buffer: ArrayBuffer, blck: untyped) : untyped =
  ## remember when you fill a buffer per frame, then you should put
  ## the usage hint of the buffer to GL_STEAM_DRAW
  buffer.expectKind nnkSym
  let ident = ident(buffer.repr)
  result = quote do:
    block:
      let `ident` {. inject .} = `buffer`.mapWrite
      defer:
        discard `buffer`.unmap
      `blck`

macro mapReadWriteBlock*(buffer: ArrayBuffer, blck: untyped) : untyped =
  buffer.expectKind nnkSym
  let ident = ident(buffer.repr)
  result = quote do:
    block:
      let `ident` {. inject .} = `buffer`.mapReadWrite
      defer:
        discard `buffer`.unmap
      `blck`

proc arrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW, label: string = nil): ArrayBuffer[T] =
  result.new
  result.bufferData(data, usage)
  result.label = label

proc arrayBuffer*[T](data : DataView[T], usage: GLenum = GL_STATIC_DRAW, label: string = nil): ArrayBuffer[T] =
  if not data.isNil:
    result.new
    result.bufferData(data, usage)
    result.label = label

proc elementArrayBuffer*[T](data : openarray[T]; usage: GLenum = GL_STATIC_DRAW; label: string = nil): ElementArrayBuffer[T] =
  result.new
  result.bufferData(data, usage)
  result.label = label

proc elementArrayBuffer*[T](data : DataView[T], usage: GLenum = GL_STATIC_DRAW, label: string = nil): ElementArrayBuffer[T] =
  if not data.isNil:
    result.new
    result.bufferData(data, usage)
    result.label = label

proc uniformBuffer*[T](data : T, usage: GLenum = GL_STATIC_DRAW, label: string = nil): UniformBuffer[T] =
  result.new
  result.bufferData(data, usage)
  result.label = label

################################################################################
################################ ArrayBufferView ###############################
################################################################################

type
  ArrayBufferView*[S,T] = object
    buffer*: ArrayBuffer[S]
    offset*: int
    stride*: int

proc len*(ab: ArrayBufferView):  int = ab.buffer.len
proc high*(ab: ArrayBufferView): int = ab.buffer.len - 1

template view*(buf: ArrayBuffer; member: untyped): untyped =
  var dummyVal : buf.T
  var res : ArrayBufferView[buf.T, dummyVal.member.type]
  res.buffer = buf
  res.offset = cast[int](dummyVal.member.addr) - cast[int](dummyVal.addr)
  res.stride = sizeof(buf.T)
  res

proc splitView*(buf: ArrayBuffer[Mat4f]): array[4, ArrayBufferView[Mat4f, Vec4f]] =
  for i in 0 .. 3:
    result[i].buffer = buf
    result[i].offset = i * 4 * sizeof(float32)
    result[i].stride = 4 * 4 * sizeof(float32)

when isMainModule:
  type TestType = object
    a,b,c,d: float32

  var testArrayBuffer = ArrayBuffer[TestType](handle : 7)

  assert testArrayBuffer.view(a) == ArrayBufferView[TestType,float32](buffer: ArrayBuffer[TestType](handle: 7), offset: 0, stride: 16)
  assert testArrayBuffer.view(b) == ArrayBufferView[TestType,float32](buffer: ArrayBuffer[TestType](handle: 7), offset: 4, stride: 16)
  assert testArrayBuffer.view(c) == ArrayBufferView[TestType,float32](buffer: ArrayBuffer[TestType](handle: 7), offset: 8, stride: 16)
  assert testArrayBuffer.view(d) == ArrayBufferView[TestType,float32](buffer: ArrayBuffer[TestType](handle: 7), offset:12, stride: 16)

#### shader

proc shaderSource(shader: Shader, source: string) =
  var source_array: array[1, cstring] = [cstring(source)]
  #var c_source_array = allocCStringArray(source_array)
  #defer: deallocCStringArray(c_source_array)
  glShaderSource(shader.handle, 1, cast[cstringArray](source_array.addr), nil)

proc compileStatus(shader: Shader): bool =
  var status: GLint
  glGetShaderiv(shader.handle, GL_COMPILE_STATUS, status.addr)
  status != 0

proc linkStatus(program: Program): bool =
  var status: GLint
  glGetProgramiv(program.handle, GL_LINK_STATUS, status.addr)
  status != 0

proc shaderInfoLog(shader: Shader): string =
  var length: GLint = 0
  glGetShaderiv(shader.handle, GL_INFO_LOG_LENGTH, length.addr)
  result = newString(length.int)
  glGetShaderInfoLog(shader.handle, length, nil, result)


# not needing nre might solve problems and speed up copilation
const parseErrors = not defined(noregexp)

when parseErrors:
  import nre
  proc showError(log: string, source: string): void =
    let lines = source.splitLines
    var problems = newSeq[tuple[lineNr: int, message: string]](0)
    # matches on intel driver
    for match in log.findIter(re"(\d+)\((\d+)\): ([^:]*): (.*)"):
      let lineNr = match.captures[0].parseInt
      #let notTheColumn = match.captures[1].parseInt
      #let kind: string = match.captures[2]
      let message: string = match.captures[3]
      problems.add( (lineNr, message) )
    # matches on nvidia driver
    for match in log.findIter(re"(\d+)\((\d+)\) : ([^:]*): (.*)"):
      let lineNr = match.captures[1].parseInt
      #let notTheColumn = match.captures[0].parseInt
      #let kind: string = match.captures[2]
      let message: string = match.captures[3]
      problems.add( (lineNr, message) )
    stdout.styledWriteLine(fgGreen, "==== start Shader Problems =======================================")
    for i, line in lines:
      let lineNr = i + 1
      stdout.styledWriteLine(fgYellow, intToStr(lineNr, 4), " ", resetStyle, line)
      for problem in problems:
        if problem.lineNr == lineNr:
          stdout.styledWriteLine("     ", fgRed, problem.message)
    stdout.styledWriteLine(fgGreen, "------------------------------------------------------------------")
    stdout.styledWriteLine(fgRed, log)
    stdout.styledWriteLine(fgGreen, "==== end Shader Problems =========================================")
else:
  proc showError(log: string; source: string): void =
    stdout.styledWriteLine(fgGreen, "==== start Shader Problems =======================================")
    let lines = source.splitLines
    for i, line in lines:
      let lineNr = i + 1
      stdout.styledWriteLine(fgYellow, intToStr(lineNr, 4), " ", resetStyle, line)
    stdout.styledWriteLine(fgGreen, "------------------------------------------------------------------")
    stdout.styledWriteLine(fgRed, log)
    stdout.styledWriteLine(fgGreen, "==== end Shader Problems =========================================")


proc infoLog(program: Program): string =
  var length: GLint = 0
  glGetProgramiv(program.handle, GL_INFO_LOG_LENGTH, length.addr);
  result = newString(length.int)
  glGetProgramInfoLog(program.handle, length, nil, result);

proc compileShader*(shaderType: GLenum, source: string): Shader =
  result.handle = glCreateShader(shaderType)
  result.shaderSource(source)
  glCompileShader(result.handle)

  if not result.compileStatus:
    showError(result.shaderInfoLog, source)

proc attachAndDeleteShader*(program: Program; shader: Shader): void =
  glAttachShader(program.handle, shader.handle)
  glDeleteShader(shader.handle)

proc linkOrDelete*(program: Program): void =
  glLinkProgram(program.handle)
  if not program.linkStatus:
    echo "Log: ", program.infoLog
    glDeleteProgram(program.handle)

proc linkShader*(shaders: varargs[Shader]): Program =
  result.handle = glCreateProgram()

  for shader in shaders:
    glAttachShader(result.handle, shader.handle)
    glDeleteShader(shader.handle)
  glLinkProgram(result.handle)

  if not result.linkStatus:
    echo "Log: ", result.infoLog
    glDeleteProgram(result.handle)
    result.handle = 0

proc use*(program: Program): void =
  glUseProgram(program.handle)

proc uniformLocation*(program: Program, name: string) : Location =
  result.index = glGetUniformLocation(program.handle, name)

proc attributeLocation*(program: Program, name: string) : Location =
  result.index = glGetAttribLocation(program.handle, name)

proc transformFeedbackVaryings*(program: Program; varyings: openarray[string]; bufferMode: GLenum): void =
  const N = 128
  var arr : array[N, cstring]
  assert(varyings.len <= N, "too many transform feedback varyings")
  for i in 0 ..< varyings.len:
    arr[i] = varyings[i]
  glTransformFeedbackVaryings(program.handle, GLsizei(varyings.len), cast[cstringArray](arr.addr), bufferMode)

proc readPixel*(x,y: int) : Color =
  let
    w = 1.GLsizei
    h = 1.GLsizei
    f = GL_RGBA
    t = GL_UNSIGNED_BYTE
    d = result.addr.pointer
  glReadPixels(x.GLint,y.GLint,w,h,f,t,d)

proc readPixel*(pos: Vec2i) : Color = readPixel(pos.x.int, pos.y.int)

##########################
# # transform feedback # #
##########################

template transformFeedbackBlock(primitiveMode: GLenum; blk: untyped): untyped =
  block:
    glBeginTransformFeedback(primitiveMode)
    defer:
      glEndTransformFeedback()
    blk

type
  TransformFeedback*[T] = object
    handle*: GLuint

proc label*(arg: TransformFeedback): string =
  const bufsize = 255
  result = newString(bufsize)
  var length: GLsizei
  glGetObjectLabel(GL_TRANSFORM_FEEDBACK, arg.handle, bufsize, length.addr, result[0].addr)
  result.setLen(length)

proc `label=`*(arg: TransformFeedback; label: string): void =
    ## does nothing when label is nil (allows nil checks on other places)
    if not isNil label:
      glObjectLabel(GL_TRANSFORM_FEEDBACK, arg.handle, GLsizei(label.len), label[0].unsafeAddr)

macro typeName(t: typedesc): untyped =
  newLit($t.getTypeImpl[1])

template offsetof*(typ, field: untyped): int = (var dummy: typ; cast[system.int](addr(dummy.field)) - cast[system.int](addr(dummy)))

macro varyingNames*(self: TransformFeedback): untyped =
  ## returns array of string literals
  result = nnkBracket.newTree
  let typeImpl = self.getTypeInst[1].getTypeImpl
  typeImpl.expectKind(nnkObjectTy)
  for identDef in typeImpl[2]:
    for i in 0 ..< identDef.len-2:
      let sym = identDef[i]
      result.add newLit($sym)

macro varyingOffsets*(self: TransformFeedback): untyped =
  ## returns array of int literals
  result = nnkBracket.newTree

  let tpe = self.getTypeInst[1]
  echo tpe.treeRepr
  let typeImpl = tpe.getTypeImpl
  typeImpl.expectKind(nnkObjectTy)
  for identDef in typeImpl[2]:
    for i in 0 ..< identDef.len-2:
      #let sym = identDef[i]
      error "not implemented"
      # result.add getAst(offsetOf(tpe,sym))

template stride*[T](self: TransformFeedback[T]): int =
  sizeof(T)

proc newTransformFeedback*[T]() : TransformFeedback[T] =
  glCreateTransformFeedbacks(GLsizei(1), result.handle.addr)
  result.label = typeName(T)
  echo "label: ", result.label
  echo @(result.varyingNames)
  echo @(result.varyingOffsets)
  echo result.stride

#[
macro transformFeedbackOutSection(self: TransformFeedback): string =
  var res = newLit("""
#extension GL_ARB_enhanced_layouts : enable
layout(xfb_buffer = 0, xfb_stride = 36) out bananas {
""")

  let tpe = self.getTypeInst[1]
  let typeImpl = tpe.getTypeImpl
  typeImpl.expectKind(nnkObjectTy)
  for identDef in typeImpl[2]:
    for i in 0 ..< identDef.len-2:
      let sym = identDef[i]
      let symName = newLit($sym)

      # glslTypeRepr
      res = head quote do:
        `res` & "layout(xfb_offset = " & $offsetOf(`tpe`, `sym`) & ") vec4 " & `symName` & ";"

  let name = self.repr
  result = res
]#

proc delete*(tf: TransformFeedback): void =
  glDeleteTransformFeedbacks(GLsizei(1), tf.handle.unsafeAddr)


proc bufferBase*(tf: TransformFeedback; index: int; buffer: ArrayBuffer): void =
  glTransformFeedbackBufferBase(tf.handle, GLuint(index), buffer.handle)

proc bufferRange*(tf: TransformFeedback; index: int; buffer: ArrayBuffer; offset: ptr int32; size: int): void =
  glTransformFeedbackBufferRange(tf.handle, GLuint(index), buffer.handle, offset, GLsizeiptr(size))

proc draw*(tf: TransformFeedback; primitiveMode: GLenum): void =
  glDrawTransformFeedback(primitiveMode, tf.handle)

proc bindIt*(tf: TransformFeedback): void =
  glBindTransformFeedback(GL_TRANSFORM_FEEDBACK, tf.handle)
