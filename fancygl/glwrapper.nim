# included from fancygl.nim

type

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

proc `[]`*[T](mb : WriteView[T], index: int) : var T {.inline.} =
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

proc isValid*(location: Location): bool =
  location.index >= 0

#### Uniform ####

## Matrix types

template dataPtr[N,M,T](arg: Mat[N,M,T]): ptr T =
  cast[ptr T](arg.unsafeAddr)

# float32

proc uniform*(program: Program; location: Location, mat: Mat4f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniformMatrix4fv != nil:
    glProgramUniformMatrix4fv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix4fv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat3f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniformMatrix3fv != nil:
    glProgramUniformMatrix3fv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix3fv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat2f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniformMatrix2fv != nil:
    glProgramUniformMatrix2fv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix2fv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat2x3f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniformMatrix2x3fv != nil:
    glProgramUniformMatrix2x3fv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix2x3fv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat3x2f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniformMatrix3x2fv != nil:
    glProgramUniformMatrix3x2fv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix3x2fv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat2x4f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniformMatrix2x4fv != nil:
    glProgramUniformMatrix2x4fv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix2x4fv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat4x2f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniformMatrix4x2fv != nil:
    glProgramUniformMatrix4x2fv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix4x2fv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat3x4f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniformMatrix3x4fv != nil:
    glProgramUniformMatrix3x4fv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix3x4fv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat4x3f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniformMatrix4x3fv != nil:
    glProgramUniformMatrix4x3fv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix4x3fv(location.index, 1, false, mat.dataPtr)

# float64

proc uniform*(program: Program; location: Location; mat: Mat4d) =
  if glProgramUniformMatrix4dv != nil:
    glProgramUniformMatrix4dv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix4dv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat3d) =
  if glProgramUniformMatrix3dv != nil:
    glProgramUniformMatrix3dv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix3dv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat2d) =
  if glProgramUniformMatrix2dv != nil:
    glProgramUniformMatrix2dv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix2dv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat2x3d) =
  if glProgramUniformMatrix2x3dv != nil:
    glProgramUniformMatrix2x3dv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix2x3dv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat3x2d) =
  if glProgramUniformMatrix3x2dv != nil:
    glProgramUniformMatrix3x2dv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix3x2dv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat2x4d) =
  if glProgramUniformMatrix2x4dv != nil:
    glProgramUniformMatrix2x4dv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix2x4dv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat4x2d) =
  if glProgramUniformMatrix4x2dv != nil:
    glProgramUniformMatrix4x2dv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix4x2dv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat3x4d) =
  if glProgramUniformMatrix3x4dv != nil:
    glProgramUniformMatrix3x4dv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix3x4dv(location.index, 1, false, mat.dataPtr)

proc uniform*(program: Program; location: Location, mat: Mat4x3d) =
  if glProgramUniformMatrix4x3dv != nil:
    glProgramUniformMatrix4x3dv(program.handle, location.index, 1, false, mat.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniformMatrix4x3dv(location.index, 1, false, mat.dataPtr)

#[

# uint32

proc uniform(program: Program; location: Location, mat: Mat4ui) =
  glProgramUniformMatrix4uiv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat3ui) =
  glProgramUniformMatrix3uiv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat2ui) =
  glProgramUniformMatrix2uiv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat2x3ui) =
  glProgramUniformMatrix2x3uiv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat3x2ui) =
  glProgramUniformMatrix3x2uiv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat2x4ui) =
  glProgramUniformMatrix2x4uiv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat4x2ui) =
  glProgramUniformMatrix4x2uiv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat3x4ui) =
  glProgramUniformMatrix3x4uiv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat4x3ui) =
  glProgramUniformMatrix4x3uiv(program.handle, location.index, 1, false, mat.dataPtr)

# int32

proc uniform(program: Program; location: Location, mat: Mat4i) =
  glProgramUniformMatrix4iv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat3i) =
  glProgramUniformMatrix3iv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat2i) =
  glProgramUniformMatrix2iv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat2x3i) =
  glProgramUniformMatrix2x3iv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat3x2i) =
  glProgramUniformMatrix3x2iv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat2x4i) =
  glProgramUniformMatrix2x4iv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat4x2i) =
  glProgramUniformMatrix4x2iv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat3x4i) =
  glProgramUniformMatrix3x4iv(program.handle, location.index, 1, false, mat.dataPtr)

proc uniform(program: Program; location: Location, mat: Mat4x3i) =
  glProgramUniformMatrix4x3iv(program.handle, location.index, 1, false, mat.dataPtr)
]#

# Vector types

template dataPtr[N,T](arg: Vec[N,T]): ptr T =
  cast[ptr T](arg.unsafeAddr)

proc uniform*(program: Program; location: Location, value: Vec2f) =
  if glProgramUniform2fv != nil:
    glProgramUniform2fv(program.handle, location.index, 1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform2fv(location.index, 1, value.dataPtr)

proc uniform*(program: Program; location: Location, value: Vec3f) =
  if glProgramUniform3fv != nil:
    glProgramUniform3fv(program.handle, location.index, 1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform3fv(location.index, 1, value.dataPtr)

proc uniform*(program: Program; location: Location, value: Vec4f) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform4fv != nil:
    glProgramUniform4fv(program.handle, location.index, 1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform4fv(location.index, 1, value.dataPtr)

proc uniform*(program: Program; location: Location, value: Vec2d) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform2dv != nil:
    glProgramUniform2dv(program.handle, location.index, 1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform2dv(location.index, 1, value.dataPtr)

proc uniform*(program: Program; location: Location, value: Vec3d) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform3dv != nil:
    glProgramUniform3dv(program.handle, location.index, 1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform3dv(location.index, 1, value.dataPtr)

proc uniform*(program: Program; location: Location, value: Vec4d) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform4dv != nil:
    glProgramUniform4dv(program.handle, location.index, 1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform4dv(location.index, 1, value.dataPtr)


proc uniform*(program: Program; location: Location, value: Vec2i) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform2iv != nil:
    glProgramUniform2iv(program.handle, location.index, 1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform2iv(location.index, 1, value.dataPtr)

proc uniform*(program: Program; location: Location, value: Vec3i) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform3iv != nil:
    glProgramUniform3iv(program.handle, location.index,  1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform3iv(location.index,  1, value.dataPtr)

proc uniform*(program: Program; location: Location, value: Vec4i) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform4iv != nil:
    glProgramUniform4iv(program.handle, location.index,  1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform4iv(location.index,  1, value.dataPtr)


proc uniform*(program: Program; location: Location, value: Vec2ui) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform2uiv != nil:
    glProgramUniform2uiv(program.handle, location.index,  1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform2uiv(location.index,  1, value.dataPtr)

proc uniform*(program: Program; location: Location, value: Vec3ui) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform3uiv != nil:
    glProgramUniform3uiv(program.handle, location.index,  1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform3uiv(location.index,  1, value.dataPtr)

proc uniform*(program: Program; location: Location, value: Vec4ui) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform4uiv != nil:
    glProgramUniform4uiv(program.handle, location.index,  1, value.dataPtr)
  else:
    glUseProgram(program.handle)
    glUniform4uiv(location.index,  1, value.dataPtr)


proc uniform*(program: Program; location: Location, value: Vec2b) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform2i != nil:
    glProgramUniform2i(program.handle, location.index, value[0].GLint, value[1].GLint)
  else:
    glUseProgram(program.handle)
    glUniform2i(location.index, value[0].GLint, value[1].GLint)

proc uniform*(program: Program; location: Location, value: Vec3b) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform3i != nil:
    glProgramUniform3i(program.handle, location.index, value[0].GLint, value[1].GLint, value[2].GLint)
  else:
    glUseProgram(program.handle)
    glUniform3i(location.index, value[0].GLint, value[1].GLint, value[2].GLint)

proc uniform*(program: Program; location: Location, value: Vec4b) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform4i != nil:
    glProgramUniform4i(program.handle, location.index, value[0].GLint, value[1].GLint, value[2].GLint, value[3].GLint)
  else:
    glUseProgram(program.handle)
    glUniform4i(location.index, value[0].GLint, value[1].GLint, value[2].GLint, value[3].GLint)

# scalar types
proc uniform*(program: Program; location: Location, value: float32) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform1f != nil:
    glProgramUniform1f(program.handle, location.index, value)
  else:
    glUseProgram(program.handle)
    glUniform1f(location.index, value)

proc uniform*(program: Program; location: Location, value: float64) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform1d != nil:
    glProgramUniform1d(program.handle, location.index, value)
  else:
    glUseProgram(program.handle)
    glUniform1d(location.index, value)

proc uniform*(program: Program; location: Location, value: int32) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform1i != nil:
    glProgramUniform1i(program.handle, location.index, value)
  else:
    glUseProgram(program.handle)
    glUniform1i(location.index, value)

proc uniform*(program: Program; location: Location, value: bool) =
  ## Typed wrapper around `glProgramUniform`.
  if glProgramUniform1i != nil:
    glProgramUniform1i(program.handle, location.index, value.GLint)
  else:
    glUseProgram(program.handle)
    glUniform1i(location.index, value.GLint)

#### Vertex Array Object ####

type
  VertexArrayObject* = object
    handle*: GLuint

  VertexArrayObjectBinding* = object
    index*: GLuint


proc createVertexArrayObject*(label: string = "") : VertexArrayObject =
  ## Typed wrapper around `glCreateVertexArrays`.
  glCreateVertexArrays(1, result.handle.addr)
  if label.len > 0:
    result.label = label

proc delete*(vao: VertexArrayObject) =
  ## Typed wrapper around `glDeleteVertexArrays`.
  glDeleteVertexArrays(1, vao.handle.unsafeAddr)

proc divisor*(vao: VertexArrayObject; binding: VertexArrayObjectBinding; divisor: GLuint) : void =
  ## Typed wrapper around `glVertexArrayBindingDivisor`.
  glVertexArrayBindingDivisor(vao.handle, binding.index, divisor)

proc enableAttrib*(vao: VertexArrayObject, location: VertexArrayObjectBinding) : void =
  ## Typed wrapper around `glEnableVertexArrayAttrib`.
  glEnableVertexArrayAttrib(vao.handle, location.index)

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

template absoluteoffset*[T](arg: SeqLikeBuffer[T]): GLsizei = 0
template stride*[T](arg: SeqLikeBuffer[T]): GLsizei         = GLsizei(sizeof(T))

## TODO call this create

proc new*[T](arrayBuffer: var ArrayBuffer[T] ) : void =
  glCreateBuffers(1, arrayBuffer.handle.addr)

proc new*[T](arrayBuffer: var ElementArrayBuffer[T] ) : void =
  glCreatebuffers(1, arrayBuffer.handle.addr)

proc new*[T](arrayBuffer: var UniformBuffer[T] ) : void =
  glCreateBuffers(1, arrayBuffer.handle.addr)

proc createArrayBuffer*[T](length: int, usage: GLenum = GL_STATIC_DRAW, label: string = ""): ArrayBuffer[T] =
  result.new
  glNamedBufferData(result.handle, length * GLsizeiptr(sizeof(T)), nil, usage)
  #glNamedBufferStorage(result.handle, length * GLsizeiptr(sizeof(T)), nil, 0)
  if label.len > 0:
    result.label = label

proc createElementArrayBuffer*[T](length: int, usage: GLenum = GL_STATIC_DRAW, label: string = ""): ElementArrayBuffer[T] =
  result.new
  glNamedBufferData(result.handle, length * GLsizeiptr(sizeof(T)), nil, usage)
  if label.len > 0:
    result.label = label

proc createUniformBuffer*[T](usage: GLenum = GL_STATIC_DRAW): UniformBuffer[T] =
  result.new
  glNamedBufferData(result.handle, GLsizeiptr(sizeof(T)), nil, usage)

proc delete*[T](arg: var AnyBuffer[T]): void =
  ## Typed wrapper around `glDeletebuffers`.
  glDeleteBuffers(1, arg.handle.addr)

proc glGetInteger(name: GLenum): GLint =
  ## Typed wrapper around `glGetIntegerv`.
  glGetIntegerv(name, result.addr)

proc currentArrayBuffer*[T](): ArrayBuffer[T] {. deprecated .} =
  ## Typed wrapper around `glGetInteger(GL_ARRAY_BUFFER_BINDING)`.
  result.handle = GLuint(glGetInteger(GL_ARRAY_BUFFER_BINDING))

proc currentElementArrayBuffer*[T](): ElementArrayBuffer[T] {. deprecated .} =
  ## Typed wrapper around `glGetInteger(GL_ELEMENT_ARRAY_BUFFER_BINDING)`.
  result.handle = GLuint(glGetInteger(GL_ELEMENT_ARRAY_BUFFER_BINDING))

proc currentUniformBuffer*[T](): UniformBuffer[T] {. deprecated .} =
  ## Typed wrapper around `glGetInteger(GL_UNIFORM_BUFFER_BINDING)`.
  result.handle = GLuint(glGetInteger(GL_UNIFORM_BUFFER_BINDING))


template bindingKind*[T](buffer: ArrayBuffer[T]) : GLenum =
  ## return `GL_ARRAY_BUFFER_BINDING`
  GL_ARRAY_BUFFER_BINDING

template bindingKind*[T](buffer: ElementArrayBuffer[T]) : GLenum =
  ## return `GL_ELEMENT_ARRAY_BUFFER_BINDING`
  GL_ELEMENT_ARRAY_BUFFER_BINDING

template bindingKind*[T](buffer: UniformBuffer[T]) : GLenum =
  ## return `GL_UNIFORM_BUFFER_BINDING`
  GL_UNIFORM_BUFFER_BINDING

template bufferKind*[T](buffer: ArrayBuffer[T]) : GLenum =
  ## return `GL_ARRAY_BUFFER`
  GL_ARRAY_BUFFER

template bufferKind*[T](buffer: ElementArrayBuffer[T]) : GLenum =
  ## return `GL_ELEMENT_ARRAY_BUFFER`
  GL_ELEMENT_ARRAY_BUFFER

template bufferKind*[T](buffer: UniformBuffer[T]) : GLenum =
  ## return `GL_UNIFORM_BUFFER`
  GL_UNIFORM_BUFFER

template withBufferBinding*[T](buffer : AnyBuffer[T], blk:untyped) =
  ## Apply the effect of `glBindBuffer` locally to `blk`. The
  ## currently bound buffer is queried with
  ## `glGetInteger(GL_..._BUFFER_BINDING, ...)` and restored after the
  ## execution of `blk`.
  let buf = buffer
  var outer : GLint
  glGetIntegerv(buf.bindingKind, outer.addr)
  glBindBuffer(buf.bufferKind, buf.handle)
  blk
  glBindBuffer(buf.bufferKind, GLuint(outer))

proc bufferData*[T](buffer: SeqLikeBuffer[T], data: openarray[T], usage: GLenum) =
  ## Wrapper for `glNamedBufferData`.
  if buffer.handle.int > 0:
    glNamedBufferData(buffer.handle, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)

proc bufferData*[T](buffer: SeqLikeBuffer[T], dataview: DataView[T], usage: GLenum) =
  ## Wrapper for `glNamedBufferData`.
  if buffer.handle.int > 0:
    glNamedBufferData( buffer.handle, GLsizeiptr(dataview.len * sizeof(T)), dataview.data, usage)

proc bufferData*[T](buffer: UniformBuffer[T], data: T, usage: GLenum) =
  ## Wrapper for `glNamedBufferData`.
  if buffer.handle.int > 0:
    glNamedBufferData(buffer.handle, GLsizeiptr(sizeof(T)), unsafeAddr(data), usage)

proc setData*[T](buffer: SeqLikeBuffer[T], data: openarray[T]) =
  if buffer.handle.int > 0:
    glNamedBufferSubData(buffer.handle, 0, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]))

proc setData*[T](buffer: UniformBuffer[T], data: T) =
  if buffer.handle.int > 0:
    glNamedBufferSubData(buffer.handle, 0, GLsizeiptr(sizeof(T)), unsafeAddr(data))

proc len*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]) : int =
  ## return the amount of elemets stored in the buffer.
  var size: GLint
  if buffer.handle != 0:
    glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_SIZE, size.addr)
    result = int(size) div int(sizeof(T))

proc access*[T](buffer: ArrayBuffer[T]) : GLenum =
  ## return buffer paremeter GL_BUFFER_ACCESS
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_ACCESS, tmp.addr)
  return tmp.GLenum

proc accessFlags*[T](buffer: ArrayBuffer[T]) : GLenum =
  ## return buffer parameter GL_BUFFER_ACCESS_FLAGS
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_ACCESS_FLAGS, tmp.addr)
  return tmp.GLenum

proc immutableStorage*[T](buffer: ArrayBuffer[T]) : bool =
  ## return buffer patameter GL_BUFFER_IMMUTABLE_STORAGE
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_IMMUTABLE_STORAGE, tmp.addr)
  return tmp != GL_FALSE

proc mapped*[T](buffer: ArrayBuffer[T]) : bool =
  ## return buffer patameter GL_BUFFER_MAPPED
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_MAPPED, tmp.addr)

  return tmp != GL_FALSE

proc mapLength*[T](buffer: ArrayBuffer[T]) : int =
  ## return buffer patameter GL_BUFFER_MAP_LENGTH
  var tmp: pointer
  glGetNamedBufferPointerv(buffer.handle, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc mapOffset*[T](buffer: ArrayBuffer[T]) : int =
  ## return buffer patameter GL_BUFFER_MAP_LENGTH
  var tmp: pointer
  glGetNamedBufferPointerv(buffer.handle, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc byteSize*[T](buffer: ArrayBuffer[T]) : int =
  ## return buffer patameter GL_BUFFER_SIZE
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_SIZE, tmp.addr)
  return int(tmp)

proc storageFlags*[T](buffer: ArrayBuffer[T]) : GLenum =
  ## return buffer patameter GL_BUFFER_STORAGE_FLAGS
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_STORAGE_FLAGS, tmp.addr)
  return tmp.GLenum

proc usage*[T](buffer: ArrayBuffer[T]) : GLenum =
  ## return buffer patameter GL_BUFFER_USAGE
  var tmp: GLint
  glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_USAGE, tmp.addr)
  return tmp.GLenum

iterator items*[T](buffer: SeqLikeBuffer[T]) : T =
  ## Map `buffer` with `GL_READ_ONLY` and iterate over the
  ## members. The buffer will be unmapped after the iterations.
  let mappedBuffer = buffer.mapRead
  defer:
    discard buffer.unmap
  for item in mappedBuffer.items:
    yield item

iterator pairs*[T](buffer: SeqLikeBuffer[T]) : tuple[key: int, val: T] =
  ## Map `buffer` with `GL_READ_ONLY` and iterate over the
  ## members. The buffer will be unmapped after the iterations.
  let mappedBuffer = buffer.mapRead
  defer:
    discard buffer.unmap
  for i, item in mappedBuffer.pairs:
    yield (i, item)

iterator mitems*[T](buffer: SeqLikeBuffer[T]) : var T =
  ## Map `buffer` with `GL_READ_WRITE` and iterate over the
  ## members. The buffer will be unmapped after the iterations.
  let mappedBuffer = buffer.mapReadWrite
  defer:
    discard buffer.unmap
  for item in mappedBuffer.mitems:
    yield item

iterator mpairs*[T](buffer: SeqLikeBuffer[T]) : tuple[key: int, val: var T] =
  ## Map `buffer` with `GL_READ_WRITE` and iterate over the
  ## members. The buffer will be unmapped after the iterations.
  let mappedBuffer = buffer.mapReadWrite
  defer:
    discard buffer.unmap
  for i, item in mappedBuffer.mpairs:
    yield (i, item)

iterator witems*[T](buffer: SeqLikeBuffer[T]) : var T =
  ## Map `buffer` with `GL_WRITE_ONLY` and iterate over the
  ## members. The buffer will be unmapped after the iterations.
  let mappedBuffer = buffer.mapWrite
  defer:
    discard buffer.unmap
  for item in mappedBuffer.mitems:
    yield item

iterator wpairs*[T](buffer: SeqLikeBuffer[T]) : tuple[key: int, val: var T] =
  ## Map `buffer` with `GL_WRITE_ONLY` and iterate over the
  ## members. The buffer will be unmapped after the iterations.
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

proc arrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW, label: string = ""): ArrayBuffer[T] =
  result.new
  result.bufferData(data, usage)
  if label.len > 0:
    result.label = label

proc arrayBuffer*[T](data : DataView[T], usage: GLenum = GL_STATIC_DRAW, label: string = ""): ArrayBuffer[T] =
  if not data.isNil:
    result.new
    result.bufferData(data, usage)
    if label.len > 0:
      result.label = label

proc elementArrayBuffer*[T](data : openarray[T]; usage: GLenum = GL_STATIC_DRAW; label: string = ""): ElementArrayBuffer[T] =
  result.new
  result.bufferData(data, usage)
  if label.len > 0:
    result.label = label

proc elementArrayBuffer*[T](data : DataView[T], usage: GLenum = GL_STATIC_DRAW, label: string = ""): ElementArrayBuffer[T] =
  if not data.isNil:
    result.new
    result.bufferData(data, usage)
    if label.len > 0:
      result.label = label

proc uniformBuffer*[T](data : T, usage: GLenum = GL_STATIC_DRAW, label: string = ""): UniformBuffer[T] =
  result.new
  result.bufferData(data, usage)
  if label.len > 0:
    result.label = label

################################################################################
################################ ArrayBufferView ###############################
################################################################################

type
  ArrayBufferView*[T] = object
    handle*: GLuint
    absoluteoffset*: GLsizei
    relativeoffset*: GLuint
    stride*: GLsizei

proc totalOffset*(arg: ArrayBufferView): GLsizei =
  arg.absoluteoffset + GLsizei(arg.relativeoffset)

proc len*(abv: ArrayBufferView):  int =
  ## return the amount of elemets stored in the buffer.
  ## (not correct, ignores offset)
  if abv.handle != 0:
    var size: GLint
    glGetNamedBufferParameteriv(abv.handle, GL_BUFFER_SIZE, size.addr)
    result = int(size) div int(abv.stride)

proc high*(abv: ArrayBufferView): int =
  abv.len - 1

template view*[T](buf: ArrayBuffer[T]; member: untyped): untyped =
  var res : ArrayBufferView[typeof(default(typedesc(T)).member)]
  res.handle = buf.handle
  res.relativeoffset = cast[GLuint](offsetof(typedesc(T), member))
  res.stride = GLsizei(sizeof(T))
  res

proc splitView*(buf: ArrayBuffer[Mat4f]): array[4, ArrayBufferView[Vec4f]] =
  for i in 0 .. 3:
    result[i].handle = buf.handle
    result[i].relativeoffset = GLuint(i * 4 * sizeof(float32))
    result[i].stride = 4 * 4 * sizeof(float32)

converter toView*[T](buf: ArrayBuffer[T]): ArrayBufferView[T] =
  result.handle = buf.handle
  result.absoluteoffset = 0
  result.relativeoffset = 0
  result.stride = sizeof(T)

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
  proc showError(log: string; source: string; lineinfo: LineInfo): void =
    stdout.styledWriteLine(fgRed, $lineinfo, " shader compile fail")

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

    stdout.styledWriteLine(fgGreen, "------------------------------------------------------------------")
    stdout.styledWriteLine(fgRed, log)
    stdout.styledWriteLine(fgGreen, "==== end Shader Problems =========================================")
else:
  proc showError(log: string; source: string, lineinfo: LineInfo): void =
    stdout.styledWriteLine(fgRed, $lineinfo, " shader compile fail")
    stdout.styledWriteLine(fgGreen, "==== start Shader Problems =======================================")
    let lines = source.splitLines
    for i, line in lines:
      let lineNr = i + 1
      stdout.styledWriteLine(fgYellow, intToStr(lineNr, 4), " ", resetStyle, line)
    stdout.styledWriteLine(fgGreen, "------------------------------------------------------------------")
    stdout.styledWriteLine(fgRed, log)
    stdout.styledWriteLine(fgGreen, "==== end Shader Problems =========================================")


proc validateShader*(src: string, shaderType: GLenum): bool {.compileTime.} =
  ## use command line ``glslangValidator`` to validate if a shader is correct.
  const
    ColorReset = "\e[0m" # background and foreground to default
    ErrorStyle = "\e[31m" # Red
    ErrorStyle2 = "\e[91m" # LightRed
    LineNumberStyle = "\e[33m" # Yellow
    BarStyle = "\e[30m\e[43m" # Black and Yellow

  let sk =
    case shaderType:
    of GL_VERTEX_SHADER:
      "vert"
    of GL_FRAGMENT_SHADER:
      "frag"
    of GL_GEOMETRY_SHADER:
      "geom"
    of GL_COMPUTE_SHADER:
      "comp"
    of GL_TESS_CONTROL_SHADER:
      "tesc"
    of GL_TESS_EVALUATION_SHADER:
      "tese"
    else:
      ""

  assert(sk != "", "illegal argument: " & $shaderType)

  let log = staticExec("glslangValidator --stdin -S " & sk, src, "true")
  if log.len > 0:
    echo "  glsl errors:  ".center(80,'#')
    var problems = newSeq[tuple[lineNr: int, message: string]](0)
    for line in log.splitLines:
      if line.startsWith("ERROR: 0:"):
        var i = 9
        while line[i].isDigit:
          i += 1
        let lineNr = parseInt(line[9 ..< i])
        let message = line[i .. ^1]

        problems.add((lineNr: lineNr, message: message))

    echo(BarStyle, " start Shader Problems ".center(80,'='), ColorReset)
    var lineNr = 0
    for line in src.splitLines:
      lineNr += 1
      echo(LineNumberStyle, intToStr(lineNr, 4), " ", ColorReset, line)
      for problem in problems:
        if problem.lineNr == lineNr:
          echo("     ", ErrorStyle, problem.message)
    echo(BarStyle, repeat("-",80), ColorReset)
    echo(ErrorStyle2, log)
    echo(BarStyle, center(" end Shader Problems ",80,'='), ColorReset)

  else:
    return true


proc infoLog(program: Program): string =
  var length: GLint = 0
  glGetProgramiv(program.handle, GL_INFO_LOG_LENGTH, length.addr);
  result = newString(length.int)
  glGetProgramInfoLog(program.handle, length, nil, result);

proc compileShader*(shaderType: GLenum, source: string, lineinfo: LineInfo): Shader =
  result.handle = glCreateShader(shaderType)
  result.shaderSource(source)
  glCompileShader(result.handle)

  if not result.compileStatus:
    showError(result.shaderInfoLog, source, lineinfo)

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

template relativeoffset*[T](arg: ArrayBuffer[T]): untyped = 0

template setFormat*[T](vao: VertexArrayObject, binding: uint32, buffer: ArrayBuffer[T]) =
  glVertexArrayAttribFormat(vao.handle, binding, attribSize(buffer.T), attribType(buffer.T), attribNormalized(buffer.T), 0);

template setFormat*[T](vao: VertexArrayObject; binding: uint32; buffer: ArrayBufferView[T]): void =
  glVertexArrayAttribFormat(vao.handle, binding, attribSize(buffer.T), attribType(buffer.T), attribNormalized(buffer.T), 0);

proc setBuffer*[T](vao: VertexArrayObject; binding: uint32; buffer: ArrayBuffer[T]): void =
  glVertexArrayVertexBuffer(vao.handle, binding, buffer.handle, buffer.totaloffset, buffer.stride)

proc setBuffer*(vao: VertexArrayObject; binding: uint32; buffer: ArrayBufferView): void =
  glVertexArrayVertexBuffer(vao.handle, binding, buffer.handle, buffer.totaloffset, buffer.stride)

macro setBuffers*(vao: VertexArrayObject; first: uint32; buffers: varargs[untyped]): untyped =
  if buffers.len == 0:
    discard
    # do nothing
  if buffers.len == 1:
    let buffer = buffers[0]
    result = newCall(bindSym"setBuffer", vao, first, buffer)
  else:
    let buffersArray = nnkBracket.newTree
    let offsetsArray = nnkBracket.newTree
    let stridesArray = nnkBracket.newTree

    for buffer in buffers:
      buffersArray.add newDotExpr(buffer, ident"handle")
      offsetsArray.add newCall(bindSym"GLintptr",(newDotExpr(buffer, ident"totaloffset")))
      stridesArray.add newDotExpr(buffer, ident"stride")

    let countLit = newLit(buffers.len)

    result = quote do:
      var buffers : array[`countLit`, GLuint]   = `buffersArray`
      var offsets : array[`countLit`, GLintptr] = `offsetsArray`
      var strides : array[`countLit`, GLsizei]  = `stridesArray`
      glVertexArrayVertexBuffers(`vao`.handle, `first`, GLsizei(`countLit`), buffers[0].addr, offsets[0].addr, strides[0].addr)

macro bindTextures*(first: uint32; textures: varargs[untyped]): untyped =
  if textures.len == 0:
    discard
    # do nothing
  elif textures.len == 1:
    result = newCall( bindSym"glBindTextureUnit", first, newDotExpr(textures[0], ident"handle"))
  else:
    let texturesArrayExpr = nnkBracket.newTree
    for texture in textures:
      texturesArrayExpr.add newDotExpr(texture, ident"handle")

    let lengthLit = newLit(GLsizei(textures.len))

    result = quote do:
      var textureHandles = `texturesArrayExpr`
      glBindTextures(`first`, `lengthLit`, textureHandles[0].addr)

##########################
# # transform feedback # #
##########################

# template transformFeedbackBlock(primitiveMode: GLenum; blk: untyped): untyped =
#   block:
#     glBeginTransformFeedback(primitiveMode)
#     defer:
#       glEndTransformFeedback()
#     blk

# macro countFields(arg: typed): int =
#   let typeImpl = arg.getTypeInst[1].getTypeImpl
#   typeImpl.expectKind(nnkObjectTy)
#   var acc = 0
#   for identDef in typeImpl[2]:
#     for i in 0 ..< identDef.len-2:
#       acc += 1
#   result = newLit(acc)

#proc numFields[T](t: typedesc[T]): int =
#  countFields(t)

type
  TransformFeedback*[T] = object
    handle*: GLuint
    varyingOffsets*: seq[int]
    varyingNames*: seq[string]

macro typeName(t: typedesc): untyped =
  newLit($t.getTypeImpl[1])

template offsetof*(typ, field: untyped): int =
  (var dummy: typ;
   let a = cast[system.uint](addr(dummy));
   let b = cast[system.uint](addr(dummy.field));
   int(b - a))

# macro genVaryingNames(self: TransformFeedback): untyped =
#   ## returns array of string literals
#   result = nnkBracket.newTree
#   let typeImpl = self.getTypeInst[1].getTypeImpl
#   typeImpl.expectKind(nnkObjectTy)
#   for identDef in typeImpl[2]:
#     for i in 0 ..< identDef.len-2:
#       let sym = identDef[i]
#       result.add newLit($sym)

#   echo result.repr

# macro genVaryingOffsets[T](self: TransformFeedback[T]): untyped =
#   when false:
#     result = nnkBracket.newTree()
#     let tpe = self.getTypeInst[1]
#     echo tpe.treeRepr
#     let typeImpl = tpe.getTypeImpl
#     typeImpl.expectKind(nnkObjectTy)
#     for identDef in typeImpl[2]:
#       for i in 0 ..< identDef.len-2:
#         let sym = identDef[i]
#         result.add getAst(offsetOf(tpe,sym))
#   else:
#     result = newLit([0, 8, 16, 28, 32])

#   echo result.repr

template stride*[T](self: TransformFeedback[T]): int =
  sizeof(T)

proc glslLayoutSpecificationRuntime[T](name: string = ""): string =
  let stride = sizeof(T)
  let name = $T
  var tmp: T

  let baseAddr = cast[uint](tmp.addr)
  result =  s"layout(xfb_buffer = 0, xfb_stride = $stride) out $name {"
  result.add "\n"
  for fieldName, field in tmp.fieldPairs:
    result.add "  layout(xfb_offset = "
    result.add $(cast[uint](field.addr) - baseAddr)
    result.add ") "
    result.add glslTypeRepr(typeof(field))
    result.add " "
    result.add fieldName
    result.add ";\n"

  result.add "};\n"

macro glslLayoutSpecification*(arg: typedesc): string =
  # strip typedesc
  let arg = arg.getTypeInst[1]
  let stride = getSize(arg)
  let name = repr(arg)
  var res: string = s"layout(xfb_buffer = 0, xfb_stride = $stride) out $name {"
  let impl = arg.getTypeImpl
  for identDefs in impl[2]:
    if identDefs.kind == nnkRecCase:
      error("Variant types not supported in transform feedback type.", identDefs)
    identDefs.expectKind nnkIdentDefs
    if identDefs[1].kind == nnkBracketExpr:
      if identDefs[1][0].eqIdent "array":
        error("Arrays not supported in transform feedback type.", identDefs[1])
      if identDefs[1][0].eqIdent "seq":
        error("Seq not supported in transform feedback type.", identDefs[1])
    if identDefs[1].eqIdent "string":
      error("String not supported in transform feedback type.", identDefs[1])

    let offset = getOffset(identDefs[0])
    let glslTyp = glslType(identDefs[1]) #glslTypeRepr(identDefs[1])
    let fieldName = $identDefs[0]

    res.add "\n  layout(xfb_offset = "
    res.addInt offset
    res.add ") "
    res.add glslTyp
    res.add " "
    res.add fieldName
    res.add ";"

  res.add "\n};\n"
  result = newLit(res)

proc createTransformFeedback*[T]() : TransformFeedback[T] =
  glCreateTransformFeedbacks(GLsizei(1), result.handle.addr)
  result.label = typeName(T)

  let layoutSpecRT = glslLayoutSpecificationRuntime[T]()
  let layoutSpec   = glslLayoutSpecification(T)

  if layoutSpecRT != layoutSpec:
    echo "layout spec outdated:"
    echo layoutSpec
    echo "you will need the following template in your codebase, just copy paste it"
    echo ""
    echo "template glslLayoutSpecification*(arg: typedesc[", $T, "]): string = \"\"\"\n"
    echo layoutSpecRT
    echo "\"\"\""

proc delete*(tf: TransformFeedback): void =
  ## Typed wrapper around `glDeleteTransformFeedbacks`.
  glDeleteTransformFeedbacks(GLsizei(1), tf.handle.unsafeAddr)

proc bufferBase*(tf: TransformFeedback; index: int; buffer: ArrayBuffer): void =
  ## Typed wrapper around `glTransformFeedbackBufferBase`.
  glTransformFeedbackBufferBase(tf.handle, GLuint(index), buffer.handle)

proc bufferRange*(tf: TransformFeedback; index: int; buffer: ArrayBuffer; offset: ptr int32; size: int): void =
  ## Typed wrapper around `glTransformFeedbackBufferRange`.
  glTransformFeedbackBufferRange(tf.handle, GLuint(index), buffer.handle, offset, GLsizeiptr(size))

proc draw*(tf: TransformFeedback; primitiveMode: GLenum): void =
  ## Typed wrapper around `glDrawTransformFeedback`.
  glDrawTransformFeedback(primitiveMode, tf.handle)

#type
#  LabelAble = Program | Shader | VertexArrayObject | AnyBuffer | AnyTexture

template glNamespace(arg: typedesc[Program]): GLenum = GL_PROGRAM
template glNamespace(arg: typedesc[Shader]): GLenum  = GL_SHADER
template glNamespace(arg: typedesc[VertexArrayObject]): GLenum = GL_VERTEX_ARRAY
template glNamespace(arg: typedesc[AnyBuffer]): GLenum = GL_BUFFER
template glNamespace(arg: typedesc[AnyTexture]): GLenum = GL_TEXTURE
template glNamespace(arg: typedesc[TransformFeedback]): GLenum = GL_TRANSFORM_FEEDBACK

proc label*[T](arg: T): string =
  ## Typed wraper around `glGetObjectLabel`.
  if glGetObjectLabel != nil:
    const bufsize = 255
    result = newString(bufsize)
    var length: GLsizei
    glGetObjectLabel(glNamespace(arg.type), arg.handle, bufsize, length.addr, result[0].addr)
    result.setLen(length)
  else:
    result = "<object label not supported>"

proc `label=`*[T](arg: T; label: string): void =
  ## Typed wrapper around `glObjectLabel`.
  if glObjectLabel != nil:
    glObjectLabel(glNamespace(arg.type), arg.handle, GLsizei(label.len), label[0].unsafeAddr)
