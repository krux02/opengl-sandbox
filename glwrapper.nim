#### Uniform ####

proc uniform(location: GLint, mat: Mat4x4[float64]) =
  var mat_float32 = mat4f(mat)
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat_float32.addr))

proc uniform(location: GLint, mat: var Mat4x4[float32]) =
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat.addr))

proc uniform(location: GLint, value: float32) =
  glUniform1f(location, value)

proc uniform(location: GLint, value: float64) =
  glUniform1f(location, value)

proc uniform(location: GLint, value: int32) =
  glUniform1i(location, value)

proc uniform(location: GLint, value: Vec2f) =
  glUniform2f(location, value[0], value[1])

proc uniform(location: GLint, value: Vec3f) =
  glUniform3f(location, value[0], value[1], value[2])

proc uniform(location: GLint, value: Vec4f) =
  glUniform4f(location, value[0], value[1], value[2], value[3])

#### Vertex Array Object ####


type VertexArrayObject* = distinct GLuint

proc newVertexArrayObject*() : VertexArrayObject =
  glGenVertexArrays(1, cast[ptr GLuint](result.addr))

const nil_vao* = VertexArrayObject(0)

proc bindIt*(vao: VertexArrayObject) =
  glBindVertexArray(GLuint(vao))

proc delete*(vao: VertexArrayObject) =
  var raw_vao = GLuint(vao)
  glDeleteVertexArrays(1, raw_vao.addr)


proc divisor(vao: VertexArrayObject, index, divisor: GLuint) : void =
  glVertexArrayVertexBindingDivisorEXT(vao.GLuint, index, divisor)

proc enableAttrib(vao: VertexArrayObject, index: GLuint) : void =
  glEnableVertexArrayAttribEXT(vao.GLuint, index)

#proc divisor(vao: VertexArrayObject, index: GLuint) : GLuint =

template blockBind*(vao: VertexArrayObject, blk: stmt) : stmt =
  vao.bindIt
  blk
  nil_vao.bindIt

#### Array Buffers ####

type ArrayBuffer*[T]        = distinct GLuint
type ElementArrayBuffer*[T] = distinct GLuint
type UniformBuffer*[T]      = distinct GLuint

type SeqLikeBuffer[T] = ArrayBuffer[T] | ElementArrayBuffer[T]
type AnyBuffer[T] = ArrayBuffer[T] | ElementArrayBuffer[T] | UniformBuffer[T]

proc create*[T](arrayBuffer: var ArrayBuffer[T] ) : void =
  glGenBuffers(1, cast[ptr GLuint](addr(arrayBuffer)))

proc create*[T](arrayBuffer: var ElementArrayBuffer[T] ) : void =
  glGenBuffers(1, cast[ptr GLuint](addr(arrayBuffer)))

proc create*[T](arrayBuffer: var UniformBuffer[T] ) : void =
  glGenBuffers(1, cast[ptr GLuint](addr(arrayBuffer)))

proc createArrayBuffer*[T](len: int, usage: GLenum): ArrayBuffer[T] =
  result.create
  glNamedBufferDataEXT(result.GLuint, len * GLsizeiptr(sizeof(T)), nil, usage)

proc createElementArrayBuffer*[T](len: int, usage: GLenum): ElementArrayBuffer[T] =
  result.create
  glNamedBufferDataEXT(result.GLuint, len * GLsizeiptr(sizeof(T)), nil, usage)

proc createUniformBuffer*[T](usage: GLenum): UniformBuffer[T] =
  result.create
  glNamedBufferDataEXT(result.GLuint, GLsizeiptr(sizeof(T)), nil, usage)

proc currentArrayBuffer*[T](): ArrayBuffer[T] =
  glGetIntegerv(GL_ARRAY_BUFFER_BINDING, cast[ptr GLint](result.addr))

proc currentElementArrayBuffer*[T](): ElementArrayBuffer[T] =
  glGetIntegerv(GL_ELEMENT_ARRAY_BUFFER_BINDING, cast[ptr GLint](result.addr))

proc currentUniformBuffer*[T](): UniformBuffer[T] =
  glGetIntegerv(GL_UNIFORM_BUFFER_BINDING, cast[ptr GLint](result.addr))

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
  glBindBuffer(buffer.bufferKind, GLuint(buffer))

template bindBlock[T](buffer : AnyBuffer[T], blk:untyped) =
  let buf = buffer
  var outer : GLint
  glGetIntegerv(buf.bindingKind, outer.addr)
  buf.bindIt
  blk
  glBindBuffer(buf.bufferKind, GLuint(outer))

proc bufferData*[T](buffer: SeqLikeBuffer[T], usage: GLenum, data: openarray[T]) =
  if buffer.int > 0:
    glNamedBufferDataEXT(buffer.GLuint, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)

proc bufferData*[T](buffer: SeqLikeBuffer[T], dataptr: ptr T, size: int, usage: GLenum) =
  if buffer.int > 0:
    glNamedBufferDataEXT(buffer.GLuint, GLsizeiptr(size), dataptr, usage)

proc bufferData*[T](buffer: UniformBuffer[T], usage: GLenum, data: T) =
  if buffer.int > 0:
    glNamedBufferDataEXT(buffer.GLuint, GLsizeiptr(sizeof(T)), unsafeAddr(data), usage)

proc len*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]) : int =
  var size: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_SIZE, size.addr)
  return size.int div sizeof(T).int

proc arrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW): ArrayBuffer[T] =
  result.create
  result.bufferData(usage, data)

proc elementArrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW): ElementArrayBuffer[T] =
  result.create
  result.bufferData(usage, data)

proc uniformBuffer*[T](data : T, usage: GLenum = GL_STATIC_DRAW): UniformBuffer[T] =
  result.create
  result.bufferData(usage, data)

proc access[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_ACCESS, tmp.addr)
  return tmp.GLenum

proc accessFlags[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_ACCESS_FLAGS, tmp.addr)
  return tmp.GLenum

proc immutableStorage[T](buffer: ArrayBuffer[T]) : bool =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_IMMUTABLE_STORAGE, tmp.addr)
  return tmp != GL_FALSE

proc mapped[T](buffer: ArrayBuffer[T]) : bool =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_MAPPED, tmp.addr)
  return tmp != GL_FALSE

proc mapLength[T](buffer: ArrayBuffer[T]) : int =
  var tmp: pointer
  glGetNamedBufferPointervEXT(buffer.GLuint, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc mapOffset[T](buffer: ArrayBuffer[T]) : int =
  var tmp: pointer
  glGetNamedBufferPointervEXT(buffer.GLuint, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc size[T](buffer: ArrayBuffer[T]) : int =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_MAPPED, tmp.addr)
  return int(tmp)

proc storageFlags[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_STORAGE_FLAGS, tmp.addr)
  return tmp.GLenum

proc usage[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  glGetNamedBufferParameterivEXT(buffer.GLuint, GL_BUFFER_USAGE, tmp.addr)
  return tmp.GLenum

type
  UncheckedArray {.unchecked.} [t] = array[0,t]

  MappedReadBuffer*[T] = object
    data: ptr UncheckedArray[T]
    size: int

  MappedWriteBuffer*[T] = object
    data: ptr UncheckedArray[T]
    size: int

  MappedReadWriteBuffer*[T] = object
    data: ptr UncheckedArray[T]
    size: int

proc len*(mb : MappedReadBuffer | MappedWriteBuffer | MappedReadWriteBuffer) : int =
  mb.size

proc `[]`*[T](mb : MappedReadBuffer[T], index: int) : T =
  mb.data[index]

proc `[]=`*[T](mb : MappedWriteBuffer[T], index: int, val: T) : void =
  mb.data[index] = val

proc `[]`*[T](mb : MappedReadWriteBuffer[T]; index: int) : var T =
  mb.data[index]

proc `[]=`*[T](mb : MappedReadWriteBuffer[T], index: int, val: T) : void =
  mb.data[index] = val


proc unmap*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): bool =
  glUnmapNamedBufferEXT(buffer.GLuint) != GL_FALSE.GLboolean

proc mapRead*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): MappedReadBuffer[T] =
  result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.GLuint, GL_READ_ONLY))
  result.size = buffer.size div sizeof(T)

proc mapWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): MappedWriteBuffer[T] =
  result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.GLuint, GL_WRITE_ONLY))
  result.size = buffer.size div sizeof(T)

proc mapReadWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): MappedReadWriteBuffer[T] =
  result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.GLuint, GL_READ_WRITE))
  result.size = buffer.size div sizeof(T)

template mapReadBufferBlock*(buffer, blck: untyped) : stmt =
  block:
    let mappedBuffer {. inject .} = buffer.mapRead
    blck

  discard buffer.unmap

template mapWriteBufferBlock*(buffer: untyped, blck: untyped) : stmt =
  block:
    let mappedBuffer {. inject .} = buffer.mapWrite
    blck

  discard buffer.unmap

template mapReadWriteBufferBlock*(buffer: untyped, blck: untyped) : stmt =
  block:
    let mappedBuffer {. inject .} = buffer.mapReadWrite
    blck

  discard buffer.unmap

#### shader

proc shaderSource(shader: GLuint, source: string) =
  var source_array: array[1, string] = [source]
  var c_source_array = allocCStringArray(source_array)
  defer: deallocCStringArray(c_source_array)
  glShaderSource(shader, 1, c_source_array, nil)

proc compileStatus(shader:GLuint): bool =
  var status: GLint
  glGetShaderiv(shader, GL_COMPILE_STATUS, status.addr)
  status != 0

proc linkStatus(program:GLuint): bool =
  var status: GLint
  glGetProgramiv(program, GL_LINK_STATUS, status.addr)
  status != 0

proc shaderInfoLog(shader: GLuint): string =
  var length: GLint = 0
  glGetShaderiv(shader, GL_INFO_LOG_LENGTH, length.addr)
  result = newString(length.int)
  glGetShaderInfoLog(shader, length, nil, result)

proc showError(log: string, source: string): void =
  let lines = source.splitLines
  for match in log.findIter(re"(\d+)\((\d+)\).*"):
    let line_nr = match.captures[1].parseInt;
    echo lines[line_nr - 1]
    echo match.match

proc programInfoLog(program: GLuint): string =
  var length: GLint = 0
  glGetProgramiv(program, GL_INFO_LOG_LENGTH, length.addr);
  result = newString(length.int)
  glGetProgramInfoLog(program, length, nil, result);

proc compileShader(shaderType: GLenum, source: string): GLuint =
  result = glCreateShader(shaderType)
  result.shaderSource(source)
  glCompileShader(result)

  if not result.compileStatus:
    echo "==== start Shader Problems ======================================="
    echo source
    echo "------------------------------------------------------------------"
    showError(result.shaderInfoLog, source)
    echo "==== end Shader Problems ========================================="

proc linkShader(shaders: varargs[GLuint]): GLuint =
  result = glCreateProgram()

  for shader in shaders:
    glAttachShader(result, shader)
    glDeleteShader(shader)
  glLinkProgram(result)

  if not result.linkStatus:
    echo "Log: ", result.programInfoLog
    glDeleteProgram(result)
    result = 0

