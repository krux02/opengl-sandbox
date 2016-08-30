# included from fancygl.nim

#### Uniform ####

proc uniform(location: GLint, mat: Mat4d) =
  var mat_var = mat.mat4f
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat_var.addr))

proc uniform(location: GLint, mat: Mat4f) =
  var mat_var = mat
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat_var.addr))

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

proc uniform(location: GLint, value: bool) =
  glUniform1i(location, value.GLint)

#### Vertex Array Object ####

type VertexArrayObject* = object
    handle: GLuint

proc newVertexArrayObject*() : VertexArrayObject =
  glGenVertexArrays(1, cast[ptr GLuint](result.addr))

const nil_vao* = VertexArrayObject(handle: 0)

proc bindIt*(vao: VertexArrayObject) =
  glBindVertexArray(vao.handle)

proc delete*(vao: VertexArrayObject) =
  glDeleteVertexArrays(1, vao.handle.unsafeAddr)

proc divisor(vao: VertexArrayObject, index, divisor: GLuint) : void =
  when false:
    glVertexArrayVertexBindingDivisorEXT(vao.handle, index, divisor)
  else:
    glVertexArrayBindingDivisor(vao.handle, index, divisor)

proc enableAttrib(vao: VertexArrayObject, index: GLuint) : void =
  when false:
    glEnableVertexArrayAttribEXT(vao.handle, index)
  else:
    glEnableVertexArrayAttrib(vao.handle, index)

#proc divisor(vao: VertexArrayObject, index: GLuint) : GLuint =

template blockBind*(vao: VertexArrayObject, blk: stmt) : stmt =
  vao.bindIt
  blk
  nil_vao.bindIt

#### Array Buffers ####

type
  ArrayBuffer*[T]        = object
    handle: GLuint
  ElementArrayBuffer*[T] = object
    handle: GLuint
  UniformBuffer*[T]      = object
    handle: GLuint

type SeqLikeBuffer[T] = ArrayBuffer[T] | ElementArrayBuffer[T]
type AnyBuffer[T] = ArrayBuffer[T] | ElementArrayBuffer[T] | UniformBuffer[T]

proc create*[T](arrayBuffer: var ArrayBuffer[T] ) : void =
  when false:
    glGenBuffers(1, arrayBuffer.handle.addr)
  else:
    glCreateBuffers(1, arrayBuffer.handle.addr)

proc create*[T](arrayBuffer: var ElementArrayBuffer[T] ) : void =
  when false:
    glGenBuffers(1, arrayBuffer.handle.addr)
  else:
    glCreatebuffers(1, arrayBuffer.handle.addr)

proc create*[T](arrayBuffer: var UniformBuffer[T] ) : void =
  when false:
    glGenBuffers(1, arrayBuffer.handle.addr)
  else:
    glCreateBuffers(1, arrayBuffer.handle.addr)

proc createArrayBuffer*[T](len: int, usage: GLenum): ArrayBuffer[T] =
  result.create
  when false:
    glNamedBufferDataEXT(result.handle, len * GLsizeiptr(sizeof(T)), nil, usage)
  else:
    glNamedBufferData(result.handle, len * GLsizeiptr(sizeof(T)), nil, usage)

proc createElementArrayBuffer*[T](len: int, usage: GLenum): ElementArrayBuffer[T] =
  result.create
  when false:
    glNamedBufferDataEXT(result.handle, len * GLsizeiptr(sizeof(T)), nil, usage)
  else:
    glNamedBufferData(result.handle, len * GLsizeiptr(sizeof(T)), nil, usage)

proc createUniformBuffer*[T](usage: GLenum): UniformBuffer[T] =
  result.create
  when false:
    glNamedBufferDataEXT(result.handle, GLsizeiptr(sizeof(T)), nil, usage)
  else:
    glNamedBufferData(result.handle, GLsizeiptr(sizeof(T)), nil, usage)

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
  glBindBuffer(buffer.bufferKind, buffer.handle)

template bindBlock[T](buffer : AnyBuffer[T], blk:untyped) =
  let buf = buffer
  var outer : GLint
  glGetIntegerv(buf.bindingKind, outer.addr)
  buf.bindIt
  blk
  glBindBuffer(buf.bufferKind, GLuint(outer))

proc bufferData*[T](buffer: SeqLikeBuffer[T], usage: GLenum, data: openarray[T]) =
  if buffer.handle.int > 0:
    when false:
      glNamedBufferDataEXT(buffer.handle, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)
    else:
      glNamedBufferData(buffer.handle, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)

proc bufferData*[T](buffer: UniformBuffer[T], usage: GLenum, data: T) =
  if buffer.handle.int > 0:
    when false:
      glNamedBufferDataEXT(buffer.handle, GLsizeiptr(sizeof(T)), unsafeAddr(data), usage)
    else:
      glNamedBufferData(buffer.handle, GLsizeiptr(sizeof(T)), unsafeAddr(data), usage)

proc len*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]) : int =
  var size: GLint
  when false:
    glGetNamedBufferParameterivEXT(buffer.handle, GL_BUFFER_SIZE, size.addr)
  else:
    glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_SIZE, size.addr)
  return size.int div sizeof(T).int

proc access*[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  when false:
    glGetNamedBufferParameterivEXT(buffer.handle, GL_BUFFER_ACCESS, tmp.addr)
  else:
    glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_ACCESS, tmp.addr)
  return tmp.GLenum

proc accessFlags*[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  when false:
    glGetNamedBufferParameterivEXT(buffer.handle, GL_BUFFER_ACCESS_FLAGS, tmp.addr)
  else:
    glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_ACCESS_FLAGS, tmp.addr)
  return tmp.GLenum

proc immutableStorage*[T](buffer: ArrayBuffer[T]) : bool =
  var tmp: GLint
  when false:
    glGetNamedBufferParameterivEXT(buffer.handle, GL_BUFFER_IMMUTABLE_STORAGE, tmp.addr)
  else:
    glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_IMMUTABLE_STORAGE, tmp.addr)
  return tmp != GL_FALSE

proc mapped*[T](buffer: ArrayBuffer[T]) : bool =
  var tmp: GLint
  when false:
    glGetNamedBufferParameterivEXT(buffer.handle, GL_BUFFER_MAPPED, tmp.addr)
  else:
    glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_MAPPED, tmp.addr)
    
  return tmp != GL_FALSE

proc mapLength*[T](buffer: ArrayBuffer[T]) : int =
  var tmp: pointer
  when false:
    glGetNamedBufferPointervEXT(buffer.handle, GL_BUFFER_MAP_LENGTH, tmp.addr)
  else:
    glGetNamedBufferPointerv(buffer.handle, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc mapOffset*[T](buffer: ArrayBuffer[T]) : int =
  var tmp: pointer
  when false:
    glGetNamedBufferPointervEXT(buffer.handle, GL_BUFFER_MAP_LENGTH, tmp.addr)
  else:
    glGetNamedBufferPointerv(buffer.handle, GL_BUFFER_MAP_LENGTH, tmp.addr)
  return int(tmp)

proc size*[T](buffer: ArrayBuffer[T]) : int =
  var tmp: GLint
  when false:
    glGetNamedBufferParameterivEXT(buffer.handle, GL_BUFFER_SIZE, tmp.addr)
  else:
    glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_SIZE, tmp.addr)
  return int(tmp)

proc storageFlags*[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  when false:
    glGetNamedBufferParameterivEXT(buffer.handle, GL_BUFFER_STORAGE_FLAGS, tmp.addr)
  else:
    glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_STORAGE_FLAGS, tmp.addr)
  return tmp.GLenum

proc usage*[T](buffer: ArrayBuffer[T]) : GLenum =
  var tmp: GLint
  when false:
    glGetNamedBufferParameterivEXT(buffer.handle, GL_BUFFER_USAGE, tmp.addr)
  else:
    glGetNamedBufferParameteriv(buffer.handle, GL_BUFFER_USAGE, tmp.addr)
  return tmp.GLenum

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

proc isNil[T](view: DataView[T] | ReadView[T] | WriteView[T]): bool =
  view.data.isNil
  
proc dataView*[T](data: pointer, size: int) : DataView[T] =
  DataView[T](data: cast[ptr UncheckedArray[T]](data), size: size)

proc len*(mb : ReadView | WriteView | DataView) : int =
  mb.size

proc `[]`*[T](mb : ReadView[T], index: int) : T =
  mb.data[index]

proc `[]=`*[T](mb : WriteView[T], index: int, val: T) : void =
  mb.data[index] = val

proc `[]`*[T](mb : DataView[T]; index: int) : var T =
  mb.data[index]

proc `[]=`*[T](mb : DataView[T], index: int, val: T) : void =
  mb.data[index] = val

iterator items*[T](dv: DataView[T]) : T =
  let p = dv.data
  var i = 0
  while i < dv.size:
    yield p[i]
    inc(i)

iterator pairs*[T](dv: DataView[T]): tuple[key: int, val: T] {.inline.} =
  let p = dv.data
  var i = 0
  while i < dv.size:
    yield (i, p[i])
    inc(i)

proc take*[T](dv: DataView[T], num: int) : DataView[T] =
  result.data = dv.data
  result.size = max(min(num, dv.size), 0)

iterator items*[T](buffer: SeqLikeBuffer[T]) : T =
  let mappedBuffer = buffer.mapRead
  defer:
    discard buffer.unmap
  for item in mappedBuffer:
    yield item

proc unmap*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): bool =
  when false:
    glUnmapNamedBufferEXT(buffer.handle) != GL_FALSE.GLboolean
  else:
    glUnmapNamedBuffer(buffer.handle) != GL_FALSE.GLboolean

proc mapRead*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): ReadView[T] =
  when false:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.handle, GL_READ_ONLY))
  else:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBuffer(buffer.handle, GL_READ_ONLY))
  result.size = buffer.size div sizeof(T)

proc mapWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): WriteView[T] =
  when false:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.handle, GL_WRITE_ONLY))
  else:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBuffer(buffer.handle, GL_WRITE_ONLY))
  result.size = buffer.size div sizeof(T)

proc mapReadWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): DataView[T] =
  when false:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.handle, GL_READ_WRITE))
  else:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBuffer(buffer.handle, GL_READ_WRITE))
  result.size = buffer.size div sizeof(T)

template mapReadBufferBlock*(buffer, blck: untyped) : stmt =
  block:
    let mappedBuffer {. inject .} = buffer.mapRead
    defer:
      discard buffer.unmap
    blck

template mapWriteBufferBlock*(buffer: untyped, blck: untyped) : stmt =
  block:
    let mappedBuffer {. inject .} = buffer.mapWrite
    defer:
      discard buffer.unmap
    blck

template mapReadWriteBufferBlock*(buffer: untyped, blck: untyped) : stmt =
  block:
    let mappedBuffer {. inject .} = buffer.mapReadWrite
    defer:
      discard buffer.unmap
    blck

proc bufferData*[T](buffer: SeqLikeBuffer[T], usage: GLenum, dataview: DataView[T]) =
  if buffer.handle.int > 0:
    when false:
      glNamedBufferDataEXT( buffer.handle, GLsizeiptr(dataview.len * sizeof(T)), dataview.data, usage)
    else:
      glNamedBufferData( buffer.handle, GLsizeiptr(dataview.len * sizeof(T)), dataview.data, usage)

proc arrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW): ArrayBuffer[T] =
  result.create
  result.bufferData(usage, data)

proc arrayBuffer*[T](data : DataView[T], usage: GLenum = GL_STATIC_DRAW): ArrayBuffer[T] =
  if not data.isNil:
    result.create
    result.bufferData(usage, data)

proc elementArrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW): ElementArrayBuffer[T] =
  result.create
  result.bufferData(usage, data)

proc elementArrayBuffer*[T](data : DataView[T], usage: GLenum = GL_STATIC_DRAW): ElementArrayBuffer[T] =
  if not data.isNil:
    result.create
    result.bufferData(usage, data)

proc uniformBuffer*[T](data : T, usage: GLenum = GL_STATIC_DRAW): UniformBuffer[T] =
  result.create
  result.bufferData(usage, data)

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
  var problems = newSeq[tuple[lineNr: int, column: int, message: string]](0)
  for match in log.findIter(re"(\d+)\((\d+)\): ([^:]*): (.*)"):
    let lineNr = match.captures[0].parseInt
    let column = match.captures[1].parseInt
    let kind: string = match.captures[2]
    let message: string = match.captures[3]
    problems.add( (lineNr, column, message) )
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
    showError(result.shaderInfoLog, source)

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
