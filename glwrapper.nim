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

iterator items*[T](view: ReadView[T]) : T =
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

iterator items*[T](view: DataView[T]) : T =
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

iterator mitems*[T](view: DataView[T]) : var T =
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

iterator mitems*[T](wv: WriteView[T]) : var T =
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
        
    
proc take*[T](view: DataView[T], num: int) : DataView[T] =
  result.data = view.data
  result.size = max(min(num, view.size), 0)

#### program type ####
  
type
  # TODO make value type
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

proc uniform(location: Location, mat: Mat4d) =
  var mat_var = mat.mat4f
  glUniformMatrix4fv(location.index, 1, false, cast[ptr GLfloat](mat_var.addr))

proc uniform(location: Location, mat: Mat4f) =
  var mat_var = mat
  glUniformMatrix4fv(location.index, 1, false, cast[ptr GLfloat](mat_var.addr))

proc uniform(location: Location, value: float32) =
  glUniform1f(location.index, value)

proc uniform(location: Location, value: float64) =
  glUniform1f(location.index, value)

proc uniform(location: Location, value: int32) =
  glUniform1i(location.index, value)

proc uniform(location: Location; value: Vec2f) =
  glUniform2f(location.index, value[0], value[1])

proc uniform(location: Location, value: Vec3f) =
  glUniform3f(location.index, value[0], value[1], value[2])

proc uniform(location: Location, value: Vec4f) =
  glUniform4f(location.index, value[0], value[1], value[2], value[3])

proc uniform(location: Location, value: bool) =
  glUniform1i(location.index, value.GLint)


proc uniform(program: Program; location: Location; mat: Mat4d) =
  var mat_var = mat.mat4f
  glProgramUniformMatrix4fv(program.handle, location.index, 1, false,
                            cast[ptr GLfloat](mat_var.addr))

proc uniform(program: Program; location: Location, mat: Mat4f) =
  var mat_var = mat
  glProgramUniformMatrix4fv(program.handle, location.index, 1, false,
                            cast[ptr GLfloat](mat_var.addr))

proc uniform(program: Program; location: Location, value: float32) =
  glProgramUniform1f(program.handle, location.index, value)

proc uniform(program: Program; location: Location, value: float64) =
  glProgramUniform1f(program.handle, location.index, value)

proc uniform(program: Program; location: Location, value: int32) =
  glProgramUniform1i(program.handle, location.index, value)

proc uniform(program: Program; location: Location, value: Vec2f) =
  glProgramUniform2f(program.handle, location.index, value[0], value[1])

proc uniform(program: Program; location: Location, value: Vec3f) =
  glProgramUniform3f(program.handle, location.index, value[0], value[1], value[2])

proc uniform(program: Program; location: Location, value: Vec4f) =
  glProgramUniform4f(program.handle, location.index, value[0], value[1], value[2], value[3])

proc uniform(program: Program; location: Location, value: bool) =
  glProgramUniform1i(program.handle, location.index, value.GLint)
  
#### Vertex Array Object ####

type VertexArrayObject* = object
    handle*: GLuint

proc newVertexArrayObject*() : VertexArrayObject =
  glCreateVertexArrays(1, cast[ptr GLuint](result.addr))

proc bindIt*(vao: VertexArrayObject): void =
  glBindVertexArray(vao.handle)
  
proc delete*(vao: VertexArrayObject) =
  glDeleteVertexArrays(1, vao.handle.unsafeAddr)

proc divisor(vao: VertexArrayObject; binding: Binding; divisor: GLuint) : void =
  when false:
    glVertexArrayVertexBindingDivisorEXT(vao.handle, location.index, divisor)
  else:
    glVertexArrayBindingDivisor(vao.handle, binding.index, divisor)
    
proc enableAttrib(vao: VertexArrayObject, location: Location) : void =
  if location.index >= 0:
    when false:
      glEnableVertexArrayAttribEXT(vao.handle, location.index)
    else:
      glEnableVertexArrayAttrib(vao.handle, location.index.GLuint)

#proc divisor(vao: VertexArrayObject, index: GLuint) : GLuint =

template blockBind*(vao: VertexArrayObject, blk: untyped) : untyped =
  vao.bindIt
  blk
  nil_vao.bindIt

#### Array Buffers ####

type
  ArrayBuffer*[T]        = object
    handle*: GLuint
  ElementArrayBuffer*[T] = object
    handle*: GLuint
  UniformBuffer*[T]      = object
    handle*: GLuint

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

proc bindIt*(vao: VertexArrayObject; indices: ElementArrayBuffer): void =
  glVertexArrayElementBuffer(vao.handle, indices.handle)
    
proc createArrayBuffer*[T](len: int, usage: GLenum = GL_STATIC_DRAW): ArrayBuffer[T] =
  result.create
  when false:
    glNamedBufferDataEXT(result.handle, len * GLsizeiptr(sizeof(T)), nil, usage)
  else:
    glNamedBufferData(result.handle, len * GLsizeiptr(sizeof(T)), nil, usage)

proc createElementArrayBuffer*[T](len: int, usage: GLenum = GL_STATIC_DRAW): ElementArrayBuffer[T] =
  result.create
  when false:
    glNamedBufferDataEXT(result.handle, len * GLsizeiptr(sizeof(T)), nil, usage)
  else:
    glNamedBufferData(result.handle, len * GLsizeiptr(sizeof(T)), nil, usage)
  
proc createUniformBuffer*[T](usage: GLenum = GL_STATIC_DRAW): UniformBuffer[T] =
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

template bindBlock*[T](buffer : AnyBuffer[T], blk:untyped) =
  let buf = buffer
  var outer : GLint
  glGetIntegerv(buf.bindingKind, outer.addr)
  buf.bindIt
  blk
  glBindBuffer(buf.bufferKind, GLuint(outer))
  
proc bufferData*[T](buffer: SeqLikeBuffer[T], data: openarray[T], usage: GLenum) =
  if buffer.handle.int > 0:
    when false:
      glNamedBufferDataEXT(buffer.handle, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)
    else:
      glNamedBufferData(buffer.handle, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)

proc bufferData*[T](buffer: SeqLikeBuffer[T], dataview: DataView[T], usage: GLenum) =
  if buffer.handle.int > 0:
    when false:
      glNamedBufferDataEXT( buffer.handle, GLsizeiptr(dataview.len * sizeof(T)), dataview.data, usage)
    else:
      glNamedBufferData( buffer.handle, GLsizeiptr(dataview.len * sizeof(T)), dataview.data, usage)
      
proc bufferData*[T](buffer: UniformBuffer[T], data: T, usage: GLenum) =
  if buffer.handle.int > 0:
    when false:
      glNamedBufferDataEXT(buffer.handle, GLsizeiptr(sizeof(T)), unsafeAddr(data), usage)
    else:
      glNamedBufferData(buffer.handle, GLsizeiptr(sizeof(T)), unsafeAddr(data), usage)

proc setData*[T](buffer: SeqLikeBuffer[T], data: openarray[T]) =
  if buffer.handle.int > 0:
    glNamedBufferSubData(buffer.handle, 0, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]))

proc setData*[T](buffer: UniformBuffer[T], data: T) =
  if buffer.handle.int > 0:
    glNamedBufferSubData(buffer.handle, 0, GLsizeiptr(sizeof(T)), unsafeAddr(data))      


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

proc byteSize*[T](buffer: ArrayBuffer[T]) : int =
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
  result.size = buffer.len

proc mapWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): WriteView[T] =
  when false:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.handle, GL_WRITE_ONLY))
  else:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBuffer(buffer.handle, GL_WRITE_ONLY))
  result.size = buffer.len

proc mapReadWrite*[T](buffer: ArrayBuffer[T] | ElementArrayBuffer[T]): DataView[T] =
  when false:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBufferEXT(buffer.handle, GL_READ_WRITE))
  else:
    result.data = cast[ptr UncheckedArray[T]](glMapNamedBuffer(buffer.handle, GL_READ_WRITE))
  result.size = buffer.len

template mapReadBlock*(buffer: ArrayBuffer[auto], blck: untyped) : untyped =
  block:
    let mappedBuffer {. inject .} = buffer.mapRead
    defer:
      discard buffer.unmap
    blck

template mapWriteBlock*(buffer: ArrayBuffer[auto], blck: untyped) : untyped =
  block:
    let mappedBuffer {. inject .} = buffer.mapWrite
    defer:
      discard buffer.unmap
    blck

template mapReadWriteBlock*(buffer: ArrayBuffer[auto], blck: untyped) : untyped =
  block:
    let mappedBuffer {. inject .} = buffer.mapReadWrite
    defer:
      discard buffer.unmap
    blck

proc arrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW): ArrayBuffer[T] =
  result.create
  result.bufferData(data, usage)

proc arrayBuffer*[T](data : DataView[T], usage: GLenum = GL_STATIC_DRAW): ArrayBuffer[T] =
  if not data.isNil:
    result.create
    result.bufferData(data, usage)

proc elementArrayBuffer*[T](data : openarray[T], usage: GLenum = GL_STATIC_DRAW): ElementArrayBuffer[T] =
  result.create
  result.bufferData(data, usage)

proc elementArrayBuffer*[T](data : DataView[T], usage: GLenum = GL_STATIC_DRAW): ElementArrayBuffer[T] =
  if not data.isNil:
    result.create
    result.bufferData(data, usage)

proc uniformBuffer*[T](data : T, usage: GLenum = GL_STATIC_DRAW): UniformBuffer[T] =
  result.create
  result.bufferData(data, usage)

#### ArrayBufferView

type
  ArrayBufferView*[S,T] = object
    buffer*: ArrayBuffer[S]
    offset*, stride*: int

proc len*(ab: ArrayBufferView): int = ab.buffer.len
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

  assert testArrayBuffer.view(a) == ArrayBufferView[TestType,int](buffer: ArrayBuffer[TestType](handle: 7), offset: 0, stride: 16)
  assert testArrayBuffer.view(b) == ArrayBufferView[TestType,int](buffer: ArrayBuffer[TestType](handle: 7), offset: 4, stride: 16)
  assert testArrayBuffer.view(c) == ArrayBufferView[TestType,int](buffer: ArrayBuffer[TestType](handle: 7), offset: 8, stride: 16)
  assert testArrayBuffer.view(d) == ArrayBufferView[TestType,int](buffer: ArrayBuffer[TestType](handle: 7), offset:12, stride: 16)
  
#### shader

proc shaderSource(shader: Shader, source: string) =
  var source_array: array[1, string] = [source]
  var c_source_array = allocCStringArray(source_array)
  defer: deallocCStringArray(c_source_array)
  glShaderSource(shader.handle, 1, c_source_array, nil)

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

proc showError(log: string, source: string): void =
  let lines = source.splitLines
  var problems = newSeq[tuple[lineNr: int, message: string]](0)
  # matches on intel driver
  for match in log.findIter(re"(\d+)\((\d+)\): ([^:]*): (.*)"):
    let lineNr = match.captures[0].parseInt
    let notTheColumn = match.captures[1].parseInt
    let kind: string = match.captures[2]
    let message: string = match.captures[3]
    problems.add( (lineNr, message) )
  # matches on nvidia driver
  for match in log.findIter(re"(\d+)\((\d+)\) : ([^:]*): (.*)"):
    let lineNr = match.captures[1].parseInt
    let notTheColumn = match.captures[0].parseInt
    let kind: string = match.captures[2]
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
  stdout.styledWriteLine(fgGreen, "------------------------------------------------------------------")
  stdout.styledWriteLine(fgGreen, "==== end Shader Problems =========================================")
  

proc programInfoLog(program: Program): string =
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

proc linkShader*(shaders: varargs[Shader]): Program =
  result.handle = glCreateProgram()

  for shader in shaders:
    glAttachShader(result.handle, shader.handle)
    glDeleteShader(shader.handle)
  glLinkProgram(result.handle)

  if not result.linkStatus:
    echo "Log: ", result.programInfoLog
    glDeleteProgram(result.handle)
    result.handle = 0

proc use*(program: Program): void =
  glUseProgram(program.handle)

proc uniformLocation(program: Program, name: string) : Location =
  result.index = glGetUniformLocation(program.handle, name)

proc attributeLocation(program: Program, name: string) : Location =
  result.index = glGetAttribLocation(program.handle, name)

proc readPixel*(x,y: int) : Color =
  let 
    w = 1.GLsizei
    h = 1.GLsizei
    f = GL_RGBA
    t = GL_UNSIGNED_BYTE
    d = result.addr.pointer
  glReadPixels(x.GLint,y.GLint,w,h,f,t,d)

  
