import opengl, glm, math, strutils, nre, macros, macroutils, sdl2, sdl2/image

################################################################################
#### macro utils ###############################################################
################################################################################

macro debugResult(arg: typed) : stmt =
  echo arg.repr
  arg

include glm_additions, shapes, samplers, framebuffer

#### blub ####

# returns a string, and true if it is a sample type
proc glslUniformType(value : NimNode): (string, bool) =
  let tpe = value.getType2
  if tpe.kind == nnkBracketExpr:
    case $tpe[0]
    of "Mat4x4":
      ("mat4", false)
    of "Mat3x3":
      ("mat3", false)
    of "Mat2x2":
      ("mat2", false)
    of "Vec4":
      ("vec4", false)
    of "Vec3":
      ("vec3", false)
    of "Vec2":
      ("vec2", false)
    else:
      ("(unknown:" & $tpe[0] & ")", false)
  else:
    case $tpe
    of "Texture1D":
      ("sampler1D", true)
    of "Texture2D":
      ("sampler2D", true)
    of "Texture3D":
      ("sampler3D", true)
    of "TextureRectangle":
      ("sampler2DRect", true)
    of "float32", "float64", "float":
      ("float", false)
    of "Mat4d", "Mat4f":
      ("mat4", false)
    of "Mat3d", "Mat3f":
      ("mat3", false)
    of "Mat2d", "Mat2f":
      ("mat2", false)
    of "Vec4d", "Vec4f":
      ("vec4", false)
    of "Vec3d", "Vec3f":
      ("vec3", false)
    of "Vec2d", "Vec2f":
      ("vec2", false)
    else:
      (($tpe).toLower, false)

proc glslAttribType(value : NimNode): string =
  # result = getAst(glslAttribType(value))[0].strVal
  let tpe = value.getType2

  if $tpe[0] == "seq" or $tpe[0] == "ArrayBuffer":
    tpe[1].glslUniformType[0]
  else:
    echo "not a compatible attribType: "
    echo tpe.repr
    "(error not a seq[..])"

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
  glBufferData(GL_ARRAY_BUFFER, GLsizeiptr(data.len * sizeof(T)), unsafeAddr(data[0]), usage)

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

proc compileTest() =
  var mb = MappedReadWriteBuffer[int](data:nil, size:1)
  let i : int = mb[0] # read
  mb[0] += 17         # modify
  mb[0] = 18          # write(error)


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


####################################################################################
#### etc ###########################################################################
####################################################################################

type ShaderParam* = tuple[name: string, gl_type: string]

const
  sourceHeader = """
#version 330
#define M_PI 3.1415926535897932384626433832795
"""

  screenTriagleVertexSource = """
#version 330

const vec4 positions[3] = vec4[](
  vec4(-1.0, -1.0, 1.0, 1.0),
  vec4( 3.0, -1.0, 1.0, 1.0),
  vec4(-1.0,  3.0, 1.0, 1.0)
);

const vec2 texCoords[3] = vec2[](
  vec2(0.0, 0.0),
  vec2(2.0, 0.0),
  vec2(0.0, 2.0)
);

out vec2 texCoord;

void main() {
  gl_Position = positions[gl_VertexID];
  texCoord = texCoords[gl_VertexID];
}
"""

proc `$`(args: openArray[string]) : string =
  result = "["
  for arg in args:
    result.add("\"" & arg & "\",")
  result.add("]")

macro printVar(args: varargs[untyped]) : stmt =

  result = newStmtList()
  for arg in args:
    result.add newCall(bindSym"echo", newLit($arg.ident & ": "), arg)

proc genShaderSource(
    sourceHeader: string,
    uniforms : openArray[string],
    inParams : openArray[string], arrayLength: int,  # for geometry shader, -1 otherwise
    outParams: openArray[string],
    includes: openArray[string], mainSrc: string): string =

  result = sourceHeader

  for i, u in uniforms:
    result.add( u & ";\n" )
  for i, paramRaw in inParams:
    let param = paramRaw.replaceWord("out", "in")
    if arrayLength >= 0:
      result.add format("$1[$2];\n", param, arrayLength)
    else:
      result.add(param & ";\n")
  for param in outParams:
    result.add(param & ";\n")
  for incl in includes:
    result.add incl

  result.add("void main() {\n")
  result.add(mainSrc)
  result.add("\n}\n")


proc forwardVertexShaderSource(sourceHeader: string,
    attribNames, attribTypes : openArray[string] ): string =

  result = sourceHeader
  for i, name in attribNames:
    let tpe = attribTypes[i]
    result.add("in " & tpe & " " & name & ";\n")

  result.add("\nout VertexData {\n")
  for i, name in attribNames:
    let tpe = attribTypes[i]
    result.add(tpe & " " & name & ";\n")
  result.add("} VertexOut;\n")

  result.add("\nvoid main() {\n")
  for name in attribNames:
    result.add("VertexOut." & name & " = " & name & ";\n")
  result.add("}\n")

  echo "forwardVertexShaderSource:\n", result


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

proc attribSize(t: typedesc[Vec4d]) : GLint = 4
proc attribType(t: typedesc[Vec4d]) : GLenum = cGL_DOUBLE
proc attribNormalized(t: typedesc[Vec4d]) : bool = false

proc attribSize(t: typedesc[Vec3d]) : GLint = 3
proc attribType(t: typedesc[Vec3d]) : GLenum = cGL_DOUBLE
proc attribNormalized(t: typedesc[Vec3d]) : bool = false

proc attribSize(t: typedesc[Vec2d]) : GLint = 2
proc attribType(t: typedesc[Vec2d]) : GLenum = cGL_DOUBLE
proc attribNormalized(t: typedesc[Vec2d]) : bool = false

proc attribSize(t: typedesc[Vec4f]) : GLint = 4
proc attribType(t: typedesc[Vec4f]) : GLenum = cGL_FLOAT
proc attribNormalized(t: typedesc[Vec4f]) : bool = false

proc attribSize(t: typedesc[Vec3f]) : GLint = 3
proc attribType(t: typedesc[Vec3f]) : GLenum = cGL_FLOAT
proc attribNormalized(t: typedesc[Vec3f]) : bool = false

proc attribSize(t: typedesc[Vec2f]) : GLint = 2
proc attribType(t: typedesc[Vec2f]) : GLenum = cGL_FLOAT
proc attribNormalized(t: typedesc[Vec2f]) : bool = false

proc makeAndBindBuffer[T](buffer: var ArrayBuffer[T], index: GLint) =
  if index >= 0:
    buffer.create
    buffer.bindIt
    glVertexAttribPointer(index.GLuint, attribSize(T), attribType(T), attribNormalized(T), 0, nil)

proc bindAndAttribPointer[T](buffer: ArrayBuffer[T], index: GLint) =
  if index >= 0:
    buffer.bindIt
    glVertexAttribPointer(index.GLuint, attribSize(T), attribType(T), attribNormalized(T), 0, nil)

proc makeAndBindElementBuffer[T](buffer: var ElementArraybuffer[T]) =
  buffer.create
  buffer.bindIt

proc myEnableVertexAttribArray(vao: VertexArrayObject, index: GLint, divisorval: GLuint): void =
  if index >= 0:
    vao.enableAttrib(index.GLuint)
    vao.divisor(index.GLuint, divisorval)

template renderBlockTemplate(numLocations: int, globalsBlock, linkShaderBlock, bufferCreationBlock,
               initUniformsBlock, setUniformsBlock, drawCommand: expr): stmt {. dirty .} =
  block:
    var vao {.global.}: VertexArrayObject
    var glProgram {.global.}: GLuint  = 0
    var locations {.global.}: array[numLocations, GLint]

    globalsBlock

    if glProgram == 0:

      gl_program = linkShaderBlock
      glUseProgram(gl_program)

      initUniformsBlock

      vao = newVertexArrayObject()
      vao.bindIt

      bufferCreationBlock

      glBindBuffer(GL_ARRAY_BUFFER, 0)

      nil_vao.bindIt

      glUseProgram(0)

      #for i, loc in locations:
      #  echo "location(", i, "): ", loc

    glUseProgram(gl_program)

    bindIt(vao)

    setUniformsBlock

    drawCommand

    bindIt(nil_vao)
    glUseProgram(0);

################################################################################
## Shading Dsl #################################################################
################################################################################

proc attribute[T](name: string, value: T, divisor: GLuint) : int = 0
proc attributes(args : varargs[int]) : int = 0
proc shaderArg[T](name: string, value: T): int = 0
proc uniforms(args: varargs[int]): int = 0
proc vertexOut(args: varargs[string]): int = 0
proc geometryOut(args: varargs[string]): int = 0
proc fragmentOut(args: varargs[string]): int = 0
proc vertexMain(src: string): int = 0
proc fragmentMain(src: string): int = 0
proc geometryMain(layout, src: string): int = 0
proc includes(args: varargs[int]): int = 0
proc incl(arg: string): int = 0
proc numVertices(num: GLSizei): int = 0
proc numInstances(num: GLSizei): int = 0

################################################################################
## Shading Dsl Inner ###########################################################
################################################################################

macro shadingDslInner(mode: GLenum, fragmentOutputs: static[openArray[string]], statement: varargs[int] ) : stmt =

  var numSamplers = 0
  var numLocations = 0
  var uniformsSection : seq[string] = @[]
  var initUniformsBlock = newStmtList()
  var setUniformsBlock = newStmtList()
  var attribNames = newSeq[string](0)
  var attribTypes = newSeq[string](0)
  #var attributesSection : seq[object(name:string, tpe: string)] = @[]
  var globalsBlock = newStmtList()
  var bufferCreationBlock = newStmtList()
  var vertexOutSection : seq[string] = @[]
  var geometryOutSection : seq[string] = @[]
  var fragmentOutSection : seq[string] = @[]
  for i,fragout in fragmentOutputs:
    fragmentOutSection.add format("layout(location = $1) out vec4 $2", $i, fragout)
  var includesSection : seq[string] = @[]
  var vertexMain: string
  var geometryLayout: string
  var geometryMain: string
  var fragmentMain: string
  var hasIndices = false
  var indexType: NimNode = nil
  var numVertices, numInstances: NimNode
  var hasInstanceData = false

  #### BEGIN PARSE TREE ####

  proc locations(i: int) : NimNode =
    newTree(nnkBracketExpr, !!"locations", newLit(i))

  for call in statement.items:
    call.expectKind nnkCall
    case $call[0]
    of "numVertices":
      numVertices = call[1]

    of "numInstances":
      numInstances = call[1]

    of "uniforms":
      for innerCall in call[1][1].items:
        innerCall[1].expectKind nnkStrLit
        let name = $innerCall[1]
        let value = innerCall[2]


        let (glslType, isSample) = value.glslUniformType
        let baseString = "uniform " & glslType & " " & name

        initUniformsBlock.add( newAssignment(
          locations(numLocations),
          newCall( bindSym"glGetUniformLocation", !!"glProgram", newLit(name) )
        ))

        if isSample:
          initUniformsBlock.add( newCall( bindSym"glUniform1i", locations(numLocations), newLit(numSamplers) ) )

          proc activeTexture(texture: int): void =
            glActiveTexture( (GL_TEXTURE0 + texture).GLenum )

          setUniformsBlock.add( newCall( bindSym"activeTexture", newLit(numSamplers) ) )
          setUniformsBlock.add( newCall( bindSym"bindIt", value ) )
          numSamplers += 1
        else:
          setUniformsBlock.add( newCall( bindSym"uniform", locations(numLocations), value ) )

        uniformsSection.add( baseString )

        numLocations += 1

    of "attributes":
      for innerCall in call[1][1].items:
        innerCall[1].expectKind nnkStrLit
        let name = $innerCall[1]
        let value = innerCall[2]
        let divisor: int =
          if innerCall[3].kind == nnkHiddenStdConv:
            innerCall[3][1].intVal.int
          elif innerCall[3].kind == nnkIntLit:
            innerCall[3].intVal.int
          else:
            0

        let buffername = !(name & "Buffer")

        let isAttrib = name != "indices"
        #echo "attribute ", value.glslAttribType, " ", name

        if not isAttrib:
          if hasIndices:
            echo "error, has already indices"

          hasIndices = true

          case value.getType2[1].typeKind
          of ntyInt8, ntyUInt8:
            indexType = bindSym"GL_UNSIGNED_BYTE"
          of ntyInt16, ntyUInt16:
            indexType = bindSym"GL_UNSIGNED_SHORT"
          of ntyInt32, ntyUInt32:
            indexType = bindSym"GL_UNSIGNED_INT"
          of ntyInt, ntyUInt:
            echo "error int type has to be explicity sized uint8 uint16 or uint32"
          of ntyInt64, ntyUInt64:
            echo "error 64 bit indices not supported"
          else:
            echo "error unknown type kind: ", value.getType2[1].typeKind


        template foobarTemplate( lhs, rhs, bufferType : expr ) : stmt {.dirty.} =
          var lhs {.global.}: bufferType[rhs[0].type]

        let isSeq:bool = $value.getType2[0] == "seq"

        if isSeq:
          let bufferType =
            if isAttrib:
              bindSym"ArrayBuffer"
            else:
              bindSym"ElementArrayBuffer"

          globalsBlock.add(getAst(foobarTemplate( !! buffername, value, bufferType )))

        let attribCount = attribNames.len

        if isAttrib:
          bufferCreationBlock.add( newAssignment(
            locations(numLocations),
            newCall( bindSym"glGetAttribLocation", !! "glProgram", newLit(name) )
          ))

          bufferCreationBlock.add(newCall(bindSym"myEnableVertexAttribArray", !!"vao", locations(numLocations), newLit(divisor)))

        if isSeq:
          if isAttrib:
            bufferCreationBlock.add(newCall(bindSym"makeAndBindBuffer",
              !! buffername,
              locations(numLocations),
            ))

          else:
            bufferCreationBlock.add(newCall(bindSym"makeAndBindElementBuffer",
              !! buffername,
            ))

          setUniformsBlock.add(newCall(bindSym"bindIt", !! buffername))
          setUniformsBlock.add(newCall(bindSym"bufferData", !! buffername, bindSym"GL_STREAM_DRAW", value))

        else:
          if isAttrib:
            bufferCreationBlock.add(newCall(bindSym"bindAndAttribPointer",
              value,
              locations(numLocations),
            ))
          else:
            bufferCreationBlock.add(newCall(bindSym"bindIt", value))

        if isAttrib:
          attribNames.add( name )
          attribTypes.add( value.glslAttribType )
          # format("in $1 $2", value.glslAttribType, name) )
          numLocations += 1

    of "vertexOut":
      #echo "vertexOut"

      for innerCall in call[1][1].items:
        vertexOutSection.add( innerCall.strVal )

    of "geometryOut":

      for innerCall in call[1][1].items:
        geometryOutSection.add( innerCall.strVal )

    of "fragmentOut":

      fragmentOutSection = @[]
      for innerCall in call[1][1].items:
        fragmentOutSection.add( innerCall.strVal )

    of "includes":

      for innerCall in call[1][1].items:
        if innerCall[1].kind == nnkSym:
          let sym = innerCall[1].symbol
          includesSection.add(sym.getImpl.strVal)


    of "vertexMain":
      vertexMain = call[1].strVal

    of "fragmentMain":
      fragmentMain = call[1].strVal

    of "geometryMain":

      geometryLayout = call[1].strVal
      geometryMain = call[2].strVal

    else:
      echo "unknownSection"
      echo call.repr


  if hasIndices and indexType == nil:
    error "has indices, but index Type was never set to anything"

  var vertexShaderSource : string

  if vertexMain == nil and geometryMain == nil:

    if vertexOutSection.len > 0:
      error("cannot create implicit screen space quad renderer with vertex out section")

    vertexShaderSource = screenTriagleVertexSource
    vertexOutSection.add("out vec2 texCoord")


  elif vertexMain == nil:
    vertexShaderSource = forwardVertexShaderSource(sourceHeader, attribNames, attribTypes)


    vertexOutSection.newSeq(attribNames.len)
    for i in 0..<attribNames.len:
       vertexOutSection[i] = format("out $1 $2", attribTypes[i], attribNames[i])

  else:
    var attributesSection = newSeq[string](attribNames.len)
    for i in 0..<attribNames.len:
       attributesSection[i] = format("in $1 $2", attribTypes[i], attribNames[i])

    vertexShaderSource = genShaderSource(sourceHeader, uniformsSection, attributesSection, -1, vertexOutSection, includesSection, vertexMain)

  var linkShaderBlock : NimNode

  if geometryMain == nil:

    let fragmentShaderSource = genShaderSource(sourceHeader, uniformsSection, vertexOutSection, -1, fragmentOutSection, includesSection, fragmentMain)

    linkShaderBlock = newCall( bindSym"linkShader",
      newCall( bindSym"compileShader", bindSym"GL_VERTEX_SHADER", newLit(vertexShaderSource) ),
      newCall( bindSym"compileShader", bindSym"GL_FRAGMENT_SHADER", newLit(fragmentShaderSource) ),
    )

  else:
    let geometryHeader = format("$1\nlayout($2) in;\n$3;\n", sourceHeader, geometryPrimitiveLayout(mode.intVal.GLenum), geometryLayout)
    let geometryShaderSource = genShaderSource(geometryHeader, uniformsSection, vertexOutSection, geometryNumVerts(mode.intVal.GLenum), geometryOutSection, includesSection, geometryMain)
    let fragmentShaderSource = genShaderSource(sourceHeader, uniformsSection, geometryOutSection, -1, fragmentOutSection, includesSection, fragmentMain)

    linkShaderBlock = newCall( bindSym"linkShader",
      newCall( bindSym"compileShader", bindSym"GL_VERTEX_SHADER", newLit(vertexShaderSource) ),
      newCall( bindSym"compileShader", bindSym"GL_GEOMETRY_SHADER", newLit(geometryShaderSource) ),
      newCall( bindSym"compileShader", bindSym"GL_FRAGMENT_SHADER", newLit(fragmentShaderSource) ),
    )



  let drawCommand =
    if hasIndices:
      if numInstances != nil:
        newCall( bindSym"glDrawElementsInstanced", mode, numVertices, indexType, newNilLit(), numInstances )
      else:
        newCall( bindSym"glDrawElements", mode, numVertices, indexType, newNilLit() )

    else:
      if numInstances != nil:
        newCall( bindSym"glDrawArraysInstanced", mode, newLit(0), numVertices, numInstances )
      else:
        newCall( bindSym"glDrawArrays", mode, newLit(0), numVertices )

  result = getAst(renderBlockTemplate(numLocations, globalsBlock, linkShaderBlock,
         bufferCreationBlock, initUniformsBlock, setUniformsBlock, drawCommand))

  # result = newCall( bindSym"debugResult", result )

################################################################################
## Shading Dsl Outer ###########################################################
################################################################################

macro shadingDsl*(mode:GLenum, statement: stmt) : stmt {.immediate.} =

  result = newCall(bindSym"shadingDslInner", mode, !! "fragmentOutputs" )
  # numVertices = result[2]
  # numInstances = result[3]

  for section in statement.items:
    section.expectKind({nnkCall, nnkAsgn})

    if section.kind == nnkAsgn:
      section.expectLen(2)
      let ident = section[0]
      ident.expectKind nnkIdent
      case $ident.ident
      of "numVertices":
        result.add( newCall(bindSym"numVertices", section[1] ) )
      of "numInstances":
        result.add( newCall(bindSym"numInstances", section[1] ) )
      else:
        error("unknown named parameter " & $ident.ident)

    elif section.kind == nnkCall:
      let ident = section[0]
      ident.expectKind nnkIdent
      let stmtList = section[1]
      stmtList.expectKind nnkStmtList

      case $ident
      of "uniforms":
        let uniformsCall = newCall(bindSym"uniforms")

        for capture in stmtList.items:
          capture.expectKind({nnkAsgn, nnkIdent})
          if capture.kind == nnkAsgn:
            capture.expectLen 2
            capture[0].expectKind nnkIdent
            uniformsCall.add( newCall(bindSym"shaderArg", newLit($capture[0]), capture[1] ) )
          elif capture.kind == nnkIdent:
            uniformsCall.add( newCall(bindSym"shaderArg",  newLit($capture), capture) )

        result.add(uniformsCall)

      of "attributes":
        let attributesCall = newCall(bindSym"attributes")

        proc handleCapture(attributesCall, capture: NimNode, divisor: int) =
          capture.expectKind({nnkAsgn, nnkIdent})
          if capture.kind == nnkAsgn:
            capture.expectLen 2
            capture[0].expectKind nnkIdent
            attributesCall.add( newCall(bindSym"attribute", newLit($capture[0]), capture[1], newLit(divisor) ) )
          elif capture.kind == nnkIdent:
            attributesCall.add( newCall(bindSym"attribute",  newLit($capture), capture, newLit(divisor)) )


        for capture in stmtList.items:
          if capture.kind == nnkCall:
            if $capture[0] == "instanceData":
              let stmtList = capture[1]
              stmtList.expectKind nnkStmtList
              for capture in stmtList.items:
                handleCapture(attributesCall, capture, 1)

            else:
              echo "error expected call to instanceData, but got: ", capture.repr
          else:
            handleCapture(attributesCall, capture, 0)

        result.add(attributesCall)

      of "vertexOut", "geometryOut", "fragmentOut":

        let outCall =
          case $ident
          of "vertexOut": newCall(bindSym"vertexOut")
          of "geometryOut": newCall(bindSym"geometryOut")
          of "fragmentOut": newCall(bindSym"fragmentOut")
          else: nil

        for section in stmtList.items:
          section.expectKind({nnkVarSection, nnkStrLit, nnkTripleStrLit})
          case section.kind
          of nnkVarSection:
            for def in section:
              def.expectKind nnkIdentDefs
              def[0].expectKind nnkIdent
              def[1].expectKind nnkIdent
              outCall.add format("out $2 $1", $def[0], $def[1]).newLit
          of nnkStrLit:
            outCall.add section
          of nnkTripleStrLit:
            for line in section.strVal.splitLines:
              outCall.add line.strip.newLit
          else:
            error("unreachable")


        result.add(outCall)

      of "vertexMain":
        stmtList.expectLen(1)
        stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
        result.add( newCall(bindSym"vertexMain", stmtList[0]) )

      of "geometryMain":
        stmtList.expectLen(2)
        stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
        stmtList[1].expectKind({nnkTripleStrLit, nnkStrLit})
        result.add( newCall(bindSym"geometryMain", stmtList[0], stmtList[1]) )

      of "fragmentMain":
        stmtList.expectLen(1)
        stmtList[0].expectKind({ nnkTripleStrLit, nnkStrLit })
        result.add( newCall(bindSym"fragmentMain", stmtList[0]) )

      of "includes":
        let includesCall = newCall(bindSym"includes")

        for statement in stmtList:
          statement.expectKind( nnkIdent )

          includesCall.add( newCall(bindSym"incl", statement) )

        result.add(includesCall)
      else:
        error("unknown section " & $ident.ident)
