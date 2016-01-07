import opengl, glm, strutils, nre, macros, sdl2, sdl2/image

#### glm additions ####

type Vec4f* = Vec4[float32]
type Vec3f* = Vec3[float32]
type Vec2f* = Vec2[float32]
type Vec4d* = Vec4[float64]
type Vec3d* = Vec3[float64]
type Vec2d* = Vec2[float64]

type Mat4f* = Mat4x4[float32]
type Mat3f* = Mat3x3[float32]
type Mat2f* = Mat2x2[float32]
type Mat4d* = Mat4x4[float64]
type Mat3d* = Mat3x3[float64]
type Mat2d* = Mat2x2[float64]

proc mat4f*(mat: Mat4d): Mat4f =
  for i in 0..<4:
   for j in 0..<4:
     result[i][j] = mat[i][j]

proc I4*() : Mat4d = mat4x4(
  vec4(1.0, 0, 0, 0),
  vec4(0.0, 1, 0, 0),
  vec4(0.0, 0, 1, 0),
  vec4(0.0, 0, 0, 1)
)

proc I4f*() : Mat4f = mat4x4[float32](
  vec4[float32](1.0, 0, 0, 0),
  vec4[float32](0.0, 1, 0, 0),
  vec4[float32](0.0, 0, 1, 0),
  vec4[float32](0.0, 0, 0, 1)
)

#### Sampler Types ####

macro nilName(name:expr) : expr =
  name.expectKind(nnkIdent)
  newIdentNode("nil_" & $name)

template textureTypeTemplate(name, nilName, target:expr, shadername:string): stmt =
  type name* = distinct GLuint
  const nilName* = name(0)
  proc bindIt*(texture: name) =
    glBindTexture(target, GLuint(texture))

template textureTypeTemplate(name: expr, target:expr, shadername:string): stmt =
  textureTypeTemplate(name, nilName(name), target, shadername)


textureTypeTemplate(Texture1D,                 nil_Texture1D,
    GL_TEXTURE_1D, "sampler1D")
textureTypeTemplate(Texture2D,                 nil_Texture2D,
    GL_TEXTURE_2D, "sampler2D")
textureTypeTemplate(Texture3D,                 nil_Texture3D,
    GL_TEXTURE_3D, "sampler3D")
textureTypeTemplate(Texture1DArray,             nil_Texture1DArray,
    GL_Texture_1D_ARRAY, "sampler1DArray")
textureTypeTemplate(Texture2DArray,            nil_Texture2DArray,
    GL_TEXTURE_2D_ARRAY, "sampler2DArray")
textureTypeTemplate(TextureRectangle,          nil_TextureRectangle,
    GL_TEXTURE_RECTANGLE, "sampler2DRect")
textureTypeTemplate(TextureCubeMap,            nil_TextureCubeMap,
    GL_TEXTURE_CUBE_MAP, "samplerCube")
textureTypeTemplate(TextureCubeMapArray,       nil_TextureCubeMapArray,
    GL_TEXTURE_CUBE_MAP_ARRAY , "samplerCubeArray")
textureTypeTemplate(TextureBuffer,             nil_TextureBuffer,
    GL_TEXTURE_BUFFER, "samplerBuffer")
textureTypeTemplate(Texture2DMultisample,      nil_Texture2DMultisample,
    GL_TEXTURE_2D_MULTISAMPLE, "sampler2DMS")
textureTypeTemplate(Texture2DMultisampleArray, nil_Texture2DMultisampleArray,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY, "sampler2DMSArray")


textureTypeTemplate(Texture1DShadow,        nil_Texture1DShadow,        GL_TEXTURE_1D,             "sampler1DShadow​")
textureTypeTemplate(Texture2DShadow,        nil_Texture2DShadow,        GL_TEXTURE_2D,             "sampler2DShadow​")
textureTypeTemplate(TextureCubeShadow,      nil_TextureCubeShadow,      GL_TEXTURE_CUBE_MAP,       "samplerCubeShadow​")
textureTypeTemplate(Texture2DRectShadow,    nil_Texture2DRectShadow,    GL_TEXTURE_RECTANGLE,      "sampler2DRectShadow​")
textureTypeTemplate(Texture1DArrayShadow,   nil_Texture1DArrayShadow,   GL_TEXTURE_1D_ARRAY,       "sampler1DArrayShadow​")
textureTypeTemplate(Texture2DArrayShadow,   nil_Texture2DArrayShadow,   GL_TEXTURE_2D_ARRAY,       "sampler2DArrayShadow​")
textureTypeTemplate(TextureCubeArrayShadow, nil_TextureCubeArrayShadow, GL_TEXTURE_CUBE_MAP_ARRAY, "samplerCubeArrayShadow​")


proc loadAndBindTextureRectangleFromFile*(filename: string): TextureRectangle =
  let surface = image.load(filename)
  defer: freeSurface(surface)
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)
  glGenTextures(1, cast[ptr GLuint](result.addr))
  result.bindIt()
  glTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGBA, surface2.w, surface2.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR)

proc loadAndBindTexture2DFromFile*(filename: string): Texture2D =
  let surface = image.load(filename)
  defer: freeSurface(surface)
  let surface2 = sdl2.convertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA8888, 0)
  defer: freeSurface(surface2)
  glGenTextures(1, cast[ptr GLuint](result.addr))
  result.bindIt()
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface2.w, surface2.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, surface2.pixels)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  glGenerateMipmap(GL_TEXTURE_2D)

#### nim -> glsl type mapping ####


# returns a string
proc glslUniformType(value : NimNode): (string, bool) =
  let tpe = value.getType2
  echo tpe.treeRepr
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
  let tpe = value.getType
  if $tpe[0] == "seq" or $tpe[0] == "ArrayBuffer":
    tpe[1].glslUniformType[0]
  else:
    "(error not a seq[..])"

#### Uniform ####

proc uniform*(location: GLint, mat: Mat4x4[float64]) =
  var mat_float32 = mat4f(mat)
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat_float32.addr))

proc uniform*(location: GLint, mat: var Mat4x4[float32]) =
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat.addr))

proc uniform*(location: GLint, value: float32) =
  glUniform1f(location, value)

proc uniform*(location: GLint, value: float64) =
  glUniform1f(location, value)

proc uniform*(location: GLint, value: int32) =
  glUniform1i(location, value)

proc uniform*(location: GLint, value: Vec2f) =
  glUniform2f(location, value[0], value[1])

proc uniform*(location: GLint, value: Vec3f) =
  glUniform3f(location, value[0], value[1], value[2])

proc uniform*(location: GLint, value: Vec4f) =
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

template blockBind*(vao: VertexArrayObject, blk: stmt) : stmt =
  vao.bindIt
  blk
  nil_vao.bindIt

#### Array Buffers ####

type ArrayBuffer*[T]        = distinct GLuint
type ElementArrayBuffer*[T] = distinct GLuint
type UniformBuffer*[T]      = distinct GLuint


proc newArrayBuffer*[T](): ArrayBuffer[T] =
  glGenBuffers(1, cast[ptr GLuint](result.addr))

proc newElementArrayBuffer*[T](): ElementArrayBuffer[T] =
  glGenBuffers(1, cast[ptr GLuint](result.addr))

proc newUniformBuffer*[T](): UniformBuffer[T] =
  glGenBuffers(1, cast[ptr GLuint](result.addr))


proc currentArrayBuffer*[T](): ArrayBuffer[T] =
  glGetIntegerv(GL_ARRAY_BUFFER_BINDING, cast[ptr GLint](result.addr))

proc currentElementArrayBuffer*[T](): ElementArrayBuffer[T] =
  glGetIntegerv(GL_ELEMENT_ARRAY_BUFFER_BINDING, cast[ptr GLint](result.addr))

proc currentUniformBuffer*[T](): UniformBuffer[T] =
  glGetIntegerv(GL_UNIFORM_BUFFER_BINDING, cast[ptr GLint](result.addr))


proc bindIt*[T](buffer: ArrayBuffer[T]) =
  glBindBuffer(GL_ARRAY_BUFFER, GLuint(buffer))

proc bindIt*[T](buffer: ElementArrayBuffer[T]) =
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, GLuint(buffer))

proc bindIt*[T](buffer: UniformBuffer[T]) =
  glBindBuffer(GL_UNIFORM_BUFFER, GLuint(buffer))


proc bufferData*[T](buffer: ArrayBuffer[T], data: var seq[T]) =
  glBufferData(GL_ARRAY_BUFFER, GLsizeiptr(data.len * sizeof(T)), data[0].addr, GL_STATIC_DRAW)

proc bufferData*[T](buffer: ElementArrayBuffer[T], data: seq[T]) =
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, GLsizeiptr(data.len * sizeof(T)), data[0].addr, GL_STATIC_DRAW)

proc bufferData*[T](buffer: UniformBuffer[T], data: T) =
  glBufferData(GL_ARRAY_BUFFER, GLsizeiptr(sizeof(T)), data.addr, GL_STATIC_DRAW)

#### etc ####

type ShaderParam* = tuple[name: string, gl_type: string]

let sourceHeader = """
#version 330
#extension GL_ARB_explicit_uniform_location : enable
#define M_PI 3.1415926535897932384626433832795
"""

proc genShaderSource*(
    uniforms : openArray[string], uniformLocations : bool,
    inParams : openArray[ShaderParam], inLocations : bool,
    outParams: openArray[ShaderParam],
    includes: openArray[string], mainSrc: string): string =
  result = sourceHeader
  for i, u in uniforms:
    if uniformLocations:
      result.add format("layout(location = $2) $1;\n", u, i)
    else:
      result.add( u & ";\n" )
  for i, a in inParams:
    if inLocations:
      result.add format("layout(location = $3) in $2 $1;\n", a.name, a.gl_type, i)
    else:
      result.add format("in $2 $1;\n", a.name, a.gl_type, i)
  for v in outParams:
    result.add format("out $2 $1;\n", v.name, v.gl_type)
  for incl in includes:
    result.add incl
  result.add("void main() {\n")
  result.add(mainSrc)
  result.add("\n}")

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

  echo "*****"
  echo source
  echo "*****"

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

proc makeAndBindBuffer[T](buffer: var ArrayBuffer[T], index: GLuint, value: var seq[T], usage: GLenum) =
  buffer = newArrayBuffer[T]()
  buffer.bindIt
  glBufferData(GL_ARRAY_BUFFER, GLsizeiptr(value.len * sizeof(T)), value[0].addr, usage)
  glVertexAttribPointer(index, attribSize(T), attribType(T), attribNormalized(T), 0, nil)

template renderBlockTemplate(globalsBlock, linkShaderBlock, bufferCreationBlock,
               initUniformsBlock, setUniformsBlock, drawCommand: expr): stmt {. dirty .} =
  block:
    var vao {.global.}: VertexArrayObject
    var glProgram {.global.}: GLuint  = 0

    globalsBlock

    if glProgram == 0:

      gl_program = linkShaderBlock
      glUseProgram(gl_program)

      initUniformsBlock

      vao = newVertexArrayObject()
      bindIt(vao)

      bufferCreationBlock

      glBindBuffer(GL_ARRAY_BUFFER, 0)
      bindIt(nil_vao)
      glUseProgram(0)

    glUseProgram(gl_program)

    bindIt(vao)

    setUniformsBlock

    drawCommand

    bindIt(nil_vao)
    glUseProgram(0);

################################################################################
## Shading Dsl #################################################################
################################################################################

proc shaderArg[T](name: string, value: T): int = 0
proc attributes(args : varargs[int]) : int = 0
proc uniforms(args: varargs[int]): int = 0
proc vertex_out(args: varargs[int]): int = 0
proc geometry_out(args: varargs[int]): int = 0
proc fragment_out(args: varargs[int]): int = 0
proc vertexMain(src: string): int = 0
proc fragmentMain(src: string): int = 0
proc geometryMain(src: string): int = 0
proc includes(args: varargs[int]): int = 0
proc incl(arg: string): int = 0

################################################################################
## Shading Dsl Inner ###########################################################
################################################################################

macro shadingDslInner(mode: GLenum, count: GLSizei, statement: varargs[typed] ) : stmt =
  var numSamplers = 0
  var uniformsSection : seq[string] = @[]
  var initUniformsBlock = newStmtList()
  var setUniformsBlock = newStmtList()
  var attributesSection : seq[ShaderParam] = @[]
  var globalsBlock = newStmtList()
  var bufferCreationBlock = newStmtList()
  var vertexOutSection : seq[ShaderParam] = @[]
  var geometryOutSection : seq[ShaderParam] = @[]
  var fragmentOutSection : seq[ShaderParam] = @[]
  var includesSection : seq[string] = @[]
  var vertexMain: string
  var geometryMain: string
  var fragmentMain: string

  #### BEGIN PARSE TREE ####

  echo "shadingDslInner"
  for call in statement.items:
    call.expectKind nnkCall
    case $call[0]
    of "uniforms":
      for innerCall in call[1][1].items:
        innerCall[1].expectKind nnkStrLit
        let name = $innerCall[1]
        let value = innerCall[2]


        let (glslType, isSample) = value.glslUniformType
        let baseString = "uniform " & glslType & " " & name
        if isSample:
          initUniformsBlock.add( newCall( bindSym"glUniform1i", newLit(uniformsSection.len), newLit(numSamplers) ) )

          proc activeTexture(texture: int): void =
            glActiveTexture( (GL_TEXTURE0 + texture).GLenum )

          setUniformsBlock.add( newCall( bindSym"activeTexture", newLit(numSamplers) ) )
          setUniformsBlock.add( newCall( bindSym"bindIt", value ) )
          numSamplers += 1
        else:
          setUniformsBlock.add( newCall( bindSym"uniform", newLit(uniformsSection.len), value ) )

        uniformsSection.add( baseString )
        #echo "uniform ", value.glslUniformType, " ", name

    of "attributes":
      for innerCall in call[1][1].items:
        innerCall[1].expectKind nnkStrLit
        let name = $innerCall[1]
        let value = innerCall[2]
        let buffername = !(name & "Buffer")

        echo "attribute ", value.glslAttribType, " ", name

        template foobarTemplate( lhs, rhs : expr ) : stmt {.dirty.} =
          var lhs {.global.}: ArrayBuffer[rhs[0].type]

        globalsBlock.add(getAst(foobarTemplate( newIdentNode(buffername), value )))

        let attribCount = attributesSection.len

        bufferCreationBlock.add(newCall(bindSym"glEnableVertexAttribArray", newLit(attribCount)))
        bufferCreationBlock.add(newCall(bindSym"makeAndBindBuffer",
            newIdentNode(buffername),
            newLit(attribCount),
            value,
            bindSym"GL_STATIC_DRAW"
        ))

        #let (size, gltype) = glVertexAttribPointerParam(value)
        #echo "size: ", size
        #echo "gltype: ", gltype
        #bufferCreationBlock.add newCall(bindSym"glVertexAttribPointer",
        #  newLit(attribCount), newLit(size), newLit(gltype.int), newLit(false), newLit(0), newNilLit() )
        attributesSection.add( (name: name, gl_type: value.glslAttribType) )


    of "vertex_out":
      echo "vertex_out"

      for innerCall in call[1][1].items:
        #echo "varying ", innerCall[2].strVal, " ",innerCall[1].strVal , ";"
        vertexOutSection.add( (name: innerCall[1].strVal, gl_Type: innerCall[2].strVal) )

    of "geometry_out":
      echo "geometry_out"

      for innerCall in call[1][1].items:
        geometryOutSection.add( (name: innerCall[1].strVal, gl_Type: innerCall[2].strVal) )


    of "fragment_out":
      echo "fragment_out"

      for innerCall in call[1][1].items:
        echo "out ", innerCall[2].strVal, " ", innerCall[1].strVal, ";"
        fragmentOutSection.add( (name: innerCall[1].strVal, gl_Type: innerCall[2].strVal) )


    of "includes":
      echo "includes"

      for innerCall in call[1][1].items:
        if innerCall[1].kind == nnkSym:
          let sym = innerCall[1].symbol
          echo sym.getImpl.strVal
          includesSection.add(sym.getImpl.strVal)


    of "vertexMain":
      echo "vertexMain"
      echo call[1].strVal

      vertexMain = call[1].strVal

    of "fragmentMain":
      echo "fragmentMain"
      echo call[1].strVal

      fragmentMain = call[1].strVal

    of "geometryMain":
      echo "geometryMain"
      echo call[1].strVal

      geometryMain = call[1].strVal

    else:
      echo "unknownSection"


  let vertexShaderSource = genShaderSource(uniformsSection, true, attributesSection, true, vertexOutSection, includesSection, vertexMain)

  var linkShaderBlock : NimNode

  if geometryMain == nil:

    let fragmentShaderSource = genShaderSource(uniformsSection, true, vertexOutSection, false, fragmentOutSection, includesSection, fragmentMain)

    linkShaderBlock = newCall( bindSym"linkShader",
      newCall( bindSym"compileShader", bindSym"GL_VERTEX_SHADER", newLit(vertexShaderSource) ),
      newCall( bindSym"compileShader", bindSym"GL_FRAGMENT_SHADER", newLit(fragmentShaderSource) ),
    )

  else:

    let geometryShaderSource = genShaderSource(uniformsSection, true, vertexOutSection, false, geometryOutSection, includesSection, geometryMain)
    let fragmentShaderSource = genShaderSource(uniformsSection, true, geometryOutSection, false, fragmentOutSection, includesSection, fragmentMain)


    linkShaderBlock = newCall( bindSym"linkShader",
      newCall( bindSym"compileShader", bindSym"GL_VERTEX_SHADER", newLit(vertexShaderSource) ),
      newCall( bindSym"compileShader", bindSym"GL_GEOMETRY_SHADER", newLit(geometryShaderSource) ),
      newCall( bindSym"compileShader", bindSym"GL_FRAGMENT_SHADER", newLit(fragmentShaderSource) ),
    )

  echo mode.repr
  let drawCommand = newCall( bindSym"glDrawArrays", mode, newLit(0), count )

  result = getAst(renderBlockTemplate(globalsBlock, linkShaderBlock, bufferCreationBlock,
                                      initUniformsBlock, setUniformsBlock, drawCommand))
  echo result.repr

################################################################################
## Shading Dsl Outer ###########################################################
################################################################################

macro shadingDsl*(mode:GLenum, count: GLsizei, statement: stmt) : stmt {.immediate.} =

  result = newCall(bindSym"shadingDslInner", mode, count)

  for section in statement.items:
    section.expectKind nnkCall
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

      for capture in stmtList.items:
        capture.expectKind({nnkAsgn, nnkIdent})

        if capture.kind == nnkAsgn:
          capture.expectLen 2
          capture[0].expectKind nnkIdent
          attributesCall.add( newCall(bindSym"shaderArg", newLit($capture[0]), capture[1] ) )
        elif capture.kind == nnkIdent:
          attributesCall.add( newCall(bindSym"shaderArg",  newLit($capture), capture) )

      result.add(attributesCall)

    of "vertex_out":
      let varyingsCall = newCall(bindSym"vertex_out")

      for varSec in stmtList.items:
        varSec.expectKind nnkVarSection
        for def in varSec:
          def.expectKind nnkIdentDefs
          def[0].expectKind nnkIdent
          def[1].expectKind nnkIdent
          varyingsCall.add( newCall(bindSym"shaderArg", newLit($def[0]) , newLit($def[1]) ) )

      result.add(varyingsCall)

    of "fragment_out":
      let fragOutCall = newCall(bindSym"fragment_out")

      for varSec in stmtList.items:
        varSec.expectKind nnkVarSection
        for def in varSec:
          def.expectKind nnkIdentDefs
          def.expectKind nnkIdentDefs
          def[0].expectKind nnkIdent
          def[1].expectKind nnkIdent
          fragOutCall.add newCall(bindSym"shaderArg", newLit($def[0]) , newLit($def[1]) )

      result.add(fragOutCall)

    of "vertex_prg":
      stmtList.expectLen(1)
      stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
      result.add( newCall(bindSym"vertexMain", stmtList[0]) )

    of "geometry_prg":
      stmtList.expectLen(1)
      stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
      result.add( newCall(bindSym"geometryMain", stmtList[0]) )

    of "fragment_prg":
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

