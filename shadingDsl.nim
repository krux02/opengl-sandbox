
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

proc bindAndAttribPointer[T](vao: VertexArrayObject, buffer: ArrayBuffer[T], location: Location) =
  if 0 <= location.index:
    let loc = location.index.GLuint
    glVertexArrayVertexBuffer(vao.handle, loc, buffer.handle, 0, GLsizei(sizeof(T)))
    glVertexArrayAttribFormat(vao.handle, loc, attribSize(T), attribType(T), attribNormalized(T), #[ relative offset ?! ]# 0);
    glVertexArrayAttribBinding(vao.handle, loc, loc)

proc bindAndAttribPointer[S,T](vao: VertexArrayObject; view: ArrayBufferView[S,T]; location: Location): void =
  if 0 <= location.index:
    let loc = location.index.GLuint
    glVertexArrayVertexBuffer(vao.handle, loc, view.buffer.handle, GLsizei(view.offset), GLsizei(view.stride))
    glVertexArrayAttribFormat(vao.handle, loc, attribSize(view.T), attribType(view.T), attribNormalized(view.T), #[ relative offset ?! ]# 0);
    glVertexArrayAttribBinding(vao.handle, loc, loc)

proc setBuffer[T](vao: VertexArrayObject; buffer: ArrayBuffer[T]; location: Location): void =
  if 0 <= location.index:
    let loc = location.index.GLuint
    glVertexArrayVertexBuffer(vao.handle, loc, buffer.handle, 0, GLsizei(sizeof(T)))
    
proc setBuffer(vao: VertexArrayObject; view: ArrayBufferView; location: Location): void =
  if 0 <= location.index:
    let loc = location.index.GLuint
    glVertexArrayVertexBuffer(vao.handle, loc, view.buffer.handle, GLsizei(view.offset), GLsizei(view.stride))
    
    
#ArrayBufferView*[S,T] = object
#    buffer*: ArrayBuffer[S]
#    offset*, stride*: int

proc binding(loc: Location): Binding =
  result.index = loc.index.GLuint
    
type
  RenderObject*[N: static[int]] = object
    vao*: VertexArrayObject
    program*: Program
    locations*: array[N, Location]
                  
##################################################################################
#### Shading Dsl #################################################################
##################################################################################

proc attribute[T](name: string, value: T, divisor: GLuint, glslType: string) : int = 0
proc attributes(args : varargs[int]) : int = 0
proc indices(arg: ElementArrayBuffer) : int = 0
proc shaderArg[T](name: string, value: T, glslType: string, isSampler: bool): int = 0
proc uniforms(args: varargs[int]): int = 0

# TODO add tag for output variables weather they are transform feedback, or not
  
proc vertexOut(args: varargs[string]): int = 0
proc geometryOut(args: varargs[string]): int = 0
proc fragmentOut(args: varargs[string]): int = 0
proc transformFeedbackVaryingNames(args: varargs[string]): int = 0
proc vertexMain(src: string): int = 0
proc fragmentMain(src: string): int = 0
proc geometryMain(layout, src: string): int = 0
proc includes(args: varargs[int]): int = 0
proc incl(arg: string): int = 0
proc numVertices(num: int): int = 0
proc numInstances(num: int): int = 0
proc vertexOffset(offset: int) : int = 0

##################################################################################
#### Shading Dsl Inner ###########################################################
##################################################################################
  
macro shadingDslInner(programIdent, vaoIdent: untyped; mode: GLenum; afterSetup, beforeRender, afterRender: untyped; fragmentOutputs: static[openArray[string]]; statement: varargs[int] ) : untyped =
  # initialize with number of global textures, as soon as that is supported
  var numSamplers = 0

  let program = if programIdent.kind == nnkNilLit: genSym(nskVar, "program") else: programIdent
  let vao = if vaoIdent.kind == nnkNilLit: genSym(nskVar, "vao") else: vaoIdent
  let locations = genSym(nskVar, "locations")

  var numLocations = 0
  var uniformsSection = newSeq[string](0)
  var initUniformsBlock = newStmtList()
  var drawBlock = newStmtList()
  var attribNames = newSeq[string](0)
  var attribTypes = newSeq[string](0)
  var bufferCreationBlock = newStmtList()
  var vertexOutSection = newSeq[string](0)
  var geometryOutSection = newSeq[string](0)
  var fragmentOutSection = newSeq[string](0)
  var transformFeedbackVaryingNames = newSeq[string](0)

  for i,fragout in fragmentOutputs:
    fragmentOutSection.add format("layout(location = $1) out vec4 $2", $i, fragout)
  var includesSection : seq[string] = @[]
  var vertexMain: NimNode
  var geometryLayout: string
  var geometryMain: string
  var fragmentMain: string
  var hasIndices = false
  var indexType: NimNode = nil
  var sizeofIndexType = 0
  var numVertices, numInstances, vertexOffset: NimNode = nil
  var bindTexturesCall = newCall(bindSym"bindTextures", newLit(numSamplers), nnkBracket.newTree)

  #### BEGIN PARSE TREE ####
  #let renderObject = ident"renderObject"
  #let vao     = newDotExpr(renderObject, ident"vao")
  #let program = newDotExpr(renderObject, ident"program")
  

  proc getlocation(i: int) : NimNode =
    newBracketExpr(locations, newLit(i))

  for call in statement.items:
    call.expectKind nnkCall
    case $call[0]
    of "numVertices":
      numVertices = newCall(ident"GLsizei", call[1])

    of "numInstances":
      numInstances = newCall(ident"GLsizei", call[1])

    of "vertexOffset":
      vertexOffset = newCall(ident"GLsizei", call[1])

    of "indices":
      let value = call[1]

      if hasIndices:
        error "has already indices", call

      hasIndices = true

      let tpe = value.getTypeInst

      if tpe[0].repr != "ElementArrayBuffer":
        error("need ElementArrayBuffer type for indices, got: " & tpe.repr, value)

      case tpe[1].typeKind
      of ntyInt8, ntyUInt8:
        indexType = bindSym"GL_UNSIGNED_BYTE"
        sizeofIndexType = 1
      of ntyInt16, ntyUInt16:
        indexType = bindSym"GL_UNSIGNED_SHORT"
        sizeofIndexType = 2
      of ntyInt32, ntyUInt32:
        indexType = bindSym"GL_UNSIGNED_INT"
        sizeofIndexType = 4
      of ntyInt, ntyUInt:
        error("int type has to be explicity sized uint8 uint16 or uint32", value)
      of ntyInt64, ntyUInt64:
        error("64 bit indices not supported", value)
      else:
        error("wrong kind for indices: " & $value.getTypeImpl[1].typeKind, value)

      drawBlock.addAll quote do:
        bindIt(`vao`, `value`)

    of "uniforms":
      for innerCall in call[1][1].items:
        innerCall[1].expectKind nnkStrLit
        let nameLit = innerCall[1]
        let name = $nameLit
        let value = innerCall[2]
        let glslType: string = innerCall[3].strVal
        let isSampler: bool  = innerCall[4].boolVal

        if value.kind in {nnkIntLit, nnkFloatLit}:
          uniformsSection.add s"const $glslType $name = ${value.repr}"
          continue

        let baseString = s"uniform $glslType $name"

        let loc = getLocation(numLocations)
        let warningLit = newLit(value.lineinfo & " Hint: unused uniform: " & name)

        initUniformsBlock.add quote do:
          `loc`.index = glGetUniformLocation(`program`.handle, `nameLit`)
          if `loc`.index < 0:
            writeLine stderr, `warningLit`

        if isSampler:
          let bindingIndexLit = newLit(numSamplers)
          initUniformsBlock.add head quote do:
            glUniform1i(`loc`.index, `bindingIndexLit`)
            
          bindTexturesCall[2].add head quote do:
            `value`.handle
            
          numSamplers += 1
        else:
          drawBlock.add head quote do:
            `program`.uniform(`loc`, `value`)

        uniformsSection.add( baseString )

        numLocations += 1

    of "attributes":
      for innerCall in call[1][1].items:
        innerCall[1].expectKind nnkStrLit
        let name = $innerCall[1]
        let value = innerCall[2]
        let divisorVal: int =
          if innerCall[3].kind == nnkHiddenStdConv:
            innerCall[3][1].intVal.int
          elif innerCall[3].kind == nnkIntLit:
            innerCall[3].intVal.int
          else:
            0
        let glslType = innerCall[4].strVal

        let location = getLocation(numLocations)
        let nameLit = newLit(name)
        #let attributeLocation = bindSym"attributeLocation"
        #let enableAttrib      = bindSym"enableAttrib"
        #let divisor           = bindSym"divisor"
        #let binding           = bindSym"binding"

        let divisorLit = newLit(divisorVal)

        let warningLit = newLit(value.lineinfo & " Hint: unused attribute: " & name)
        
        bufferCreationBlock.addAll quote do:
          `location` = attributeLocation(`program`, `nameLit`)
          # this needs to change, when multiple
          # attributes per buffer should be supported
          if 0 <= `location`.index:
            enableAttrib(`vao`, `location`)
            divisor(`vao`, binding(`location`), `divisorLit`)
          else:
            writeLine stderr, `warningLit`

          bindAndAttribPointer(`vao`, `value`, `location`)

        drawBlock.addAll quote do:
          setBuffer(`vao`, `value`, `location`)

        attribNames.add( name )
        attribTypes.add( glslType )
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

    of "transformFeedbackVaryingNames":
      for innerCall in call[1][1].items:
        transformFeedbackVaryingNames.add( innerCall.strVal )

    of "includes":

      for innerCall in call[1][1].items:
        if innerCall[1].kind == nnkSym:
          let sym = innerCall[1].symbol
          includesSection.add(sym.getImpl.strVal)
        if innerCall[1].kind in {nnkStrLit, nnkTripleStrLit}:
          includesSection.add(innerCall[1].strVal)


    of "vertexMain":
      vertexMain = call[1]

    of "fragmentMain":
      fragmentMain = call[1].strVal

    of "geometryMain":

      geometryLayout = call[1].strVal
      geometryMain = call[2].strVal

    else:
      error "unknown section", call[0]

  if bindTexturesCall[2].len > 0: #actually got any uniform textures
    drawBlock.add bindTexturesCall
      
  if vertexOffset.isNil:
    vertexOffset = newLit(0)

  if hasIndices and indexType.isNil:
    error "has indices, but index Type was never set to anything"

  var vertexShaderSource : string

  if vertexMain.isNil and geometryMain.isNil:
    # implicit screen space triangle

    if vertexOutSection.len > 0:
      error "cannot create implicit screen space quad renderer with vertex out section"

    numVertices = newLit(3)

    vertexShaderSource = screenTriagleVertexSource
    vertexOutSection.add("out vec2 texCoord")

  elif vertexMain.isNil:
    vertexShaderSource = forwardVertexShaderSource(sourceHeader, attribNames, attribTypes)


    vertexOutSection.newSeq(attribNames.len)
    for i in 0..<attribNames.len:
       vertexOutSection[i] = format("out $1 $2", attribTypes[i], attribNames[i])

  else:
    var attributesSection = newSeq[string](attribNames.len)
    for i in 0 ..< attribNames.len:
       attributesSection[i] = format(s"in ${attribTypes[i]} ${attribNames[i]}")

    vertexShaderSource = genShaderSource(sourceHeader, uniformsSection, attributesSection, -1, vertexOutSection, includesSection, vertexMain.strVal)

  if not vertexMain.isNil:
    let
      li = vertexMain.lineinfo
      p0 = li.find(".nim(")
      p1 = li.find(',',p0)
      # p2 = li.find(')',p1)
      basename = li.substr(0, p0-1)
      line     = li.substr(p0+5, p1-1).parseInt
      filename = joinPath(getTempDir(), s"${basename}_${line}.vert")

    #writeFile(filename, vertexShaderSource)

  if numVertices.isNil:
    error "numVertices needs to be assigned"

  let vsSrcLit = newLit vertexShaderSource
  
  var compileShaderBlock = quote do:
    `program`.attachAndDeleteShader(compileShader(GL_VERTEX_SHADER, `vsSrcLit`))
      
  if not geometryMain.isNil:
    var geometryHeader = sourceHeader
    geometryHeader &= "\n"
    geometryHeader &= s"layout(${geometryPrimitiveLayout(mode.intVal.GLenum)}) in;"
    geometryHeader &= "\n"
    geometryHeader &= geometryLayout
    geometryHeader &= ";\n"
    let gsSrcLit = newLit genShaderSource(geometryHeader, uniformsSection, vertexOutSection, geometryNumVerts(mode.intVal.GLenum), geometryOutSection, includesSection, geometryMain)

    compileShaderBlock.addAll quote do:
      `program`.attachAndDeleteShader(compileShader(GL_GEOMETRY_SHADER, `gsSrcLit`))

  if not fragmentMain.isNil:
    var fsSrcLit =
      if geometryMain.isNil:
        newLit genShaderSource(sourceHeader, uniformsSection, vertexOutSection, -1, fragmentOutSection, includesSection, fragmentMain)
      else:
        newLit genShaderSource(sourceHeader, uniformsSection, geometryOutSection, -1, fragmentOutSection, includesSection, fragmentMain)
    compileShaderBlock.addAll quote do:
      `program`.attachAndDeleteShader(compileShader(GL_FRAGMENT_SHADER, `fsSrcLit`))

  if transformFeedbackVaryingNames.len > 0:
    let namesLit = nnkBracket.newTree
    for name in transformFeedbackVaryingNames:
      namesLit.add newLit(name)

    compileShaderBlock.addAll quote do:
      `program`.transformFeedbackVaryings(`namesLit`, GL_INTERLEAVED_ATTRIBS)
    
  if hasIndices:
    var indicesPtr = newTree( nnkCast, bindSym"pointer", newInfix(bindSym"*", vertexOffset, newLit(sizeofIndexType)))
    if numInstances.isNil:
      drawBlock.add newCall( bindSym"glDrawElements", mode, numVertices, indexType, indicesPtr )
    else:
      drawBlock.add newCall( bindSym"glDrawElementsInstanced", mode, numVertices, indexType, indicesPtr, numInstances )
  else:
    if numInstances.isNil:
      drawBlock.add newCall( bindSym"glDrawArrays", mode, vertexOffset, numVertices )
    else:
      drawBlock.add newCall( bindSym"glDrawArraysInstanced", mode, vertexOffset, numVertices, numInstances )

  let numLocationsLit = newLit(numLocations)
                            
  result = quote do:
    var `vao` {.global.}: VertexArrayObject
    var `program` {.global.}: Program
    var `locations` {.global.}: array[`numLocationsLit`, Location]

    glPushDebugGroup(GL_DEBUG_SOURCE_THIRD_PARTY, 1, 10, "shadingDsl");

    if `program`.isNil:
      `program`.handle = glCreateProgram()

      `compileShaderBlock`

      `program`.linkOrDelete
      
      glUseProgram(`program`.handle)

      `initUniformsBlock`

      `vao` = newVertexArrayObject()

      `bufferCreationBlock`

      `afterSetup`

    glUseProgram(`program`.handle)
    
    glBindVertexArray(`vao`.handle)
    
    `beforeRender`
    
    `drawBlock`

    `afterRender`

    glBindVertexArray(0)
    glUseProgram(0);

    glPopDebugGroup();

                    

##################################################################################
#### Shading Dsl Outer ###########################################################
######################################l############################################
                    
macro shadingDsl*(statement: untyped) : untyped =
  var wrapWithDebugResult = false
  
  result = newCall(bindSym"shadingDslInner", newNilLit(), newNilLit(), bindSym"GL_TRIANGLES", newStmtList(), newStmtList(), newStmtList(), ident"fragmentOutputs", )

  
  
  # numVertices = result[2]
  # numInstances = result[3]

  for section in statement.items:
    section.expectKind({nnkCall, nnkAsgn, nnkIdent})

    if section.kind == nnkIdent:
      if section == ident("debugResult"):
        wrapWithDebugResult = true
      else:
        error("unknown identifier: " & $section.ident & " did you mean debugResult?")
    elif section.kind == nnkAsgn:
      section.expectLen(2)
      let ident = section[0]
      let value = section[1]
      ident.expectKind nnkIdent
      case $ident.ident
      of "numVertices":
        result.add( newCall(bindSym"numVertices", value ) )
      of "numInstances":
        result.add( newCall(bindSym"numInstances", value ) )
      of "vertexOffset":
        result.add( newCall(bindSym"vertexOffset", value ) )
      of "indices":
        result.add( newCall(bindSym"indices", value ) )
      of "primitiveMode":
        result[3] = value
      of "programIdent":
        if result[1].kind == nnkNilLit:
          result[1] = value
        else:
          error("double declaration of programIdent", section)
      of "vaoIdent":
        if result[2].kind == nnkNilLit:
          result[2] = value
        else:
          error("double declaration of vaoIdent", section)

      else:
        error("unknown named parameter " & $ident.ident)

    elif section.kind == nnkCall:
      let ident = section[0]
      ident.expectKind nnkIdent
      let stmtList = section[1]
      stmtList.expectKind nnkStmtList

      case $ident
      of "afterSetup":
        result[4] = stmtList
      of "beforeRender":
        result[5] = stmtList
      of "afterRender":
        result[6] = stmtList
      of "uniforms":
        let uniformsCall = newCall(bindSym"uniforms")

        for capture in stmtList.items:
          capture.expectKind({nnkAsgn, nnkIdent})

          var nameNode, identNode: NimNode

          if capture.kind == nnkAsgn:
            capture.expectLen 2
            capture[0].expectKind nnkIdent
            nameNode = newLit($capture[0])
            identNode = capture[1]
          elif capture.kind == nnkIdent:
            identNode = capture
            nameNode  = newLit($capture)
            
          let isSampler = newCall(bindSym"glslIsSampler", newCall(bindSym"type", identNode))
          let glslType  = newCall(bindSym"glslTypeRepr",  newCall(bindSym"type", identNode))
          uniformsCall.add( newCall(
            bindSym"shaderArg",  nameNode, identNode, glslType, isSampler) )

        result.add(uniformsCall)

      of "attributes":
        let attributesCall = newCall(bindSym"attributes")
        
        proc handleCapture(attributesCall, capture: NimNode, divisor: int) =
          capture.expectKind({nnkAsgn, nnkIdent})

          var nameNode, identNode: NimNode
          
          if capture.kind == nnkAsgn:
            capture.expectLen 2
            capture[0].expectKind nnkIdent
            nameNode = newLit($capture[0])
            identNode = capture[1]
          elif capture.kind == nnkIdent:
            nameNode  = newLit($capture)
            identNode = capture
            
          let glslType  = newCall(bindSym"glslTypeRepr",  newCall(
            bindSym"type", newDotExpr(identNode,ident"T")))
          attributesCall.add( newCall(
            bindSym"attribute", nameNode, identNode, newLit(divisor), glslType ) )


        for capture in stmtList.items:
          if capture.kind == nnkCall:
            if $capture[0] == "instanceData":
              let stmtList = capture[1]
              stmtList.expectKind nnkStmtList
              for capture in stmtList.items:
                handleCapture(attributesCall, capture, 1)

            else:
              error "expected call to instanceData, but got: " & capture.repr
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

        var transformFeedbackVaryingNamesCall = newCall(bindSym"transformFeedbackVaryingNames")

        for section in stmtList.items:
          section.expectKind({nnkStrLit, nnkTripleStrLit, nnkAsgn, nnkIdent})
          case section.kind
          of nnkAsgn:
            let name = section[0].repr
            let nameLit = newLit(name)
            let identNode = section[1]
            
            outCall.add head quote do:
              "out " & glslTypeRepr(type(`identNode`.T)) & " " & `nameLit`

            transformFeedbackVaryingNamesCall.add nameLit
          
          of nnkIdent:
            let name = section.repr
            let nameLit = newLit(name)
            let identNode = section
            
            outCall.add head quote do:
              "out " & glslTypeRepr(type(`identNode`.T)) & " " & `nameLit`

            transformFeedbackVaryingNamesCall.add nameLit

          of nnkStrLit:
            outCall.add section
          of nnkTripleStrLit:
            for line in section.strVal.splitLines:
              outCall.add line.strip.newLit
          else:
            error "unreachable"


        result.add(outCall)
        if transformFeedbackVaryingNamesCall.len > 1:
          result.add transformFeedbackVaryingNamesCall

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
          statement.expectKind({nnkIdent,nnkStrLit,nnkTripleStrLit})

          includesCall.add( newCall(bindSym"incl", statement) )

        result.add(includesCall)
      else:
        error("unknown section", ident)
        
  if wrapWithDebugResult:
    result = newCall( bindSym"debugResult", result )
