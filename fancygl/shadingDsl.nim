const
  sourceHeader = """
#version 330
#extension GL_ARB_enhanced_layouts : enable
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

const
  userCodePrefix = """
////////////////////////////////////////////////////////////////////////////////
//                            user code begins here                           //
"""
  userCodePostfix = """

//                            user code ends here                             //
////////////////////////////////////////////////////////////////////////////////
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

  result.add(mainSrc)

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
proc vertexSrc(src: string): int = 0
proc fragmentSrc(src: string): int = 0
proc geometrySrc(layout, src: string): int = 0
proc includes(args: varargs[int]): int = 0
proc incl(arg: string): int = 0
proc numVertices(num: int): int = 0
proc numInstances(num: int): int = 0
proc vertexOffset(offset: int) : int = 0
proc baseInstance(base: int): int = 0
proc baseVertex(base: int): int = 0

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
  var drawBlock = newStmtList()
  var attribNames = newSeq[string](0)
  var attribTypes = newSeq[string](0)
  var afterCompileBlock = newStmtList()
  var vertexOutSection = newSeq[string](0)
  var geometryOutSection = newSeq[string](0)
  var fragmentOutSection = newSeq[string](0)
  var transformFeedbackVaryingNames = newSeq[string](0)

  for i,fragout in fragmentOutputs:
    fragmentOutSection.add format("layout(location = $1) out vec4 $2", $i, fragout)
  var includesSection : seq[string] = @[]
  var vertexSrc: NimNode
  var geometryLayout: string
  var geometrySrc: NimNode
  var fragmentSrc: NimNode
  var hasIndices = false
  var indexType: NimNode = nil
  var sizeofIndexType = 0
  var numVertices, numInstances, vertexOffset, baseVertex, baseInstance: NimNode = nil
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

      drawBlock.add(quote do:
        glVertexArrayElementBuffer(`vao`.handle, `value`.handle)
      )

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
        let warningLit = newLit($value.lineinfoObj & " Hint: unused uniform: " & name)

        afterCompileBlock.add(quote do:
          `loc`.index = glGetUniformLocation(`program`.handle, `nameLit`)
          if `loc`.index < 0:
            writeLine stderr, `warningLit`
        )

        if isSampler:
          let bindingIndexLit = newLit(numSamplers)
          afterCompileBlock.add quote do:
            glUniform1i(`loc`.index, `bindingIndexLit`)


          bindTexturesCall[2].add quote do:
            `value`.handle


          numSamplers += 1
        else:
          drawBlock.add quote do:
            `program`.uniform(`loc`, `value`)


        uniformsSection.add( baseString )

        numLocations += 1

    of "attributes":
      var binding: uint32

      let setBuffersCall = newCall(bindSym"setBuffers", vao, newLit(binding))

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

        let warningLit = newLit($value.lineinfoObj & " Hint: unused attribute: " & name)


        let bindingLit : NimNode = newLit(binding)

        afterCompileBlock.add(quote do:
          # this needs to change, when multiple attributes per buffer
          # should be supported (offset in buffer)
          glEnableVertexArrayAttrib(`vao`.handle, `bindingLit`)
          glVertexArrayBindingDivisor(`vao`.handle, `bindingLit`, `divisorLit`)
          setFormat(`vao`, `bindingLit`, `value`)
          `location` = attributeLocation(`program`, `nameLit`)
          if `location`.index < 0:
            writeLine stderr, `warningLit`
          else:
            glVertexArrayAttribBinding(`vao`.handle, `bindingLit`, uint32(`location`.index))
        )

        setBuffersCall.add value


        attribNames.add( name )
        attribTypes.add( glslType )
        # format("in $1 $2", value.glslAttribType, name) )
        numLocations += 1
        binding += 1

      echo setBuffersCall.repr
      drawBlock.add(setBuffersCall)

    of "vertexOut":
      ##echo "vertexOut"

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
          let sym = innerCall[1]
          includesSection.add(sym.getImpl.strVal)
        if innerCall[1].kind in {nnkStrLit, nnkTripleStrLit}:
          includesSection.add(innerCall[1].strVal)


    of "vertexSrc":
      vertexSrc = call[1]

    of "fragmentSrc":
      fragmentSrc = call[1]

    of "geometrySrc":

      geometryLayout = call[1].strVal
      geometrySrc = call[2]

    of "baseVertex":
      baseVertex = newCall(bindSym"GLint", call[1])
    of "baseInstance":
      baseInstance = newCall(bindSym"GLuint", call[1])
    else:
      error "unknown internal section " & $call[0], call[0]

  if bindTexturesCall[2].len > 0: #actually got any uniform textures
    drawBlock.add bindTexturesCall

  if vertexOffset.isNil:
    vertexOffset = newLit(0)

  if hasIndices and indexType.isNil:
    error "has indices, but index Type was never set to anything"

  var vertexShaderSource : string

  if vertexSrc.isNil and geometrySrc.isNil:
    # implicit screen space triangle

    if vertexOutSection.len > 0:
      error "cannot create implicit screen space quad renderer with vertex out section"

    numVertices = newLit(3)

    vertexShaderSource = screenTriagleVertexSource
    vertexOutSection.add("out vec2 texCoord")

  elif vertexSrc.isNil:
    vertexShaderSource = forwardVertexShaderSource(sourceHeader, attribNames, attribTypes)


    vertexOutSection.newSeq(attribNames.len)
    for i in 0..<attribNames.len:
       vertexOutSection[i] = format("out $1 $2", attribTypes[i], attribNames[i])

  else:
    var attributesSection = newSeq[string](attribNames.len)
    for i in 0 ..< attribNames.len:
       attributesSection[i] = format(s"in ${attribTypes[i]} ${attribNames[i]}")

    vertexShaderSource = genShaderSource(sourceHeader, uniformsSection, attributesSection, -1, vertexOutSection, includesSection, vertexSrc.strVal)

  # TODO do something with this unused block of code
  # if not vertexSrc.isNil:
  #   let
  #     li = vertexSrc.lineinfo
  #     p0 = li.find(".nim(")
  #     p1 = li.find(',',p0)
  #     p2 = li.find(')',p1)
  #     basename = li.substr(0, p0-1)
  #     line     = li.substr(p0+5, p1-1).parseInt
  #     filename = joinPath(getTempDir(), s"${basename}_${line}.vert")
  #   writeFile(filename, vertexShaderSource)

  if numVertices.isNil:
    error "numVertices needs to be assigned"

  let vsSrcLit = newLit vertexShaderSource
  let lineinfoLit = newLit(vertexSrc.lineinfoObj)

  var compileShaderBlock = newStmtList()
  compileShaderBlock.add quote do:
    `program`.attachAndDeleteShader(compileShader(GL_VERTEX_SHADER, `vsSrcLit`, `lineinfoLit`))

  if not geometrySrc.isNil:
    var geometryHeader = sourceHeader
    geometryHeader &= "\n"
    geometryHeader &= s"layout(${geometryPrimitiveLayout(mode.intVal.GLenum)}) in;"
    geometryHeader &= "\n"
    geometryHeader &= geometryLayout
    geometryHeader &= ";\n"
    let gsSrcLit = newLit genShaderSource(geometryHeader, uniformsSection, vertexOutSection, geometryNumVerts(mode.intVal.GLenum), geometryOutSection, includesSection, geometrySrc.strVal)
    let lineinfoLit = newLit(geometrySrc.lineinfoObj)
    compileShaderBlock.add(quote do:
      `program`.attachAndDeleteShader(compileShader(GL_GEOMETRY_SHADER, `gsSrcLit`, `lineinfoLit`))
    )

  if not fragmentSrc.isNil:
    var fsSrcLit =
      if geometrySrc.isNil:
        newLit genShaderSource(sourceHeader, uniformsSection, vertexOutSection, -1, fragmentOutSection, includesSection, fragmentSrc.strVal)
      else:
        newLit genShaderSource(sourceHeader, uniformsSection, geometryOutSection, -1, fragmentOutSection, includesSection, fragmentSrc.strVal)
    let lineinfoLit = newLit(fragmentSrc.lineinfoObj)
    compileShaderBlock.add(quote do:
      `program`.attachAndDeleteShader(compileShader(GL_FRAGMENT_SHADER, `fsSrcLit`, `lineinfoLit`))
    )

  if transformFeedbackVaryingNames.len > 0:
    let namesLit = nnkBracket.newTree
    for name in transformFeedbackVaryingNames:
      namesLit.add newLit(name)

    compileShaderBlock.add(quote do:
      `program`.transformFeedbackVaryings(`namesLit`, GL_INTERLEAVED_ATTRIBS)
    )

  template indicesPtr: NimNode =
    newTree( nnkCast, bindSym"pointer", newInfix(bindSym"*", vertexOffset, newLit(sizeofIndexType)))

  if hasIndices:
    if numInstances.isNil:
      if not baseInstance.isNil:
        error "no instancing, unable to use parameter baseInstance", baseInstance[1]
      if baseVertex.isNil:
        drawBlock.add newCall(
          bindSym"glDrawElements", mode, numVertices, indexType, indicesPtr )
      else:
        drawBlock.add newCall(
          bindSym"glDrawElementsBaseVertex",
          mode, numVertices, indexType, indicesPtr, baseVertex )
    else:
      if baseInstance.isNil:
        if baseVertex.isNil:
          drawBlock.add newCall(
            bindSym"glDrawElementsInstanced",
            mode, numVertices, indexType, indicesPtr, numInstances )
        else:
          drawBlock.add newCall(
            bindSym"glDrawElementsInstancedBaseVertex",
            mode, numVertices, indexType, indicesPtr, numInstances, baseVertex )
      else:
        if baseVertex.isNil:
          drawBlock.add newCall(
            bindSym"glDrawElementsInstancedBaseInstance",
            mode, numVertices, indexType, indicesPtr, numInstances, baseInstance )
        else:
          drawBlock.add newCall(
            bindSym"glDrawElementsInstancedBaseVertexBaseInstance",
            mode, numVertices, indexType, indicesPtr, numInstances, baseVertex, baseInstance )

  else:
    if not baseVertex.isNil:
      error "no indices, unable to use parameter baseVertex", baseVertex[1]
    if numInstances.isNil:
      if not baseInstance.isNil:
        error "no instancing, unable to use parameter baseInstance", baseInstance[1]
      drawBlock.add newCall(
        bindSym"glDrawArrays",
        mode, vertexOffset, numVertices )
    else:
      if baseInstance.isNil:
        drawBlock.add newCall(
          bindSym"glDrawArraysInstanced",
          mode, vertexOffset, numVertices, numInstances )
      else:
        drawBlock.add newCall(
          bindSym"glDrawArraysInstancedBaseInstance",
          mode, vertexOffset, numVertices, numInstances, baseInstance )


  let numLocationsLit = newLit(numLocations)

  result = quote do:
    var `vao` {.global.}: VertexArrayObject
    var `program` {.global.}: Program
    var `locations` {.global.}: array[`numLocationsLit`, Location]

    if glPushDebugGroup != nil:
       glPushDebugGroup(GL_DEBUG_SOURCE_THIRD_PARTY, 1, 10, "shadingDsl");


    if `program`.handle == 0:
      `program`.handle = glCreateProgram()

      `compileShaderBlock`

      `program`.linkOrDelete

      glUseProgram(`program`.handle)

      `vao` = newVertexArrayObject()

      `afterCompileBlock`

      `afterSetup`

    glUseProgram(`program`.handle)

    glBindVertexArray(`vao`.handle)

    `beforeRender`

    `drawBlock`

    `afterRender`

    glBindVertexArray(0)
    glUseProgram(0);

    if glPopDebugGroup != nil:
      glPopDebugGroup();




##################################################################################
#### Shading Dsl Outer ###########################################################
##################################################################################

macro transformFeedbackOutSection(self: TransformFeedback): string =
  result = newLit("")

macro shadingDsl*(statement: untyped) : untyped =
  var wrapWithDebugResult = false

  result = newCall(bindSym"shadingDslInner", newNilLit(), newNilLit(), bindSym"GL_TRIANGLES", newStmtList(), newStmtList(), newStmtList(), ident"fragmentOutputs", )

  for section in statement.items:
    section.expectKind({nnkCall, nnkAsgn, nnkIdent})

    if section.kind == nnkIdent:
      if section == ident("debug"):
        wrapWithDebugResult = true
      else:
        error("unknown identifier: " & $section & " did you mean debug?")
    elif section.kind == nnkAsgn:
      section.expectLen(2)
      let ident = section[0]
      let value = section[1]
      ident.expectKind nnkIdent
      case $ident
      of "numVertices":
        result.add( newCall(bindSym"numVertices", value ) )
      of "numInstances":
        result.add( newCall(bindSym"numInstances", value ) )
      of "vertexOffset":
        result.add( newCall(bindSym"vertexOffset", value ) )
      of "indices":
        result.add( newCall(bindSym"indices", value ) )
      of "baseVertex":
        result.add( newCall(bindSym"baseVertex", value ) )
      of "baseInstance":
        result.add( newCall(bindSym"baseInstance", value ) )
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
        error("unknown named parameter " & ident.strVal)

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

          let isSampler = newCall(bindSym("glslIsSampler", brForceOpen), newCall(bindSym"type", identNode))
          let glslType  = newCall(bindsym("glslTypeRepr", brForceOpen),  newCall(bindSym"type", identNode))
          uniformsCall.add( newCall(
            bindSym"shaderArg",  nameNode, identNode, glslType, isSampler) )

        result.add(uniformsCall)

      of "attributes":
        let attributesCall = newCall(bindSym"attributes")

        proc matchDivisorPragma(node: NimNode): NimNode =
          node.expectKind(nnkPragma)
          node.expectLen(1)
          node[0].expectKind(nnkExprColonExpr)
          node[0][0].expectIdent("divisor")
          node[0][1]

        proc handleCapture(attributesCall, capture: NimNode, default_divisor: int) =
          capture.expectKind({nnkAsgn, nnkIdent, nnkPragmaExpr})

          var
            nameNode, identNode: NimNode
            divisor: NimNode

          if capture.kind == nnkAsgn:
            capture.expectLen 2
            capture[0].expectKind nnkIdent
            nameNode = newLit($capture[0])
            if capture[1].kind == nnkPragmaExpr:
              identNode = capture[1][0]
              divisor = capture[1][1].matchDivisorPragma
            else:
              identNode = capture[1]
          elif capture.kind == nnkIdent:
            nameNode  = newLit($capture)
            identNode = capture
          elif capture.kind == nnkPragmaExpr:
            capture[0].expectKind(nnkIdent)
            nameNode  = newLit($capture[0])
            identNode = capture[0]
            divisor   = capture[1].matchDivisorPragma

          if divisor.isNil:
            divisor = newLit(default_divisor)

          let glslType  = newCall(bindSym("glslTypeRepr", brForceOpen),  newCall(
            bindSym"type", newDotExpr(identNode,ident"T")))
          attributesCall.add( newCall(
            bindSym"attribute", nameNode, identNode, divisor, glslType ) )

        for capture in stmtList.items:
          if capture.kind == nnkCall:
            if $capture[0] == "instanceData":
              let stmtList = capture[1]
              stmtList.expectKind nnkStmtList
              for capture in stmtList.items:
                handleCapture(attributesCall, capture, 1)

            else:
              error "expected call to instanceData, but got: " & capture.repr, capture
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
          section.expectKind({
            nnkStrLit, nnkTripleStrLit,
            nnkAsgn, nnkIdent, nnkDiscardStmt
          })
          case section.kind
          of nnkAsgn:
            let name = section[0].repr
            let nameLit = newLit(name)
            let identNode = section[1]
            let glslTypeReprSym = bindSym("glslTypeRepr", brForceOpen)

            outCall.add quote do:
              "out " & `glslTypeReprSym`(type(`identNode`.T)) & " " & `nameLit`

            transformFeedbackVaryingNamesCall.add nameLit

          of nnkIdent:
            # TODO this is very much WIP
            let name = section.repr
            if name != "transformFeedback":
              error("foobar")

            #let nameLit = newLit(name)
            #let identNode = section

            #outCall.add head(quote do:
            #  transformFeedbackOutSection(`identNode`)
            #)
            #outCall.add head quote do:
            #  "out " & glslTypeRepr(type(`identNode`.T)) & " " & `nameLit`
            #transformFeedbackVaryingNamesCall.add nameLit

          of nnkStrLit:
            outCall.add section
          of nnkTripleStrLit:
            for line in section.strVal.splitLines:
              outCall.add line.strip.newLit
          of nnkDiscardStmt:
            discard
          else:
            error "unreachable"


        result.add(outCall)
        if transformFeedbackVaryingNamesCall.len > 1:
          result.add transformFeedbackVaryingNamesCall

      of "vertexMain":
        stmtList.expectLen(1)
        if stmtList[0].kind != nnkDiscardStmt:
          var buf = "void main() {\n"
          buf.add(userCodePrefix)
          buf.add(stmtList[0].strVal)
          buf.add(userCodePostfix)
          buf.add("\n}\n")
          result.add( newCall(bindSym"vertexSrc", newLit(buf)) )
      of "vertexSrc":
        stmtList.expectLen(1)
        if stmtList[0].kind != nnkDiscardStmt:
          var buf = userCodePrefix
          buf.add(stmtList[0].strVal)
          buf.add(userCodePostfix)
          result.add( newCall(bindSym"vertexSrc", newLit(buf)) )

      of "geometryMain":
        if stmtList.len == 1:
          stmtList[0].expectKind nnkDiscardStmt
        else:
          stmtList.expectLen(2)
          stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
          stmtList[1].expectKind({nnkTripleStrLit, nnkStrLit})

          var buf = "void main() {\n"
          buf.add(userCodePrefix)
          buf.add(stmtList[1].strVal)
          buf.add(userCodePostfix)
          buf.add("\n}\n")

          result.add(newCall(bindSym"geometrySrc", stmtList[0], newLit(buf)))

      of "geometrySrc":
        if stmtList.len == 1:
          stmtList[0].expectKind nnkDiscardStmt
        else:
          stmtList.expectLen(2)
          stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
          stmtList[1].expectKind({nnkTripleStrLit, nnkStrLit})

          var buf = userCodePrefix
          buf.add(stmtList[1].strVal)
          buf.add(userCodePostfix)

          result.add(newCall(bindSym"geometrySrc", stmtList[0], newLit(buf)))

      of "fragmentMain":
        stmtList.expectLen(1)
        if stmtList[0].kind != nnkDiscardStmt:
          stmtList[0].expectKind({ nnkTripleStrLit, nnkStrLit })

          var buf = "void main() {\n"
          buf.add(userCodePrefix)
          buf.add(stmtList[0].strVal)
          buf.add(userCodePostfix)
          buf.add("\n}\n")

          result.add( newCall(bindSym"fragmentSrc", newLit(buf)) )

      of "fragmentSrc":
        stmtList.expectLen(1)
        if stmtList[0].kind != nnkDiscardStmt:
          stmtList[0].expectKind({ nnkTripleStrLit, nnkStrLit })

          var buf = userCodePrefix
          buf.add(stmtList[0].strVal)
          buf.add(userCodePostfix)

          result.add( newCall(bindSym"fragmentSrc", newLit(buf)) )

      of "includes":
        let includesCall = newCall(bindSym"includes")

        for statement in stmtList:
          if statement.kind != nnkDiscardStmt:
            statement.expectKind({nnkIdent,nnkStrLit,nnkTripleStrLit})
            includesCall.add( newCall(bindSym"incl", statement) )

        result.add(includesCall)
      else:
        error("unknown section", ident)

  if wrapWithDebugResult:
    result = newCall( bindSym"debugResult", result )
