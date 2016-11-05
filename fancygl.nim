########################################################################
############################### fancy gl ###############################
########################################################################
import arnelib, opengl, glm, math, random, strutils, nre, macros,
       macroutils, sdl2, sdl2/image, os, terminal
include etc, glm_additions, default_setup, shapes, samplers, framebuffer, glwrapper, heightmap, iqm, typeinfo
export opengl, glm, sdl2

type ShaderParam* = tuple[name: string, gl_type: string]

proc screenshot*(window : sdl2.WindowPtr; basename : string) : bool {.discardable.} =
  var
    (w,h) = window.getSize
    data = newSeq[uint32](w * h)

  glReadPixels(0,0,w,h,GL_RGBA, GL_UNSIGNED_BYTE, data[0].addr)

  for y in 0 .. < h div 2:
    for x in 0 .. < w:
      swap(data[y*w+x], data[(h-y-1)*w+x])

  let surface = createRGBSurfaceFrom(data[0].addr,w,h,32,w*4,
                                     0x0000ffu32,0x00ff00u32,0xff0000u32,0xff000000u32)
  if surface.isNil:
    echo "Could not create SDL_Surface from pixel data: ", sdl2.getError()
    return false

  defer: surface.freeSurface

  os.createDir "screenshots"

  var i = 0
  template filename() : string = "screenshots/" & basename & "_" & intToStr(i,4) & ".bmp"
  while os.fileExists(filename()):
    i += 1

  if surface.saveBMP(filename()):
    echo sdl2.getError()
    return false

  true

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

proc makeAndBindBuffer[T](buffer: var ArrayBuffer[T], index: GLint) =
  if index >= 0:
    buffer.create
    buffer.bindIt
    glVertexAttribPointer(index.GLuint, attribSize(T), attribType(T), attribNormalized(T), 0, nil)
    
proc bindAndAttribPointer[T](vao: VertexArrayObject, buffer: ArrayBuffer[T], location: Location) =
  if location.index >= 0:
    buffer.bindIt
    glVertexAttribPointer(location.index.GLuint, attribSize(T), attribType(T), attribNormalized(T), 0, nil)
    #let
    #  loc = location.index.GLuint
    #  binding = location.index.GLuint
    #glVertexArrayVertexBuffer(vao.handle, loc, buffer.handle, 0, 0)
    #glVertexArrayAttribFormat(vao.handle, loc, attribSize(T),
    #                          attribType(T), attribNormalized(T), #[ relative offset ?! ]# 0);
    #glVertexArrayAttribBinding(vao.handle, loc, binding)

proc bindAndAttribPointer(vao: VertexArrayObject; view: ArrayBufferView; location: Location): void =
  if location.index >= 0:
    view.buffer.bindIt
    glVertexAttribPointer(
      location.index.GLuint,
      attribSize(view.T),
      attribType(view.T),
      attribNormalized(view.T),
      view.stride.GLsizei,
      cast[pointer](view.offset)
    )
    
#ArrayBufferView*[S,T] = object
#    buffer*: ArrayBuffer[S]
#    offset*, stride*: int
    
proc makeAndBindElementBuffer[T](buffer: var ElementArraybuffer[T]) =
  buffer.create
  buffer.bindIt

proc enableAndSetDivisor(vao: VertexArrayObject, index: GLint, divisorval: GLuint): void =
  if index >= 0:
    vao.enableAttrib(Location(index: index))
    vao.divisor(Binding(index: index.GLuint), divisorval)

template renderBlockTemplate(numLocations: int; globalsBlock, linkShaderBlock,
                             bufferCreationBlock, initUniformsBlock, setUniformsBlock,
                             drawCommand: untyped): untyped =
  block:
    var vao {.global, inject.}: VertexArrayObject
    var glProgram {.global, inject.}: Program
    var locations {.global, inject.}: array[numLocations, Location]

    globalsBlock

    if glProgram.isNil:
      glProgram = linkShaderBlock
      glUseProgram(glProgram.handle)

      initUniformsBlock

      vao = newVertexArrayObject()
      vao.bindIt

      bufferCreationBlock

      glBindBuffer(GL_ARRAY_BUFFER, 0)

      #for i, loc in locations:
      #  echo "location(", i, "): ", loc

    glUseProgram(glProgram.handle)

    bindIt(vao)

    setUniformsBlock

    drawCommand

    glBindVertexArray(0)
    glUseProgram(0);

##################################################################################
#### Shading Dsl #################################################################
##################################################################################

proc attribute[T](name: string, value: T, divisor: GLuint, glslType: string) : int = 0
proc attributes(args : varargs[int]) : int = 0
proc shaderArg[T](name: string, value: T, glslType: string, isSampler: bool): int = 0
proc uniforms(args: varargs[int]): int = 0
proc vertexOut(args: varargs[string]): int = 0
proc geometryOut(args: varargs[string]): int = 0
proc fragmentOut(args: varargs[string]): int = 0
proc vertexMain(src: string): int = 0
proc fragmentMain(src: string): int = 0
proc geometryMain(layout, src: string): int = 0
proc includes(args: varargs[int]): int = 0
proc incl(arg: string): int = 0
proc numVertices(num: GLsizei): int = 0
proc numInstances(num: GLsizei): int = 0
proc vertexOffset(offset: GLsizei) : int = 0

##################################################################################
#### Shading Dsl Inner ###########################################################
##################################################################################
  
macro shadingDslInner(mode: GLenum, fragmentOutputs: static[openArray[string]], statement: varargs[int] ) : untyped =
  var numSamplers = 0
  var numLocations = 0
  var uniformsSection = newSeq[string](0)
  var initUniformsBlock = newStmtList()
  var setUniformsBlock = newStmtList()
  var attribNames = newSeq[string](0)
  var attribTypes = newSeq[string](0)
  #var attributesSection : seq[object(name:string, tpe: string)] = @[]
  var globalsBlock = newStmtList()
  var bufferCreationBlock = newStmtList()
  var vertexOutSection = newSeq[string](0)
  var geometryOutSection = newSeq[string](0)
  var fragmentOutSection = newSeq[string](0)

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

    of "vertexOffset":
      vertexOffset = call[1]

    of "uniforms":
      for innerCall in call[1][1].items:
        innerCall[1].expectKind nnkStrLit
        let name = $innerCall[1]
        let value = innerCall[2]
        let glslType: string = innerCall[3].strVal
        let isSampler: bool  = innerCall[4].boolVal

        if value.kind in {nnkIntLit, nnkFloatLit}:
          uniformsSection.add "const " & glslType & " " & name & " = " & value.repr
          continue

        let baseString = "uniform " & glslType & " " & name

        initUniformsBlock.add( newAssignment(
          newDotExpr(locations(numLocations), !!"index"),
          newCall( bindSym"glGetUniformLocation", newDotExpr(!!"glProgram", !!"handle"), newLit(name) )
        ))

        
          
        if isSampler:
          initUniformsBlock.add( newCall( bindSym"glUniform1i", newDotExpr(locations(numLocations),
              !!"index"), newLit(numSamplers) ) )

          proc activeTexture(texture: int): void =
            glActiveTexture( (GL_TEXTURE0.int + texture).GLenum )

          setUniformsBlock.add( newCall( bindSym"bindToUnit", value, newLit(numSamplers) ) )
          numSamplers += 1
        else:
          setUniformsBlock.add( newCall( bindSym"uniform", !!"glProgram", locations(numLocations), value ) )

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
        let glslType = innerCall[4].strVal

        let buffername = !(name & "Buffer")

        let isAttrib = name != "indices"

        if not isAttrib:
          if hasIndices:
            error "has already indices"

          hasIndices = true

          let tpe = value.getTypeInst

          if $tpe[0].symbol != "ElementArrayBuffer" and $tpe[0].symbol != "seq":
            error("incompatible container type for indices: " & tpe.repr, value)
          
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


        template foobarTemplate( lhs, rhs, bufferType : untyped ) : untyped {.dirty.} =
          var lhs {.global.}: bufferType[rhs[0].type]

        let isSeq:bool = $value.getTypeInst[0] == "seq"

        if isSeq:
          let bufferType =
            if isAttrib:
              bindSym"ArrayBuffer"
            else:
              bindSym"ElementArrayBuffer"

          globalsBlock.add(getAst(foobarTemplate( !! buffername, value, bufferType )))

        if isAttrib:
          bufferCreationBlock.add( newAssignment(
            locations(numLocations),
            newCall( bindSym"attributeLocation", !! "glProgram", newLit(name) )
          ))

          # this needs to change, when multiple
          # attributes per buffer should be supported
          bufferCreationBlock.add(newCall(bindSym"enableAndSetDivisor", !!"vao",
            newDotExpr(locations(numLocations), !!"index"),
            newLit(divisor)))

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
              !!"vao",
              value,
              locations(numLocations),
            ))
          else:
            bufferCreationBlock.add(newCall(bindSym"bindIt", value))

        if isAttrib:
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

    of "includes":

      for innerCall in call[1][1].items:
        if innerCall[1].kind == nnkSym:
          let sym = innerCall[1].symbol
          includesSection.add(sym.getImpl.strVal)


    of "vertexMain":
      vertexMain = call[1]

    of "fragmentMain":
      fragmentMain = call[1].strVal

    of "geometryMain":

      geometryLayout = call[1].strVal
      geometryMain = call[2].strVal

    else:
      echo "unknownSection"
      echo call.repr

  if fragmentMain.isNil:
    error("no fragment main")

  if numVertices.isNil:
    error "numVertices needs to be assigned"

  if vertexOffset.isNil:
    vertexOffset = newLit(0)

  if hasIndices and indexType.isNil:
    error "has indices, but index Type was never set to anything"

  var vertexShaderSource : string

  if vertexMain.isNil and geometryMain.isNil:

    if vertexOutSection.len > 0:
      error "cannot create implicit screen space quad renderer with vertex out section"

    vertexShaderSource = screenTriagleVertexSource
    vertexOutSection.add("out vec2 texCoord")

  elif vertexMain.isNil:
    vertexShaderSource = forwardVertexShaderSource(sourceHeader, attribNames, attribTypes)


    vertexOutSection.newSeq(attribNames.len)
    for i in 0..<attribNames.len:
       vertexOutSection[i] = format("out $1 $2", attribTypes[i], attribNames[i])

  else:
    var attributesSection = newSeq[string](attribNames.len)
    for i in 0..<attribNames.len:
       attributesSection[i] = format("in $1 $2", attribTypes[i], attribNames[i])

    vertexShaderSource = genShaderSource(sourceHeader, uniformsSection, attributesSection, -1, vertexOutSection, includesSection, vertexMain.strVal)

  if not vertexMain.isNil:
    let
      li = vertexMain.lineinfo
      p0 = li.find(".nim(")
      p1 = li.find(',',p0)
      # p2 = li.find(')',p1)
      basename = li.substr(0, p0-1)
      line     = li.substr(p0+5, p1-1).parseInt
      filename = joinPath(getTempDir(), basename & "_" & $line & ".vert")

    writeFile(filename, vertexShaderSource)

  var linkShaderBlock : NimNode

  if geometryMain.isNil:

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
    if hasIndices: (block:
      var indicesPtr = newTree( nnkCast, bindSym"pointer", newInfix(bindSym"*", vertexOffset, newLit(sizeofIndexType)))
      if numInstances.isNil:
        newCall( bindSym"glDrawElements", mode, numVertices, indexType, indicesPtr )
      else:
        newCall( bindSym"glDrawElementsInstanced", mode, numVertices, indexType, indicesPtr, numInstances )
    ) else:
      if numInstances.isNil:
        newCall( bindSym"glDrawArrays", mode, vertexOffset, numVertices )
      else:
        newCall( bindSym"glDrawArraysInstanced", mode, vertexOffset, numVertices, numInstances )

  result = getAst(renderBlockTemplate(numLocations, globalsBlock, linkShaderBlock,
         bufferCreationBlock, initUniformsBlock, setUniformsBlock, drawCommand))

##################################################################################
#### Shading Dsl Outer ###########################################################
##################################################################################

macro shadingDsl*(mode:GLenum, statement: untyped) : untyped =
  var wrapWithDebugResult = false
  result = newCall(bindSym"shadingDslInner", mode, !! "fragmentOutputs" )
  
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
      ident.expectKind nnkIdent
      case $ident.ident
      of "numVertices":
        result.add( newCall(bindSym"numVertices", section[1] ) )
      of "numInstances":
        result.add( newCall(bindSym"numInstances", section[1] ) )
      of "vertexOffset":
        result.add( newCall(bindSym"vertexOffset", section[1] ) )

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
            bindSym"type", newDotExpr(identNode,!!"T")))
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
            error "unreachable"


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
        
  if wrapWithDebugResult:
    result = newCall( bindSym"debugResult", result )
