import typetraits, streams, macros, strutils

type ArrayBuffer[T]        = distinct uint32
#type ElementArrayBuffer[T] = distinct GLuint
#type UniformBuffer[T]      = distinct GLuint

type ShaderParam =
  tuple[name: string, gl_type: string]


##### THE_TEMPLATE #####

template renderBlockTemplate(globalsBlock, sequenceInitBlock,
               bufferCreationBlock, setUniformsBlock: expr): stmt {. dirty .} =
  block:
    var vao {.global.}: VertexArrayObject
    var glProgram {.global.}: GLuint  = 0

    globalsBlock

    if glProgram == 0:

      sequenceInitBlock

      gl_program = linkShader(
        compileShader(GL_VERTEX_SHADER,   genShaderSource(uniforms, true, attributes, true, varyings, vertexSrc)),
        compileShader(GL_FRAGMENT_SHADER, genShaderSource(uniforms, true, varyings, false, fragOut, fragmentSrc)),
      )

      glUseProgram(gl_program)
      vao = newVertexArrayObject()
      bindIt(vao)

      bufferCreationBlock

      glBindBuffer(GL_ARRAY_BUFFER, 0)
      bindIt(nil_vao)
      glUseProgram(0)

    glUseProgram(gl_program)

    bindIt(vao)

    setUniformsBlock

    glDrawArrays(GL_TRIANGLES, 0, GLsizei(len(vertex)))

    bindIt(nil_vao)
    glUseProgram(0);


macro macro_test(statement: expr) : stmt =

  let lhsName = "projection"
  let rhsName = "projection_mat"

  let attributesSection = newNimNode(nnkBracket)
  let uniformsSection = newNimNode(nnkBracket)
  let varyingsSection = newNimNode(nnkBracket)
  let fragOutSection = newNimNode(nnkBracket)

  let globalsBlock = newStmtList()
  let bufferCreationBlock = newStmtList()
  let setUniformsBlock = newStmtList()

  var attribCount = 0;
  proc addAttrib(lhsIdent, rhsIdent: NimNode): void =
    let lhsStrLit = newLit($lhsIdent)
    let shaderParam = quote do:
      (`lhsStrLit`, glslAttribType(type(`rhsIdent`)))

    attributesSection.add(shaderParam)

    template foobarTemplate( lhs, rhs : expr ) : stmt{.dirty.} =
      var lhs {.global.}: ArrayBuffer[rhs[0].type]

    let line = getAst(foobarTemplate( newIdentNode($lhsIdent & "Buffer"), rhsIdent ))

    globalsBlock.add line
    bufferCreationBlock.add(newCall("glEnableVertexAttribArray", newLit(attribCount)))
    bufferCreationBlock.add(newCall("makeAndBindBuffer",
        newIdentNode(!(lhsName & "_buffer")),
        newLit(attribCount),
        newIdentNode(!rhsName),
        newIdentNode(!"GL_STATIC_DRAW")
    ))

    attribCount += 1

  var uniformCount = 0
  proc addUniform(lhsName, rhsName: string): void =
    let shaderParam = "(\"" & lhsName & "\", glslUniformType(type(" & rhsName & ")))"
    uniformsSection.add(parseExpr(shaderParam))

    setUniformsBlock.add newCall("uniform", newLit(uniformCount), newIdentNode(rhsName))

    uniformCount += 1

  var varyingCount = 0
  proc addVarying(name, typ: string): void =
    let shaderParam = newPar( newLit(name), newLit(typ) )
    varyingsSection.add shaderParam

    varyingCount += 1

  var fragOutCount = 0
  proc addFragOut(name, typ: string): void =
    let  shaderParam = newPar( newLit(name), newLit(typ) )
    fragOutSection.add shaderParam

    fragOutCount += 1

  var vertexSourceNode = newLit("")
  var fragmentSourceNode = newLit("")

  #### BEGIN PARSE TREE ####

  for section in statement.items:
    section.expectKind nnkCall
    let ident = section[0]
    ident.expectKind nnkIdent
    let stmtList = section[1]
    stmtList.expectKind nnkStmtList
    if $ident.ident == "uniforms":
      warning("yay got uniforms with StmtList")
      for capture in stmtList.items:
        capture.expectKind({nnkAsgn, nnkIdent})
        if capture.kind == nnkAsgn:
          capture.expectLen 2
          capture[0].expectKind nnkIdent
          capture[1].expectKind nnkIdent
          addUniform($capture[0], $capture[1])
        elif capture.kind == nnkIdent:
          addUniform($capture, $capture)

    elif $ident.ident == "attributes":
      for capture in stmtList.items:
        capture.expectKind({nnkAsgn, nnkIdent})
        if capture.kind == nnkAsgn:
          capture.expectLen 2
          capture[0].expectKind nnkIdent
          capture[1].expectKind nnkIdent
          addAttrib(capture[0], capture[1])
        elif capture.kind == nnkIdent:
          addAttrib(capture, capture)

    elif $ident.ident == "varyings":
      warning("yay got varyings with StmtList")
      for varSec in stmtList.items:
        varSec.expectKind nnkVarSection
        for def in varSec:
          def.expectKind nnkIdentDefs
          echo " varying "
          def[0].expectKind nnkIdent
          def[1].expectKind nnkIdent
          addVarying( $def[0] , $def[1] )

    elif $ident.ident == "frag_out":
      warning("yay got frag_out with StmtList")
      for varSec in stmtList.items:
        varSec.expectKind nnkVarSection
        for def in varSec:
          def.expectKind nnkIdentDefs
          def.expectKind nnkIdentDefs
          echo " varying "
          def[0].expectKind nnkIdent
          def[1].expectKind nnkIdent
          addFragOut( $def[0] , $def[1] )

    elif $ident.ident == "vertex_prg":
      stmtList.expectLen(1)
      stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
      vertexSourceNode = stmtList[0]
    elif $ident.ident ==  "fragment_prg":
      stmtList.expectLen(1)
      stmtList[0].expectKind({ nnkTripleStrLit, nnkStrLit })
      fragmentSourceNode = stmtList[0]
    else:
      error("unknown section " & $ident.ident)

  #### END PARSE TREE ####

  let sequenceInitBlock = newStmtList()

  var statement:NimNode

  statement = parseStmt(" let attributes: seq[ShaderParam] = @[] ")
  statement[0][0][2][1] = attributesSection
  sequenceInitBlock.add statement

  statement = parseStmt(" let uniforms: seq[ShaderParam] = @[] ")
  statement[0][0][2][1] = uniformsSection
  sequenceInitBlock.add statement

  statement = parseStmt(" let varyings: seq[ShaderParam] = @[] ")
  statement[0][0][2][1] = varyingsSection
  sequenceInitBlock.add statement

  statement = parseStmt(" let fragOut: seq[ShaderParam] = @[] ")
  statement[0][0][2][1] = fragOutSection
  sequenceInitBlock.add statement

  sequenceInitBlock.add newLetStmt(newIdentNode("vertexSrc"), vertexSourceNode)
  sequenceInitBlock.add newLetStmt(newIdentNode("fragmentSrc"), fragmentSourceNode)

  echo "------------------------"
  echo repr(globalsBlock)
  echo "------------------------"
  echo repr(sequenceInitBlock)
  echo "------------------------"
  echo repr(bufferCreationBlock)
  echo "------------------------"
  echo repr(setUniformsBlock)
  echo "------------------------"

  result = getAst( renderBlockTemplate(globalsBlock, sequenceInitBlock,
                                       bufferCreationBlock, setUniformsBlock))

  echo repr(result)


macro_test:
  uniforms:
    projection = projection_mat
    modelview = modelview_mat
    time
  attributes:
    pos = vertex
    col = color
  varyings:
    var v_col : vec4
  frag_out:
    var color : vec4
  vertex_prg:
    """
    gl_Position = projection * modelview * vec4(pos,1);
    v_col = vec4(col,1);
    """
  fragment_prg:
    """
    color = mymix(v_col, time);
    """
