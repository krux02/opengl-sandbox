import typetraits, streams, macros, strutils

type ArrayBuffer[T]        = distinct uint32
#type ElementArrayBuffer[T] = distinct GLuint
#type UniformBuffer[T]      = distinct GLuint

type ShaderParam =
  tuple[name: string, gl_type: string]

macro macro_test(statement: expr) : stmt =

  let lhsName = "projection"
  let rhsName = "projection_mat"

  let attributesSection = newNimNode(nnkBracket)
  let uniformsSection = newNimNode(nnkBracket)
  let varyingsSection = newNimNode(nnkBracket)
  let fragOutSection = newNimNode(nnkBracket)

  let globalsBlock = quote do:
    var vao {. global .} : VertexArrayObject
    var gl_program {.global.}: GLuint  = 0

  let bufferCreationBlock = quote do:
    glUseProgram(gl_program)
    vao = newVertexArrayObject()
    bindIt(vao)

  let bufferCreationBlockPost = quote do:
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    bindIt(nil_vao)
    glUseProgram(0)

  let setUniformsBlock = newStmtList()

  var attribCount = 0;
  proc addAttrib(lhsName, rhsName: string): void =
    let shaderParam = "(\"" & lhsName & "\", glslAttribType(type(" & rhsName & ")))"
    attributesSection.add(parseExpr(shaderParam))

    let line = "var " & lhsName & "_buffer {.global.}: ArrayBuffer[" & rhsName & "[0].type]"
    globalsBlock.add parseStmt(line)
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
          addAttrib($capture[0], $capture[1])
        elif capture.kind == nnkIdent:
          addAttrib($capture, $capture)

    elif $ident.ident == "varyings":
      warning("yay got varyings with StmtList")
      for varSec in stmtList.items:
        varSec.expectKind nnkVarSection
        for def in varSec:
          def.expectKind nnkIdentDefs
          warning("  yay got IdentDefs")
    elif $ident.ident == "frag_out":
      warning("yay got frag_out with StmtList")
      for varSec in stmtList.items:
        varSec.expectKind nnkVarSection
        for def in varSec:
          def.expectKind nnkIdentDefs
          warning("  yay got IdentDefs")
    elif $ident.ident == "vertex_prg":
      if stmtList.len == 1 and stmtList[0].kind == nnkTripleStrLit:
        let tripleStrLit = stmtList[0]
        echo "vertvex_prg"
        echo tripleStrLit.strVal
    elif $ident.ident ==  "fragment_prg":
      stmtList.expectLen 1
      let tripleStrLit = stmtList[0]
      tripleStrLit.expectKind nnkTripleStrLit
      echo "fragment_prg"
      echo tripleStrLit.strVal
    else:
      error("unknown section " & $ident.ident)

  for item in bufferCreationBlockPost:
    bufferCreationBlock.add(item)

  echo repr(globalsBlock)

  result = quote do:
    echo "hallo Welt!"

  # parseExpr(" let attributes: seq[ShaderParam] = @[] ")
  # parseExpr(" let uniforms: seq[ShaderParam] = @[] ")
  # parseExpr(" let varyings: seq[ShaderParam] = @[] ")
  # parseExpr(" let fragOut: seq[ShaderParam] = @[] ")

  echo "------------------------"
  echo repr(result)
  echo "------------------------"
  echo repr(bufferCreationBlock)
  echo "------------------------"
  echo repr(attributesSection)
  echo "------------------------"

discard """ dumpTree:
  var myprog = Program(
    uniforms: @[ ("projection", projection_mat.type.uniformType),
                 ("modelview", modelview_mat.type.uniformType),
                 ("time", time.type.uniformType) ],
    attributes: @[ ("pos", vertex.type.glslType),
                   ("col", color.type.glslType) ],
    varyings: @[ ("v_col", "vec4") ],
    frag_out: @[ ("color", "vec4") ],
    vertex_prg: "gl_Position = projection * modelview * vec4(pos,1); v_col = vec4(col,1);",
    fragment_prg: "color = mymix(v_col, time);"
  )
  """

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
