import typetraits, streams, macros

template value(T:type int): string = "int"
template value(T:type float): string = "float"
template value(T:type string): string = "string"

type ShaderParam =
  tuple[name: string, gl_type: string]

type Program =
  ref object
    uniforms: seq[ShaderParam]
    attributes: seq[ShaderParam]
    varyings: seq[ShaderParam]
    frag_out: seq[ShaderParam]
    vertex_prg: string
    fragment_prg: string

proc mySource(): string =
  result = ""
  var fs = newFileStream("tent.nim", fmRead)
  if not isNil(fs):
    var line = ""
    while fs.readLine(line):
      result.add(line)
    fs.close()

macro macro_test(statement: expr) : stmt =
  for section in statement.items:
    if section.kind == nnkCall:
      let ident = section[0]
      if ident.kind == nnkIdent:
        if $ident.ident == "uniforms":
          let stmtList = section[1]
          if stmtList.kind == nnkStmtList:
            warning("yay got uniforms with StmtList")
            for capture in stmtList.items:
              if capture.kind == nnkAsgn:
                warning("  yay got assign")
              elif capture.kind == nnkIdent:
                warning("  yay got ident")
              else:
                error("expected ident or assignment, but got " & $capture.kind)
          else:
            error("expected StmtList node, but got " & $stmtList.kind)
        elif $ident.ident == "attributes":
          let stmtList = section[1]
          if stmtList.kind == nnkStmtList:
            warning("yay got attributes with StmtList")
            for capture in stmtList.items:
              if capture.kind == nnkAsgn:
                warning("  yay got assign")
              elif capture.kind == nnkIdent:
                warning("  yay got ident")
              else:
                error("expected ident or assignment, but got " & $capture.kind)
          else:
            error("expected StmtList node, but got " & $stmtList.kind)
        elif $ident.ident == "varyings":
          let stmtList = section[1]
          if stmtList.kind == nnkStmtList:
            warning("yay got varyings with StmtList")
            for varSec in stmtList.items:
              if varSec.kind == nnkVarSection:
                for def in varSec:
                  if def.kind == nnkIdentDefs:
                    warning("  yay got IdentDefs")
                  else:
                    error("expected IdentDefs, but got " & $def.kind)
              else:
                error("expected VarSection, but got " & $varSec.kind)
          else:
            error("expected StmtList node, but got " & $stmtList.kind)
        elif $ident.ident == "frag_out":
          let stmtList = section[1]
          if stmtList.kind == nnkStmtList:
            warning("yay got frag_out with StmtList")
            for varSec in stmtList.items:
              if varSec.kind == nnkVarSection:
                for def in varSec:
                  if def.kind == nnkIdentDefs:
                    warning("  yay got IdentDefs")
                  else:
                    error("expected IdentDefs, but got " & $def.kind)
              else:
                error("expected VarSection, but got " & $varSec.kind)
          else:
            error("expected StmtList node, but got " & $stmtList.kind)
        elif $ident.ident == "vertex_prg":
          let stmtList = section[1]
          if stmtList.kind == nnkStmtList:
            if stmtList.len == 1 and stmtList[0].kind == nnkTripleStrLit:
              let tripleStrLit = stmtList[0]
              echo "vertvex_prg"
              echo tripleStrLit.strVal
            else:
              error("expected StmtList with one TripleStriLit but got " & $stmtList[0])
        elif $ident.ident ==  "fragment_prg":
          let stmtList = section[1]
          if stmtList.kind == nnkStmtList:
            if stmtList.len == 1 and stmtList[0].kind == nnkTripleStrLit:
              let tripleStrLit = stmtList[0]
              echo "fragment_prg"
              echo tripleStrLit.strVal
            else:
              error("expected StmtList with one TripleStriLit but got " & $stmtList[0])
        else:
          error("unknown section " & $ident.ident)
      else:
        error("expected Ident node but got " & $ident.kind)
    else:
      error("expected Call node but got " & $section.kind)
      discard



  result = quote do:
    var myprog = Program(
      uniforms: @[ ],
      attributes: @[ ],
      varyings: @[ ],
      frag_out: @[ ],
      vertex_prg: "",
      fragment_prg: ""
    )
  echo repr(result)

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
