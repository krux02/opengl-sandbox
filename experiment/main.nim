

# TODO document current steps
# TODO frame code generation for rendering
# TODO optimization for pulling up variables into CPU
# TODO variables should not conflict with glsl keywords

# stdlib
import sugar, algorithm, macros, strutils, tables, sequtils
# packages
import ../fancygl, ast_pattern_matching
# local stuff
import normalizeType, glslTranslate, boring_stuff


type
  ShaderKind* = enum
    skVert = "vert"
    skTesc = "tesc",
    skTese = "tese",
    skGeom = "geom",
    skFrag = "frag",
    skComp = "comp"

const
  ColorReset = "\e[0m" # background and foreground to default
  ErrorStyle = "\e[31m" # Red
  ErrorStyle2 = "\e[91m" # LightRed
  LineNumberStyle = "\e[33m" # Yellow
  BarStyle = "\e[30m\e[43m" # Black and Yellow


proc deduplicate(arg: var seq[NimNode]): void =
  arg = sequtils.deduplicate(arg)

proc validateShader(src: string, sk: ShaderKind): void {.compileTime.} =
  let log = staticExec("glslangValidator --stdin -S " & $sk, src, "true")
  if log.len > 0:
    echo "  glsl errors:  ".center(80,'#')
    var problems = newSeq[tuple[lineNr: int, message: string]](0)
    for line in log.splitLines:
      if line.startsWith("ERROR: 0:"):
        var i = 9
        while line[i].isDigit:
          i += 1
        let lineNr = parseInt(line[9 ..< i])
        let message = line[i .. ^1]

        problems.add((lineNr: lineNr, message: message))

    echo(BarStyle, " start Shader Problems ".center(80,'='), ColorReset)
    var lineNr = 0
    for line in src.splitLines:
      lineNr += 1
      echo(LineNumberStyle, intToStr(lineNr, 4), " ", ColorReset, line)
      for problem in problems:
        if problem.lineNr == lineNr:
          echo("     ", ErrorStyle, problem.message)
    echo(BarStyle, repeat("-",80), ColorReset)
    echo(ErrorStyle2, log)
    echo(BarStyle, center(" end Shader Problems ",80,'='), ColorReset)

  else:
    echo "compile ",sk,":"," OK"


proc indentCode(arg: string): string =
  result = ""
  var indentation = 0
  for line in splitLines(arg):
    indentation -= line.count("}")
    result.add "  ".repeat(max(indentation, 0))
    result.add line
    result.add "\n"
    indentation += line.count("{")

proc symKind(arg: NimNode): NimSymKind =
  if arg.kind == nnkHiddenDeref:
    symKind(arg[0])
  else:
    macros.symKind(arg)

var buffer: string

proc compileToGlsl(result: var string; arg: NimNode): void =
  arg.matchAst(errorSym):
  of {nnkFloat32Lit,nnkFloat64Lit,nnkFloatLit}:
    result.add arg.floatVal
  of {nnkInt32Lit, nnkInt64Lit, nnkIntLit}:
    result.add arg.intVal
  of nnkEmpty:
    result.add "/* empty */"
  of nnkCommentStmt:
    result.add "/// "
    result.add arg.strVal
    result.add "\n"
  of nnkStmtList:
    for stmt in arg:
      result.compileToGlsl(stmt)
      result.add ";\n"
  of {nnkIdent, nnkSym}:
    buffer = ""
    for c in arg.repr:
      if c != '_': # underscore is my personal separator
        buffer.add c
    if glslKeywords.binarySearch(buffer) < 0:
      result.add buffer
    else:
      result.add buffer
      result.add "_XXX"

  of nnkDotExpr(`lhs`, `rhs`):
    # I am pretty sure this is a big hack
    let symKind =
      if lhs.kind == nnkHiddenDeref:
        lhs[0].symKind
      else:
        lhs.symKind
    result.compileToGlsl(lhs)
    if symKind in {nskParam, nskResult}:
      result.add '_'
    else:
      result.add '.'
    result.compileToGlsl(rhs)
  of nnkAsgn(`lhs`, `rhs`):
    result.compileToGlsl(lhs)
    result.add " = "
    result.compileToGlsl(rhs)
  of {nnkHiddenDeref, nnkHiddenAddr}(`sym`):
    result.compileToGlsl(sym)
  of nnkInfix(`op`, `lhs`, `rhs`):
    result.add "("
    result.compileToGlsl(lhs)
    result.add " "
    result.add op.strVal
    result.add " "
    result.compileToGlsl(rhs)
    result.add ")"
  of {nnkLetSection, nnkVarSection}:
    for memberSym, value in arg.fieldValuePairs:
      let typeStr = memberSym.getTypeInst.glslType
      result.add typeStr
      result.add ' '
      result.compileToGlsl(memberSym)
      result.add " = "
      if value.kind != nnkEmpty:
        result.compileToGlsl(value)
      else:
        result.add typeStr
        result.add "(0)"

  of nnkCall:
    arg[0].expectKind nnkSym
    # TODO, do something about also translating not builtin functions

    result.add arg[0].strVal
    result.add "("
    for arg in arg.args:
      result.compileToGlsl(arg)
      result.add ", "

    result[^2] = ')'

  of nnkBracketExpr(`syma`, `symb`):
    result.compileToGlsl(syma)
    result.add '['
    result.compileToGlsl(symb)
    result.add ']'

  of nnkBlockStmt(
    `sym1` @ nnkSym,
    nnkStmtList(
      nnkVarSection(
        nnkIdentDefs( `loopVar` @ nnkSym, nnkEmpty, nnkEmpty)
      ),
      nnkStmtList(
        nnkCommentStmt,
        nnkVarSection( nnkIdentDefs( `loopIndex` @ nnkSym, nnkEmpty, 0)),
        nnkIfStmt(
          nnkElifBranch(
            nnkInfix( ident"<=", _ #[ `loopIndex` ]#, `upperBound` @ nnkIntLit),
            nnkBlockStmt(
              `blockSym2`,
              nnkWhileStmt(
                1,
                nnkStmtList(
                  nnkStmtList(
                    nnkFastAsgn(_ #[`loopVar`]#, nnkBracketExpr(`collectionSym`,_ #[`loopIndex`]#)),
                    `body`
                  ),
                  nnkIfStmt(
                    nnkElifBranch(
                      nnkStmtListExpr(
                        nnkCommentStmt,
                        nnkInfix(ident"<=", _ #[`upperBound`]#, _ #[`loopIndex`]#)
                      ),
                      nnkBreakStmt( _ #[`sym1`]#)
                    ),
                  ),
                  nnkCall( ident"inc", _ #[`loopIndex`]# , 1 )
                )
              )
            )
          )
        )
      )
    )
  ):
    let loopIndexTrue = genSym(nskVar, "i")
    let irepr = loopIndexTrue.repr
    result.add "for(int "
    result.add irepr
    result.add " = 0; "
    result.add irepr
    result.add " < "
    result.add(upperBound.intVal+1)
    result.add "; ++"
    result.add irepr
    result.add ") {\n"

    # TODO this is actually correct, but for now I cheat around fixing it
    # result.add loopVar.getTypeInst.glslType
    result.add loopVar.getTypeInst.strVal
    result.add ' '
    result.compileToGlsl(loopVar)
    result.add " = "
    result.compileToGlsl(collectionSym)
    result.add "[", irepr, "];\n{\n"
    result.compileToGlsl body
    result.add "\n}}"



macro render_inner(debug: static[bool], mesh, arg: typed): untyped =

  arg.expectKind nnkDo
  arg.matchAst:
  of nnkDo(
    _,_,_,
    nnkFormalParams(
      _, `vertexDef` @ nnkIdentDefs, `glDef` @ nnkIdentDefs
    ),
    _,_,
    `body` @ nnkStmtList,
    `resultSym`
  ):
    if debug:
      echo "<render_inner>"
      #echo arg.treeRepr

    # this is really a hack because arguments are not symbols for some reason
    # ¯\_(ツ)_/¯
    let vertexSym = body.findSymbolWithName(vertexDef[0].strVal)
    let glSym     = body.findSymbolWithName(glDef[0].strVal)

    let vertexPart = nnkStmtList.newTree
    let fragmentPart = nnkStmtList.newTree
    block:
      var currentNode = vertexPart
      for stmt in body:
        if stmt.kind == nnkCommentStmt and stmt.strVal == "rasterize":
          currentNode = fragmentPart
        currentNode.add stmt

    var usedProcSymbols   = newSeq[NimNode](0)
    var usedVarLetSymbols = newSeq[NimNode](0)
    var symCollection = newSeq[NimNode](0)
    var usedTypes = newSeq[NimNode](0)

    body.matchAstRecursive:
    of `sym` @ nnkSym:
      let symKind = sym.symKind
      if symKind == nskProc:
        usedProcSymbols.add sym
      elif symKind in {nskLet, nskVar, nskForVar}:
        usedVarLetSymbols.add sym
      elif symKind == nskType:
        usedTypes.add sym.normalizeType
      else:
        symCollection.add sym

    usedVarLetSymbols.deduplicate
    for sym in usedVarLetSymbols:
      usedTypes.add sym.getTypeInst.normalizeType
    usedTypes.deduplicate

    var typesToGenerate = newSeq[NimNode](0)
    for tpe in usedTypes:
      # TODO, well this is not really a correct filtering mechanism for gathering types.
      # There must be a smartey way to find out what is defined in glsl and what is not.
      if tpe.eqIdent "Light":
        typesToGenerate.add tpe

    symCollection.deduplicate   # TODO unused(symCollection)
    usedProcSymbols.deduplicate # TODO unused(usedProcSymbols)

    # collect all symbols that have been declared in the vertex shader

    var localSymbolsVS: seq[NimNode] = @[]
    vertexPart.matchAstRecursive:
    of `section` @ {nnkVarSection, nnkLetSection}:
      for sym, _ in section.fieldValuePairs:
        localSymbolsVS.add sym

    var uniformSamplers = newSeq[NimNode](0)
    var uniformRest = newSeq[NimNode](0)

    var attributesFromVS = newSeq[NimNode](0)
    vertexPart.matchAstRecursive:
    of `attribAccess` @ nnkDotExpr(`lhs`, `rhs`) |= lhs == vertexSym:
      attributesFromVS.add attribAccess
    of `attribAccess` @ nnkDotExpr(`lhs`, `rhs`) |= lhs == glSym:
      discard
    of `sym` @ nnkSym |= sym.symKind in {nskLet, nskVar} and sym notin localSymbolsVS:
      if sym.isSampler:
        uniformSamplers.add sym
      else:
        uniformRest.add sym
    # TODO this is O(n^2), why does sortAndUnique fail?
    attributesFromVS.deduplicate

    var localSymbolsFS: seq[NimNode] = @[]
    fragmentPart.matchAstRecursive:
    of `section` @ {nnkVarSection, nnkLetSection}:
      for sym, _ in section.fieldValuePairs:
        localSymbolsFS.add sym

    # now look for all usages of symbols from the vertex shader in the fragment shader
    # TODO optimization skip symbols declarations
    var simpleVaryings = newSeq[Nimnode](0)
    var attributesFromFS = newSeq[Nimnode](0)
    fragmentPart.matchAstRecursive:
    of `sym` @ nnkSym |= sym.symkind in {nskLet, nskVar}:
      # a symbol that is used in the fragment shader
      if sym in localSymbolsVS:
        # that is declared in the fragment sader is a varying
        simpleVaryings.add sym
      elif sym notin localSymbolsFS:
        # when it is not local to the fragment shader, it is a uniform
        if sym.isSampler:
          uniformSamplers.add sym
        else:
          uniformRest.add sym

    of `attribAccess` @ nnkDotExpr(`lhs`, `rhs`) |= lhs == glSym:
      discard
    of `attribAccess` @ nnkDotExpr(`lhs`, `rhs`) |= lhs == vertexSym:
      attributesFromFS.add attribAccess

    attributesFromFS.deduplicate
    simpleVaryings.deduplicate
    uniformSamplers.deduplicate
    uniformRest.deduplicate

    ## split uniforms into texture/sampler uniforms and non-texture uniforms

    let allAttributes = mergeUnique(attributesFromVS, attributesFromFS)
    let allVaryings   = simpleVaryings & attributesFromFS

    ############################################################################
    ################################ shared code ###############################
    ############################################################################

    var sharedCode = ""

    # generate types
    sharedCode.add "// types section\n"
    for tpe in typesToGenerate:
      let impl = tpe.getTypeImpl

      impl.matchAst:
      of nnkObjectTy(
        nnkEmpty, nnkEmpty,
        `recList` @ nnkRecList
      ):
        sharedCode.add "struct ", tpe.repr, "{\n"
        for field,tpe in recList.fields:
          sharedCode.add tpe.glslType, " "
          sharedCode.compileToGlsl field
          sharedCode.add ";\n"
        sharedCode.add "};\n"

    # uniforms
    sharedCode.add "// uniforms section\n"
    sharedCode.add "layout (std140) uniform shader_data {\n"
    for uniform in uniformRest:
      sharedCode.add uniform.getTypeInst.glslType, " "
      sharedCode.compileToGlsl(uniform)
      sharedCode.add ";\n"
    sharedCode.add "};\n"
    for i, uniform in uniformSamplers:
      sharedCode.add "layout(binding=", 0, ") "
      sharedCode.add "uniform ", uniform.getTypeInst.glslType, " "
      sharedCode.compileToGlsl(uniform)
      sharedCode.add ";\n"

    ############################################################################
    ########################## generate vertex shader ##########################
    ############################################################################

    var vertexShader   = "#version 450\n"

    vertexShader.add sharedCode

    vertexShader.add "// all attributes\n"
    for i, attrib in allAttributes:
      vertexShader.add "in layout(location=", i, ") ", attrib.getTypeInst.glslType, " in_"
      vertexShader.compileToGlsl(attrib)
      vertexShader.add ";\n"

    vertexShader.add "// all varyings\n"
    for i, varying in allVaryings:
      vertexShader.add "out layout(location=", i, ") ", varying.getTypeInst.glslType, " out_"
      vertexShader.compileToGlsl(varying)
      vertexShader.add ";\n"

    vertexShader.add "void main() {\n"

    vertexShader.add "// convert used attributes to local variables (because reasons)\n"
    for i, attrib in attributesFromVS:
      vertexShader.add attrib.getTypeInst.glslType, " "
      vertexShader.compileToGlsl(attrib)
      vertexShader.add " = in_"
      vertexShader.compileToGlsl(attrib)
      vertexShader.add ";\n"

    vertexShader.add "// glsl translation of main body\n"
    vertexShader.compileToGlsl(vertexPart)

    vertexShader.add "// forward attributes that are used in the fragment shader\n"
    for i, attrib in attributesFromFS:
      vertexShader.add "out_"
      vertexShader.compileToGlsl(attrib)
      vertexShader.add " = in_"
      vertexShader.compileToGlsl(attrib)
      vertexShader.add ";\n"

    vertexShader.add "// forward other varyings\n"
    for i, varying in simpleVaryings:
      vertexShader.add "out_"
      vertexShader.compileToGlsl(varying)
      vertexShader.add " = "
      vertexShader.compileToGlsl(varying)
      vertexShader.add ";\n"

    vertexShader.add "}\n"

    ############################################################################
    ######################### generate fragment shaders ########################
    ############################################################################

    # TODO uniform locations are incorrect, use uniform buffer.


    var fragmentShader = "#version 450\n"

    fragmentShader.add sharedCode

    fragmentShader.add "// fragment output symbols\n"
    var i = 0
    for memberSym,typeSym in resultSym.getTypeImpl.fields:
      fragmentShader.add "out layout(location=", i,") ", typeSym.glslType, " "
      fragmentShader.compileToGlsl(resultSym)
      fragmentShader.add "_"
      fragmentShader.compileToGlsl(memberSym)
      fragmentShader.add ";\n"
      i += 1

    fragmentShader.add "// all varyings\n"
    for i, varying in allVaryings:
      fragmentShader.add "in layout(location=", i, ") ", varying.getTypeInst.glslType, " in_"
      fragmentShader.compileToGlsl(varying)
      fragmentShader.add ";\n"

    fragmentShader.add "void main() {\n"
    fragmentShader.add "// convert varyings to local variables (because reasons)\n"
    for i, varying in allVaryings:
      fragmentShader.add varying.getTypeInst.glslType, " "
      fragmentShader.compileToGlsl(varying)
      fragmentShader.add " = in_"
      fragmentShader.compileToGlsl(varying)
      fragmentShader.add ";\n"

    fragmentShader.compileToGlsl(fragmentPart)
    fragmentShader.add "}\n"

    echo "|> vertex shader <|".center(80,'=')
    echo vertexShader.indentCode
    echo "|> fragment shader <|".center(80,'=')
    echo fragmentShader.indentCode
    echo "=".repeat(80)

    validateShader(vertexShader, skVert)
    validateShader(fragmentShader, skFrag)

    ############################################################################
    ############################# generate nim code ############################
    ############################################################################

    let vertexShaderLit = newLit(vertexShader)
    let fragmentShaderLit = newLit(fragmentShader)

    let lineinfoLit = newLit(body.lineinfoObj)
    let vaoSym = genSym(nskVar, "vao")

    let programSym = genSym(nskVar, "program")

    let initialization = newStmtList()

    ## attribute initialization

    let dummySym = genSym(nskVar, "dummy")
    let vertexType = vertexSym.getTypeInst

    initialization.add quote do:
      glCreateVertexArrays(1, `vaoSym`.handle.addr)
      var `dummySym`: `vertexType`

    for i, attrib in allAttributes:
      let iLit = newLit(uint32(i))
      let memberSym = attrib[1]
      let memberType = attrib.getTypeInst.normalizeType

      #echo attrib.repr, " -- ", attrib.getTypeInst.normalizeType.repr
      initialization.add quote do:
        # TODO there is no divisor inference
        glVertexArrayBindingDivisor(`vaoSym`.handle, `iLit`, 0)
        let size           = attribSize(`memberType`)
        let typeArg        = attribType(`memberType`)
        let normalized     = attribNormalized(`memberType`)
        let relativeoffset = GLuint(cast[int](`dummySym`.`memberSym`.addr) - cast[int](`dummySym`.addr))
        glVertexArrayAttribFormat(`vaoSym`.handle, `iLit`, size, typeArg, normalized, relativeoffset)
        glVertexArrayAttribBinding(`vaoSym`.handle, `iLit`, `iLit`)

    ## uniform initialization


    let draw = newStmtList()

    draw.add newCommentStmtNode("passing uniform")


    if uniformRest.len > 0:
      let uniformTupleType = nnkTupleTy.newTree
      for uniform in uniformRest:
        uniformTupleType.add nnkIdentDefs.newTree(
          ident(uniform.strVal),
          uniform.getTypeInst,
          newEmptyNode()
        )

      let uniformsTupleSym = genSym(nskVar, "uniforms")

      draw.add quote do:
        var `uniformsTupleSym`: `uniformTupleType`


      for uniform in uniformRest:
        let member = ident(uniform.strVal)
        draw.add newAssignment(newDotExpr(uniformsTupleSym, member), uniform)

    if uniformSamplers.len > 0:
       discard



    # result = quote do:
    #   var `programSym`: Program
    #   var `vaoSym`: VertexArrayObject
    #   var `uniformBlockIndexSym`: GLuint
    #   var `uniformBlockBufferSym`: GLuint

    #   if `programSym`.handle == 0:
    #     `programSym`.handle = glCreateProgram()
    #     `programSym`.attachAndDeleteShader( compileShader(GL_VERTEX_SHADER, `vertexShaderLit`, `lineinfoLit`) )
    #     `programSym`.attachAndDeleteShader( compileShader(GL_FRAGMENT_SHADER, `fragmentShader`, `lineinfoLit`) )
    #     `programSym`.linkOrDelete

    #      `uniformBlockIndexSym` = glGetUniformBlockIndex(programSym.handle, ​"shader_data")
    #      glUniformBlockBinding(`programSym`.handle, `uniformBlockIndexSym`, `uniformBlockIndexSym`)


    #      glCreateUniformBlockBuffer

    #     `initialization`

    #   `draw`



    # sharedCode.add "// uniforms section\n"
    # sharedCode.add "layout (std140) uniform shader_data {\n"
    # for uniform in uniformRest:
    #   sharedCode.add uniform.getTypeInst.glslType, " "
    #   sharedCode.compileToGlsl(uniform)
    #   sharedCode.add ";\n"
    # sharedCode.add "};\n"
    # for i, uniform in uniformSamplers:
    #   sharedCode.add "layout(binding=", 0, ") "
    #   sharedCode.add "uniform ", uniform.getTypeInst.glslType, " "
    #   sharedCode.compileToGlsl(uniform)
    #   sharedCode.add ";\n"



  if debug:
    echo result.repr
    echo "</render_inner>"


proc `or`(arg, alternative: NimNode): NimNode {.compileTime.} =
  if arg.kind != nnkEmpty:
    arg
  else:
    alternative

type
  Mesh[VertexType] = object

  Framebuffer[FragmentType] = object
    handle: GLuint
    size: Vec2i

  GlEnvironment = object
    Position: Vec4f
    PointSize: float32
    ClipDistance: UncheckedArray[float32]


proc create[FragmentType](result: var Framebuffer[FragmentType]): void =
  glCreateFramebuffers(1, result.handle.addr)

  var depth_texture: GLuint
  glCreateTextures(GL_TEXTURE_2D, 1, depth_texture.addr)
  glTextureParameteri(depth_texture, GL_TEXTURE_MIN_FILTER, GLint(GL_NEAREST))
  glTextureParameteri(depth_texture, GL_TEXTURE_MAG_FILTER, GLint(GL_NEAREST))
  glTextureParameteri(depth_texture, GL_TEXTURE_WRAP_S, GLint(GL_CLAMP_TO_EDGE))
  glTextureParameteri(depth_texture, GL_TEXTURE_WRAP_T, GLint(GL_CLAMP_TO_EDGE))
  glTextureStorage2D(depth_texture, 1, GL_DEPTH_COMPONENT24, result.size.x, result.size.y)

  glNamedFramebufferTexture(result.handle, GL_DEPTH_ATTACHMENT, depth_texture, 0)

  var fragment: FragmentType

  var i = 0
  for field in fragment.fields:
    # Build the texture that will serve as the color attachment for the framebuffer.
    var texture_attachment: GLuint
    glCreateTextures(GL_TEXTURE_2D, 1, texture_attachment.addr)
    glTextureParameteri(texture_attachment, GL_TEXTURE_MIN_FILTER, GLint(GL_LINEAR));
    glTextureParameteri(texture_attachment, GL_TEXTURE_MAG_FILTER, GLint(GL_LINEAR));
    glTextureParameteri(texture_attachment, GL_TEXTURE_WRAP_S, GLint(GL_CLAMP_TO_BORDER));
    glTextureParameteri(texture_attachment, GL_TEXTURE_WRAP_T, GLint(GL_CLAMP_TO_BORDER));
    glTextureStorage2D(texture_attachment, 1, GL_RGBA8, result.size.x, result.size.y)
    glNamedFramebufferTexture(result.handle, GL_COLOR_ATTACHMENT0 + GLenum(i), texture_attachment, 0)

    i += 1

  let status = glCheckNamedFramebufferStatus(result.handle, GL_FRAMEBUFFER)
  assert status == GL_FRAMEBUFFER_COMPLETE

  echo "creating framebuffer is ok"

proc createFramebuffer[FragmentType](size: Vec2i): Framebuffer[FragmentType] =
  result.size = size
  result.create


proc injectTypes(framebuffer, mesh, arg: NimNode): NimNode {.compileTime.} =
  ## Inject types to the ``render .. do: ...`` node.
  arg.expectKind(nnkDo)
  let formalParams = arg[3]
  formalParams.expectKind(nnkFormalParams)

  let vertexTypeExpr = quote do:
    `mesh`.type.VertexType

  let fragmentTypeExpr = quote do:
    `framebuffer`.type.FragmentType

  let resultType = formalParams[0] or fragmentTypeExpr
  var vertexArg, vertexType, envArg, envType: NimNode

  formalParams.matchAst:
  of nnkFormalParams(_,nnkIdentDefs(`vertexArgX`, `envArgX`,nnkEmpty,_)):
    vertexArg = vertexArgX
    vertexType = vertexTypeExpr
    envArg = envArgX
    envType = nnkVarTy.newTree(bindSym"GlEnvironment")
  of nnkFormalParams(_,nnkIdentDefs(`vertexArgX`, `vertexTypeX`, _), nnkIdentDefs(`envArgX`, `envTypeX`,_)):
    vertexArg = vertexArgX
    vertexType = vertexTypeX or vertexTypeExpr
    envArg = envArgX
    envType = envTypeX or nnkVarTy.newTree(bindSym"GlEnvironment")

  let newParams =
    nnkFormalParams.newTree(
      resultType,
      nnkIdentDefs.newTree(
        vertexArg, vertexType, newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        envArg, envType, newEmptyNode()
      )
    )

  result = arg
  result[3] = newParams

macro render*(framebuffer, mesh: typed; arg: untyped): untyped =
  let arg = injectTypes(framebuffer, mesh, arg)
  result = quote do:
    let fb = `framebuffer`
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fb.handle)
    render_inner(false, `mesh`, `arg`)
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0)

macro renderDebug(framebuffer, mesh: typed; arg: untyped): untyped =
  let arg = injectTypes(framebuffer, mesh, arg)
  result = quote do:
    let fb = `framebuffer`
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fb.handle)
    render_inner(true, `mesh`, `arg`)
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0)

################################################################################
################################## user  code ##################################
################################################################################

type
  MyFragmentType = object
    color: Vec4f

  MyVertexType = object
    position_os: Vec4f
    normal_os: Vec4f
    texCoord: Vec2f

  Light = object
    position_ws : Vec4f
    color : Vec4f

  MyMesh        = Mesh[MyVertexType]
  MyFramebuffer = Framebuffer[MyFragmentType]

let (window, context) = defaultSetup()

var myTexture: Texture2D = loadTexture2DFromFile(getResourcePath("crate.png"))
var myMeshArrayBuffer = newArrayBuffer[MyVertexType](boxVertices.len)

for i, vertex in myMeshArrayBuffer.wPairs:
  vertex.position_os = boxVertices[i]
  vertex.normal_os   = boxNormals[i]
  vertex.texCoord    = boxTexCoords[i]

var mesh: MyMesh
var framebuffer: MyFramebuffer = createFramebuffer[MyFragmentType](window.size)

var M,V,P: Mat4f

M = mat4f(1)
V = mat4f(1).translate( 0, 0, -7)
P = perspective(45'f32, window.aspectRatio, 0.1, 100.0)

var lights: array[4,Light]


lights[0].position_ws = vec4f(-4,-4,0,1)
lights[1].position_ws = vec4f(-4, 4,0,1)
lights[2].position_ws = vec4f( 4,-4,0,1)
lights[3].position_ws = vec4f( 4, 4,0,1)

lights[0].color = vec4f(1,0,1,1)
lights[1].color = vec4f(1,0,0,1)
lights[2].color = vec4f(0,1,0,1)
lights[3].color = vec4f(0,0,1,1)


framebuffer.renderDebug(mesh) do (v, gl):
  gl.Position     = P * V * M * v.position_os
  let position_cs = V*M*v.position_os
  let normal_cs   = inverse(transpose(V*M)) * v.normal_os

  var inout = 123   # test identifier that is keyword in glsl
  ## rasterize
  var lighting: Vec4f
  for light in lights:
    let light_position_cs = V * light.position_ws
    let light_direction_cs = light_position_cs-position_cs
    let light_intensity = max(dot(light_direction_cs, normal_cs), 0)
    lighting += light_intensity * light.color

  let textureSample = texture(myTexture, v.texCoord)
  result.color = textureSample * lighting


proc manualDrawCode(): void

var
  buffer0 = myMeshArrayBuffer.view(position_os) # setBuffer(vao, 0, buffer0)
  buffer1 = myMeshArrayBuffer.view(normal_os)   # setBuffer(vao, 1, buffer1)
  buffer2 = myMeshArrayBuffer.view(texCoord)    # setBuffer(vao, 2, buffer2)

proc render2(): void =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  manualDrawCode()

  glSwapWindow(window)

var runGame = true
while runGame:
  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SCANCODE_ESCAPE:
        runGame = false
      of SCANCODE_F10:
        window.screenshot
      else:
        discard


  M.rotateInplZ(0.01)
  M.rotateInplX(0.0354)

  render2()

################################################################################
################################# manual  code #################################
################################################################################

# test code before generating it

const
  vertexShaderSrc = """
#version 450
// types section
struct Light{
  vec4 positionws;
  vec4 color;
};
// uniforms section
layout (std140) uniform shader_data {
  mat4 P;
  mat4 V;
  mat4 M;
  Light[10] lights;
};
layout(binding=0) uniform sampler2D myTexture;
// all attributes
in layout(location=0) vec4 in_v_positionos;
in layout(location=1) vec4 in_v_normalos;
in layout(location=2) vec2 in_v_texCoord;
// all varyings
out layout(location=0) vec4 out_positioncs;
out layout(location=1) vec4 out_normalcs;
out layout(location=2) vec2 out_v_texCoord;
void main() {
  // convert used attributes to local variables (because reasons)
  vec4 v_positionos = in_v_positionos;
  vec4 v_normalos = in_v_normalos;
  // glsl translation of main body
  gl_Position = (((P * V) * M) * v_positionos);
  vec4 positioncs = ((V * M) * v_positionos);
  vec4 normalcs = (inverse(transpose((V * M)) )  * v_normalos);
  int inout_XXX = 123;
  // forward attributes that are used in the fragment shader
  out_v_texCoord = in_v_texCoord;
  // forward other varyings
  out_positioncs = positioncs;
  out_normalcs = normalcs;
}
"""
  fragmentShaderSrc = """
#version 450
// types section
struct Light{
  vec4 positionws;
  vec4 color;
};
// uniforms section
layout (std140) uniform shader_data {
  mat4 P;
  mat4 V;
  mat4 M;
  Light[10] lights;
};
layout(binding=0) uniform sampler2D myTexture;
// fragment output symbols
out layout(location=0) vec4 result_color;
// all varyings
in layout(location=0) vec4 in_positioncs;
in layout(location=1) vec4 in_normalcs;
in layout(location=2) vec2 in_v_texCoord;
void main() {
  // convert varyings to local variables (because reasons)
  vec4 positioncs = in_positioncs;
  vec4 normalcs = in_normalcs;
  vec2 v_texCoord = in_v_texCoord;
  /// rasterize
  ;
  vec4 lighting = vec4(0);
  for(int i575802 = 0; i575802 < 10; ++i575802) {
    Light light = lights[i575802];
    {
      vec4 lightpositioncs = (V * light.positionws);
      vec4 lightdirectioncs = (lightpositioncs - positioncs);
      float lightintensity = max(dot(lightdirectioncs, normalcs),0);
      (lighting += (lightintensity * light.color));

  }};
  vec4 textureSample = texture(myTexture, v_texCoord, 0.0) ;
  result_color = (textureSample * lighting);
  //result_color = vec4(1);
}
"""


echo "############################## manual code running frome here ##############################"

type
  UniformBufferType = tuple[P: Mat4f, V: Mat4f, M: Mat4f, lights: array[0 .. 3, Light]]

var
  program: Program
  vao: VertexArrayObject
  uniformBuffer: UniformBuffer[UniformBufferType]

const lineinfo = LineInfo(filename: "main.nim", line: 748, column: 2)


echo buffer0
echo buffer1
echo buffer2

proc manualDrawCode(): void =
  ## this code block should eventually be generated

  if program.handle == 0:
    program.handle = glCreateProgram()
    program.attachAndDeleteShader(
      compileShader(GL_VERTEX_SHADER, vertexShaderSrc, lineinfo))
    program.attachAndDeleteShader(
      compileShader(GL_FRAGMENT_SHADER, fragmentShaderSrc, lineinfo))

    program.linkOrDelete

    glCreateVertexArrays(1, vao.handle.addr)

    glCreateBuffers(1, uniformBuffer.handle.addr)
    glNamedBufferStorage(
      uniformBuffer.handle, sizeof(UniformBufferType), nil,
      GL_MAP_WRITE_BIT or GL_DYNAMIC_STORAGE_BIT or GL_MAP_PERSISTENT_BIT
    )

    glEnableVertexArrayAttrib(vao.handle, 0'u32)
    glVertexArrayBindingDivisor(vao.handle, 0'u32, 0)
    glVertexArrayAttribFormat(vao.handle, 0'u32, attribSize(buffer0.T), attribType(buffer0.T), attribNormalized(buffer0.T), buffer0.relativeoffset);
    glVertexArrayAttribBinding(vao.handle, 0'u32, 0'u32)

    glEnableVertexArrayAttrib(vao.handle, 1'u32)
    glVertexArrayBindingDivisor(vao.handle, 1'u32, 0)
    glVertexArrayAttribFormat(vao.handle, 1'u32, attribSize(buffer1.T), attribType(buffer1.T), attribNormalized(buffer1.T), buffer1.relativeoffset);
    glVertexArrayAttribBinding(vao.handle, 1'u32, 1'u32)

    glEnableVertexArrayAttrib(vao.handle, 2'u32)
    glVertexArrayBindingDivisor(vao.handle, 2'u32, 0)
    glVertexArrayAttribFormat(vao.handle, 2'u32, attribSize(buffer2.T), attribType(buffer2.T), attribNormalized(buffer2.T), buffer2.relativeoffset);
    glVertexArrayAttribBinding(vao.handle, 2'u32, 2'u32)

  ## passing uniform

  let uniformPointer = cast[ptr UniformBufferType](glMapNamedBufferRange(
    uniformBuffer.handle, 0, sizeof(UniformBufferType),
    GL_MAP_WRITE_BIT
  ))

  uniformPointer.P = P
  uniformPointer.V = V
  uniformPointer.M = M
  uniformPointer.lights = lights

  doAssert glUnmapNamedBuffer(uniformBuffer.handle)

  glUseProgram(program.handle)
  glBindVertexArray(vao.handle)

  glBindBufferBase(GL_UNIFORM_BUFFER, 0, uniformBuffer.handle)

  setBuffers(vao, 0, buffer0, buffer1, buffer2)

  var textureHandles = [myTexture.handle]
  glBindTextures(0, GLsizei(textureHandles.len), textureHandles[0].addr)

  let numVertices = GLsizei(len(myMeshArrayBuffer))
  glDrawArrays(GL_TRIANGLES, 0, numVertices)
