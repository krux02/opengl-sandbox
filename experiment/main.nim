

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
    defer:
      if debug:
        echo result.lispRepr
        echo "</render_inner>"

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

    symCollection.deduplicate   # TODO unused
    echo "symCollection:   ", symCollection
    usedProcSymbols.deduplicate # TODO unused
    echo "usedProcSymbols: ", usedProcSymbols

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

      echo impl.treeRepr

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

proc `or`(arg, alternative: NimNode): NimNode {.compileTime.} =
  if arg.kind != nnkEmpty:
    arg
  else:
    alternative

type
  Mesh[VertexType] = object
  Framebuffer[FragmentType] = object
  GlEnvironment = object
    Position: Vec4f
    PointSize: float32
    ClipDistance: UncheckedArray[float32]

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
  result = newCall(
    bindSym"render_inner", newLit(false), mesh,
    injectTypes(framebuffer, mesh, arg))

macro renderDebug(framebuffer, mesh: typed; arg: untyped): untyped =
  result = newCall(
    bindSym"render_inner", newLit(true), mesh,
    injectTypes(framebuffer, mesh, arg))

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

var myTexture: Texture2D
var mesh: MyMesh
var framebuffer: MyFramebuffer
var mvp: Mat4f
var M,V,P: Mat4f
var lights: array[10,Light]

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
    let light_intensity = dot(light_direction_cs, normal_cs)
    lighting += light_intensity * light.color

  let textureSample = texture(myTexture, v.texCoord)
  result.color = textureSample * lighting
