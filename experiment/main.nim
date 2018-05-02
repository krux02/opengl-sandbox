# stdlib
import sugar, algorithm, macros, strutils, tables, sequtils
# packages
import glm, ast_pattern_matching
# local stuff
import normalizeType, glslTranslate, boring_stuff

type
  ShaderKind = enum
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

proc validateShader(src: string, sk: ShaderKind): void {.compileTime.} =
  let log = staticExec("glslangValidator --stdin -S " & $sk, src, "true")
  if log.len > 0:
    echo "#####  glsl errors:  ####"
    var problems = newSeq[tuple[lineNr: int, message: string]](0)
    for line in log.splitLines:
      if line.startsWith("ERROR: 0:"):
        var i = 9
        while line[i].isDigit:
          i += 1
        let lineNr = parseInt(line[9 ..< i])
        let message = line[i .. ^1]

        problems.add((lineNr: lineNr, message: message))

    echo(BarStyle, "==== start Shader Problems =======================================", ColorReset)
    var lineNr = 0
    for line in src.splitLines:
      lineNr += 1
      echo(LineNumberStyle, intToStr(lineNr, 4), " ", ColorReset, line)
      for problem in problems:
        if problem.lineNr == lineNr:
          echo("     ", ErrorStyle, problem.message)
    echo(BarStyle, "------------------------------------------------------------------", ColorReset)
    echo(ErrorStyle2, log)
    echo(BarStyle, "==== end Shader Problems =========================================", ColorReset)

  else:
    echo "compile: OK"


proc indentCode(arg: string, indentation: string): string =
  let N = arg.countLines
  result = newStringOfCap(indentation.len * N + arg.len)
  for line in splitLines(arg):
    result.add indentation
    result.add line


proc compileToGlsl(result: var string; arg: NimNode): void =
  arg.matchAst(errorSym):
  of nnkFloatLit:
    result.add arg.floatVal
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
    for c in arg.strVal:
      if c != '_': # underscore is my personal separator
        result.add c
  of nnkDotExpr(`lhs`, `rhs`):
    result.compileToGlsl(lhs)
    result.add '_'
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
                    nnkFastAsgn(_ #[`loopVar`]#, `loopVarExpr`),
                    `body` @ nnkStmtList
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
    let irepr = loopIndex.repr

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
    result.compileToGlsl(loopVarExpr)
    result.add ";\n"


    result.compileToGlsl body
    result.add "\n}"

proc extractVaryingSymbols(vertexPart, fragmentPart: NimNode): seq[NimNode] =

  # collect all symbols that have been declared in the vertex shader
  var vSymbols: seq[NimNode] = @[]
  vertexPart.matchAstRecursive:
  of `section` @ {nnkVarSection, nnkLetSection}:
    for sym, _ in section.fieldValuePairs:
      vSymbols.add sym


  var vUsedSymbols: seq[NimNode] = @[]
  # TODO optimization skip symbols declarations
  fragmentPart.matchAstRecursive:
  of `sym` @ nnkSym:
    if sym in vSymbols:
      vUsedSymbols.add sym
  #of nnkDotExpr(`lhs`, `rhs`):
  #  discard

  echo vSymbols
  echo vUsedSymbols
  # now look for all usages of symbols from the vertex shader in the fragment shader



macro render_inner(debug: static[bool], mesh, arg: typed): untyped =

  if debug:
    echo "<render_inner>"
    echo arg.treeRepr

  defer:
    if debug:
      echo result.lispRepr
      echo "</render_inner>"

  arg.expectKind nnkDo

  let vertexPart = nnkStmtList.newTree
  let fragmentPart = nnkStmtList.newTree

  block:
    var currentNode = vertexPart
    for stmt in arg[6]:
      if stmt.kind == nnkCommentStmt and stmt.strVal == "rasterize":
        currentNode = fragmentPart

      currentNode.add stmt


  var vertexShader = "void main() {\n"
  vertexShader.compileToGlsl(vertexPart)
  vertexShader.add "}\n"
  var fragmentShader = "void main() {\n"
  fragmentShader.compileToGlsl(fragmentPart)
  fragmentShader.add "}\n"
  let varyings = extractVaryingSymbols(vertexPart, fragmentPart)

  echo "=".repeat(80)
  echo "vertex part"
  echo vertexShader
  echo "-".repeat(80)
  echo "fragment part"
  echo fragmentShader
  echo "=".repeat(80)


  #echo compileToGlsl(arg);

#[

  #let uniformsSection = ssaList2.createGlslUniformsSection
  #let vertexTypeSection = formalParams[1].createGlslAttributesSection
  #let fragmentTypeSection = formalParams[0].createGlslFragmentTypeSection
  #let glslMain = createGlslMain(ssaList2)

  let (_,_,_) = ssaList2.splitVertexFragmentShader(vertexSym, resultSym)

  #let shaderSource = uniformsSection & "\n" & vertexTypeSection & "\n" & fragmentTypeSection & "\n" & glslMain
  #echo shaderSource
]#

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
  ## Inject types to the ``rendor .. do: ...`` node.
  arg.expectKind(nnkDo)
  let formalParams = arg[3]
  formalParams.expectKind(nnkFormalParams)

  let vertexTypeExpr = quote do:
    `mesh`.type.VertexType

  let fragmentTypeExpr = quote do:
    `framebuffer`.type.FragmentType

  let resultType = formalParams[0] or fragmentTypeExpr

  var vertexArg, vertexType, envArg, envType: NimNode

  case formalParams.len
  of 2:
    let identDefs = formalParams[1]
    identDefs.expectKind nnkIdentDefs

    identDefs[2].expectKind nnkEmpty

    vertexArg = identDefs[0]
    vertexType = vertexTypeExpr
    envArg = identDefs[1]
    envType = nnkVarTy.newTree(bindSym"GlEnvironment")

  of 3:
    formalParams[1].expectKind nnkIdentDefs
    formalParams[2].expectKind nnkIdentDefs

    vertexArg = formalParams[1][0]
    vertexType = formalParams[1][1] or vertexTypeExpr

    envArg = formalParams[2][0]
    envType = formalParams[2][1] or nnkVarTy.newTree(bindSym"GlEnvironment")

  else:
    error("invalid ast", formalParams)

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

macro render(framebuffer, mesh: typed; arg: untyped): untyped =
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

  ## rasterize

  var lighting: Vec4f
  for light in lights:
    let light_position_cs = V * light.position_ws
    let light_direction_cs = light_position_cs-position_cs
    let light_intensity = dot(light_direction_cs, normal_cs)
    lighting += light_intensity * light.color

  let textureSample = texture(myTexture, v.texCoord)
  result.color = textureSample * lighting
