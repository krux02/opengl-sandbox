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
  arg.matchAst:
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




macro render_inner(debug: static[bool], mesh, arg: typed): untyped =

  if debug:
    echo "<render_inner>"
    echo arg.repr

  defer:
    if debug:
      echo result.lispRepr
      echo "</render_inner>"




  arg.expectKind nnkDo

  echo arg.treeRepr

  let vertexPart = nnkStmtList.newTree
  let fragmentPart = nnkStmtList.newTree

  block:
    var currentNode = vertexPart
    for stmt in arg[6]:
      if stmt.kind == nnkCommentStmt:
        echo stmt
        currentNode = fragmentPart

      currentNode.add stmt


  var vertexShader = ""
  vertexShader.compileToGlsl(vertexPart)

  #let fragmentShader = compileToGlsl(fragmentPart)

  echo vertexShader


  echo "=".repeat(80)
  echo "vertex part"
  echo vertexPart.repr
  echo "-".repeat(80)
  echo "fragment part"
  echo fragmentPart.repr
  echo "=".repeat(80)






  #echo compileToGlsl(arg);

#[


  let ssaList1 = transform_to_single_static_assignment(arg[6])

  if debug:
    echo "<ssaList1>"
    echo ssaList1.repr.indentCode("  ")
    echo "</ssaList1>"

  let resultSym = arg.last

  # make each assingnment into an assignment that has constraint
  # attached to it
  ssaList1.expectKind nnkStmtList
  let ssaList2 = newStmtList()

  var vertexSym = arg[3][1][0]

  if vertexSym.kind == nnkIdent:
    echo "TODO: patch the compiler and make vertexSym a symbol in the compiler"
    let node = arg.findSymbolWithName($vertexSym)
    if node != nil:
      echo "replacing vertexSym `", vertexSym.lispRepr, "` with `", node.lispRepr, "` this could be wrong"
      vertexSym = node
  else:
    echo "TODO: when you see this, the other code path is dead and can be removed"

  var glSym = arg[3][2][0]
  if glSym.kind == nnkIdent:
    echo "TODO: patch the compiler and make vertexSym a symbol in the compiler"
    let node = arg.findSymbolWithName($glSym)
    if node != nil:
      echo "replacing glSym `", glSym.lispRepr, "` with `", node.lispRepr, "` this could be wrong"
      glSym = node

  else:
    echo "TODO: when you see this, the other code path is dead and can be removed"


  constraintsTable.clear
  for asgn in ssaList1:
    ssaList2.add withDefaultConstraint(asgn, glSym, resultSym)

  let formalParams = arg[3]
  formalParams.expectKind nnkFormalParams

  ssaList2.resolveConstraints(vertexSym)

  if debug:
    echo "<ssaList2>"
    for asgn in ssaList2:
      let constraint = constraintsTable[asgn.firstSymbol]
      echo "  ", asgn.repr, " # ", constraint
    echo "</ssaList2>"

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
