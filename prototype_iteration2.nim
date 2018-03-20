import glm, future, algorithm, macros, strutils, tables, sequtils, strutils

import resolveAlias

proc makeUniqueData[T](arg: var openarray[T]): int =
  ## removes consecutive duplicate elements from `arg`. Since this
  ## operates on an `openarray` elements are not really removed, but
  ## rearranged, so that all the non-unique elements are at the end,
  ## and the the elements in the range `0 ..< result` will be
  ## unique. This functions expects ordered input. `result` will
  ## contain the amount of unique elements in arg.

  if arg.len <= 1:
    # sequentces with one or less elements can never have duplicates
    return

  var i,j : int
  while i < arg.len:
    template a: var T = arg[i]
    template b: var T = arg[j]
    if arg[j] != arg[i]:
      # found an element that is different than the previeous element
      j += 1
      # this increment before the swap is correct, because arg[0]
      # should not be changed. Neither should the last written element
      # be overwritten.
      swap(arg[i], arg[j])

  # plus 1 because j is the index of the last elemnt, not the length
  j + 1

proc makeUnique[T](arg: var seq[T]): void =
  ## removes consecutive duplicate elements from `arg`. This works
  ## like `makeUniqueData`, but it changes the length of `arg`
  ## the amount of unique elements.
  arg.setLen(arg.makeUniqueData)


proc sortAndUnique[T](arg: var seq[T]): void =
  arg.sort(cmp)
  arg.makeUnique

## gl wrapper ##

type
  Texture1D            = object
    handle: uint32
  Texture2D            = object
    handle: uint32
  Texture3D            = object
    handle: uint32
  TextureCube          = object
    handle: uint32
  Texture2DShadow      = object
    handle: uint32
  TextureCubeShadow    = object
    handle: uint32
  Texture2DArray       = object
    handle: uint32
  Texture2DArrayShadow = object
    handle: uint32

proc texture*(sampler: Texture2D;            P: Vec2f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture3D;            P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: TextureCube;          P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture2DShadow;      P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: TextureCubeShadow;    P: Vec4f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture2DArray;       P: Vec3f; bias: float32 = 0): Vec4f =
  quit("only implemented in shader")

proc texture*(sampler: Texture2DArrayShadow; P: Vec4f): Vec4f =
  quit("only implemented in shader")

# other types

type
  Mesh[VertexType] = object

  Framebuffer[FragmentType] = object

  GlEnvironment = object
    Position: Vec4f
    PointSize: float32
    ClipDistance: UncheckedArray[float32]

  DefaultFragmentType = object
    color: Vec4f

  GlslConstraint = enum
    gcFS
    gcVS
    gcCPU

  ConstraintRange = tuple[min,max: GlslConstraint]

iterator typeFields(typeAst: NimNode): tuple[memberSym, typeSym: NimNode] =
  let parent =
    if typeAst.kind == nnkObjectTy:
      typeAst[2]
    else:
      typeAst

  for identDefs in parent:
    let typeSym = identDefs[^2]
    for i in 0 ..< identDefs.len-2:
      let memberSym = identDefs[i]
      yield((memberSym: memberSym, typeSym: typeSym))

iterator arguments*(n: NimNode): NimNode {.inline.} =
  ## Iterates over the arguments of a call ``n``.
  for i in 1 ..< n.len:
    yield n[i]

iterator depthFirstTraversal*(n: NimNode): NimNode =
  var stack = newSeq[tuple[n: NimNode,i: int]](0)
  stack.add((n: n, i: 0))
  yield stack[^1].n
  while stack.len > 0:
    template i: untyped = stack[^1].i
    template n: untyped = stack[^1].n
    while i < n.len:
      let child = n[i]
      i += 1
      stack.add((n: child, i: 0))
      yield stack[^1].n
    discard stack.pop

proc findSymbolWithName(arg: NimNode; symName: string): NimNode =
  ## searches though a tree and returns the first symbol node with the
  ## given identifier, on nil if no such symbol could be found.
  for node in arg.depthFirstTraversal:
    if node.kind == nnkSym and eqIdent(node, symName):
      return node

proc expectIdent*(arg: NimNode; identName: string): void =
  if not arg.eqIdent(identName):
    error("expect identifier or symbol of name " & identName, arg)

proc expectInt*(arg: NimNode; value: int): void =
  if arg.intVal != value:
    error("expect integer literal of value " & $value, arg)

proc expectIntIn*(arg: NimNode; slice: Slice[int]): void =
  if arg.intVal notin slice:
    error("expect integer literal in range: " & $slice.a & " .. " & $slice.b & " but got " & $arg.intVal, arg)


import future, strutils, sequtils

proc glslType(arg: NimNode): string {.compileTime.} =
  let arg = arg.resolveAlias[^1]

  if arg.kind == nnkSym:
    if arg == bindSym"float32":
      return "float"
    elif arg == bindSym"float64":
      return "double"
    elif arg == bindSym"int32":
      return "int"
    elif arg == bindSym"bool":
      return "bool"
    elif arg == bindSym"Texture1D":
      return "sampler1D"
    elif arg == bindSym"Texture2D":
      return "sampler2D"
    elif arg == bindSym"Texture3D":
      return "sampler3D"
    elif arg == bindSym"TextureCube":
      return "samplerCube"
    elif arg == bindSym"Texture2DShadow":
      return "sampler2DShadow"
    elif arg == bindSym"TextureCubeShadow":
      return "samplerCubeShadow"
    elif arg == bindSym"Texture2DArray":
      return "sampler2DArray"
    elif arg == bindSym"Texture2DArrayShadow":
      return "sampler2DArrayShadow"
    else:
      error("symbol not handled yet: " & arg.repr, arg)

  if arg.kind == nnkBracketExpr:
    if arg[0] == bindSym"Vec":
      if arg[2] == bindSym"float32":
        result = "vec"
      elif arg[2] == bindSym"float64":
        result = "dvec"
      elif arg[2] == bindSym"int32":
        result = "ivec"
      elif arg[2] == bindSym"bool":
        result = "bvec"

      arg[1].expectIntIn 2..4
      result.add arg[1].intVal

    elif arg[0] == bindSym"Mat":
      let Tsym = arg[3]
      if Tsym == bindSym"float32":
        result = "mat"
      elif Tsym == bindSym"float64":
        result = "dmat"
      elif Tsym == bindSym"int32":
        result = "imat"
      elif Tsym == bindSym"bool":
        result = "bmat"
      else:
        echo arg.treeRepr
        result = "<error type 4>"

      arg[1].expectIntIn 2..4
      arg[2].expectIntIn 2..4

      let intVal1 = arg[1].intVal
      let intVal2 = arg[2].intVal

      result.add intVal1
      if intVal2 != intVal1:
        result.add "x"
        result.add intVal2
    else:
      error("incompatible type for glsl " & arg.repr, arg)
  else:
    error("don't know what to do with " & arg.repr, arg)


when defined(testGlslTypes):
  macro testGlslType(arg: typed): untyped =
    var expected: string
    for varSection in arg:
      if varSection.kind == nnkVarSection:
        let identDefs = varSection[0]
        let sym = identDefs[0]

        let glslType = sym.getTypeInst.glslType
        assert glslType == expected
      else:
        expected = varSection.strVal

  testGlslType:
    ## vec4
    var a: Vec4f
    ## vec4
    var b: Vec4[float32]
    ## vec4
    var c: Vec[4,float32]
    ## mat4
    var d: Mat4f
    ## mat4
    var e: Mat4[float32]
    ## mat4
    var f: Mat[4,4,float32]
    ## float
    var g: float32
    ## mat3x4
    var h: Mat[3,4, float32]
    ## mat4x2
    var i: Mat[4,2, float32]

proc transform_to_single_static_assignment(stmtList: NimNode): NimNode {.compileTime.} =
  ## Transforms the argument AST into a list of assignments. The
  ## assignments are either declaring a new identifier, like ``let x =
  ## foo(y,z)``, or they are assignments to already existing variables
  ## like ``gl.Position = foo(x,y)``
  stmtList.expectKind nnkStmtList
  var assignments = newStmtList()

  proc genSymForExpression(expr: NimNode): NimNode {.compileTime.}=
    let typ = expr.getTypeInst
    if expr.kind in nnkCallKinds or expr.kind == nnkDotExpr:
      result = genSym(nskLet, "tmp")
      let call = expr.kind.newTree(expr[0])
      for i in 1 ..< expr.len:
        call.add(genSymForExpression(expr[i]))
      assignments.add(nnkLetSection.newTree(
        nnkIdentDefs.newTree(result, typ, call))
      )
    else:
      result = expr

  for asgn in stmtList:
    asgn.expectKind({nnkAsgn, nnkLetSection})

    if asgn.kind == nnkAsgn:
      let sym = genSymForExpression(asgn[1])

      if assignments[^1][0][0] != sym:
        echo "current assignment: ", asgn.repr
        echo "generated sym:      ", sym.repr
        echo "assignments[^1]:    ", assignments[^1].repr
        error("foobar", assignments[^1][0][0])

      let rhs = assignments[^1][0][2]
      assignments[^1] = nnkAsgn.newTree(asgn[0], rhs)

    elif asgn.kind == nnkLetSection:
      for identDefs in asgn:
        identDefs.expectKind nnkIdentDefs
        for i in 0 ..< identDefs.len - 2:
          let sym = identDefs[i]
          let value = identDefs[^1]

          #asgn.kind == nnkLetSection
          #asgn.kind == nnkLetsection

  return assignments

iterator iterateSSAList(arg: NimNode, backwards: bool = false): tuple[kind:NimNodeKind; lhs, typ, rhs: NimNode] {.inline.} =
  arg.expectKind nnkStmtList
  for i in 1 .. arg.len:
    let asgn = if backwards: arg[^i] else: arg[i-1]
    asgn.expectKind {nnkLetSection,nnkAsgn}
    var lhs,typ,rhs: NimNode
    if asgn.kind == nnkLetSection:
      lhs = asgn[0][0]
      typ = asgn[0][1]
      rhs = asgn[0][2]
    elif asgn.kind == nnkAsgn:
      lhs = asgn[0]
      rhs = asgn[1]
      typ = asgn[0].getTypeInst

    yield((asgn.kind, lhs, typ, rhs))

import hashes
proc hash(arg: NimNode): Hash {.compileTime.} =
  result = result !& arg.kind.int
  case arg.kind
  of nnkCharLit..nnkUInt64Lit:
    result = result !& hash(arg.intVal)
  of nnkFloatLit..nnkFloat64Lit:
    result = result !& hash(arg.floatVal)
  of nnkStrLit..nnkTripleStrLit:
    result = result !& hash(arg.strVal)
  of nnkSym, nnkIdent:
    result = result !& hashIgnoreStyle(arg.repr)
  else:
    for child in arg.children:
      result = result !& hash(child)
    result = !$result

proc firstSymbol(arg : NimNode; default:NimNode): NimNode {.compileTime.} =
  result = arg
  while result.kind != nnkSym:
    if result.len < 1:
      return default
    else:
      result = result[0]

proc firstSymbol(arg:NimNode): NimNode {.compileTime.} =
  result = firstSymbol(arg, nil)
  if result == nil:
    error("Cedrić " & arg.lispRepr)

static:
  # not really great, software design.
  # this table is created in each call to the render macro and used used to store the constraints of symbols.
  var constraintsTable = initTable[NimNode,ConstraintRange]()
  constraintsTable.clear

proc `constraint=`(arg:NimNode, constraint: ConstraintRange): void  {.compileTime.} =
  constraintsTable[arg.firstSymbol] = constraint

proc constraint(arg:NimNode): ConstraintRange {.compileTime.} =
  constraintsTable[arg.firstSymbol]

proc hasConstraint(arg: NimNode): bool {.compileTime.} =
  let sym = arg.firstSymbol(nil)
  if sym != nil:
    constraintsTable.hasKey(sym)
  else:
    false


proc withDefaultConstraint(asgn, glSym, resultSym: NimNode): NimNode {.compileTime.} =
  result = asgn
  if asgn.kind == nnkPragmaExpr:
    asgn[1].expectKind nnkPragma
    asgn[1].expectLen 2
  elif asgn.kind == nnkAsgn:
    let dotExpr = asgn[0]
    dotExpr.expectKind nnkDotExpr
    let lhsSym = if dotExpr[0].kind == nnkHiddenDeref: dotExpr[0][0] else: dotExpr[0]
    lhsSym.expectKind nnkSym
    if lhsSym == glSym:
      lhsSym.constraint = (min: gcVS, max: gcVS)
    elif lhsSym == resultSym:
      lhsSym.constraint = (min: gcFS, max: gcFS)
    else:
      error("needs to be result or gl", lhsSym)
  elif asgn.kind == nnkLetSection:
    asgn.expectLen 1
    let identDefs = asgn[0]
    identDefs.expectLen(3)
    let lhsSym = identDefs[0]
    lhsSym.constraint = (min: gcFS, max: gcCPU)
  else:
    error("Bernhardt", asgn)



proc add[T,U](arg: var Table[T, seq[U]]; key: T; value: U): void =
  if arg.hasKey(key):
    let s: ptr seq[U] = arg[key].addr
    if value notin s[]:
      s[].add(value)
  else:
    arg[key] = @[value]


#[
two pass compilation

 * pass 1: bottom up, increase lower bounds. Lower bounds are semantic meaning only.
 * pass 2: top down, limit upper bounds. Upper boinds are allowed optimizations.


# pass1

let sym = foo(a,b,c)
raise low(sym) to min(low(a),low(b),low(c))

example:
                                                         .
    framebuffer.render(mesh) do (v, gl):                /|\ Pass1 min:   |  Pass2 max
      result.color1  = texture(mytexture, v.texcoord)    |     FS        |    FS
      gl.Position    = texture(mytexture, v.texcoord)    |     VS        |    VS
      result.color2  = texture(mytexture, time)          |     FS       \|/   VS
                                                                         '

logic for `let s = texture(a,b)`:

  if low(s) == gcFS:
    if low(b) == gcCPU:  # result is constant over all vertices
      high(s) = min(high(s), gcVS)
    if low(s) < gcCPU:
      high(s)  = min(high(s), gcFS)

  if low(s) == gcVS:
    if high(b) = gcVS

logic for `let s = v1 + v2`:

  high(s) = min(high(v1), high(v2))

]#


proc resolveConstraints(arg, vertexSym: NimNode): void {.compileTime.} =
  # build dependency graph
  # let a = foo(b,c,d)  --> a depends on {b,c,d}
  # and the inverse:    --> b,c,d all depend on a

  #[
  var dependencyGraph = initTable[NimNode, seq[NimNode]]()
  var inverseDependencyGraph = initTable[NimNode, seq[NimNode]]()

  for kind, lhs, typ, rhs in arg.iterateSSAList:
    let lhs = lhs.firstSymbol
    if rhs.kind in nnkCallKinds:
      for arg in rhs.arguments:
        if arg.hasConstraint:
          dependencyGraph.add(lhs,arg)
          inverseDependencyGraph.add(arg,lhs)

    elif rhs.kind == nnkDotExpr:
      discard
      # TODO maybe do something with this
    else:
      echo rhs.kind
      error("David")

  for key, dependencies in dependencyGraph:
    dependencyGraph[key] = deduplicate(dependencyGraph[key])
  ]#

  # done building dependency graph

  proc growMinConstraint(arg: NimNode; newMinConstraint: GlslConstraint): void =
    if arg.hasConstraint:
      var argConstraint = arg.constraint
      if argConstraint.min < newMinConstraint:
        argConstraint.min = newMinConstraint
        arg.constraint = argConstraint
        #arg.updateDependentConstraints(newMinConstraint = newMinConstraint)

  proc shrinkMaxConstraint(arg: NimNode; newMaxConstraint: GlslConstraint): void =
    if arg.hasConstraint:
      var argConstraint = arg.constraint
      if argConstraint.max > newMaxConstraint:
        argConstraint.max = newMaxConstraint
        arg.constraint = argConstraint
        #arg.updateDependentConstraints(newMaxConstraint = newMaxConstraint)


  for _, lhs, _, rhs in arg.iterateSSAList(backwards = true):
    let lhsConstraint = lhs.constraint
    if rhs.kind in nnkCallKinds:
      for i in 1 ..< rhs.len:
        let arg = rhs[i]
        arg.growMinConstraint(lhsConstraint.min)


  for _, lhs, _, rhs in arg.iterateSSAList(backwards = false):
    if rhs.kind == nnkDotExpr:
      if rhs[0] == vertexSym:
        shrinkMaxConstraint(lhs, gcVS)
      else:
        error("can't do anything with this dot expr ", rhs)
    elif rhs.kind in nnkCallKinds:
      if eqIdent(rhs[0], "texture"):
        rhs.expectLen(4)
        #let sampler = rhs[1]
        let P = rhs[2]
        # let bias = rhs[3]

        case lhs.constraint.min
        of gcFS:
          if P.constraint.max >= gcCPU:
            shrinkMaxConstraint(lhs, gcVS)
          else:
            shrinkMaxConstraint(lhs, gcFS)
        of gcVS:
          shrinkMaxConstraint(lhs, gcVS)
        of gcCPU:
          error("texture sampling is not supported on the CPU", rhs)
    else:
      echo "call: ", rhs.repr

proc flatDotExpr(arg: NimNode): string {.compileTime.} =
  if arg.kind == nnkDotExpr:
    result = arg[0].repr & "_" & arg[1].repr
  else:
    result = arg.repr


macro blockTag(arg: untyped): untyped =
  arg.expectKind nnkProcDef
  arg[0].expectKind nnkIdent

  let name = $arg[0]

  let lit1 = newLit("<" & name & ">")
  let lit2 = newLit("<" & name & "/>")

  result = arg


  let stmtList = newStmtList(
    newCall(bindSym"echo", lit1),
    nnkDefer.newTree(
      newCall(bindSym"echo", lit2)
    )
  )

  result[6].insert(0, stmtList)


proc lhs(arg: NimNode): NimNode {.compileTime.} =
  arg.expectKind({nnkLetSection, nnkAsgn})
  if arg.kind == nnkLetSection:
    return arg[0][0]
  else:
    return arg[0]

proc rhs(arg: NimNode): NimNode {.compileTime.} =
  arg.expectKind({nnkLetSection, nnkAsgn})
  if arg.kind == nnkLetSection:
    return arg[0][2]
  else:
    return arg[1]

proc createGlslMain(arg: NimNode; outputSymbols: seq[NimNode] = @[]): string {.compileTime.} =
  result = "void main() {\n"
  for assignment in arg:
    var line = "    "
    if assignment.kind == nnkLetSection:
      if assignment[0][0] notin outputSymbols:
        line.add assignment[0][1].glslType
        line.add " "
      line.add assignment[0][0].repr
      line.add " = "
      line.add assignment[0][2].flatDotExpr
      line.add ";   // "
      line.add $assignment[0][0].constraint
    elif assignment.kind == nnkAsgn:
      assignment[0].expectKind nnkDotExpr
      let dotExpr = assignment[0]
      let lhsSym = if dotExpr[0].kind == nnkHiddenDeref: dotExpr[0][0] else: dotExpr[0]
      if cmpIgnoreStyle(lhsSym.repr, "result") == 0:
        line.add dotExpr[1].repr
      elif cmpIgnoreStyle(lhsSym.repr, "gl") == 0:
        line.add "gl_" & dotExpr[1].repr
      line.add " = "
      line.add assignment[1].flatDotExpr
      line.add ";   // "
      line.add $lhsSym.constraint
    else:
      echo assignment.treerepr
      error "Achim", assignment
    line.add ";\n"
    result.add line
  result.add "}\n"


type
  ShaderKind = enum
    skVert = "vert"
    skTesc = "tesc",
    skTese = "tese",
    skGeom = "geom",
    skFrag = "frag",
    skComp = "comp"


import terminal

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

const shaderHeader = "#version 440\n"

proc splitVertexFragmentShader(arg, vertexSym, resultSym: NimNode): tuple[cpu,vs,fs: NimNode] {.compileTime, blockTag.} =
  var symbolTypeMap = newTable[NimNode,NimNode]()
  for _, lhs, typ, rhs in arg.iterateSSAList(backwards = false):
    symbolTypeMap[lhs] = typ

  proc getGlslType(arg: NimNode): string =
    if symbolTypeMap.hasKey(arg):
      symbolTypeMap[arg].glslType
    else:
      arg.getTypeInst.glslType


  let cpu = newStmtList()
  let vs = newStmtList()
  let fs = newStmtList()

  for asgn in arg:
    let lhs = asgn.lhs
    let constraint = lhs.constraint

    case constraint.max
    of gcCPU:
      cpu.add asgn
    of gcVS:
      vs.add asgn
    of gcFS:
      fs.add asgn

  var attributeSymbols = newSeq[NimNode](0)
  for _, lhs, _, rhs in arg.iterateSSAList(backwards = false):
    if rhs.kind == nnkDotExpr:
      if rhs[0] == vertexSym:
        attributeSymbols.add rhs
      else:
        error("can't do anything with this dot expression", rhs)

  attributeSymbols.sortAndUnique
  var attributes = ""
  for attrib in attributeSymbols:
    let typ = attrib[1].getGlslType
    attributes.add("in " & typ & " " & attrib[0].repr & "_" & attrib[1].repr & ";\n")

  # TODO filter out unnecessary intermediate results

  #[
  for i, asgn in cpu:
    asgn.expectKind nnkLetSection
    let name = asgn[0][0].repr
    uniforms.add("layout(location = " & $i & ") uniform " & asgn[0][1].glslType & " " & name & ";\n")

    let call = newCall(ident"glUniform", newLit(i), asgn[0][0])
    glUniformCalls.add call
  ]#


  var uniformSymbols = newSeq[NimNode](0)
  var vsSymbols = newSeq[NimNode](0)
  var varyingSymbols = newSeq[NimNode](0)
  var fsSymbols = newSeq[NimNode](0)

  for cmd in vs:
    let rhs = cmd.rhs

    if rhs.kind in nnkCallKinds:
      for arg in rhs.arguments:
        if arg.kind == nnkSym:
          if arg notin vsSymbols:
            uniformSymbols.add arg

    let lhs = cmd.lhs
    if lhs.kind == nnkSym:
      vsSymbols.add lhs


  for cmd in fs:
    let rhs = cmd.rhs

    if rhs.kind in nnkCallKinds:
      for arg in rhs.arguments:
        if arg.kind == nnkSym:
          if arg in vsSymbols:
            varyingSymbols.add arg
          elif arg notin fsSymbols:
            uniformSymbols.add arg

    let lhs = cmd.lhs
    if lhs.kind == nnkSym:
      fsSymbols.add lhs

  uniformSymbols.sortAndUnique
  varyingSymbols.sortAndUnique


  var glslUniforms = ""
  for i, uniform in uniformSymbols:
    let call = newCall(ident"glUniform", newLit(i), uniform)
    cpu.add call

    let name = uniform.repr

    let glslType = uniform.getGlslType

    glslUniforms.add("layout(location = " & $i & ") uniform " & glslType & " " & name & ";\n")


  var fragmentOutputs = ""
  ## calculate fragment output section
  block:
    var i = 0
    for sym, typ in  resultSym.getTypeImpl.typeFields:
      fragmentOutputs.add("layout(location = " & $i & ") out " & typ.glslType & " " & $sym & ";\n")
      i += 1


  var vertexShader = shaderHeader & glsluniforms & attributes & "\n"
  var fragmentShader = shaderHeader & glsluniforms & "\n"

  echo "varyingSymbols: ", varyingSymbols
  for varying in varyingSymbols:
    vertexShader.add("out " & varying.getGlslType & " " & varying.repr & ";\n")
    fragmentShader.add("in " & varying.getGlslType & " " & varying.repr & ";\n")

  vertexShader.add  vs.createGlslMain(varyingSymbols)
  fragmentShader.add fragmentOutputs
  fragmentShader.add fs.createGlslMain


  echo "CPU:"
  echo cpu.repr
  echo "VS:"
  echo vertexShader
  vertexShader.validateShader(skVert)

  echo "FS:"
  echo fragmentShader
  fragmentShader.validateShader(skFrag)


proc indentCode(arg: string, indentation: string): string =
  let N = arg.countLines
  result = newStringOfCap(indentation.len * N + arg.len)
  for line in splitLines(arg):
    result.add indentation
    result.add line

macro render_inner(debug: static[bool], mesh, arg: typed): untyped =

  if debug:
    echo "<render_inner>"
    echo arg.repr
  defer:
    if debug:
      echo "</render_inner>"

  arg.expectKind nnkDo
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

proc `or`(arg, alternative: NimNode): NimNode {.compileTime.} =
  if arg.kind != nnkEmpty:
    arg
  else:
    alternative

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
  result = newCall(bindSym"render_inner", newLit(false), mesh, injectTypes(framebuffer, mesh, arg))

macro renderDebug(framebuffer, mesh: typed; arg: untyped): untyped =
  result = newCall(bindSym"render_inner", newLit(true), mesh, injectTypes(framebuffer, mesh, arg))


## user code ##

type
  MyFragmentType = object
    color: Vec4f

  MyVertexType = object
    position: Vec4f
    color: Vec4f
    normal: Vec4f
    texCoord: Vec2f

  MyMesh        = Mesh[MyVertexType]
  MyFramebuffer = Framebuffer[MyFragmentType]

var myTexture: Texture2D
var mesh: MyMesh
var framebuffer: MyFramebuffer
var mvp: Mat4f


static:
  echo "vertex colors"

#framebuffer.renderDebug(mesh) do (v, gl):
#  gl.Position = v.position
#  let tmp = v.color
#  result.color = tmp
#  ^ doesn.t work for some reason ???


## Qustiones when not doing SSA anymore

const myConstant = 0.123456
var myUniform = -0.123456

proc foo(): int =
  result = 1
  result += 2


framebuffer.renderDebug(mesh) do (v, gl):
  result.color.r = 0.0f                              #       | | | |
  result.color.r += myConstant                       # const-' | | |
  result.color.r += myUniform                        #     CPU-' | |
  result.color.r += v.position.x                     #        VS-' |  {.VS.}
  result.color.r += texture(myTexture, v.texcoord).r #          FS-'


template glsl(arg: string): string = arg


echo glsl"""  vec4 a = vec4(17);  attribute vec2 pos; """


"""

    const myConstant: float32 = 0.123456

    const const_result_color = (block:
      var const_result_color: Vec4f
      const_result_color.r += myConstant
      const_result_color
    )

    cpu:
      var cpu_result_color = const_result_color
      cpu_result_color.r += myUniform

      glAttrib("attrib_position", ...)
      glUniform("uniform_result_color", cpu_result_color)

    vertex shader:

      uniform vec4 uniform_result_color;
      in vec4 attrib_position;
      in vec2 attrib_texcoord;

      out vec2 vert2frag_texcoord;
      out vec4 vert2frag_result_color;

      void main() {
        // assign local names
        vec4 vert_result_color = uniform_result_color;
        vec4 vert_position = attrib_position;
        vec2 vert_texcoord = attrib_texcoord;

        // vertex shader body
        vert_result_color.r += vert_position.x;

        // forwarding for next shader stage:
        // this section might look different when geometry or tesselation shader is present.
        vert2frag_texcoord     = vert_texcoord;
        vert2frag_result_color = vert_result_color;
      }

    fragment shader:
      uniform sampler2D myTexture;

      in vec2 vert2frag_texcoord;
      in vec4 vert2frag_result_color;

      // there is no next shader stage, so assignment to final variable names is possible
      out vec4 frag_result_color;

      void main() {
        // assign local names
        vec2 frag_texcoord = vert2frag_texcoord;
        frag_result_color   = vert2frag_result_color;

        // fragment shader body
        frag_result_color.r +=  texture(myTexture, frag_texcoord).r;

        // no forwarding section required anymore
      }
    """




framebuffer.render(mesh) do (v, gl):
  gl.Position     = result.color.r
  # OK
  # Should be OK, because vert_result_color can be introduced without creating any conflicts.

framebuffer.render(mesh) do (v, gl):
  gl.Position     = result.color.r                   # assignment from vert_result_color
  result.color.r  = texture(myTexture, v.texcoord).r # assignment to frag_result_color
  # Still ok, just forward result.color after it is used for gl.Position

framebuffer.render(mesh) do (v, gl):
  result.color.r  = texture(myTexture, v.texcoord).r # assignment to frag_result_color
  gl.Position     = result.color.r                   # error
  # gl.Position needs to be assigned to from the vertex shader, but vert_result_color is invalid now.

var mvp: Mat4f

framebuffer.render(mesh) do (v, gl):
  result.color.r  = texture(myTexture, v.texcoord).r # assignment to frag_result_color
  gl.Position     = mvp * v.position                 # OK even though
  # this statement is after a statement that has to be executed in the
  # fragment shader, it has no dependency on any value that has to be
  # executed in the fragment shader. Therefore the order can be
  # changed so that this line is evaluated in the vertex shader without breaking any semantic.


# what would this thing generate?

framebuffer.render(mesh) do (v, gl):
  if (gl.Position > 0.5f){.VS.}:
    result.color.rgb = vec3f(1,0,0)
  else:
    result.color.rgb = vec3f(0,1,0)

"""
    vertex shader:

      out flat int vert2frag_tmp0;

      void main() {
        // assign local names
        bool vert_tmp0;

        // vertex shader body
        vert_tmp0 = gl_Position.z > 0.5f;

        // forwarding for next shader stage:
        // this section might look different when geometry or tesselation shader is present.
        vert2frag_tmp0 = vert_tmp0 ? 1 : 0;
      }

    fragment shader:
      flat in int vert2frag_tmp0;

      // there is no next shader stage, so assignment to final variable names is possible
      out vec4 frag_result_color;

      void main() {
        // assign local names
        bool frag_tmp0 = vert2frag_tmp0 != 0;

        // fragment shader body
        if ( frag_tmp0 ) {
          frag_result_color.rgb = vec3(1,0,0);
        } else {
          frag_result_color.rgb = vec3(0,1,0);
        }
        // no forwarding section required anymore
      }
    """
"""



framebuffer.render(mesh) do (v, gl):



static:
  echo "################################################################################\n"

framebuffer.render(mesh) do (v, gl):
  gl.Position = (mvp * v.position).normalize.normalize.normalize
  result.color = v.color



static:
  echo "################################################################################\n"

framebuffer.render(mesh) do (v, gl):
  gl.Position = mvp * v.position
  result.color = texture(myTexture, v.texCoord) + vec4(sin(float32(Pi)))

static:
  echo "################################################################################\n"

discard """
raw ast
StmtList
  IfStmt
    ElifBranch
      PragmaExpr
        Par
          Infix
            Ident ident">"
            DotExpr
              Ident ident"gl"
              Ident ident"Position"
            Float32Lit 0.5
        Pragma
          Ident ident"VS"
      StmtList
        Asgn
          DotExpr
            Ident ident"result"
            Ident ident"color"
          Call
            Ident ident"vec3f"
            IntLit 1
            IntLit 0
            IntLit 0
    Else
      StmtList
        Asgn
          Ident ident"color"
          Call
            Ident ident"vec3"
            IntLit 0
            IntLit 1
            IntLit 0
"""
# should generate



discard """
// vertex shader
#version 410

flat out int vs_f3;

void main() {
  bool f3;
  f3 = gl_Position.z > 0.5;
  vs_f3 = f3 ? 1 : 0;
}

// fragment shader

#version 410

flat in int vs_f3;

void main() {
  vec3 f1;
  vec3 f2;
  bool f3;
  vec3 f4;

  f1 = vec3(1,0,0);
  f2 = vec3(0,1,0);
  f3 = vs_f3 != 0;
  if(f3)
    f4 = f1;
  else
    f4 = f2;
}


"""


static:
  echo "################################################################################\n"
