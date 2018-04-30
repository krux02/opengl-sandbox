# stdlib
import sugar, algorithm, macros, strutils, tables, sequtils
# packages
import glm, ast_pattern_matching
# local stuff
import normalizeType, glslTranslate, boring_stuff



#[ IR:
(Block
  (Asgn `gl.Position` (Mult `P` `V` `M` `v.position_os`))
  (Asgn `position_cs` (Mult `V` `M` `v.position_os`))
  (Asgn `t1` (Mult `V` `M`))
  (Asgn `t2` (Call `transpose` `t1`))
  (Asgn `t3` (Call `inverse` `t2`))
  (Asgn `normal_cs` (Mult `t3` `v.normal_os`))
  (Asgn `lighting`  (Call `vec4f` 0))
  (Loop `light` `lights`
    (Asgn `t4` (Dot `light` `position_ws`))
    (Asgn `light_position_cs` (Mult `V` `t4`))
    (Asgn `t5` (Neg `position_cs`))
    (Asgn `light_direction_cs` (Add `t5` `light_position_cs`))
    (Asgn `light_intensity`  (Call `dot` `light_direction_cs` `normal_cs`))
    (Asgn `t6` (Dot `light` `color`))
    (Asgn `t7` (Mult `light_intensity` `t6`))
    (Asgn `lighting` (Add `lighting` `t7`))
  )
  (Asgn `textureSample` (Call `texture` `myTexture` `v.texCoord`))
  (Asgn `result.color` (Mult `texturesample` `lighting`))
)
]#



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


type
  Mesh[VertexType] = object
  Framebuffer[FragmentType] = object

  GlEnvironment = object
    Position: Vec4f
    PointSize: float32
    ClipDistance: UncheckedArray[float32]

  GlslConstraint = enum
    gcFS
    gcVS
    gcCPU

  ConstraintRange = tuple[min,max: GlslConstraint]

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

## the following function does generate symbols for each
## subexpression, but this is not true SSA form. The name should be
## changed.

proc transform_to_single_static_assignment(stmtList: NimNode): NimNode {.compileTime.} =
  ## Transforms the argument AST into a list of assignments. The
  ## assignments are either declaring a new identifier, like ``let x =
  ## foo(y,z)``, or they are assignments to already existing variables
  ## like ``gl.Position = foo(x,y)``

  let stmtList =
    if stmtList.kind != nnkStmtList:
      nnkStmtList.newTree(stmtList)
    else:
      stmtList

  var assignments: NimNode = newStmtList()

  proc genSymForExpression(expr: NimNode): NimNode {.compileTime.} =
    ## creates a new assignment in `assignments` for expr, and returns
    ## the generated symbol.
    let typ = expr.getTypeInst
    expr.expectKind(nnkCallKinds + {nnkDotExpr})
    result = genSym(nskLet, "tmp")
    let call = expr.kind.newTree(expr[0])
    for i in 1 ..< expr.len:
      if expr[i].kind in nnkCallKinds:
        call.add(genSymForExpression(expr[i]))
      else:
        call.add expr[i]

    assignments.add(nnkLetSection.newTree(
      nnkIdentDefs.newTree(result, typ, call))
    )

  for asgn in stmtList:
    asgn.matchAst:
    of {nnkAsgn,nnkFastAsgn}(`lhs`, `expr`):

      if expr.kind in nnkCallKinds:
        let sym = genSymForExpression(expr)

        if assignments[^1][0][0] != sym:
          echo "current assignment: ", asgn.repr
          echo "generated sym:      ", sym.repr
          echo "assignments[^1]:    ", assignments[^1].repr
          error("foobar", assignments[^1][0][0])

        assignments[^1][0][0] = lhs # override the gensym
      else:
        assignments.add asgn



    of {nnkLetSection, nnkVarSection}:
      for identDefs in asgn:
        identDefs.expectKind nnkIdentDefs
        for i in 0 ..< identDefs.len - 2:
          #let sym = identDefs[i]
          #let value = identDefs[^1]
          warning("not doing anythng here")
          #asgn.kind == nnkLetSection
          #asgn.kind == nnkLetsection

    of nnkBlockStmt(`sym`, `body`):
      let transformedBody = transform_to_single_static_assignment(body)
      assignments.add nnkBlockStmt.newTree(sym, transformedBody)
    of `body` @ nnkStmtList:
      let transformedBody = transform_to_single_static_assignment(body)
      assignments.add transformedBody
    of `comment` @ nnkCommentStmt:
      assignments.add comment
    of `ifStmt` @ nnkIfStmt:
      let transformedIfStmt = nnkIfStmt.newTree
      for branch in ifStmt:
        branch.matchAst:
        of nnkElifBranch(`cond`, `body`):
          transformedIfStmt.add nnkElifBranch.newTree(
            cond,
            transform_to_single_static_assignment(body)
          )
    of nnkWhileStmt(`cond`,`body`):
      let transformedBody = transform_to_single_static_assignment(body)
      assignments.add nnkWhileStmt.newTree(cond, transformedBody)

    of nnkInfix(ident"+=", `lhs`, `rhs`):
      assignments.add transform_to_singleStatic_assignment(nnkAsgn.newTree(lhs, nnkInfix.newTree(ident"+", lhs, rhs)))


        #of nnkElse(`body`):


  while assignments.len == 1 and assignments.kind == nnkStmtList:
    assignments = assignments[0]

  return assignments

iterator iterateSSAList(arg: NimNode, backwards: bool = false): tuple[kind:NimNodeKind; lhs, typ, rhs: NimNode] {.inline.} =
  arg.expectKind nnkStmtList
  for i in 1 .. arg.len:
    let asgn = if backwards: arg[^i] else: arg[i-1]
    var lhs,typ,rhs: NimNode
    matchAst asgn:
    of nnkLetSection(_(`a`,`b`,`c`)):
      lhs = a
      typ = b
      rhs = c
    of nnkAsgn(`a`,`b`):
      lhs = a
      rhs = b
      typ = a.getTypeInst

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

const shaderHeader = "#version 440\n"


proc validateShader(arg: string, kind: string): void =
  discard



proc flatDotExpr(arg: NimNode): string {.compileTime.} =
  if arg.kind == nnkDotExpr:
    result = arg[0].repr & "_" & arg[1].repr
  else:
    result = arg.repr

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
    for sym, typ in  resultSym.getTypeImpl.fields:
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
  vertexShader.validateShader("vert")

  echo "FS:"
  echo fragmentShader
  fragmentShader.validateShader("frag")
