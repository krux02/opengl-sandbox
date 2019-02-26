import macros

proc back*(node: NimNode): NimNode = node[node.len-1]

proc head*(node: NimNode): NimNode = node[0]

proc newPrefix*(n1,n2: NimNode): NimNode {. compileTime .} =
  result = newNimNode(nnkPrefix)
  result.add n1
  result.add n2

proc newInfix*(op, n1, n2: NimNode): NimNode {. compileTime .} =
  result = newNimNode(nnkInfix)
  result.add op
  result.add n1
  result.add n2

proc newRecList*(args: varargs[NimNode]): NimNode {.compileTime.} =
  result = newNimNode(nnkRecList)
  for arg in args:
    result.add arg

proc newDotExpr*(a,b,c: NimNode): NimNode =
  newDotExpr(a,b).newDotExpr(c)

proc newBracketExpr*(a,b: NimNode): NimNode =
  nnkBracketExpr.newTree(a,b)

proc newRangeUntil*(upper: int): NimNode {.compileTime.} =
  nnkInfix.newTree(ident"..<", newLit(0), newLit(upper))


proc newTypeDef(name, tpe: NimNode): NimNode {.compileTime.} =
  result = nnkTypeSection.newTree(
    nnkTypeDef.newTree(
      name,
      newEmptyNode(),
      tpe,
    )
  )

proc newObjectTy*( name, recList: NimNode ): NimNode {.compileTime.} =
  result = newTypeDef(name,
    nnkObjectTy.newTree(newEmptyNode(), newEmptyNode(), recList)
  )

proc newExpIdentDef*(name: string, tpe: NimNode): NimNode {. compileTime .} =
  result = nnkIdentDefs.newTree(
    nnkPostfix.newTree( ident"*", ident(name)),
    tpe,
    newEmptyNode()
  )

proc addAll*(dst, src: NimNode): NimNode {.discardable.} =
  for node in src:
    dst.add(node)
  dst

proc addAll*(dst: NimNode; src: openarray[NimNode]): NimNode {.discardable.} =
  for node in src:
    dst.add(node)
  dst

proc isIdentChar(c: char): bool =
  'A' <= c and c <= 'Z' or 'a' <= c and c <= 'z' or '0' <= c and c <= '9' or c == '_'

proc expectIdent*(n: NimNode, name: string) {.compileTime.} =
  ## checks that `n` is an identifier with name `name`. If this is not the case,
  ## compilation aborts with an error message. This is useful for writing
  ## macros that check the AST that is passed to them.
  n.expectKind(nnkIdent)
  if not n.eqIdent(name): error("Expected a node with identifier " & name & ", got " & $n, n)

template myAdd(a: var string, b: untyped): untyped =
  ## simply tries to minimize temporary strings
  when compiles(add(a, b)):
    a.add(b)
  else:
    a.add($b)

macro s*(arg: static[string]): string =
  # does not handle utf8 runes properly
  # pretents everything is ASCII
  # no way to escape the $ sign implemented. Should probably be $$.

  result = nnkStmtListExpr.newTree()
  let str = genSym(nskVar, "str")

  result.add newVarStmt(str, newLit(""))

  var i = 0
  var j = 0
  while true:
    while j < len(arg) and arg[j] != '$':
      j += 1

    let lit = newLit(arg[i..<j])
    result.add newCall(bindSym"myAdd", str, lit)

    if j == len(arg):
      break

    var exprString : string

    if arg[j+1] == '{':
      j += 2
      i = j
      while j < len(arg) and arg[j] != '}':
        if arg[j] == '{':
          error "{ not allowed after ${"
        j += 1

      exprString = arg[i ..< j]

      j += 1
    else:
      j += 1
      i = j
      while j < len(arg) and arg[j].isIdentChar:
        # does not deal with the fact that identifiers may not start or end on _,
        # also does not deal with the fact that the first char may not be a digit
        j += 1
      exprString = arg[i ..< j]

    let expr = parseExpr(exprString)
    result.add newCall(bindSym"myadd", str, expr)

    if j == len(arg):
      break

    i = j

  result.add str

when isMainModule:
  let a = 123
  let b = "abc"
  let c = (a: 7, b: 8)
  var str = s" $a $b $c "
  echo str
  str.add 123
  echo str
