import macros

proc back*(node: NimNode): NimNode = node[node.len-1]

proc head*(node: NimNode): NimNode = node[0]

proc newPrefix*(n1,n2: NimNode): NimNode {. compileTime .} =
  result = newNimNode(nnkPrefix)
  result.add n1
  result.add n2

proc newPostfix*(n1,n2: NimNode): NimNode {. compileTime .} =
  result = newNimNode(nnkPostfix)
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

proc newNimNode2*(kind: NimNodeKind; children: varargs[NimNode]): NimNode {. compileTime .} =
  result = newNimNode(kind)
  for child in children:
    result.add child

proc newTypeDef(name, tpe: NimNode): NimNode {.compileTime.} =
  result = newNimNode2(nnkTypeSection,
    newNimNode2(nnkTypeDef,
      name,
      newEmptyNode(),
      tpe,
    )
  )

proc newObjectTy*( name, recList: NimNode ): NimNode {.compileTime.} =
  result = newTypeDef(name,
    newNimNode2(nnkObjectTy, newEmptyNode(), newEmptyNode(), recList)
  )

proc `!!`*(name: string): NimNode {. compileTime .} =
  result = newIdentNode(name)

proc newExpIdentDef*(name: NimIdent, tpe: NimNode): NimNode {. compileTime .} =
  result = newNimNode2(nnkIdentDefs,
    newPostfix( !!"*", newIdentNode(name)),
    tpe,
    newEmptyNode()
  )

proc toConstExpr*(s : string): NimNode {. compileTime .} = s.newLit

proc toConstExpr*(i : int64): NimNode {. compileTime .} = i.newLit

proc toConstExpr*[T](s : seq[T]): NimNode {. compileTime .} =
  let bracketExpr = newNimNode(nnkBracket)
  for element in s:
    bracketExpr.add( element.toConstExpr )

  result = newPrefix(!!"@", bracketExpr)


proc `!!`*(name: NimIdent): NimNode {. compileTime .} =
  result = newIdentNode(name)

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
  
macro s*(arg: static[string]): string =
  # does not handle utf8 runes properly
  # pretents everything is ASCII 
  # no way to escape the $ sign implemented. Should probably be $$.
  
  result = nnkStmtListExpr.newTree()
  let str = genSym(nskVar, "str")
  result.add quote do:
    var `str`: string = ""

  var i = 0
  var j = 0
  while true:
    while j < len(arg) and arg[j] != '$':
      j += 1

    let lit = newLit(arg[i..<j])
    result.add quote do:
      `str`.add(`lit`)
      
    if j == len(arg):    
      break

    var exprString : string
    
    if arg[j+1] == '{':
      j += 2
      i = j
      while j < len(arg) and arg[j] != '}':
        if arg[j] == '{':
          error "{ not allowed here"
        j += 1

      exprString = arg[i..<j]

      j += 1
    else:
      j += 1
      i = j
      while j < len(arg) and arg[j].isIdentChar:
        # does not deal with the fact that identifiers may not start or end on _,
        # also does not deal with the fact that the first char may not be a digit 
        j += 1
      exprString = arg[i..<j]

    let expr = parseExpr(exprString)
    result.add quote do:
      `str`.add($`expr`) ## a
      
    if j == len(arg):
      break
    
    i = j
  for i in 0 ..< result.len:
    # remove unnecessary stmtList wrapping
    # of each statement 
    result[i] = result[i][0]
    
  result.add str
