import macros

proc toConstExpr*(s : string): NimNode {. compileTime .} = s.newLit

proc toConstExpr*(i : int64): NimNode {. compileTime .} = i.newLit

proc toConstExpr*[T](s : seq[T]): NimNode {. compileTime .} =
  result = newNimNode(nnkPrefix)
  let bracketExpr = newNimNode(nnkBracket)
  for element in s:
    bracketExpr.add( element.toConstExpr )

  result.add(newIdentNode("@"))
  result.add(bracketExpr)

proc newNimNode2*(kind: NimNodeKind; children: varargs[NimNode]): NimNode {. compileTime .} =
  result = newNimNode(kind)
  for child in children:
    result.add child


proc `!!`*(name: string): NimNode {. compileTime .} =
  result = newIdentNode(name)


proc `!!`*(name: NimIdent): NimNode {. compileTime .} =
  result = newIdentNode(name)
