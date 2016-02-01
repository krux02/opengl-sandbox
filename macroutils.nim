import macros

proc newPrefix(n1,n2: NimNode): NimNode {. compileTime .} =
  result = newNimNode(nnkPrefix)
  result.add n1
  result.add n2

proc newPostfix(n1,n2: NimNode): NimNode {. compileTime .} =
  result = newNimNode(nnkPostfix)
  result.add n1
  result.add n2

proc newRecList(args: varargs[NimNode]): NimNode {.compileTime.} =
  result = newNimNode(nnkRecList)
  for arg in args:
    result.add arg


proc newNimNode2*(kind: NimNodeKind; children: varargs[NimNode]): NimNode {. compileTime .} =
  result = newNimNode(kind)
  for child in children:
    result.add child

proc newTypeDef(name: NimIdent, tpe: NimNode): NimNode {.compileTime.} =
  result = newNimNode2(nnkTypeSection,
    newNimNode2(nnkTypeDef,
      newIdentNode(name),
      newEmptyNode(),
      tpe,
    )
  )

proc newDotExpr*(a,b,c: NimNode): NimNode {. compileTime .} =
  newDotExpr(newDotExpr(a,b),c)

proc newObjectTy*( name: NimIdent, recList: NimNode ): NimNode {.compileTime.} =
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
