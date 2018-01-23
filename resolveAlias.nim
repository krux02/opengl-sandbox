import macros

proc substitute(arg, symbol, replacement: NimNode): NimNode =
  if arg.len == 0:
    if symbol == arg:
      return replacement
    elif arg.kind == nnkIdent and eqIdent(symbol, $arg):
      # with identifier substitution it is enough when the identifier
      # looks the same. It is kind of manual symbol resolution.
      # ¯\_(ツ)_/¯
      return replacement
    else:
      return arg
  else:
    result = arg.kind.newTree
    for child in arg:
      result.add child.substitute(symbol, replacement)

proc resolveAliasInternal(typ: NimNode): NimNode =

  case typ.kind
  of nnkSym:
    let impl = typ.symbol.getImpl
    if impl.kind == nnkNilLit:
      return typ
    result = impl.resolveAliasInternal

  of nnkTypeDef:
    result = typ
    result[2] = typ[2].resolveAliasInternal

  of nnkBracketExpr:
    typ[0].expectKind nnkSym
    result = typ[0].resolveAliasInternal
    result.expectKind nnkTypeDef

    # substitute all generic parameters with the corresponding generic
    # argument
    for i, sym in result[1]:
      let arg = typ[i+1]
      result = result.substitute(sym, arg)

  of nnkObjectTy, nnkTupleTy, nnkDistinctTy:
    result = typ

  else:
    echo typ.treeRepr
    error("illegal argument: " & typ.repr, typ)

proc sequenceTransform(arg: NimNode): seq[NimNode] =
  ## `resolveAliasInternal` will return a recursive datastructure of
  ## nnkTypeDef nodes. This ast is as an AST illegal. This
  ## transformation so that each ast is legal again.
  var arg = arg

  result.newSeq(0)
  while arg.kind == nnkTypeDef:
    arg[0].expectKind nnkSym
    let sym = arg[0]
    case arg[1].kind
    of nnkEmpty:
      # no genric parameter, so no bracket expression generated
      result.add sym
    of nnkGenericParams:
      let genericParams = arg[1]
      let bracketExpr = nnkBracketExpr.newTree(sym)
      for param in genericParams:
        bracketExpr.add(param)
      result.add bracketExpr
    else:
      error("")
    arg = arg[2] # this is almost end recursion

  if arg.kind notin {nnkObjectTy,   nnkDistinctTy}:
    result.add arg

proc resolveAlias*(arg: NimNode): seq[NimNode] =
  ## Will return a list of type trees. All element of the list will
  ## represent the same type. The last element of this list should be
  ## some normalized version of the type. Just look at the example to
  ## get an idea what this function does.
  ##
  ## .. code-block:: nim
  ##     type
  ##       Vec[N: static[int],T] = object
  ##         arr: array[N,T]
  ##       Vec4[T] = Vec[4,T]
  ##       Vec4f = Vec4[float32]
  ##       MyObject = object
  ##         a,b,c: int
  ##       MyObjectAlias = MyObject
  ##       MyObjectSubAlias = MyObjectAlias
  ##
  ##     var
  ##       a: MyObjectSubAlias
  ##       d: Vec4f
  ##
  ##     import macros, future, sequtils, strutils
  ##
  ##     macro foobar(arg: typed): untyped =
  ##       let types = arg.getTypeInst.resolveAlias
  ##       echo "seq(", types.map( x => x.repr).join(", "), ")"
  ##
  ##     foobar(a) # prints: seq(MyObjectSubAlias, MyObjectAlias, MyObject)
  ##     foobar(d) # prints: seq(Vec4f, Vec4[float32], Vec[4, float32])

  let typeDef = arg.resolveAliasInternal
  typeDef.sequenceTransform

when isMainModule:
  type
    Vec[N: static[int],T] = object
      arr: array[N,T]
    Vec4[T] = Vec[4,T]
    Vec4f = Vec4[float32]
    MyObject = object
      a,b,c: int
    MyObjectAlias = MyObject
    MyObjectSubAlias = MyObjectAlias
    MyTuple = tuple[a,b: int]
    MyFloatAlias = float32
    MyFloatSubAlias = MyFloatAlias
    MyOtherFloat = distinct float32

  import macros, future, sequtils, strutils

  macro foobar(arg: typed): untyped =
    let types = arg.getTypeInst.resolveAlias
    echo "seq(`", types.map( x => x.repr).join("`, `"), "`)"

  var
    a: MyObjectSubAlias
    b: Vec4f
    c: MyTuple
    d: float32
    e: MyFloatAlias
    f: MyFloatSubAlias
    g: MyOtherFloat

  foobar(a) # prints: seq(`MyObjectSubAlias`, `MyObjectAlias`, `MyObject`)
  foobar(b) # prints: seq(`Vec4f`, `Vec4[float32]`, `Vec[4, float32]`)
  foobar(c) # prints: seq(`MyTuple`, `tuple[a, b: int]`)
  foobar(d) # prints: seq(`float32`)
  foobar(e) # prints: seq(`MyFloatAlias`, `float32`)
  foobar(f) # prints: seq(`MyFloatSubAlias`, `MyFloatAlias`, `float32`)
  foobar(g) # prints: seq(`MyOtherFloat`)
