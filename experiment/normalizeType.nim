import macros, ast_pattern_matching

## this package here is just to provide a function so that glm alias
## types such as ``Vec4f`` or ``Mat4f`` can be normalized to bracket
## expressions such as ``Vec[4,float32]``, ``Mat[4,4,float32]``.

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

proc normalizeType*(arg: NimNode): NimNode

proc resolveAliasInternal(typ: NimNode): NimNode =
  typ.matchAst:
  of nnkSym:
    if typ == bindSym"bool":
      return typ

    let impl = typ.getImpl
    if impl.kind == nnkNilLit:
      # this is just bad design on the Nim side of things: if the type
      # of the type is the type itself, it is a builtin type,
      # otherwise it is is the broken typedesc type. I just hate this
      # design.

      let typeInst = typ.getTypeInst
      if typeInst.kind == nnkBracketExpr:
        if typeInst[0].eqIdent "typeDesc":
          return typeInst[1].resolveAliasInternal
        else:
          error("expected typedes here", typ)
      else:
        return typ

      return typ

    result = impl.resolveAliasInternal

  of nnkTypeDef:
    result = typ
    result[2] = typ[2].resolveAliasInternal

  of nnkBracketExpr(ident"range", nnkIntLit , nnkIntLit):
    result = bindSym"int32"

  of nnkBracketExpr( `arr` @ ident"array", `size`, `innerType`):
    result = nnkBracketExpr.newTree(arr, size, normalizeType(innerType))

  of nnkBracketExpr |= typ[0].kind == nnkSym:
    result = typ[0].resolveAliasInternal
    result.expectKind nnkTypeDef

    # substitute all generic parameters with the corresponding generic
    # argument
    for i, sym in result[1]:
      let arg = typ[1 + i]
      result = result.substitute(sym, arg)

  of nnkCall |= typ[0].kind == nnkOpenSymChoice and typ[0][0].eqIdent("[]") and typ[1].kind == nnkSym:

    result = typ[1].resolveAliasInternal
    result.expectKind nnkTypeDef

    # substitute all generic parameters with the corresponding generic
    # argument
    for i, sym in result[1]:
      let arg = typ[2 + i]
      result = result.substitute(sym, arg)

  of {nnkObjectTy, nnkTupleTy, nnkDistinctTy}:
    result = typ


proc sequenceTransform(arg: NimNode): seq[NimNode] =
  ## `resolveAliasInternal` will return a recursive datastructure of
  ## nnkTypeDef nodes. This ast is as an illegal AST. This
  ## transformation so that each ast is legal again.  You ask yourself
  ## why I return the entire list instead of just the last element
  ## that?  Well I don't know anymore  ¯\_(ツ)_/¯
  var arg = arg

  result.newSeq(0)
  while arg.kind == nnkTypeDef:
    let sym = if arg[0].kind == nnkPragmaExpr: arg[0][0] else: arg[0]
    sym.expectKind nnkSym
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

proc normalizeType*(arg: NimNode): NimNode =
  ## Returns a normalized form of the types. This can be used for type
  ## equality comparison. But it is developed to translate types
  ## reliably to glsl.
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
  ##     foobar(a) # prints: MyObject
  ##     foobar(d) # prints: Vec[4, float32]

  arg.resolveAliasInternal.sequenceTransform[^1]

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

  import macros, sugar, sequtils, strutils

  macro foobar(arg: typed): untyped =
    let typ = arg.getTypeInst.normalizeType
    echo typ.repr

  var
    a: MyObjectSubAlias
    b: Vec4f
    c: MyTuple
    d: float32
    e: MyFloatAlias
    f: MyFloatSubAlias
    g: MyOtherFloat

  foobar(a) # prints: MyObject
  foobar(b) # prints: Vec[4, float32]
  foobar(c) # prints: tuple[a, b: int]
  foobar(d) # prints: float32
  foobar(e) # prints: float32
  foobar(f) # prints: float32
  foobar(g) # prints: MyOtherFloat
