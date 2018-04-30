import glm, macros

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


proc sortAndUnique*[T](arg: var seq[T]): void =
  arg.sort(cmp)
  arg.makeUnique

## NimNode utils

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

iterator fields*(typeAst: NimNode): tuple[memberSym, typeSym: NimNode] =
  let parent: NimNode =
    if typeAst.kind == nnkObjectTy:
      typeAst[2]
    elif typeAst.kind == nnkLetSection:
      typeAst
    else:
      warning("just a guessing game here")
      echo typeAst.treeRepr
      typeAst

  for identDefs in parent:
    identDefs.expectKind nnkIdentDefs
    let typeSym = identDefs[^2]
    for i in 0 ..< identDefs.len-2:
      let memberSym = identDefs[i]
      yield((memberSym: memberSym, typeSym: typeSym))

iterator fieldValuePairs*(arg: NimNode): tuple[memberSym, valueSym: NimNode] =
  arg.expectkind({nnkLetSection, nnkVarSection})
  for identDefs in arg:
    identDefs.expectKind nnkIdentDefs
    let valueSym = identDefs[^1]
    for i in 0 ..< identDefs.len-2:
      let memberSym = identDefs[i]
      yield((memberSym: memberSym, valueSym: valueSym))


iterator args*(arg: NimNode): NimNode =
  arg.expectKind nnkCallKinds
  for i in 1 ..< arg.len:
    yield arg[i]



## gl wrapper ##

type
  Texture1D*            = object
    handle: uint32
  Texture2D*            = object
    handle: uint32
  Texture3D*            = object
    handle: uint32
  TextureCube*          = object
    handle: uint32
  Texture2DShadow*      = object
    handle: uint32
  TextureCubeShadow*    = object
    handle: uint32
  Texture2DArray*       = object
    handle: uint32
  Texture2DArrayShadow* = object
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
