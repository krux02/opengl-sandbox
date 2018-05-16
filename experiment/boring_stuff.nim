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
    return arg.len

  var insertionIndex = 1
  var readIndex = 1

  while readIndex < arg.len:
    # is the last inserted (past tense == -1) element different that the current one?
    if arg[insertionIndex-1] != arg[readIndex]:
      swap(arg[insertionIndex], arg[readIndex])
      insertionIndex += 1
    readIndex += 1
  insertionIndex


proc makeUnique[T](arg: var seq[T]): void =
  ## removes consecutive duplicate elements from `arg`. This works
  ## like `makeUniqueData`, but it changes the length of `arg`
  ## the amount of unique elements.
  arg.setLen(arg.makeUniqueData)

proc sortAndUnique*[T](arg: var seq[T]): void =
  arg.sort(cmp)
  arg.makeUnique

proc mergeUnique*[T](a,b: seq[T]): seq[T] =
  ## merge two sorted sequences into a single seq.
  result = @[]
  var i,j = 0

  while i < a.len and j < b.len:
    if a[i] < b[j]:
      result.add a[i]
      i += 1
    elif b[j] < a[i]:
      result.add b[j]
      j += 1
    else:
      result.add a[i]
      i += 1
      j += 1

  # append rest
  while i < a.len:
    result.add a[i]
    i += 1

  while j < b.len:
    result.add b[j]
    j += 1


macro add*(dst: var string, arg1, arg2: untyped, rest: varargs[untyped]): untyped =
  result = quote do:
    `dst`.add(`arg1`)
    `dst`.add(`arg2`)
  for arg in rest:
    result.add newCall(bindSym"add", dst, arg)

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

proc findSymbolWithName*(arg: NimNode; symName: string): NimNode =
  ## searches though a tree and returns the first symbol node with the
  ## given identifier, on nil if no such symbol could be found.
  for node in arg.depthFirstTraversal:
    if node.kind == nnkSym and eqIdent(node, symName):
      return node

iterator fields*(typeAst: NimNode): tuple[memberSym, typeSym: NimNode] =
  let parent: NimNode =
    if typeAst.kind == nnkObjectTy:
      typeAst[2]
    elif typeAst.kind in {nnkLetSection,nnkRecList}:
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



const glslKeywords* = @[
  "active",
  "asm",
  "attribute",
  "bool",
  "break",
  "bvec2",
  "bvec3",
  "bvec4",
  "case",
  "cast",
  "centroid",
  "class",
  "common partition",
  "const",
  "continue",
  "default",
  "discard",
  "do",
  "double",
  "dvec2",
  "dvec3",
  "dvec4",
  "else",
  "enum typedef",
  "extern",
  "external",
  "false",
  "filter",
  "fixed",
  "flat",
  "float",
  "for",
  "fvec2",
  "fvec3",
  "fvec4",
  "goto",
  "half",
  "highp",
  "hvec2 hvec3",
  "hvec4",
  "if",
  "iimage1D",
  "iimage1DArray",
  "iimage2D",
  "iimage2DArray",
  "iimage3D",
  "iimageBuffer",
  "iimageCube",
  "image1D",
  "image1DArray",
  "image1DArrayShadow",
  "image1DShadow",
  "image2D",
  "image2DArray",
  "image2DArrayShadow",
  "image2DShadow",
  "image3D",
  "imageBuffer",
  "imageCube",
  "in",
  "inline",
  "inout",
  "input output",
  "int",
  "interface",
  "invariant",
  "isampler1D",
  "isampler1DArray",
  "isampler2D",
  "isampler2DArray",
  "isampler2DRect",
  "isampler3D",
  "isamplerBuffer",
  "isamplerCube",
  "ivec2",
  "ivec3",
  "ivec4",
  "layout",
  "long",
  "lowp",
  "mat2",
  "mat2x2",
  "mat2x3",
  "mat2x4",
  "mat3",
  "mat3x2",
  "mat3x3",
  "mat3x4",
  "mat4",
  "mat4x2",
  "mat4x3",
  "mat4x4",
  "mediump",
  "namespace",
  "noinline",
  "noperspective",
  "out",
  "precision",
  "public",
  "return",
  "row_major",
  "sampler1D",
  "sampler1DArray",
  "sampler1DArrayShadow",
  "sampler1DShadow",
  "sampler2D",
  "sampler2DArray",
  "sampler2DArrayShadow",
  "sampler2DRect",
  "sampler2DRectShadow",
  "sampler2DShadow",
  "sampler3D",
  "sampler3DRect",
  "samplerBuffer",
  "samplerCube",
  "samplerCubeShadow",
  "short",
  "sizeof",
  "smooth",
  "static",
  "struct",
  "superp",
  "switch",
  "template this packed",
  "true",
  "uimage1D",
  "uimage1DArray",
  "uimage2D",
  "uimage2DArray",
  "uimage3D",
  "uimageBuffer",
  "uimageCube",
  "uint",
  "uniform",
  "union",
  "unsigned",
  "usampler1D",
  "usampler1DArray",
  "usampler2D",
  "usampler2DArray",
  "usampler3D",
  "usamplerBuffer",
  "usamplerCube",
  "using",
  "uvec2",
  "uvec3",
  "uvec4",
  "varying",
  "vec2",
  "vec3",
  "vec4",
  "void",
  "volatile",
  "while",
]
