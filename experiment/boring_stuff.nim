## this file contains only stuff that you don't want to know the exact implementation of

import glm, macros, algorithm

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

iterator depthFirstTraversal(n: NimNode): NimNode =
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
    elif typeAst.kind in {nnkLetSection, nnkVarSection, nnkTupleTy, nnkRecList, nnkFormalParams}:
      typeAst
    else:
      warning("just a guessing game here")
      echo typeAst.treeRepr
      typeAst

  var first = true
  for identDefs in parent:
    if first:
      first = false
      ## the first element in FormalParams is the result type
      if parent.kind == nnkFormalParams:
        continue

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

proc isSwizzle*(funcName: string): bool =
  if funcName.len == 0:
    return false

  const
    setA = {'x','y','z','w'}
    setB = {'r','g','b','a'}
    setC = {'s','t','p','q'}

  let c = funcName[0]

  let swizzleSet =
    if c in setA:
      setA
    elif c in setB:
      setB
    else:
      setC

  if funcName[^1] == '=':
    if funcName.len > 5:
      return false
    for c in funcName[0 .. ^2]:
      if c notin swizzleSet:
        return false
    return true
  else:
    if funcName.len > 4:
      return false
    for c in funcName:
      if c notin swizzleSet:
        return false
    return true

const glslBuiltInProc* = [
  "EmitStreamVertex",
  "EmitVertex",
  "EndPrimitive",
  "EndStreamPrimitive",
  "abs",
  "acos",
  "acosh",
  "all",
  "any",
  "asin",
  "asinh",
  "atan",
  "atanh",
  "atomicAdd",
  "atomicAnd",
  "atomicCompSwap",
  "atomicCounter",
  "atomicCounterDecrement",
  "atomicCounterIncrement",
  "atomicExchange",
  "atomicMax",
  "atomicMin",
  "atomicOr",
  "atomicXor",
  "barrier",
  "bitCount",
  "bitfieldExtract",
  "bitfieldInsert",
  "bitfieldReverse",
  "ceil",
  "clamp",
  "cos",
  "cosh",
  "cross",
  "degrees",
  "determinant",
  "dFdx",
  "dFdxCoarse",
  "dFdxFine",
  "dFdy",
  "dFdyCoarse",
  "dFdyFine",
  "distance",
  "dot",
  "equal",
  "exp",
  "exp2",
  "faceforward",
  "findLSB",
  "findMSB",
  "floatBitsToInt",
  "floatBitsToUint",
  "floor",
  "fma",
  "fract",
  "frexp",
  "fwidth",
  "fwidthCoarse",
  "fwidthFine",
  "greaterThan",
  "greaterThanEqual",
  "groupMemoryBarrier",
  "imageAtomicAdd",
  "imageAtomicAnd",
  "imageAtomicCompSwap",
  "imageAtomicExchange",
  "imageAtomicMax",
  "imageAtomicMin",
  "imageAtomicOr",
  "imageAtomicXor",
  "imageLoad",
  "imageSamples",
  "imageSize",
  "imageStore",
  "imulExtended",
  "intBitsToFloat",
  "interpolateAtCentroid",
  "interpolateAtOffset",
  "interpolateAtSample",
  "inverse",
  "inversesqrt",
  "isinf",
  "isnan",
  "ldexp",
  "length",
  "lessThan",
  "lessThanEqual",
  "log",
  "log2",
  "matrixCompMult",
  "max",
  "memoryBarrier",
  "memoryBarrierAtomicCounter",
  "memoryBarrierBuffer",
  "memoryBarrierImage",
  "memoryBarrierShared",
  "min",
  "mix",
  "mod",
  "modf",
  "noise",
  "noise1",
  "noise2",
  "noise3",
  "noise4",
  "normalize",
  "not",
  "notEqual",
  "outerProduct",
  "packDouble2x32",
  "packHalf2x16",
  "packSnorm2x16",
  "packSnorm4x8",
  "packUnorm",
  "packUnorm2x16",
  "packUnorm4x8",
  "pow",
  "radians",
  "reflect",
  "refract",
  "removedTypes",
  "round",
  "roundEven",
  "sign",
  "sin",
  "sinh",
  "smoothstep",
  "sqrt",
  "step",
  "tan",
  "tanh",
  "texelFetch",
  "texelFetchOffset",
  "texture",
  "textureGather",
  "textureGatherOffset",
  "textureGatherOffsets",
  "textureGrad",
  "textureGradOffset",
  "textureLod",
  "textureLodOffset",
  "textureOffset",
  "textureProj",
  "textureProjGrad",
  "textureProjGradOffset",
  "textureProjLod",
  "textureProjLodOffset",
  "textureProjOffset",
  "textureQueryLevels",
  "textureQueryLod",
  "textureSamples",
  "textureSize",
  "transpose",
  "trunc",
  "uaddCarry",
  "uintBitsToFloat",
  "umulExtended",
  "unpackDouble2x32",
  "unpackHalf2x16",
  "unpackSnorm2x16",
  "unpackSnorm4x8",
  "unpackUnorm",
  "unpackUnorm2x16",
  "unpackUnorm4x8",
  "usubBorrow",
]

const glslConvProc* = [
  "mat2",
  "mat2d",
  "mat2f",
  "mat2i",
  "mat3",
  "mat3d",
  "mat3f",
  "mat3i",
  "mat4",
  "mat4d",
  "mat4f",
  "mat4i",
  "vec2",
  "vec2d",
  "vec2f",
  "vec2i",
  "vec3",
  "vec3d",
  "vec3f",
  "vec3i",
  "vec4",
  "vec4d",
  "vec4f",
  "vec4i",
]

const glslBuiltInProcSecondary* = [
  "and",
  "inc",
  "modulo",
  "or",
  "shl",
  "shr",
]

proc isBuiltIn*(procName: string): bool {.compileTime.} =

  if binarySearch(glslBuiltInProc, procName) >= 0 :
    return true
  if binarySearch(glslConvProc, procname) >= 0:
    return true
  if binarySearch(glslBuiltInProcSecondary, procName) >= 0:
    return true
  if procName.isSwizzle:
    return true
  # is operator ?
  for c in procName:
    if c notin "=+-*/<>@$~&%|!?^.:\\":
      break
    return true
  # is primitive constructor ?
  if procname.len == 4 and procName[0..2] == "vec" and procName[3] in "234":
    return true

  return false


#[

gl_ClipDistance
gl_CullDistance
gl_FragCoord
gl_FragDepth
gl_FrontFacing
gl_GlobalInvocationID
gl_HelperInvocation
gl_InstanceID
gl_InvocationID
gl_Layer
gl_LocalInvocationID
gl_LocalInvocationIndex
gl_NumSamples
gl_NumWorkGroups
gl_PatchVerticesIn
gl_PointCoord
gl_PointSize
gl_Position
gl_PrimitiveID
gl_PrimitiveIDIn
gl_SampleID
gl_SampleMask
gl_SampleMaskIn
gl_SamplePosition
gl_TessCoord
gl_TessLevelInner
gl_TessLevelOuter
gl_VertexID
gl_ViewportIndex
gl_WorkGroupID
gl_WorkGroupSize

]#


when isMainModule:
  import algorithm, sequtils
  echo binarySearch(glslBuiltInProc, "dot")
  echo binarySearch(glslBuiltInProc, "EmitStreamVertex")
