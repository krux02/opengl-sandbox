import macros, typetraits

iterator typeFields(typeAst: NimNode): tuple[memberSym, typeSym: NimNode] =
  let parent =
    if typeAst.kind == nnkObjectTy:
      typeAst[2]
    else:
      typeAst

  for identDefs in parent:
    let typeSym = identDefs[^2]
    for i in 0 ..< identDefs.len-2:
      let memberSym = identDefs[i]
      yield((memberSym: memberSym, typeSym: typeSym))


proc align(offset, alignment: int): int =
  if alignment == 0 or offset mod alignment == 0:
    return offset
  else:
    return offset - offset mod alignment + alignment

template alignof[N,T](arg: typedesc[array[N,T]]): int = alignof(type(T))
template sizeof[N,T](arg: typedesc[array[N,T]]): int = (high(N) + 1 - low(N)) * system.sizeof(type(T))

template alignof[T : SomeInteger | enum](arg: typedesc[T]): int = system.sizeof(arg)

macro alignof[T : object | tuple](arg: typedesc[T]): int =
  if eqIdent(arg.getTypeInst.last, "TestAlign4"):
    echo arg.getTypeInst.last.getTypeImpl.treeRepr
  result = newCall(bindSym"max")

  for memberSym, typeSym in typeFields(arg.getTypeInst.last.getTypeImpl):
    result.add last quote do:
      alignof(type(`typeSym`))

macro sizeof[T : object | tuple](arg: typedesc[T]): int =
  result = newLit(0)
  for memberSym, typeSym in typeFields(arg.getTypeInst.last.getTypeImpl):
    result = last quote do:
      `result` + sizeof(type(`typeSym`))

  result = last quote do:
    align(`result`, alignof(type(`arg`)))

proc calcOffset(data: openarray[tuple[size,align: int]]): int {.compileTime.} =
  # just for internal use of the offsetof macro
  # the last tuple passed is the data from the object member, where the offset is wanted

  for size, alignment in data.items:
    # align for current data type
    result = align(result, alignment) + size

  # beginning of the last element
  result -= data[^1].size

macro offsetof[T: object](arg: typedesc[T], field: untyped) : int =
  field.expectKind(nnkIdent)

  let fieldIdentStr = field.repr

  result = newLit(0)
  for fieldSym, typeSym in typeFields(arg.getTypeInst.last.getTypeImpl):
    if eqIdent(fieldSym, fieldIdentStr):
      result = last quote do:
        align(`result`, alignof(type(`typeSym`)))
      return
    else:
      result = last quote do:
        align(`result`, alignof(type(`typeSym`))) + sizeof(type(`typeSym`))

  error("field not part of object|tuple", field)


dumpTree:
  type
    TestAlign2 = object
      a: byte
      b: array[8, byte]

    TestAlign4 = object {.packed.}
      a: byte
      b: int64

type
  TestAlign1 = object
    a: byte
    b: int

  TestAlign2 = object
    a: byte
    b: array[8, byte]

  TestAlign3 = object
    a: byte
    b: array[8, byte]
    c: int64

  TestAlign4 = object {.packed.}
    a: byte
    b: int64

  TestEnum {.size: 4.} = enum
    A
    B
    C
    D

const
  ao1 = alignof(TestAlign1)
  ao2 = alignof(TestAlign2)
  ao3 = alignof(TestAlign3)
  ao4 = alignof(TestAlign4)
  so1 = sizeof(TestAlign1)
  so2 = sizeof(TestAlign2)
  so3 = sizeof(TestAlign3)
  so4 = sizeof(TestAlign4)
  so5 = sizeof(TestEnum)

echo "sizeof(TestAlign1): ", so1, " ", system.sizeof(TestAlign1)
echo "sizeof(TestAlign2): ", so2, " ", system.sizeof(TestAlign2)
echo "sizeof(TestAlign3): ", so3, " ", system.sizeof(TestAlign3)
echo "sizeof(TestAlign4): ", so4, " ", system.sizeof(TestAlign4)
echo "sizeof(TestEnum):   ", so5, " ", system.sizeof(TestEnum)
echo "alignof(TestAlign1): ", alignof(TestAlign1)
echo "alignof(TestAlign2): ", alignof(TestAlign2)
echo "alignof(TestAlign3): ", alignof(TestAlign3)
echo "alignof(TestAlign4): ", alignof(TestAlign4)
echo "alignof(TestEnum):   ", alignof(TestEnum)

const
  offset1 = offsetof(TestAlign1, a)
  offset2 = offsetof(TestAlign1, b)
  offset3 = offsetof(TestAlign2, a)
  offset4 = offsetof(TestAlign2, b)
  offset5 = offsetof(TestAlign3, c)


echo "offsetof(TestAlign1, a): ", offset1
echo "offsetof(TestAlign1, b): ", offset2
echo "offsetof(TestAlign2, a): ", offset3
echo "offsetof(TestAlign2, b): ", offset4
echo "offsetof(TestAlign3, c): ", offset5
