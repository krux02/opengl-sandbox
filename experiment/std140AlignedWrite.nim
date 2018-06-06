
import glm

template write[T](dst: pointer; offset: var int32; value: T): untyped =
  (cast[ptr T](cast[uint](dst) + cast[uint](offset)))[] = value
  offset += int32(sizeof(value))

proc align(address, alignment: int32): int32 =
  result = (address + (alignment - 1)) and not (alignment - 1)

proc getAlignment(value: bool): int32 =
  result = 4

proc getAlignment(value: SomeNumber): int32 =
  result = int32(sizeof(SomeNumber))

proc getAlignment[T](value: Vec4[T]): int32 =
  result = int32(sizeof(T) * 4)

proc getAlignment[T](value: Vec3[T]): int32 =
  result = int32(sizeof(T) * 4)

proc getAlignment[T](value: Vec2[T]): int32 =
  result = int32(sizeof(T) * 2)

proc getAlignment[N,M,T](value: Mat[N,M,T]): int32 =
  result = max(16'i32, value.arr[0].getAlignment)

proc getAlignment[N,T](value: array[N,T]): int32 =
  result = max(16'i32, value[0].getAlignment)

proc getAlignment(value : tuple | object): int32 =
  result = 16
  for x in value.fields:
    result = max(result, x.getAlignment)

var offsets = newSeq[int32](0)

template debug(result: tuple[offset, align: int32]): untyped =
  when isMainModule:
    offsets.add result.offset

proc std140AlignedWrite*(dst: pointer, offset: int32, value: bool): tuple[offset, align: int32] =
  result.align = 4
  result.offset = align(offset, 4)

  debug result
  dst.write(result.offset, value)

proc std140AlignedWrite*(dst: pointer, offset: int32, value: SomeNumber): tuple[offset, align: int32] =
  result.align = max(sizeof(SomeNumber), 4)
  result.offset = align(offset, result.align)

  debug result
  dst.write(result.offset, value)

proc std140AlignedWrite*[T](dst: pointer, offset: int32, value: Vec4[T]): tuple[offset, align: int32] =
  result.align = sizeof(T) * 4
  result.offset = align(offset, result.align)

  debug result
  dst.write(result.offset, value)

proc std140AlignedWrite*[T](dst: pointer, offset: int32, value: Vec3[T]): tuple[offset, align: int32] =
  result.align = sizeof(T) * 4
  result.offset = align(offset, result.align)

  debug result
  dst.write(result.offset, value)

proc std140AlignedWrite*[T](dst: pointer, offset: int32, value: Vec2[T]): tuple[offset, align: int32] =
  result.align = sizeof(T) * 2
  result.offset = align(offset, result.align)

  debug result
  dst.write(result.offset, value)

proc std140AlignedWrite*[N,M,T](dst: pointer, offset: int32, value: Mat[N,M,T]): tuple[offset, align: int32] =

  result.align = getAlignment(value)
  result.offset = align(offset, result.align)

  debug result
  for v in value.arr:
    dst.write(result.offset, v)
    result.offset = align(result.offset, result.align)

proc std140AlignedWrite*[N,T](dst: pointer, offset: int32; value: array[N,T]): tuple[offset, align: int32] =
  result.align  = getAlignment(value)
  result.offset = align(offset, result.align)

  for i in 0 ..< len(value):
    let tmp = std140AlignedWrite(dst, result.offset, value[i])
    result.offset = align(tmp.offset, result.align)

proc std140AlignedWrite*(dst: pointer, offset: int32, value: tuple | object): tuple[offset, align: int32] =
  result.align = getAlignment(value)
  result.offset = align(offset, result.align)

  var tmp = result
  for x in value.fields:
    tmp = std140AlignedWrite(dst, tmp.offset, x)

  result.offset = align(tmp.offset, result.align)

when isMainModule:
  template foo(value: untyped, expectedOffsets: untyped): untyped =
    offsets.setLen 0
    discard std140AlignedWrite(data, 0, value)
    if offsets != expectedOffsets:
      echo "offsets != expectedOffsets"
      echo offsets
      echo expectedOffsets

  type
    Light = object
      position_ws : Vec4f
      color : Vec4f

    MyStruct = object
      f1: float32
      f2: float32

    StructA = object
      m0: Vec4f
      m1: Vec4f
      f1: float32

    StructB = object
      m0: Vec4d
      f1: float32

  var buffer = newSeq[byte](1024)
  var data = buffer[0].addr

  var b00: tuple[
    b0a: int32,
    b0c: int64,
  ]
  foo b00, @[0'i32, 8]

  var b01: tuple[
    b1a: int32,
    b1b: int32,
    b1c: int64,
  ]
  foo b01, @[0'i32, 4, 8]


  var b02: tuple[
    b2a: array[2,int32],
    b2c: int64,
  ]
  foo b02, @[0'i32, 16, 32]

  var b03: tuple[
    b3a: bool,
    b3b: bool,
    b3c: bool,
    b3d: float32,
  ]
  foo b03, @[0'i32, 4, 8, 12]

  var b04: tuple[
    b4a: Mat2f,
    b4b: Mat2f,
    b4c: Mat4f,
  ]
  foo b04, @[0'i32, 32, 64]

  var b05: tuple[
    b5a: array[2,Mat2f],
    b5c: Mat4f,
  ]
  foo b05, @[0'i32, 32, 64]

  var b06: tuple[
    b6a: MyStruct,
    b6b: MyStruct,
  ]
  foo b06, @[0'i32, 4, 16, 20]

  var b07: tuple[
    b7a: Vec3f,
    b7b: float32
  ]
  foo b07, @[0'i32, 12]

  var b08: tuple[
    b8a: Vec3f,
    b8b: float64,
  ]
  foo b08, @[0'i32, 16]

  var b09: tuple[
    b9a: Vec3d,
    b9b: Vec2f,
  ]
  foo b09, @[0'i32, 24]

  var b10: tuple[
    b10a: bool,
    b10b: bool,
  ]
  foo b10, @[0'i32, 4]

  var b11: tuple[
    b11a: StructA,
    b11b: float32,
  ]
  foo b11, @[0'i32, 16, 32, 48]

  var b12: tuple[
    b12a: StructB,
    b12b: float32,
  ]
  foo b12, @[0'i32, 32, 64]

  var b13: tuple[
    b13a: float32,
    b13b: StructB,
  ]
  foo b13, @[0'i32, 32, 64]

  var value1: tuple[
    a: Vec3f;
    b: Vec4f;
    c: float32
  ]
  foo value1, @[0'i32, 16, 32]

  var value2: tuple[
    time: float32,
    M, V, P: Mat4f,
    lights: array[4,Light]
  ]
  foo value2, @[0'i32, 16, 80, 144, 208, 224, 240, 256, 272, 288, 304, 320]

  var value3: tuple[
    m: Mat3f,
    f: float32
  ]
  foo value3, @[0'i32, 48]

  var value4: tuple[
    b: bool;
    m: Mat4f
  ]
  foo value4, @[0'i32, 16]
