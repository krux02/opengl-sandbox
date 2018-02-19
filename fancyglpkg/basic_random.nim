import mersenne, ziggurat_normal_dist, math, times

var mt = newMersenneTwister(uint32(epochTime()))

proc randNormal*(): float64 =
  ## Return a random number with normal distribution.
  mt.nextGaussian()

proc rand_u64*(): uint64 =
  ## Return 64 random bits. Every number in `uint64` has the same probability.
  (mt.getNum.uint64 shl 32) or mt.getNum.uint64

proc rand_i64*(): int64  =
  ## Return 63 random bits. Every positive number in `int64` has tha same probability.
  int64(rand_u64() and 0x7fffffffffffffff'u64)

proc rand_u32*(): uint32 =
  ## Return 32 random bits. Every number in `uint32` has the same probability.
  mt.getNum

proc rand_i32*(): int32  =
  ## Return 31 random bits. Every positive number in `int32` has the same probability.
  int32(mt.getNum  and 0x7fffffff)

proc rand_u16*(): uint16 = uint16(mt.getNum  and 0xffff)
  ## Return 16 random bits. Every number in `uint16` has the same probability.

proc rand_i16*(): int16  =
  ## Return 15 random bits. Every positive number in `int16` has the same probability.
  int16(mt.getNum  and 0x7fff)

proc rand_u8*(): uint8   =
  ## Return 8 random bits. Every number in `uint8` has the same probability.
  uint8(mt.getNum  and 0xff)

proc rand_i8*(): int8    =
  ## Return 7 random bits. Every positive number in `int8` has the same probability.
  int8(mt.getNum  and 0x7f)

proc rand_f64*(): float64 =
  ## Returns a random number in the range `[0, 1)`.

  # 52 bit mantissa
  # 11 bit exponent with exponent bias,  represents exponent 0
  # 1 bit sign
  # exponent bias: 0x3FF = 1023

  const a =  0x000fffffffffffff'u64  # mask for mantissa
  const b =  0x3ff0000000000000'u64  # exponent 0
  let x = (rand_u64() and a) or b
  cast[float64](x) - 1.0'f64


proc rand_f32*(): float32 =
  ## Return a random number in the range `[0, 1)`.

  # 23 bit mantissa
  #  8 bit exponent with exponent bias,
  #  1 bit sign
  # exponent bias: 0x7F = 127 = 0b1111111

  const a = 0x007fffff # mask for mantissa
  const b = 0x3f800000 # exponent 0
  let x = (rand_u32() and a) or b
  cast[float32](x) - 1.0'f32

#proc rand_f64*(): float64 =
#  const factor = pow(2'f64, -64'f64)
#  float64(rand_u64()) * factor

#proc rand_f32*(): float32 =
#  const factor = pow(2'f32, -32'f32)
#  float32(rand_u32()) * factor

proc rand_bool*(): bool =
  ## Return 1 random bit, a random `bool`.
  (mt.getNum and 1) != 0

proc rand*(maxval: uint64): uint64 =
  ## Return a random number from 0 to the exclusive upper bound `maxval`.
  let limit = uint64(-1) - (uint64(-1) mod maxval)
  var bits = rand_u64()
  while bits > limit:
    bits = rand_u64()
  result = bits mod maxval

proc rand*(maxval: uint32): uint32 =
  ## Return a random number from 0 to the exclusive upper bound `maxval`.
  let limit = uint32(high(uint32)) - uint32(high(uint32)) mod maxval
  var bits = rand_u32()
  while bits > limit:
    bits = rand_u32()
  result = bits mod maxval

proc rand*(maxval: uint): uint =
  ## Return a random number from 0 to the exclusive upper bound `maxval`.
  assert(maxval != 0)
  when sizeof(int) == 8:
    rand(maxval.uint64).uint
  else:
    rand(maxval.uint32).uint

proc rand*(maxval: int32): int32 =
  ## Return a random number from 0 to the exclusive upper bound `maxval`.
  ## `maxval` needs to be positive.
  assert(maxval > 0)
  rand(maxval.uint32).int32

proc rand*(maxval: int64): int64 =
  ## Return a random number from 0 to the exclusive upper bound `maxval`.
  ## `maxval` needs to be positive.
  assert(maxval > 0)
  rand(maxval.uint64).int64

proc rand*(maxval: int): int =
  ## Return a random number from 0 to the exclusive upper bound `maxval`.
  ## `maxval` needs to be positive.
  assert(maxval > 0)
  when sizeof(int) == 8:
    rand(maxval.uint64).int
  else:
    rand(maxval.uint32).int
