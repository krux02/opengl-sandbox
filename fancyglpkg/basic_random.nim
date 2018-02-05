import mersenne, ziggurat_normal_dist, math, times

var mt = newMersenneTwister(uint32(epochTime()))

proc randNormal*(): float64 = mt.nextGaussian()

proc rand_u64*(): uint64 =
  (mt.getNum.uint64 shl 32) or mt.getNum.uint64

proc rand_i64*(): int64  = int64(rand_u64() and 0x7fffffffffffffff'u64)

proc rand_u32*(): uint32 = mt.getNum

proc rand_i32*(): int32  = int32(mt.getNum  and 0x7fffffff)

proc rand_u16*(): uint16 = uint16(mt.getNum  and 0xffff)

proc rand_i16*(): int16  = int16(mt.getNum  and 0x7fff)

proc rand_u8*(): uint8   = uint8(mt.getNum  and 0xff)

proc rand_i8*(): int8    = int8(mt.getNum  and 0x7f)

proc rand_f64*(): float64 =
  rand_u64().float64 / pow(2'f64, 64'f64)

proc rand_f32*(): float32 =
  rand_u32().float32 / pow(2'f32, 32'f32)

proc rand*(maxval: uint64): uint64 =
  let limit = uint64(-1) - (uint64(-1) mod maxval)
  var bits = rand_u64()
  while bits > limit:
    bits = rand_u64()
  result = bits mod maxval

proc rand*(maxval: uint32): uint32 =
  let limit = uint32(high(uint32)) - uint32(high(uint32)) mod maxval
  var bits = rand_u32()
  while bits > limit:
    bits = rand_u32()
  result = bits mod maxval

proc rand*(maxval: uint): uint =
  assert(maxval != 0)
  when sizeof(int) == 8:
    rand(maxval.uint64).uint
  else:
    rand(maxval.uint32).uint

proc rand*(maxval: int32): int32 =
  assert(maxval > 0)
  rand(maxval.uint32).int32

proc rand*(maxval: int64): int64 =
  assert(maxval > 0)
  rand(maxval.uint64).int64

proc rand*(maxval: int): int =
  assert(maxval > 0)
  when sizeof(int) == 8:
    rand(maxval.uint64).int
  else:
    rand(maxval.uint32).int
