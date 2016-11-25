import mersenne, ziggurat_normal_dist, math

var mt = newMersenneTwister(seed = 123)

proc randNormal*(): float64 = mt.nextGaussian()

proc rand_u64*(): uint64 =
  (uint64(mt.getNum) shl 32) or uint64(mt.getNum)

proc rand_u32*(): uint32 = mt.getNum

proc rand_i32*(): int32 = rand_u32().int32

proc rand_i64*(): int64 = rand_u64().int64

proc rand_u16*(): uint16 = mt.getNum.uint16
  
proc rand_i16*(): int16 = mt.getNum.int16
  
proc rand_u8*(): uint8   = mt.getNum.uint8
  
proc rand_i8*(): int8   = mt.getNum.int8
  
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

proc rand*(maxval: int32): int32 =
  assert(maxval > 0)
  rand(maxval.uint32).int32

proc rand*(maxval: int64): int64 =
  assert(maxval > 0)
  rand(maxval.uint64).int64

