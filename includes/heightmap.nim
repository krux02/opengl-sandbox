# included from fancygl.nim

type HeightMap* = object
  w,h: int
  dataseq: seq[float32]

proc w*(hm: HeightMap): int = hm.w
proc h*(hm: HeightMap): int = hm.h
proc size*(hm: HeightMap): Vec2i = vec2i(hm.w.int32, hm.h.int32)
proc data*(hm: HeightMap): seq[float32] = hm.dataseq

template linearIndex(x,y): untyped =
  (x and (hm.w - 1)) + hm.w * (y and hm.h - 1)
  
proc `[]`*(hm: HeightMap, x,y: int): float32 {. inline .} =
  hm.dataseq[linearIndex(x,y)]

proc `[]=`*(hm: var HeightMap, x,y: int; value: float32)  {. inline .} =
  hm.dataseq[linearIndex(x,y)] = value

proc `[]`*(hm: HeightMap, x,y: float32): float32 {. inline .} =
  let
    bx = x.floor.int
    by = y.floor.int
    
    d1 = hm.dataseq[linearIndex(bx  , by  )]
    d2 = hm.dataseq[linearIndex(bx+1, by  )]
    d3 = hm.dataseq[linearIndex(bx  , by+1)]
    d4 = hm.dataseq[linearIndex(bx+1, by+1)]

    rx = x - x.floor
    ry = y - y.floor

  mix( mix(d1,d2, rx), mix(d3,d4, rx), ry )

proc `[]`*(hm: HeightMap, pos: Vec2f) : float32 {. inline .} =
  hm[pos.x, pos.y]

proc `[]`*(hm: HeightMap, pos: Vec2i) : float32 {. inline .} =
  hm.dataseq[linearIndex(pos.x, pos.y)]

proc vertices*(hm: HeightMap) : seq[Vec4f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      result.add vec4f(x.float32 + 0.5f,y.float32 + 0.5f,hm[x,y],1'f32)

proc normals*(hm: HeightMap) : seq[Vec4f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      let v1 = vec3f(2, 0, hm[x+1, y] - hm[x-1, y])
      let v2 = vec3f(0, 2, hm[x, y+1] - hm[x, y-1])
      result.add(vec4f(normalize(cross(v1,v2)), 0))

proc flatVertices*(hm: HeightMap) : seq[Vec4f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      result.add vec4f(x.float32 + 0.5f,y.float32 + 0.5f,0,1)


proc indicesTriangles*(hm: HeightMap) : seq[int32] =
  result.newSeq(hm.w * hm.h * 6)
  result.setLen(0)

  for y in 0 ..< hm.h:
    for x in 0 ..< hm.w:
      let
        i1 = int32(x     + (hm.w + 1) * y      )
        i2 = int32(x + 1 + (hm.w + 1) * y      )
        i3 = int32(x     + (hm.w + 1) * (y + 1))
        i4 = int32(x + 1 + (hm.w + 1) * (y + 1))

        h1 = hm[x+0,y+0]
        h2 = hm[x+1,y+0]
        h3 = hm[x+0,y+1]
        h4 = hm[x+1,y+1]

      if abs(h1 - h4) < abs(h2 - h3):
        result.add([i3,i1,i4,i4,i1,i2])
      else:
        result.add([i1,i2,i3,i3,i2,i4])

proc indicesTriangleStrip*(hm: HeightMap): seq[int32] =
  result.newSeq(0)

  for y in 0 ..< hm.h:
    for x in 0 .. hm.w:
      result.add int32(x     + (hm.w + 1) * (y + 1))
      result.add int32(x     + (hm.w + 1) *  y     )
      #if y != 0 and x == 0:
      #  result.add result.back
      #if x == hm.w:
      #  result.add result.back

    result.add -1

proc indicesQuads*(hm: HeightMap): seq[int32] =
  result.newSeq(0)
  for y in 0 ..< hm.h:
    for x in 0 ..< hm.w:
      result.add int32(x     + (hm.w + 1) * y      )
      result.add int32(x + 1 + (hm.w + 1) * y      )
      result.add int32(x     + (hm.w + 1) * (y + 1))
      result.add int32(x + 1 + (hm.w + 1) * (y + 1))


proc texCoords*(hm: HeightMap) : seq[Vec2f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  let scale = vec2f(1.0f / hm.w.float32, 1.0f / hm.h.float32)

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      let pos = vec2f(x.float32 + 0.5f, y.float32 + 0.5f)
      result.add(pos * scale)

proc minMax*(hm: HeightMap): (float,float) =
  result[0] = Inf
  result[1] = NegInf

  for v in hm.dataseq:
    result[0] = min(result[0], v)
    result[1] = max(result[1], v)

# TODO add to glm  
proc linMap(v,min,max, newMin, newMax: float32): float32 =
  (v - min) * (newMax - newMin) / (max - min) + newMin

proc printMap*(hm: var HeightMap): void =
  let (min,max) = hm.minMax
  if min == max:
    return

  const chars = " .:;+X# .:;+X#"

  for y in 0 ..< hm.h:
    for x in 0 ..< hm.w:
      let idx = hm[x,y].linMap(min,max, 0, chars.len-1).int
      #stdout.write( idx )
      stdout.write( chars[idx] )
    stdout.writeLine("")
  echo ""

proc DiamondSquare*(hm: var HeightMap, startfactor: float32): void =
  let
    w = hm.w
    h = hm.h

  var
    stepSize = w
    factor = startfactor

  proc squares(hm: var HeightMap): void =
    for i in countup(0, w-1, stepSize):
      for j in countup(0, h-1, stepSize):
        var sum =  hm[i,j]
        sum += hm[i + stepSize, j]
        sum += hm[i, j + stepSize]
        sum += hm[i + stepSize, j + stepSize]

        let x = i + stepSize div 2
        let y = j + stepSize div 2
        hm[x,y] = sum * 0.25f + (rand_f32() - 0.5f) * factor

  proc diamonds(hm: var HeightMap): void =
    for i in countup(0, w-1, stepSize):
      for j in countup(0, h-1, stepSize):
        if ((i+j) div stepSize) mod 2 == 1:
          var sum = 0.0f
          var count = 0.0f
          if i != 0:
            sum += hm[i-stepSize, j]
            count += 1
          if i < w-1:
            sum += hm[i+stepSize, j]
            count += 1
          if j != 0:
            sum += hm[i, j-stepSize]
            count += 1
          if j < h-1:
            sum += hm[i, j+stepSize]
            count += 1

          hm[i,j] = (sum / count) + (rand_f32() - 0.5f) * factor


  while stepSize > 0:
    squares(hm)
    stepSize = stepSize div 2
    if stepSize == 0:
      break

    diamonds(hm)
    factor *= 0.5f

proc newHeightMap*(width,height: int): HeightMap =
  result.w = width
  result.h = height
  result.dataseq = newSeq[float32](width*height)


