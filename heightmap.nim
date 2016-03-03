proc nextRnd(): float32 =
  random(1.0).float32 - 0.5f

proc lerp(start, stop, amt: float32) : float32 =
  (1 - amt) * start + amt * stop

type HeightMap* = object
  w,h: int
  dataseq: seq[float32]

proc `[]`*(hm: var HeightMap, x,y: int): float32 {. inline .} =
  let
    nx = x and (hm.w - 1)
    ny = y and (hm.h - 1)

  hm.dataseq[nx + hm.w * ny]

proc `[]=`*(hm: var HeightMap, x,y: int; value: float32)  {. inline .} =
  let
    nx = x and (hm.w - 1)
    ny = y and (hm.h - 1)

  hm.dataseq[nx + hm.w * ny] = value

proc `[]`*(hm: HeightMap, x,y: float32): float32 {. inline .} =
  let
    bx = x.floor.int
    by = y.floor.int

    nx1 = bx and (hm.w - 1)
    nx2 = (bx + 1) and (hm.w - 1)
    ny1 = by and (hm.h - 1)
    ny2 = (by + 1) and (hm.h - 1)

    d1 = hm.dataseq[nx1 + hm.w * ny1]
    d2 = hm.dataseq[nx2 + hm.w * ny1]
    d3 = hm.dataseq[nx1 + hm.w * ny2]
    d4 = hm.dataseq[nx2 + hm.w * ny2]

    rx = x - x.floor
    ry = y - y.floor

  lerp( lerp(d1,d2, rx), lerp(d3,d4, rx), ry )

proc `[]`*(hm: HeightMap, pos: Vec2f) : float32 {. inline .} =
  hm[pos.x, pos.y]

proc vertices*(hm: var HeightMap) : seq[Vec3f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      result.add vec3f(x.float32,y.float32,hm[x,y])

proc normals*(hm: var HeightMap) : seq[Vec3f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      let v1 = vec3f(2, 0, hm[x+1, y] - hm[x-1, y])
      let v2 = vec3f(0, 2, hm[x, y+1] - hm[x, y-1])
      result.add(normalize(cross(v1,v2)))

proc flatVertices*(hm: var HeightMap) : seq[Vec3f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      result.add vec3f(x.float32,y.float32,0)


proc indices*(hm: var HeightMap) : seq[int32] =
  result.newSeq(hm.w * hm.h * 6)
  result.setLen(0)

  for y in 0 ..< hm.h:
    for x in 0 ..< hm.w:
      let
        i1 = int32(x     + (hm.w + 1) * y)
        i2 = int32(x + 1 + (hm.w + 1) * y)
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



proc texCoords*(hm: var HeightMap) : seq[Vec2f] =
  result.newSeq(hm.w * hm.h)
  result.setLen(0)

  let
    wf = hm.w.float32
    hf = hm.h.float32

  for y in 0 .. hm.h:
    for x in 0 .. hm.w:
      let
        xf = x.float32
        yf = y.float32

      result.add vec2f(xf / wf, yf / hf)

proc minMax*(hm: HeightMap): (float,float) =
  result[0] = Inf
  result[1] = NegInf

  for v in hm.dataseq:
    result[0] = min(result[0], v)
    result[1] = max(result[1], v)

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
        hm[x,y] = sum * 0.25f + nextRnd() * factor

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

          hm[i,j] = (sum / count) + nextRnd() * factor


  while stepSize > 0:
    squares(hm)
    stepSize = stepSize div 2
    if stepSize == 0:
      break

    diamonds(hm)
    factor *= 0.5f

proc createFlatMap*(width,height: int): HeightMap =
  result.w = width
  result.h = height
  result.dataseq = newSeq[float32](width*height)


