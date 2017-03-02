# included from fancygl.nim

when isMainModule:
  import glm

proc uvSphereVertices*(segments, rings: int): seq[Vec4f] =
  result.newSeq((segments+1) * rings)
  result.setLen(0)

  for j in 0 .. segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32

    for i in 0 .. < rings:
      let
        alpha = (i / (rings-1)) * PI
        h = cos(alpha).float32
        r = sin(alpha).float32

      result.add( vec4f(x * r, y * r, h, 1) )

proc uvSphereNormals*(segments, rings: int): seq[Vec4f] =
  result.newSeq((segments+1) * rings)
  result.setLen(0)

  for j in 0 .. segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32

    for i in 0 .. < rings:
      let
        alpha = (i / (rings-1)) * PI
        h = cos(alpha).float32
        r = sin(alpha).float32

      result.add( vec4f(x * r, y * r, h, 0) )


proc uvSphereTexCoords*(segments, rings: int): seq[Vec2f] =
  result.newSeq((segments+1) * rings)
  result.setLen(0)

  for j in 0 .. segments:
    let beta = (j / segments).float32

    for i in 0 .. < rings:
      let alpha = (i / (rings-1)).float32

      result.add( vec2f(beta, alpha) )


proc uvSphereIndices*(segments, rings: int): seq[int16] =
  result.newSeq((segments+1) * rings * 6)
  result.setLen(0)

  for segment in 0 ..< segments:
    for ring in 0 ..< rings - 1:
      let
        i1 = int16( ring +     segment * rings )
        i2 = int16( ring + 1 + segment * rings )
        i3 = int16( ring +     segment * rings + rings )
        i4 = int16( ring + 1 + segment * rings + rings )
      result.add([i1,i2,i3,i3,i2,i4])

### cylinder ###

proc cylinderVertices*(segments: int, topRadius: float32 = 1): seq[Vec4f] =
  result.newSeq((segments+1) * 4 + 2)

  result[2 * (segments+1)] = vec4f(0,0,-1,1)
  result[3 * (segments+1) + 1] = vec4f(0,0, 1,1)

  for j in 0 .. segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32
      top =    vec4f(vec2f(x,y) * topRadius,  1, 1)
      bottom = vec4f(x, y, -1, 1)

    result[2*j+0] = bottom
    result[2*j+1] = top
    result[2*(segments+1) + 1 + j] = bottom
    result[3*(segments+1) + 2 + j] = top

proc cylinderNormals*(segments: int, topRadius: float32 = 1): seq[Vec4f] =
  result.newSeq((segments+1) * 4 + 2)

  result[2 * (segments+1)] = vec4f(0,0,-1, 0)
  result[3 * (segments+1) + 1] = vec4f(0,0, 1, 0)

  let n = vec2f(2,1-topRadius).normalize

  for j in 0 .. segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32

    result[2*j+0] = vec4f( vec2(x, y) * n.x, n.y, 0)
    result[2*j+1] = vec4f( vec2(x, y) * n.x, n.y, 0)
    result[2*(segments+1) + 1 + j] = vec4f(0,0,-1, 0)
    result[3*(segments+1) + 2 + j] = vec4f(0,0, 1, 0)

proc cylinderTexCoords*(segments: int): seq[Vec2f] =
  result.newSeq((segments+1) * 4 + 2)

  result[2 * (segments+1)] = vec2f(0.5f)
  result[3 * (segments+1) + 1] = vec2f(0.5f)

  for j in 0 .. segments:
    let
      u = (j / segments).float32
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32 * 0.5f + 0.5f
      y = sin(beta).float32 * 0.5f + 0.5f

    result[2*j+0] = vec2f(u, 0)
    result[2*j+1] = vec2f(u, 1)
    result[2*(segments+1) + 1 + j] = vec2f(x,y)
    result[3*(segments+1) + 2 + j] = vec2f(x,y)

proc cylinderIndices*(segments: int): seq[int16] =
  result.newSeq(0)

  for i in 0 ..< segments:
    let
      i1 = int16( i * 2 + 0 )
      i2 = int16( i * 2 + 1 )
      i3 = int16( i * 2 + 2 )
      i4 = int16( i * 2 + 3 )

    result.add([i1,i3,i2,i2,i3,i4])

  var base = int16(2 * (segments+1))

  for i in 0 ..< int16(segments):
    let ii = i.int16
    result.add( [ base , base + ii + 2, base + ii + 1 ] )

  base = int16(3 * (segments+1) + 1)

  for i in 0 ..< segments:
    let ii = i.int16
    result.add( [ base, base + ii + 1, base + ii + 2 ] )

### cone ###

proc coneVertices*(segments: int): seq[Vec4f] =
  result.newSeq((segments+1) * 4 + 2)

  result[2 * (segments+1)] = vec4f(0,0,-1,1)
  result[3 * (segments+1) + 1] = vec4f(0,0, 1,1)

  for j in 0 .. segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32
      top =    vec4f(0, 0,  1, 1)
      bottom = vec4f(x, y, -1, 1)

    result[2*j+0] = bottom
    result[2*j+1] = top
    result[2*(segments+1) + 1 + j] = bottom
    result[3*(segments+1) + 2 + j] = top

proc coneNormals*(segments: int): seq[Vec4f] =
  result.newSeq((segments+1) * 4 + 2)

  result[2 * (segments+1)] = vec4f(0,0,-1, 0)
  result[3 * (segments+1) + 1] = vec4f(0,0, 1, 0)

  let n = vec2f(2).normalize

  for j in 0 .. segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32

    result[2*j+0] = vec4f( vec2(x, y) * n.x, n.y, 0)
    result[2*j+1] = vec4f( vec2(x, y) * n.x, n.y, 0)
    result[2*(segments+1) + 1 + j] = vec4f(0,0,-1, 0)
    result[3*(segments+1) + 2 + j] = vec4f(0,0, 1, 0)

proc coneTexCoords*(segments: int): seq[Vec2f] =
  result.newSeq((segments+1) * 4 + 2)

  result[2 * (segments+1)] = vec2f(0.5f)
  result[3 * (segments+1) + 1] = vec2f(0.5f)

  for j in 0 .. segments:
    let
      u = (j / segments).float32
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32 * 0.5f + 0.5f
      y = sin(beta).float32 * 0.5f + 0.5f

    result[2*j+0] = vec2f(u, 0)
    result[2*j+1] = vec2f(u, 1)
    result[2*(segments+1) + 1 + j] = vec2f(x,y)
    result[3*(segments+1) + 2 + j] = vec2f(x,y)

proc coneIndices*(segments: int): seq[int16] =
  result.newSeq(0)

  for i in 0 ..< segments:
    let
      i1 = int16( i * 2 + 0 )
      i2 = int16( i * 2 + 1 )
      i3 = int16( i * 2 + 2 )

    result.add([i1,i3,i2])

  var base = int16(2 * (segments+1))

  for i in 0 ..< int16(segments):
    let ii = i.int16
    result.add( [ base , base + ii + 2, base + ii + 1 ] )

  base = int16(3 * (segments+1) + 1)

  for i in 0 ..< segments:
    let ii = i.int16
    result.add( [ base, base + ii + 1, base + ii + 2 ] )

### box ###

const
  boxVertices* = [
    vec4f(+1, +1, -1, 1), vec4f(-1, +1, -1, 1), vec4f(-1, +1, +1, 1),
    vec4f(+1, +1, +1, 1), vec4f(+1, +1, -1, 1), vec4f(-1, +1, +1, 1),
    vec4f(+1, -1, +1, 1), vec4f(-1, -1, +1, 1), vec4f(-1, -1, -1, 1),
    vec4f(+1, -1, -1, 1), vec4f(+1, -1, +1, 1), vec4f(-1, -1, -1, 1),
    vec4f(+1, +1, +1, 1), vec4f(-1, +1, +1, 1), vec4f(-1, -1, +1, 1),
    vec4f(+1, -1, +1, 1), vec4f(+1, +1, +1, 1), vec4f(-1, -1, +1, 1),
    vec4f(+1, -1, -1, 1), vec4f(-1, -1, -1, 1), vec4f(-1, +1, -1, 1),
    vec4f(+1, +1, -1, 1), vec4f(+1, -1, -1, 1), vec4f(-1, +1, -1, 1),
    vec4f(-1, +1, +1, 1), vec4f(-1, +1, -1, 1), vec4f(-1, -1, -1, 1),
    vec4f(-1, -1, +1, 1), vec4f(-1, +1, +1, 1), vec4f(-1, -1, -1, 1),
    vec4f(+1, +1, -1, 1), vec4f(+1, +1, +1, 1), vec4f(+1, -1, +1, 1),
    vec4f(+1, -1, -1, 1), vec4f(+1, +1, -1, 1), vec4f(+1, -1, +1, 1)
  ]

  boxNormals* = [
    vec4f( 0, +1,  0, 0), vec4f( 0, +1,  0, 0), vec4f( 0, +1,  0, 0),
    vec4f( 0, +1,  0, 0), vec4f( 0, +1,  0, 0), vec4f( 0, +1,  0, 0),
    vec4f( 0, -1,  0, 0), vec4f( 0, -1,  0, 0), vec4f( 0, -1,  0, 0),
    vec4f( 0, -1,  0, 0), vec4f( 0, -1,  0, 0), vec4f( 0, -1,  0, 0),
    vec4f( 0,  0, +1, 0), vec4f( 0,  0, +1, 0), vec4f( 0,  0, +1, 0),
    vec4f( 0,  0, +1, 0), vec4f( 0,  0, +1, 0), vec4f( 0,  0, +1, 0),
    vec4f( 0,  0, -1, 0), vec4f( 0,  0, -1, 0), vec4f( 0,  0, -1, 0),
    vec4f( 0,  0, -1, 0), vec4f( 0,  0, -1, 0), vec4f( 0,  0, -1, 0),
    vec4f(-1,  0,  0, 0), vec4f(-1,  0,  0, 0), vec4f(-1,  0,  0, 0),
    vec4f(-1,  0,  0, 0), vec4f(-1,  0,  0, 0), vec4f(-1,  0,  0, 0),
    vec4f(+1,  0,  0, 0), vec4f(+1,  0,  0, 0), vec4f(+1,  0,  0, 0),
    vec4f(+1,  0,  0, 0), vec4f(+1,  0,  0, 0), vec4f(+1,  0,  0, 0)
  ]

  boxColors* = [
    vec4f(0.0, 1.0, 0.0, 1), vec4f(0.0, 1.0, 0.0, 1), vec4f(0.0, 1.0, 0.0, 1),
    vec4f(0.0, 1.0, 0.0, 1), vec4f(0.0, 1.0, 0.0, 1), vec4f(0.0, 1.0, 0.0, 1),
    vec4f(1.0, 0.5, 0.0, 1), vec4f(1.0, 0.5, 0.0, 1), vec4f(1.0, 0.5, 0.0, 1),
    vec4f(1.0, 0.5, 0.0, 1), vec4f(1.0, 0.5, 0.0, 1), vec4f(1.0, 0.5, 0.0, 1),
    vec4f(1.0, 0.0, 0.0, 1), vec4f(1.0, 0.0, 0.0, 1), vec4f(1.0, 0.0, 0.0, 1),
    vec4f(1.0, 0.0, 0.0, 1), vec4f(1.0, 0.0, 0.0, 1), vec4f(1.0, 0.0, 0.0, 1),
    vec4f(1.0, 1.0, 0.0, 1), vec4f(1.0, 1.0, 0.0, 1), vec4f(1.0, 1.0, 0.0, 1),
    vec4f(1.0, 1.0, 0.0, 1), vec4f(1.0, 1.0, 0.0, 1), vec4f(1.0, 1.0, 0.0, 1),
    vec4f(0.0, 0.0, 1.0, 1), vec4f(0.0, 0.0, 1.0, 1), vec4f(0.0, 0.0, 1.0, 1),
    vec4f(0.0, 0.0, 1.0, 1), vec4f(0.0, 0.0, 1.0, 1), vec4f(0.0, 0.0, 1.0, 1),
    vec4f(1.0, 0.0, 1.0, 1), vec4f(1.0, 0.0, 1.0, 1), vec4f(1.0, 0.0, 1.0, 1),
    vec4f(1.0, 0.0, 1.0, 1), vec4f(1.0, 0.0, 1.0, 1), vec4f(1.0, 0.0, 1.0, 1)
  ]

  boxTexCoords* = [
    vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),

    vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),

    vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
    vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),

    vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
    vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),

    vec2f(1, 1), vec2f(1, 0), vec2f(0, 0),
    vec2f(0, 1), vec2f(1, 1), vec2f(0, 0),

    vec2f(1, 0), vec2f(1, 1), vec2f(0, 1),
    vec2f(0, 0), vec2f(1, 0), vec2f(0, 1)
  ]


proc genTetraederVertices(): array[12, Vec4f] {.compileTime.} =
  let verts = [ vec4f(-1,-1,-1,1), vec4f(1,1,-1,1), vec4f(1,-1,1,1), vec4f(-1,1,1,1) ]
  let vertIndices = [ 0, 3, 1,  0,2,3,  0,1,2,  1,3,2]

  for i, pos in result.mpairs:
    pos = verts[vertIndices[i]]

proc genTetraederNormals(): array[12, Vec4f] {.compileTime.} =
  let s = sqrt(3.0'f32)
  let normals    = [ vec4f(-s, s,-s,0), vec4f(-s,-s, s,0), vec4f( s,-s,-s,0), vec4f( s, s, s,0)]
  let normIndices = [ 0, 0, 0,  1,1,1,  2,2,2,  3,3,3]

  for i, n in result.mpairs:
    n = normals[normIndices[i]]

proc getTetraederTexCoords(): array[12, Vec2f] {.compileTime.} =
  let texCoords   = [ vec2f(0,0), vec2f(1,1), vec2f(1,0), vec2f(0,1) ]
  let vertIndices = [ 0, 3, 1,  0,2,3,  0,1,2,  1,3,2]

  for i, texCoord in result.mpairs:
    texCoord = texCoords[vertIndices[i]]

const
  tetraederVertices*  = genTetraederVertices()
  tetraederNormals*   = genTetraederNormals()
  tetraederTexCoords* = getTetraederTexCoords()


proc gridVerticesXMajor*(size: Vec2i): seq[Vec4f] =
  result = newSeqOfCap[Vec4f](size.x * size.y)

  for y in 0 ..< size.y:
    for x in 0 ..< size.x:
      result.add vec4f(float32(x), float32(y), 0, 1)

proc gridVerticesYMajor*(size: Vec2i): seq[Vec4f] =
  result = newSeqOfCap[Vec4f](size.x * size.y)

  for x in 0 ..< size.x:
    for y in 0 ..< size.y:
      result.add vec4f(float32(x), float32(y), 0, 1)

proc gridIndicesTriangles*(size: Vec2i) : seq[int32] =
  result = newSeqOfCap[int32](size.x * size.y * 6)

  for y in 0 ..< (size.y - 1):
    for x in 0 ..< (size.x - 1):
      let
        a = int32(x     + size.x * y      )
        b = int32(x + 1 + size.x * y      )
        c = int32(x     + size.x * (y + 1))
        d = int32(x + 1 + size.x * (y + 1))

      result.add([c,a,d,d,a,b])
      # this would be the other diagonal
      # result.add([a,b,c,c,b,d])

proc gridIndicesTriangleStrip*(size: Vec2i): seq[int32] =
  result.newSeq(0)

  for y in 0 ..< (size.y - 1):
    for x in 0 ..< size.x:
      let
        a = int32(x     + size.x * (y + 1) )
        b = int32(x     + size.x *  y      )

      if y != 0 and x == 0:
        result.add([a,a,b])
      elif y != size.y - 2 and x == (size.x - 1):
        result.add([a,b,b])
      else:
        result.add([a,b])

proc gridIndicesQuads*(size: Vec2i): seq[int32] =
  result = newSeqOfCap[int32]((size.x-1) * (size.y-1) * 4)
  for y in 0 ..< (size.y - 1):
    for x in 0 ..< (size.x - 1):
      result.add int32(x     + size.x * y         )
      result.add int32(x + 1 + size.x * y         )
      result.add int32(x     + size.x * y + size.x)
      result.add int32(x + 1 + size.x * y + size.x)

proc torusVertices*(majorSegments,minorSegments: int; majorRadius, minorRadius: float32): seq[Vec4f] =
  result = newSeqOfCap[Vec4f]((minorSegments+1) * (majorSegments+1))
  let a = minorRadius
  let b = majorRadius
  for i in 0 .. majorSegments:
    let u = float32(i / majorSegments) * 2 * Pi
    for j in 0 .. minorSegments:
      let v = Pi - float32(j / minorSegments) * 2 * Pi
      let c = (b + a * cos(v))
      let x = c * cos(u)
      let y = c * sin(u)
      let z = a * sin(v)
      result.add vec4f(x,y,z,1)

proc torusNormals*(majorSegments,minorSegments: int): seq[Vec4f] =
  result = newSeqOfCap[Vec4f]((minorSegments+1) * (majorSegments+1))
  for i in 0 .. majorSegments:
    let u = float32(i / majorSegments) * 2 * Pi
    for j in 0 .. minorSegments:
      let v = Pi - float32(j / minorSegments) * 2 * Pi

      let x = cos(v) * cos(u)
      let y = cos(v) * sin(u)
      let z = sin(v)

      result.add vec4f(x,y,z,0)

proc torusTexCoords*(majorSegments,minorSegments: int): seq[Vec2f] =
  result = newSeqOfCap[Vec2f]((minorSegments+1) * (majorSegments+1))
  for i in 0 .. majorSegments:
    for j in 0 .. minorSegments:
      let u = float32(i / majorSegments)
      let v = float32(j / minorSegments)
      result.add vec2f(u,v)

proc torusIndicesTriangles*(majorSegments,minorSegments: int) : seq[int32]    =
  gridIndicesTriangles(vec2i(minorSegments.int32+1, majorSegments.int32+1))

proc torusIndicesTriangleStrip*(majorSegments,minorSegments: int): seq[int32] =
  gridIndicesTriangleStrip(vec2i(minorSegments.int32+1, majorSegments.int32+1))

proc torusIndicesQuads*(majorSegments,minorSegments: int): seq[int32]         =
  gridIndicesQuads(vec2i(minorSegments.int32+1, majorSegments.int32+1))

proc circleVertices*(segments: int): seq[Vec4f] =
  result = newSeqOfCap[Vec4f](segments + 2)
  result.add vec4f(0,0,0,1)

  for i in 0 .. segments:
    let u = float32(i / segments) * 2 * Pi
    result.add vec4f(cos(u), sin(u), 0, 1)


const
  xxx = (1.0f + sqrt(5.0f)) / 2.0f

  icosphereVertices*: array[12, Vec4f] = [
    vec4f(-1,  xxx,  0, 1),
    vec4f( 1,  xxx,  0, 1),
    vec4f(-1, -xxx,  0, 1),
    vec4f( 1, -xxx,  0, 1),

    vec4f( 0, -1,  xxx, 1),
    vec4f( 0,  1,  xxx, 1),
    vec4f( 0, -1, -xxx, 1),
    vec4f( 0,  1, -xxx, 1),

    vec4f( xxx,  0, -1, 1),
    vec4f( xxx,  0,  1, 1),
    vec4f(-xxx,  0, -1, 1),
    vec4f(-xxx,  0,  1, 1)
  ]

  icosphereIndicesTriangles*: array[20 * 3, int32] = [
  # 5 faces around point 0
    0'i32, 11, 5,
    0'i32, 5, 1,
    0'i32, 1, 7,
    0'i32, 7, 10,
    0'i32, 10, 11,

  # 5 adjacent faces
    1'i32, 5, 9,
    5'i32, 11, 4,
    11'i32, 10, 2,
    10'i32, 7, 6,
    7'i32, 1, 8,

  # 5 faces around point 3
    3'i32, 9, 4,
    3'i32, 4, 2,
    3'i32, 2, 6,
    3'i32, 6, 8,
    3'i32, 8, 9,

  # 5 adjacent faces
    4'i32, 9, 5,
    2'i32, 4, 11,
    6'i32, 2, 10,
    8'i32, 6, 7,
    9'i32, 8, 1
  ]
