# included from fancygl.nim

proc uvSphereVertices*(segments, rings: int): seq[Vec3f] =
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

      result.add( vec3f(x * r, y * r, h) )


proc uvSphereNormals*(segments, rings: int): seq[Vec3f] =
  uvSphereVertices(segments, rings)

proc uvSphereTexCoords*(segments, rings: int): seq[Vec2f] =
  result.newSeq((segments+1) * rings)
  result.setLen(0)

  for j in 0 .. segments:
    let beta = (j / segments).float32

    for i in 0 .. < rings:
      let alpha = (i / (rings-1)).float32

      result.add( vec2f(alpha,beta) )


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

proc cylinderVertices*(segments: int, topRadius: float32 = 1): seq[Vec3f] =
  result.newSeq(segments * 4 + 2)

  result[2 * segments] = vec3f(0,0,-1)
  result[3 * segments + 1] = vec3f(0,0, 1)

  for j in 0 .. < segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32
      top =    vec3f(vec2f(x,y) * topRadius,  1)
      bottom = vec3f(x, y, -1)

    result[2*j+0] = bottom
    result[2*j+1] = top
    result[2*segments + 1 + j] = bottom
    result[3*segments + 2 + j] = top

proc cylinderNormals*(segments: int, topRadius: float32 = 1): seq[Vec3f] =
  result.newSeq(segments * 4 + 2)

  result[2 * segments] = vec3f(0,0,-1)
  result[3 * segments + 1] = vec3f(0,0, 1)

  let n = vec2f(2,1-topRadius).normalize

  echo n

  for j in 0 .. < segments:
    let
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32
      y = sin(beta).float32

    result[2*j+0] = vec3f( vec2(x, y) * n.x, n.y)
    result[2*j+1] = vec3f( vec2(x, y) * n.x, n.y)
    result[2*segments + 1 + j] = vec3f(0,0,-1)
    result[3*segments + 2 + j] = vec3f(0,0, 1)

proc cylinderTexCoords*(segments: int): seq[Vec2f] =
  result.newSeq(segments * 4 + 2)

  result[2 * segments] = vec2f(0.5f)
  result[3 * segments + 1] = vec2f(0.5f)

  for j in 0 .. < segments:
    let
      u = (j / segments).float32
      beta = (j / segments) * 2 * PI
      x = cos(beta).float32 * 0.5f + 0.5f
      y = sin(beta).float32 * 0.5f + 0.5f

    result[2*j+0] = vec2f(u, 0)
    result[2*j+1] = vec2f(u, 1)
    result[2*segments + 1 + j] = vec2f(x,y)
    result[3*segments + 2 + j] = vec2f(x,y)

proc cylinderIndices*(segments: int): seq[int16] =
  result.newSeq(0)

  for i in 0 ..< segments - 1:
    let
      i1 = int16( i * 2 + 0 )
      i2 = int16( i * 2 + 1 )
      i3 = int16( i * 2 + 2 )
      i4 = int16( i * 2 + 3 )

    result.add([i1,i3,i2,i2,i3,i4])

  let
    i1 = int16( segments * 2 - 2 )
    i2 = int16( segments * 2 - 1 )
    i3 = int16( 0 )
    i4 = int16( 1 )

  result.add([i1,i3,i2,i2,i3,i4])


  var base = int16(2 * segments)

  for i in 0 ..< int16(segments - 1):
    let ii = i.int16
    result.add( [ base , base + ii + 2, base + ii + 1 ] )

  result.add( [ base , base + 1, base + segments.int16 ] )

  base = int16(3 * segments + 1)

  for i in 0 ..< segments - 1:
    let ii = i.int16
    result.add( [ base, base + ii + 1, base + ii + 2 ] )

  result.add( [ base , base + segments.int16, base + 1 ] )


### box ###

const
  boxVertices* = @[
    vec3f(+1, +1, -1), vec3f(-1, +1, -1), vec3f(-1, +1, +1),
    vec3f(+1, +1, +1), vec3f(+1, +1, -1), vec3f(-1, +1, +1),
    vec3f(+1, -1, +1), vec3f(-1, -1, +1), vec3f(-1, -1, -1),
    vec3f(+1, -1, -1), vec3f(+1, -1, +1), vec3f(-1, -1, -1),
    vec3f(+1, +1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, +1),
    vec3f(+1, -1, +1), vec3f(+1, +1, +1), vec3f(-1, -1, +1),
    vec3f(+1, -1, -1), vec3f(-1, -1, -1), vec3f(-1, +1, -1),
    vec3f(+1, +1, -1), vec3f(+1, -1, -1), vec3f(-1, +1, -1),
    vec3f(-1, +1, +1), vec3f(-1, +1, -1), vec3f(-1, -1, -1),
    vec3f(-1, -1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, -1),
    vec3f(+1, +1, -1), vec3f(+1, +1, +1), vec3f(+1, -1, +1),
    vec3f(+1, -1, -1), vec3f(+1, +1, -1), vec3f(+1, -1, +1)
  ]

  boxNormals* = @[
    vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
    vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
    vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
    vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
    vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
    vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
    vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
    vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
    vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
    vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
    vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0),
    vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0)
  ]

  boxColors* = @[
    vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
    vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
    vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
    vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
    vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
    vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
    vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
    vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
    vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
    vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
    vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0),
    vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0)
  ]

  boxTexCoords* = @[
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

  
