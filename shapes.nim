# included from fancygl.nim

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

  let
    i1 = int16( (segments+1) * 2 - 2 )
    i2 = int16( (segments+1) * 2 - 1 )
    i3 = int16( 0 )
    i4 = int16( 1 )

  result.add([i1,i3,i2,i2,i3,i4])


  var base = int16(2 * (segments+1))

  for i in 0 ..< int16(segments):
    let ii = i.int16
    result.add( [ base , base + ii + 2, base + ii + 1 ] )

  result.add( [ base , base + 1, base + segments.int16 ] )

  base = int16(3 * (segments+1) + 1)

  for i in 0 ..< segments:
    let ii = i.int16
    result.add( [ base, base + ii + 1, base + ii + 2 ] )

  result.add( [ base , base + segments.int16, base + 1 ] )


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

  

  
proc genTetraederVertices(): array[12, Vec4f] = 
  let verts = [ vec4f(-1,-1,-1,1), vec4f(1,1,-1,1), vec4f(1,-1,1,1), vec4f(-1,1,1,1) ]
  let vertIndices = [ 0, 3, 1,  0,2,3,  0,1,2,  1,3,2]

  for i, pos in result.mpairs:
    pos = verts[vertIndices[i]]
  
proc genTetraederNormals(): array[12, Vec4f] =
  let s = sqrt(3.0'f32)
  let normals    = [ vec4f(-s, s,-s,0), vec4f(-s,-s, s,0), vec4f( s,-s,-s,0), vec4f( s, s, s,0)]
  let normIndices = [ 0, 0, 0,  1,1,1,  2,2,2,  3,3,3]

  for i, n in result.mpairs:
    n = normals[normIndices[i]]

proc getTetraederTexCoords(): array[12, Vec2f] =
  let texCoords   = [ vec2f(0,0), vec2f(1,1), vec2f(1,0), vec2f(0,1) ]
  let vertIndices = [ 0, 3, 1,  0,2,3,  0,1,2,  1,3,2]
  
  for i, texCoord in result.mpairs:
    texCoord = texCoords[vertIndices[i]]

const
  tetraederVertices*  = genTetraederVertices()
  tetraederNormals*   = genTetraederNormals()
  tetraederTexCoords* = getTetraederTexCoords()
