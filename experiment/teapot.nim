#[ Rim, body, lid, and bottom data must be reflected in x and
   y; handle and spout data across the y axis only.  ]#

import glm

type
  Grid[T] = object
    size: Vec2i
    dataseq: seq[T]


proc createGrid[T](size:Vec2i): Grid[T] =
  result.size = size
  result.dataseq.setLen size.x * size.y

proc `[]`[T](grid: Grid[T]; pos: Vec2i): T {. inline .} =
  grid.dataseq[pos.x + pos.y * grid.size.x]

proc `[]=`[T](grid: var Grid[T]; pos: Vec2i; value: T): void {. inline .} =
  grid.dataseq[pos.x + pos.y * grid.size.x] = value


let patchdata*: seq[array[16,int32]] = @[
    #[ rim ]#
  [102'i32, 103, 104, 105, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    #[ body ]#
  [12'i32, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27],
  [24'i32, 25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40],
    #[ lid ]#
  [96'i32, 96, 96, 96, 97, 98, 99, 100, 101, 101, 101, 101, 0, 1, 2, 3,],
  [0'i32, 1, 2, 3, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117],
    #[ bottom ]#
  [118'i32, 118, 118, 118, 124, 122, 119, 121, 123, 126, 125, 120, 40, 39, 38, 37],
    #[ handle ]#
  [41'i32, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56],
  [53'i32, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 28, 65, 66, 67],
    #[ spout ]#
  [68'i32, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83],
  [80'i32, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95]
]

#[ *INDENT-OFF* ]#

type
  MyVertexType = tuple
    position_os: Vec4f
    normal_os: Vec4f
    texCoord: Vec2f

var cpdata* = [
    vec3f( 0.2000f,  0.0000f,  2.70000f),
    vec3f( 0.2000f, -0.1120f,  2.70000f),
    vec3f( 0.1120f, -0.2000f,  2.70000f),
    vec3f( 0.0000f, -0.2000f,  2.70000f),
    vec3f( 1.3375f,  0.0000f,  2.53125f),
    vec3f( 1.3375f, -0.7490f,  2.53125f),
    vec3f( 0.7490f, -1.3375f,  2.53125f),
    vec3f( 0.0000f, -1.3375f,  2.53125f),
    vec3f( 1.4375f,  0.0000f,  2.53125f),
    vec3f( 1.4375f, -0.8050f,  2.53125f),
    vec3f( 0.8050f, -1.4375f,  2.53125f),
    vec3f( 0.0000f, -1.4375f,  2.53125f),
    vec3f( 1.5000f,  0.0000f,  2.40000f),
    vec3f( 1.5000f, -0.8400f,  2.40000f),
    vec3f( 0.8400f, -1.5000f,  2.40000f),
    vec3f( 0.0000f, -1.5000f,  2.40000f),
    vec3f( 1.7500f,  0.0000f,  1.87500f),
    vec3f( 1.7500f, -0.9800f,  1.87500f),
    vec3f( 0.9800f, -1.7500f,  1.87500f),
    vec3f( 0.0000f, -1.7500f,  1.87500f),
    vec3f( 2.0000f,  0.0000f,  1.35000f),
    vec3f( 2.0000f, -1.1200f,  1.35000f),
    vec3f( 1.1200f, -2.0000f,  1.35000f),
    vec3f( 0.0000f, -2.0000f,  1.35000f),
    vec3f( 2.0000f,  0.0000f,  0.90000f),
    vec3f( 2.0000f, -1.1200f,  0.90000f),
    vec3f( 1.1200f, -2.0000f,  0.90000f),
    vec3f( 0.0000f, -2.0000f,  0.90000f),
    vec3f(-2.0000f,  0.0000f,  0.90000f),
    vec3f( 2.0000f,  0.0000f,  0.45000f),
    vec3f( 2.0000f, -1.1200f,  0.45000f),
    vec3f( 1.1200f, -2.0000f,  0.45000f),
    vec3f( 0.0000f, -2.0000f,  0.45000f),
    vec3f( 1.5000f,  0.0000f,  0.22500f),
    vec3f( 1.5000f, -0.8400f,  0.22500f),
    vec3f( 0.8400f, -1.5000f,  0.22500f),
    vec3f( 0.0000f, -1.5000f,  0.22500f),
    vec3f( 1.5000f,  0.0000f,  0.15000f),
    vec3f( 1.5000f, -0.8400f,  0.15000f),
    vec3f( 0.8400f, -1.5000f,  0.15000f),
    vec3f( 0.0000f, -1.5000f,  0.15000f),
    vec3f(-1.6000f,  0.0000f,  2.02500f),
    vec3f(-1.6000f, -0.3000f,  2.02500f),
    vec3f(-1.5000f, -0.3000f,  2.25000f),
    vec3f(-1.5000f,  0.0000f,  2.25000f),
    vec3f(-2.3000f,  0.0000f,  2.02500f),
    vec3f(-2.3000f, -0.3000f,  2.02500f),
    vec3f(-2.5000f, -0.3000f,  2.25000f),
    vec3f(-2.5000f,  0.0000f,  2.25000f),
    vec3f(-2.7000f,  0.0000f,  2.02500f),
    vec3f(-2.7000f, -0.3000f,  2.02500f),
    vec3f(-3.0000f, -0.3000f,  2.25000f),
    vec3f(-3.0000f,  0.0000f,  2.25000f),
    vec3f(-2.7000f,  0.0000f,  1.80000f),
    vec3f(-2.7000f, -0.3000f,  1.80000f),
    vec3f(-3.0000f, -0.3000f,  1.80000f),
    vec3f(-3.0000f,  0.0000f,  1.80000f),
    vec3f(-2.7000f,  0.0000f,  1.57500f),
    vec3f(-2.7000f, -0.3000f,  1.57500f),
    vec3f(-3.0000f, -0.3000f,  1.35000f),
    vec3f(-3.0000f,  0.0000f,  1.35000f),
    vec3f(-2.5000f,  0.0000f,  1.12500f),
    vec3f(-2.5000f, -0.3000f,  1.12500f),
    vec3f(-2.6500f, -0.3000f,  0.93750f),
    vec3f(-2.6500f,  0.0000f,  0.93750f),
    vec3f(-2.0000f, -0.3000f,  0.90000f),
    vec3f(-1.9000f, -0.3000f,  0.60000f),
    vec3f(-1.9000f,  0.0000f,  0.60000f),
    vec3f( 1.7000f,  0.0000f,  1.42500f),
    vec3f( 1.7000f, -0.6600f,  1.42500f),
    vec3f( 1.7000f, -0.6600f,  0.60000f),
    vec3f( 1.7000f,  0.0000f,  0.60000f),
    vec3f( 2.6000f,  0.0000f,  1.42500f),
    vec3f( 2.6000f, -0.6600f,  1.42500f),
    vec3f( 3.1000f, -0.6600f,  0.82500f),
    vec3f( 3.1000f,  0.0000f,  0.82500f),
    vec3f( 2.3000f,  0.0000f,  2.10000f),
    vec3f( 2.3000f, -0.2500f,  2.10000f),
    vec3f( 2.4000f, -0.2500f,  2.02500f),
    vec3f( 2.4000f,  0.0000f,  2.02500f),
    vec3f( 2.7000f,  0.0000f,  2.40000f),
    vec3f( 2.7000f, -0.2500f,  2.40000f),
    vec3f( 3.3000f, -0.2500f,  2.40000f),
    vec3f( 3.3000f,  0.0000f,  2.40000f),
    vec3f( 2.8000f,  0.0000f,  2.47500f),
    vec3f( 2.8000f, -0.2500f,  2.47500f),
    vec3f( 3.5250f, -0.2500f,  2.49375f),
    vec3f( 3.5250f,  0.0000f,  2.49375f),
    vec3f( 2.9000f,  0.0000f,  2.47500f),
    vec3f( 2.9000f, -0.1500f,  2.47500f),
    vec3f( 3.4500f, -0.1500f,  2.51250f),
    vec3f( 3.4500f,  0.0000f,  2.51250f),
    vec3f( 2.8000f,  0.0000f,  2.40000f),
    vec3f( 2.8000f, -0.1500f,  2.40000f),
    vec3f( 3.2000f, -0.1500f,  2.40000f),
    vec3f( 3.2000f,  0.0000f,  2.40000f),
    vec3f( 0.0000f,  0.0000f,  3.15000f),
    vec3f( 0.8000f,  0.0000f,  3.15000f),
    vec3f( 0.8000f, -0.4500f,  3.15000f),
    vec3f( 0.4500f, -0.8000f,  3.15000f),
    vec3f( 0.0000f, -0.8000f,  3.15000f),
    vec3f( 0.0000f,  0.0000f,  2.85000f),
    vec3f( 1.4000f,  0.0000f,  2.40000f),
    vec3f( 1.4000f, -0.7840f,  2.40000f),
    vec3f( 0.7840f, -1.4000f,  2.40000f),
    vec3f( 0.0000f, -1.4000f,  2.40000f),
    vec3f( 0.4000f,  0.0000f,  2.55000f),
    vec3f( 0.4000f, -0.2240f,  2.55000f),
    vec3f( 0.2240f, -0.4000f,  2.55000f),
    vec3f( 0.0000f, -0.4000f,  2.55000f),
    vec3f( 1.3000f,  0.0000f,  2.55000f),
    vec3f( 1.3000f, -0.7280f,  2.55000f),
    vec3f( 0.7280f, -1.3000f,  2.55000f),
    vec3f( 0.0000f, -1.3000f,  2.55000f),
    vec3f( 1.3000f,  0.0000f,  2.40000f),
    vec3f( 1.3000f, -0.7280f,  2.40000f),
    vec3f( 0.7280f, -1.3000f,  2.40000f),
    vec3f( 0.0000f, -1.3000f,  2.40000f),
    vec3f( 0.0000f,  0.0000f,  0.00000f),
    vec3f( 1.4250f, -0.7980f,  0.00000f),
    vec3f( 1.5000f,  0.0000f,  0.07500f),
    vec3f( 1.4250f,  0.0000f,  0.00000f),
    vec3f( 0.7980f, -1.4250f,  0.00000f),
    vec3f( 0.0000f, -1.5000f,  0.07500f),
    vec3f( 0.0000f, -1.4250f,  0.00000f),
    vec3f( 1.5000f, -0.8400f,  0.07500f),
    vec3f( 0.8400f, -1.5000f,  0.07500f),
]

let tex: array[2, array[2, array[2, float32]]] = [
  [ [0.0f, 0.0f],
    [1.0f, 0.0f]],
  [ [0.0f, 1.0f],
    [1.0f, 1.0f]]
]

#[ *INDENT-ON* ]#

static:
  var buffer: seq[seq[int]]
  buffer.add @[1]

  var current: seq[int]
  for i in 0 ..< 64:
    current.add 1
    for i in 1 ..< buffer[^1].len:
      current.add buffer[^1][i-1] + buffer[^1][i]
    current.add 1
    buffer.add current
    current.setLen(0)

const binomBuffer = buffer

proc binomial*(n,k: int): int =
  binomBuffer[n][k]

proc binomial*(n,k: float32): float32 =
  float32(binomBuffer[int(n)][int(k)])


when isMainModule:
  doAssert binomial(10,0) == 1
  doAssert binomial(10,10) == 1
  doAssert binomial(10,6) == 210
  doAssert binomial(49,6) == 13983816


template kron_delta(a,b: float32): float32 =
  if a == b: 1.0f else: 0.0f


proc bernsteinPoly(i,n, u: float32): float32 =
  if i < 0 or n < i:
    return 0.0
  else:
    return binomial(n,i) * pow(u, i) * pow(1-u, n-i)

converter tofloat32(arg: int): float32 = float32(arg)



proc bernsteinPoly(i,n: float32, powU, powOneMinusU: openarray[float32]): float32 =
  if i < 0 or n < i:
    return 0.0
  else:
    return binomial(n,i) * powU[int(i)] * powOneMinusU[int(n-i)]

proc precalcPowers(arg: var openarray[float32], value: float32): void =
  arg[0] = 1.0f
  for i in 1 ..< arg.len:
    arg[i] = arg[i-1] * value

proc teapot*(grid: int32): tuple[data: seq[MyVertexType], indices: seq[int32]] =
  ## this is not the fastest implementation of the teapot, but it is a
  ## direct port of the glutTeapot, that was originally written with
  ## the OpenGL fixed function pipeline, that is not available anymore
  ## in core OpenGL.  I implemented those features according to the
  ## OpenGL 1.5 specification, to get the exact same result as the
  ## glutTeapot.

  var data: seq[MyVertexType] = @[]
  var indices: seq[int32] = @[]

  var
    p: array[4, array[4, Vec3f]]
    q: array[4, array[4, Vec3f]]
    r: array[4, array[4, Vec3f]]
    s: array[4, array[4, Vec3f]]

  proc evalCoord[UN,VN](u,v: float32, controlPoints: array[UN, array[VN, Vec3f]]): MyVertexType =
    assert low(UN) == 0
    assert low(VN) == 0

    let texCoord = vec2f(u,v)
    var position: Vec3f
    var position_du: Vec3f
    var position_dv: Vec3f

    var weightsU: array[UN, float32]
    var weightsV: array[VN, float32]

    let n = int(high(UN))
    let m = int(high(VN))

    var powU:         array[UN, float32]
    var powOneMinusU: array[UN, float32]
    var powV:         array[VN, float32]
    var powOneMinusV: array[VN, float32]

    precalcPowers(powU, u)
    precalcPowers(powOneMinusU, 1-u)
    precalcPowers(powV, v)
    precalcPowers(powOneMinusV, 1-v)

    for i in 0 .. n:
      for j in 0 .. m:
        let cp = controlPoints[i][j]

        let i = float32(i)
        let j = float32(j)
        let n = float32(n)
        let m = float32(m)

        # bernstein
        let Bi = bernsteinPoly(i,n, powU, powOneMinusU)
        let Bj = bernsteinPoly(j,m, powV, powOneMinusV)
        position += Bi * Bj * cp

        # diff(bernstein_poly(i,n,u) * bernstein_poly(j,m,v) * cp, u);
        let a = bernstein_poly(i-1,n-1, powU, powOneMinusU)
        let b = bernstein_poly(i,n-1,powU, powOneMinusU)
        position_du += cp*(a-b)*Bj*n

        # diff(bernstein_poly(i,n,u) * bernstein_poly(j,m,v) * cp, v);
        let c = bernstein_poly(j-1,m-1,powV, powOneMinusV)
        let d = bernstein_poly(j,m-1,powV, powOneMinusV)
        position_dv += cp*Bi*(c-d)*float32(m)

    var normal = cross(position_du, position_dv)

    result = (vec4f(position,1), vec4f(normal,0), texCoord)

  proc evalMesh[UN,VN](controlPoints: array[UN, array[VN, Vec3f]], n: Vec2i, uv1, uv2: Vec2f): void =
    let
      un = n.x
      vn = n.y
      u1 = uv1.x
      u2 = uv2.x
      v1 = uv1.y
      v2 = uv2.y

      du = (u2 - u1) / float32(un)
      dv = (v2 - v1) / float32(vn)

    let baseIndex = data.len

    let gridWidth = n.x+1

    for i in 0 .. un:
      for j in 0 .. vn:
        let u = u1 + float32(i) * du
        let v = v1 + float32(j) * dv
        data.add evalCoord(u, v, controlPoints)

    # for i in 0 ..< un:
    #   # glEvalMesh2(typ, 0, grid, 0, grid)
    #   # glBegin( GL_Triangle_STRIP )

    #   for j in 0 .. vn:
    #     let idx = vec2i(int32(i),int32(j))
    #     data.add grid[idx + vec2i(0,0)]
    #     if j == 0:
    #       data.add data[^1]
    #     data.add grid[idx + vec2i(1,0)]
    #     if j == vn:
    #       data.add data[^1]

    #   #glEnd( GL_QUAD_STRIP )

    for i in 0 ..< un:
      # glEvalMesh2(typ, 0, grid, 0, grid)
      # glBegin( GL_Triangle_STRIP )

      for j in 0 .. vn:
        let idx = vec2i(int32(i),int32(j))
        indices.add int32(baseIndex + j + gridWidth * i)
        if j == 0:
          # repeat first vertex of strip
          indices.add indices[^1]
        indices.add int32(baseIndex + j + gridWidth * (i+1))
        if j == vn:
          # repeat last vertext of strip
          indices.add indices[^1]

      #glEnd( GL_QUAD_STRIP )


  for i in 0 ..< 10:
    for j in 0 ..< 4:
      for k in 0 ..< 4:
        p[j][k] = cpdata[patchdata[i][j * 4 + k]]
        q[j][k] = cpdata[patchdata[i][j * 4 + (3 - k)]]
        q[j][k].y *= -1.0f

        if i < 6:
          r[j][k] = cpdata[patchdata[i][j * 4 + (3 - k)]]
          r[j][k].x *= -1.0f
          s[j][k] = cpdata[patchdata[i][j * 4 + k]]
          s[j][k].xy *= -1.0f

    #glMap2f(GL_MAP2_TEXTURE_COORD_2, 0, 1, 2, 2, 0, 1, 2*2, 2, tex[0][0][0].addr)

    # glMapGrid2f(grid, 0.0, 1.0, grid, 0.0, 1.0)

    evalMesh(p, vec2i(grid), vec2f(0), vec2f(1))
    evalMesh(q, vec2i(grid), vec2f(0), vec2f(1))

    # only the first six surfaces are mirrored
    if i < 6:
      evalMesh(r, vec2i(grid), vec2f(0), vec2f(1))
      evalMesh(s, vec2i(grid), vec2f(0), vec2f(1))



  # some post process fixes for correct normals (specific to this mesh)
  for vertex in data.mitems:
    # I don't know why normals are flipped
    vertex.normal_os *= -1
    # normals at the top and at the bottom are undefined because
    # control points overlap
    if vertex.position_os.xy == vec2f(0):
      if vertex.position_os.z == 0:
        vertex.normal_os = vec4f(0,0,-1,0)
      else:
        vertex.normal_os = vec4f(0,0, 1,0)
    else:
      vertex.normal_os = normalize(vertex.normal_os)

  echo "teapot datalen: ", data.len
  return (data, indices)
