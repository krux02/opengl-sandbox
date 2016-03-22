
type Vec4f* = Vec4[float32]
type Vec3f* = Vec3[float32]
type Vec2f* = Vec2[float32]

proc vec4f*(x,y,z,w:float32)             : Vec4f = [  x,   y,   z,   w].Vec4f
proc vec4f*(v:Vec3f,w:float32)           : Vec4f = [v.x, v.y, v.z,   w].Vec4f
proc vec4f*(x:float32,v:Vec3f)           : Vec4f = [  x, v.x, v.y, v.z].Vec4f
proc vec4f*(a,b:Vec2f)                   : Vec4f = [a.x, a.y, b.x, b.y].Vec4f
proc vec4f*(v:Vec2f,z,w:float32)         : Vec4f = [v.x, v.y,   z,   w].Vec4f
proc vec4f*(x:float32,v:Vec2f,w:float32) : Vec4f = [  x, v.x, v.y,   w].Vec4f
proc vec4f*(x,y:float32,v:Vec2f)         : Vec4f = [  x,   y, v.x, v.y].Vec4f
proc vec4f*(x:float32)                   : Vec4f = [  x,   x,   x,   x].Vec4f

proc vec3f*(x,y,z:   float32)  : Vec3f = [  x,   y,   z].Vec3f
proc vec3f*(v:Vec2f,z:float32) : Vec3f = [v.x, v.y,   z].Vec3f
proc vec3f*(x:float32,v:Vec2f) : Vec3f = [  x, v.x, v.y].Vec3f
proc vec3f*(x:float32)         : Vec3f = [  x,   x,   x].Vec3f

proc vec2f*(x,y:float32) : Vec2f = [x,y].Vec2f
proc vec2f*(x:float32)   : Vec2f = [x,x].Vec2f

proc vec4f*(a:array[0..3, float32]) : Vec4f = [a[0], a[1], a[2], a[3]].Vec4f
proc vec3f*(a:array[0..2, float32]) : Vec3f = [a[0], a[1], a[2]].Vec3f
proc vec2f*(a:array[0..1, float32]) : Vec2f = [a[0], a[1]].Vec2f

type Vec4d* = Vec4[float64]
type Vec3d* = Vec3[float64]
type Vec2d* = Vec2[float64]

proc vec4d*(x,y,z,w:float64)             : Vec4d = [  x,   y,   z,   w].Vec4d
proc vec4d*(v:Vec3d,w:float64)           : Vec4d = [v.x, v.y, v.z,   w].Vec4d
proc vec4d*(x:float64,v:Vec3d)           : Vec4d = [  x, v.x, v.y, v.z].Vec4d
proc vec4d*(a,b:Vec2d)                   : Vec4d = [a.x, a.y, b.x, b.y].Vec4d
proc vec4d*(v:Vec2d,z,w:float64)         : Vec4d = [v.x, v.y,   z,   w].Vec4d
proc vec4d*(x:float64,v:Vec2d,w:float64) : Vec4d = [  x, v.x, v.y,   w].Vec4d
proc vec4d*(x,y:float64,v:Vec2d)         : Vec4d = [  x,   y, v.x, v.y].Vec4d
proc vec4d*(x:float64)                   : Vec4d = [  x,   x,   x,   x].Vec4d

proc vec3d*(x,y,z:   float64)  : Vec3d = [  x,   y,   z].Vec3d
proc vec3d*(v:Vec2d,z:float64) : Vec3d = [v.x, v.y,   z].Vec3d
proc vec3d*(x:float64,v:Vec2d) : Vec3d = [  x, v.x, v.y].Vec3d
proc vec3d*(x:float64)         : Vec3d = [  x,   x,   x].Vec3d

proc vec2d*(x,y:float64) : Vec2d = [x,y].Vec2d
proc vec2d*(x:float64)   : Vec2d = [x,x].Vec2d

proc vec4f*(v: Vec4d) : Vec4f = [v.x.float32,v.y.float32,v.z.float32,v.w.float32].Vec4f
proc vec3f*(v: Vec3d) : Vec3f = [v.x.float32,v.y.float32,v.z.float32].Vec3f
proc vec2f*(v: Vec2d) : Vec2f = [v.x.float32,v.y.float32].Vec2f
proc vec4d*(v: Vec4f) : Vec4d = [v.x.float64,v.y.float64,v.z.float64,v.w.float64].Vec4d
proc vec3d*(v: Vec3f) : Vec3d = [v.x.float64,v.y.float64,v.z.float64].Vec3d
proc vec2d*(v: Vec2f) : Vec2d = [v.x.float64,v.y.float64].Vec2d

type Mat4f* = Mat4x4[float32]
type Mat3f* = Mat3x3[float32]
type Mat2f* = Mat2x2[float32]
type Mat4d* = Mat4x4[float64]
type Mat3d* = Mat3x3[float64]
type Mat2d* = Mat2x2[float64]

proc floor*(v : Vec2f) : Vec2f =
  result.x = floor(v.x)
  result.y = floor(v.y)

proc floor*(v : Vec3f) : Vec3f =
  result.x = floor(v.x)
  result.y = floor(v.y)
  result.z = floor(v.z)

proc floor*(v : Vec4f) : Vec4f =
  result.x = floor(v.x)
  result.y = floor(v.y)
  result.z = floor(v.z)
  result.w = floor(v.w)

proc mat4f*(mat: Mat4d): Mat4f =
  for i in 0..<4:
   for j in 0..<4:
     result[i][j] = mat[i][j]

proc mat4d*(mat: Mat4f): Mat4d =
  for i in 0..<4:
   for j in 0..<4:
     result[i][j] = mat[i][j]

proc I4*() : Mat4d = mat4x4(
  vec4d(1, 0, 0, 0),
  vec4d(0, 1, 0, 0),
  vec4d(0, 0, 1, 0),
  vec4d(0, 0, 0, 1)
)

proc I4f*() : Mat4f = mat4x4[float32](
  vec4f(1, 0, 0, 0),
  vec4f(0, 1, 0, 0),
  vec4f(0, 0, 1, 0),
  vec4f(0, 0, 0, 1)
)

proc diag*(v : Vec2f) : Mat2f =
  result[0][0] = v[0]
  result[1][1] = v[1]

proc diag*(m : Mat2f) : Vec2f =
  result[0] = m[0][0]
  result[1] = m[1][1]

proc diag*(v : Vec3f) : Mat3f =
  result[0][0] = v[0]
  result[1][1] = v[1]
  result[2][2] = v[2]

proc diag*(m : Mat3f) : Vec3f =
  result[0] = m[0][0]
  result[1] = m[1][1]
  result[2] = m[2][2]

proc diag*(v : Vec4f) : Mat4f =
  result[0][0] = v[0]
  result[1][1] = v[1]
  result[2][2] = v[2]
  result[3][3] = v[3]

proc diag*(m : Mat4f) : Vec4f =
  result[0] = m[0][0]
  result[1] = m[1][1]
  result[2] = m[2][2]
  result[3] = m[3][3]

#quaternion

type Quatf* = distinct array[0..3, float32]

proc `[]`(q : Quatf, i : int) : float32 =
  array[0..3,float32](q)[i]

proc `[]=`(q : var Quatf, i : int, val : float32) =
  array[0..3,float32](q)[i] = val

iterator items*(q: Quatf) : float32 =
  for i in 0 .. 3:
    yield q[i]

proc `$`*(q : Quatf) : string = q.mkString("quatf(", ", ", ")")

proc x*(q : Quatf) : float32 = q[0]
proc y*(q : Quatf) : float32 = q[1]
proc z*(q : Quatf) : float32 = q[2]
proc w*(q : Quatf) : float32 = q[3]

proc quatf*(x,y,z,w : float32) : Quatf =
 [x,y,z,w].Quatf

# untestet
proc `*`*(q1,q2 : Quatf) : Quatf = quatf(
  q1.w * q2.x + q1.x * q2.w + q1.y * q2.z - q1.z * q2.y,
  q1.w * q2.y - q1.x * q2.z + q1.y * q2.w + q1.z * q2.x,
  q1.w * q2.z + q1.x * q2.y - q1.y * q2.x + q1.z * q2.w,
  q1.w * q2.w - q1.x * q2.x - q1.y * q2.y - q1.z * q2.z
)

proc `*`*(q : Quatf, s : float32) : Quatf =
  for i in 0 .. 3:
    result[i] = q[i] * s

proc `+`*(q1,q2 : Quatf) : Quatf =
  for i in 0 .. 3:
    result[i] = q1[i] + q2[i]

proc `-`*(q1,q2 : Quatf) : Quatf =
  for i in 0 .. 3:
    result[i] = q1[i] - q2[1]

proc length2*(q : Quatf) : float32 =
  for i in 0 .. 3:
    result += q[i] * q[i]

proc length*(q : Quatf) : float32 =
  q.length2.sqrt

proc normalize*(q : Quatf) : Quatf =
  q * (1.0f / q.length)

proc mat3*(q : Quatf) : Mat3f =
  let
    txx = 2*q.x*q.x
    tyy = 2*q.y*q.y
    tzz = 2*q.z*q.z
    txy = 2*q.x*q.y
    txz = 2*q.x*q.z
    tyz = 2*q.y*q.z
    txw = 2*q.x*q.w
    tyw = 2*q.y*q.w
    tzw = 2*q.z*q.w


  result[0] = vec3(1 - (tyy + tzz),      txy + tzw ,      txz - tyw);
  result[1] = vec3(     txy - tzw , 1 - (txx + tzz),      tyz + txw);
  result[2] = vec3(     txz + tyw ,      tyz - txw , 1 - (txx + tyy));

type JointPose* = object
  translate* : Vec3f
  rotate*    : Quatf
  scale*     : Vec3f

proc `[]`(pose : var JointPose, index : int) : var float32 =
  cast[ptr array[0..9, float32]](pose.addr)[index]

proc poseMatrix*(jp : JointPose) : Mat4f =
  let scalerot_mat = jp.rotate.normalize.mat3 * jp.scale.diag
  result[0] = vec4(scalerot_mat[0], 0)
  result[1] = vec4(scalerot_mat[1], 0)
  result[2] = vec4(scalerot_mat[2], 0)
  result[3] = vec4(jp.translate,    1)


