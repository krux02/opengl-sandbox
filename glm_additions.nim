export glm

export math.floor
export math.sin
export math.cos

const
  I4d* : Mat4d = diag(vec4d(1))
  I3d* : Mat3d = diag(vec3d(1))
  I2d* : Mat2d = diag(vec2d(1))
  I4f* : Mat4f = diag(vec4f(1))
  I3f* : Mat3f = diag(vec3f(1))
  I2f* : Mat2f = diag(vec2f(1))

#[

type
  Vec4u8 = Vec4[uint8]
  Vec4f* = Vec4[float32]
  Vec3f* = Vec3[float32]
  Vec2f* = Vec2[float32]
  Vec4d* = Vec4[float64]
  Vec3d* = Vec3[float64]
  Vec2d* = Vec2[float64]
  Vec4i* = Vec4[int32]
  Vec3i* = Vec3[int32]
  Vec2i* = Vec2[int32]
  Vec4l* = Vec4[int64]
  Vec3l* = Vec3[int64]
  Vec2l* = Vec2[int64]
  Mat4f* = Mat4x4[float32]
  Mat3f* = Mat3x3[float32]
  Mat2f* = Mat2x2[float32]
  Mat4d* = Mat4x4[float64]
  Mat3d* = Mat3x3[float64]
  Mat2d* = Mat2x2[float64]
  Mat4i* = Mat4x4[int32]
  Mat3i* = Mat3x3[int32]
  Mat2i* = Mat2x2[int32]
  Mat4l* = Mat4x4[int64]
  Mat3l* = Mat3x3[int64]
  Mat2l* = Mat2x2[int64]

type
  SomeVec* = Vec4 | Vec3 | Vec2
  SomeMat* = Mat4x4 | Mat4x3 | Mat4x2 | Mat3x4 | Mat3x3 | Mat3x2 | Mat2x4 | Mat2x3 | Mat2x2

template numCols*(t : typedesc[Mat4x4] | typedesc[Mat4x3] | typedesc[Mat4x2]) : int = 4
template numCols*(t : typedesc[Mat3x4] | typedesc[Mat3x3] | typedesc[Mat3x2]) : int = 3
template numCols*(t : typedesc[Mat2x4] | typedesc[Mat2x3] | typedesc[Mat2x2]) : int = 2

template numRows*(t : typedesc[Mat4x4] | typedesc[Mat3x4] | typedesc[Mat2x4]) : int = 4
template numRows*(t : typedesc[Mat4x3] | typedesc[Mat3x3] | typedesc[Mat2x3]) : int = 3
template numRows*(t : typedesc[Mat4x2] | typedesc[Mat3x2] | typedesc[Mat2x2]) : int = 2

proc numCols*[T: SomeMat](mat: T): int = numCols(T)
proc numRows*[T: SomeMat](mat: T): int = numRows(T)
  
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


proc vec4i*(x,y,z,w:int32)             : Vec4i = [  x,   y,   z,   w].Vec4i
proc vec4i*(v:Vec3i; w:int32)          : Vec4i = [v.x, v.y, v.z,   w].Vec4i
proc vec4i*(x:int32; v:Vec3i)          : Vec4i = [  x, v.x, v.y, v.z].Vec4i
proc vec4i*(a,b:Vec2i)                 : Vec4i = [a.x, a.y, b.x, b.y].Vec4i
proc vec4i*(v:Vec2i; z,w:int32)        : Vec4i = [v.x, v.y,   z,   w].Vec4i
proc vec4i*(x:int32; v:Vec2i; w:int32) : Vec4i = [  x, v.x, v.y,   w].Vec4i
proc vec4i*(x,y:int32; v:Vec2i)        : Vec4i = [  x,   y, v.x, v.y].Vec4i
proc vec4i*(x:int32)                   : Vec4i = [  x,   x,   x,   x].Vec4i

proc vec3i*(x,y,z:int32)      : Vec3i = [  x,   y,   z].Vec3i
proc vec3i*(v:Vec2i; z:int32) : Vec3i = [v.x, v.y,   z].Vec3i
proc vec3i*(x:int32; v:Vec2i) : Vec3i = [  x, v.x, v.y].Vec3i
proc vec3i*(x:int32)          : Vec3i = [  x,   x,   x].Vec3i

proc vec2i*(x,y:int32) : Vec2i = [x,y].Vec2i
proc vec2i*(x:int32)   : Vec2i = [x,x].Vec2i

proc vec4i*(a:array[0..3, int32]) : Vec4i = [a[0], a[1], a[2], a[3]].Vec4i
proc vec3i*(a:array[0..2, int32]) : Vec3i = [a[0], a[1], a[2]].Vec3i
proc vec2i*(a:array[0..1, int32]) : Vec2i = [a[0], a[1]].Vec2i

# conversions

proc vec4f*(v: Vec4d) : Vec4f = [v.x.float32, v.y.float32, v.z.float32, v.w.float32].Vec4f
proc vec4f*(v: Vec4i) : Vec4f = [v.x.float32, v.y.float32, v.z.float32, v.w.float32].Vec4f
proc vec4f*(v: Vec4l) : Vec4f = [v.x.float32, v.y.float32, v.z.float32, v.w.float32].Vec4f
proc vec4d*(v: Vec4f) : Vec4d = [v.x.float64, v.y.float64, v.z.float64, v.w.float64].Vec4d
proc vec4d*(v: Vec4i) : Vec4d = [v.x.float64, v.y.float64, v.z.float64, v.w.float64].Vec4d
proc vec4d*(v: Vec4l) : Vec4d = [v.x.float64, v.y.float64, v.z.float64, v.w.float64].Vec4d
proc vec4i*(v: Vec4f) : Vec4i = [v.x.int32, v.y.int32, v.z.int32, v.w.int32].Vec4i
proc vec4i*(v: Vec4i) : Vec4i = [v.x.int32, v.y.int32, v.z.int32, v.w.int32].Vec4i
proc vec4i*(v: Vec4l) : Vec4i = [v.x.int32, v.y.int32, v.z.int32, v.w.int32].Vec4i
proc vec4l*(v: Vec4f) : Vec4l = [v.x.int64, v.y.int64, v.z.int64, v.w.int64].Vec4l
proc vec4l*(v: Vec4i) : Vec4l = [v.x.int64, v.y.int64, v.z.int64, v.w.int64].Vec4l
proc vec4l*(v: Vec4l) : Vec4l = [v.x.int64, v.y.int64, v.z.int64, v.w.int64].Vec4l
proc vec3f*(v: Vec3d) : Vec3f = [v.x.float32, v.y.float32, v.z.float32].Vec3f
proc vec3f*(v: Vec3i) : Vec3f = [v.x.float32, v.y.float32, v.z.float32].Vec3f
proc vec3f*(v: Vec3l) : Vec3f = [v.x.float32, v.y.float32, v.z.float32].Vec3f
proc vec3d*(v: Vec3f) : Vec3d = [v.x.float64, v.y.float64, v.z.float64].Vec3d
proc vec3d*(v: Vec3i) : Vec3d = [v.x.float64, v.y.float64, v.z.float64].Vec3d
proc vec3d*(v: Vec3l) : Vec3d = [v.x.float64, v.y.float64, v.z.float64].Vec3d
proc vec3i*(v: Vec3f) : Vec3i = [v.x.int32, v.y.int32, v.z.int32].Vec3i
proc vec3i*(v: Vec3i) : Vec3i = [v.x.int32, v.y.int32, v.z.int32].Vec3i
proc vec3i*(v: Vec3l) : Vec3i = [v.x.int32, v.y.int32, v.z.int32].Vec3i
proc vec3l*(v: Vec3f) : Vec3l = [v.x.int64, v.y.int64, v.z.int64].Vec3l
proc vec3l*(v: Vec3i) : Vec3l = [v.x.int64, v.y.int64, v.z.int64].Vec3l
proc vec3l*(v: Vec3l) : Vec3l = [v.x.int64, v.y.int64, v.z.int64].Vec3l
proc vec2f*(v: Vec2d) : Vec2f = [v.x.float32, v.y.float32].Vec2f
proc vec2f*(v: Vec2i) : Vec2f = [v.x.float32, v.y.float32].Vec2f
proc vec2f*(v: Vec2l) : Vec2f = [v.x.float32, v.y.float32].Vec2f
proc vec2d*(v: Vec2f) : Vec2d = [v.x.float64, v.y.float64].Vec2d
proc vec2d*(v: Vec2i) : Vec2d = [v.x.float64, v.y.float64].Vec2d
proc vec2d*(v: Vec2l) : Vec2d = [v.x.float64, v.y.float64].Vec2d
proc vec2i*(v: Vec2f) : Vec2i = [v.x.int32, v.y.int32].Vec2i
proc vec2i*(v: Vec2i) : Vec2i = [v.x.int32, v.y.int32].Vec2i
proc vec2i*(v: Vec2l) : Vec2i = [v.x.int32, v.y.int32].Vec2i
proc vec2l*(v: Vec2f) : Vec2l = [v.x.int64, v.y.int64].Vec2l
proc vec2l*(v: Vec2i) : Vec2l = [v.x.int64, v.y.int64].Vec2l
proc vec2l*(v: Vec2l) : Vec2l = [v.x.int64, v.y.int64].Vec2l

# functions

proc floor*(v : Vec2) : Vec2 =
  result.x = floor(v.x)
  result.y = floor(v.y)

proc floor*(v : Vec3) : Vec3 =
  result.x = floor(v.x)
  result.y = floor(v.y)
  result.z = floor(v.z)

proc floor*(v : Vec4) : Vec4 =
  result.x = floor(v.x)
  result.y = floor(v.y)
  result.z = floor(v.z)
  result.w = floor(v.w)


proc clamp*[T](arg: Vec4[T]; minVal, maxVal: T): Vec4[T] =
  result.x = clamp(arg.x, minVal, maxVal)
  result.y = clamp(arg.y, minVal, maxVal)
  result.z = clamp(arg.z, minVal, maxVal)
  result.w = clamp(arg.w, minVal, maxVal)

proc clamp*[T](arg, minVal, maxVal: Vec4[T]): Vec4[T] =
  result.x = clamp(arg.x, minVal.x, maxVal.x)
  result.y = clamp(arg.y, minVal.y, maxVal.y)
  result.z = clamp(arg.z, minVal.z, maxVal.z)
  result.w = clamp(arg.w, minVal.w, maxVal.w)

proc clamp*[T](arg: Vec3[T]; minVal, maxVal: T): Vec3[T] =
  result.x = clamp(arg.x, minVal, maxVal)
  result.y = clamp(arg.y, minVal, maxVal)
  result.z = clamp(arg.z, minVal, maxVal)

proc clamp*[T](arg, minVal, maxVal: Vec3[T]): Vec3[T] =
  result.x = clamp(arg.x, minVal.x, maxVal.x)
  result.y = clamp(arg.y, minVal.y, maxVal.y)
  result.z = clamp(arg.z, minVal.z, maxVal.z)

proc clamp*[T](arg: Vec2[T]; minVal, maxVal: T): Vec2[T] =
  result.x = clamp(arg.x, minVal, maxVal)
  result.y = clamp(arg.y, minVal, maxVal)

proc clamp*[T](arg, minVal, maxVal: Vec2[T]): Vec2[T] =
  result.x = clamp(arg.x, minVal.x, maxVal.x)
  result.y = clamp(arg.y, minVal.y, maxVal.y)

proc `+`*(mat1, mat2 : Mat4f) : Mat4f =
  result[0] = mat1[0] + mat2[0]
  result[1] = mat1[1] + mat2[1]
  result[2] = mat1[2] + mat2[2]
  result[3] = mat1[3] + mat2[3]

proc `+=`*[T : SomeVec](lhs: var T; rhs: T): auto =
  lhs = lhs + rhs
  
proc `-`*[T](v: Vec4[T]): Vec4[T] =
  result[0] = -v[0]
  result[1] = -v[1]
  result[2] = -v[2]
  result[3] = -v[3]
  
proc `-`*[T](v: Vec3[T]): Vec3[T] =
  result[0] = -v[0]
  result[1] = -v[1]
  result[2] = -v[2]

proc `-`*[T](v: Vec2[T]): Vec2[T] =
  result[0] = -v[0]
  result[1] = -v[1]
  
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

proc diag*(v : Vec2d) : Mat2d =
  result[0][0] = v[0]
  result[1][1] = v[1]

proc diag*(m : Mat2d) : Vec2d =
  result[0] = m[0][0]
  result[1] = m[1][1]

proc diag*(v : Vec3d) : Mat3d =
  result[0][0] = v[0]
  result[1][1] = v[1]
  result[2][2] = v[2]

proc diag*(m : Mat3d) : Vec3d =
  result[0] = m[0][0]
  result[1] = m[1][1]
  result[2] = m[2][2]

proc diag*(v : Vec4d) : Mat4d =
  result[0][0] = v[0]
  result[1][1] = v[1]
  result[2][2] = v[2]
  result[3][3] = v[3]

proc diag*(m : Mat4d) : Vec4d =
  result[0] = m[0][0]
  result[1] = m[1][1]
  result[2] = m[2][2]
  result[3] = m[3][3]
  
proc `-`*(m1,m2: SomeMat): SomeMat =
  for col in 0 ..< numCols(SomeMat):
    for row in 0 ..< numRows(SomeMat):
      result[col][row] = m1[col][row] - m2[col][row]

proc transpose*(m: Mat4x4): Mat4x4 =
  for col in 0 ..< 4:
    for row in 0 ..< 4:
      result[col][row] = m[row][col]

#quaternion

type Quatf* = distinct array[0..3, float32]

proc `[]`*(q : Quatf, i : int) : float32 =
  array[0..3,float32](q)[i]

proc `[]=`*(q : var Quatf, i : int, val : float32) =
  array[0..3,float32](q)[i] = val

iterator items*(q: Quatf) : float32 =
  for i in 0 .. 3:
    yield q[i]
    
proc `$`*(q : Quatf) : string = q.mkString("quatf(", ", ", ")")

proc x*(q : Quatf) : float32 = q[0]
proc y*(q : Quatf) : float32 = q[1]
proc z*(q : Quatf) : float32 = q[2]
proc w*(q : Quatf) : float32 = q[3]

proc `x=`*(q : var Quatf; v: float32) : void =
  q[0] = v

proc `y=`*(q : var Quatf; v: float32) : void =
  q[1] = v

proc `z=`*(q : var Quatf; v: float32) : void =
  q[2] = v

proc `w=`*(q : var Quatf; v: float32) : void =
  q[3] = v

proc quatf*(x,y,z,w : float32) : Quatf =
 [x,y,z,w].Quatf

proc quatf*(axis: Vec3f; angle: float32): Quatf =
  let s = sin(angle / 2)
  result.x = axis.x * s
  result.y = axis.y * s
  result.z = axis.z * s
  result.w = cos(angle / 2)
  
proc quatf*(mat: Mat3f): Quatf =
  ## mat needs to be rotation matrix (orthogonal, det(mat) = 1
  let qw = sqrt(1 + mat[0][0] + mat[1][1] + mat[2][2]) * 0.5'f32
  result.x = (mat[1][2] - mat[2][1]) / (4 * qw)
  result.y = (mat[2][0] - mat[0][2]) / (4 * qw)
  result.z = (mat[0][1] - mat[1][0]) / (4 * qw)
  result.w = qw
  
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

proc `*=`*(q1: var Quatf; q2: Quatf): auto =
  q1 = q1 * q2
  
proc `+=`*(q1: var Quatf; q2: Quatf): auto =
  q1 = q1 + q2
  
proc `-=`*(q1: var Quatf; q2: Quatf): auto =
  q1 = q1 - q2
  
proc length2*(q : Quatf) : float32 =
  for i in 0 .. 3:
    result += q[i] * q[i]

proc length*(q : Quatf) : float32 =
  q.length2.sqrt

proc normalize*(q : Quatf) : Quatf =
  q * (1.0f / q.length)

proc mix*[S,T](a,b: S; alpha: T) : S =
  a * (1 - alpha) + b * alpha

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

  result[0] = vec3f(1 - tyy - tzz,     txy + tzw,     txz - tyw);
  result[1] = vec3f(    txy - tzw, 1 - txx - tzz,     tyz + txw);
  result[2] = vec3f(    txz + tyw,     tyz - txw, 1 - txx - tyy);

proc mat4*(q: Quatf; v: Vec4f = vec4f(0,0,0,1)): Mat4f =
  let tmp = q.mat3
  result[0] = vec4f(tmp[0],0)
  result[1] = vec4f(tmp[1],0)
  result[2] = vec4f(tmp[2],0)
  result[3] = v

proc mat4*(q: Quatf; v: Vec3f): Mat4f = mat4(q, vec4f(v,1))

]#

type JointPose* = object
  translate* : Vec3f
  rotate*    : Quatf
  scale*     : Vec3f

proc matrix*(jp : JointPose) : Mat4f =
  poseMatrix(jp.translate, jp.rotate, jp.scale)

