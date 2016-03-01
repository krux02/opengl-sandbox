
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

proc mat4f*(mat: Mat4d): Mat4f =
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
