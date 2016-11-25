when isMainModule:
  import fancygl

type
  Camera* = object
    dir*: Quatf
    pos*: Vec4f

proc newCamera*() : Camera =
  result.dir.z = 1
  result.pos.w = 1
    
proc modelmat*(cam: Camera): Mat4f =
  result = mat4(cam.dir, cam.pos)
  
proc viewmat*(cam: Camera): Mat4f =
  cam.modelmat.inverse

proc moveRelative*(cam: var Camera; offset: Vec3f): void =
  cam.pos += vec4f(cam.dir.mat3 * offset, 0)
  
proc moveAbolute*(cam: var Camera; offset: Vec3f): void =
  cam.pos += vec4f(offset, 0)

proc turnRelative*(cam: var Camera; q: Quatf): void =
  cam.dir *= q

proc turnAbsolute*(cam: var Camera; q: Quatf): void =
  cam.dir = q * cam.dir
  
proc turnRelative*(cam: var Camera; axis: Vec3f; angle: float32): void =
  cam.turnRelative(quatf(axis, angle))

proc turnAbsolute*(cam: var Camera; axis: Vec3f; angle: float32): void =
  cam.turnAbsolute(quatf(axis, angle))
  
proc turnRelativeX*(cam: var Camera; angle: float32): void =
  cam.turnRelative(quatf(vec3f(1,0,0), angle))
  
proc turnRelativeY*(cam: var Camera; angle: float32): void =
  cam.turnRelative(quatf(vec3f(0,1,0), angle))
  
proc turnRelativeZ*(cam: var Camera; angle: float32): void =
  cam.turnRelative(quatf(vec3f(0,0,1), angle))

proc turnAbsoluteX*(cam: var Camera; angle: float32): void =
  cam.turnAbsolute(quatf(vec3f(1,0,0), angle))
  
proc turnAbsoluteY*(cam: var Camera; angle: float32): void =
  cam.turnAbsolute(quatf(vec3f(0,1,0), angle))
  
proc turnAbsoluteZ*(cam: var Camera; angle: float32): void =
  cam.turnAbsolute(quatf(vec3f(0,0,1), angle))

  
proc lookAt*(cam: var Camera; pos: Vec3f; up: Vec3f = vec3f(0,0,1)): void =
  var f = normalize(cam.pos.xyz - pos)
  var s = normalize(cross(up, f))
  var u = cross(f, s)
  cam.dir = quatf(mat3(s,u,f))

proc lookAtCamera*(eye, center, up: Vec3f) : Camera =
  result.pos = vec4f(eye,1)
  result.lookAt(center,up)
  
proc lookAtMat*(eye, center, up: Vec3f) : Mat4f =
  let f = normalize(eye-center)
  let s = normalize(cross(up,f))
  let u = cross(f,s)

  result[0][0] = s.x
  result[1][0] = s.y
  result[2][0] = s.z
  
  result[0][1] = u.x
  result[1][1] = u.y
  result[2][1] = u.z
  
  result[0][2] = f.x
  result[1][2] = f.y
  result[2][2] = f.z

  result[3][0] = -dot(s, eye)
  result[3][1] = -dot(u, eye)
  result[3][2] = -dot(f, eye)

  result[3][3] = 1

when isMainModule:
  
  var eye, center, up: Vec3f
  for i in 0 .. 2:
    eye[i]    = float32(randNormal())
    center[i] = float32(randNormal())
    up[i]     = float32(randNormal())

  let matA = lookAtMat(eye, center, up)
  
  var cam  = newCamera()
  cam.pos.xyz = eye
  cam.lookAt(center, up)

  let matB = cam.viewmat
  
  var matC = matA - matB
  for i in 0 .. 3:
    for j in 0 .. 3:
      assert matC[i][j] < 1e-6
