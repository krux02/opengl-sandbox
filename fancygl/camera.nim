
type
  WorldNode* = object
    pos*: Vec4f
    dir*: Quatf

proc newWorldNode*() : WorldNode =
  result.dir.w = 1
  result.pos.w = 1

proc newWorldNode*(x,y,z: float32): WorldNode =
  result.dir.w = 1
  result.pos   = vec4f(x,y,z,1)

proc newWorldNode*(pos: Vec4f): WorldNode =
  result.dir.w = 1
  result.pos   = pos

proc newWorldNode*(pos: Vec3f): WorldNode =
  result.dir.w = 1
  result.pos   = vec4(pos, 1)

proc modelmat*(cam: WorldNode): Mat4f =
  result = mat4(cam.dir, cam.pos)

proc viewmat*(cam: WorldNode): Mat4f =
  cam.modelmat.inverse

proc dirVec*(cam: WorldNode): Vec4f =
  # this is not optimized
  # from camera (object) coordinates I look in negative z
  cam.modelmat * vec4f(0,0,-1,0)

proc moveRelative*(cam: var WorldNode; offset: Vec3f): void =
  cam.pos += vec4f(cam.dir.mat3 * offset, 0)

proc moveAbsolute*(cam: var WorldNode; offset: Vec3f): void =
  cam.pos += vec4f(offset, 0)

proc turnRelative*(cam: var WorldNode; q: Quatf): void =
  cam.dir *= q

proc turnAbsolute*(cam: var WorldNode; q: Quatf): void =
  cam.dir = q * cam.dir

proc turnRelative*(cam: var WorldNode; axis: Vec3f; angle: float32): void =
  cam.turnRelative(quatf(axis, angle))

proc turnAbsolute*(cam: var WorldNode; axis: Vec3f; angle: float32): void =
  cam.turnAbsolute(quatf(axis, angle))

proc turnRelativeX*(cam: var WorldNode; angle: float32): void =
  cam.turnRelative(quatf(vec3f(1,0,0), angle))

proc turnRelativeY*(cam: var WorldNode; angle: float32): void =
  cam.turnRelative(quatf(vec3f(0,1,0), angle))

proc turnRelativeZ*(cam: var WorldNode; angle: float32): void =
  cam.turnRelative(quatf(vec3f(0,0,1), angle))

proc turnAbsoluteX*(cam: var WorldNode; angle: float32): void =
  cam.turnAbsolute(quatf(vec3f(1,0,0), angle))

proc turnAbsoluteY*(cam: var WorldNode; angle: float32): void =
  cam.turnAbsolute(quatf(vec3f(0,1,0), angle))

proc turnAbsoluteZ*(cam: var WorldNode; angle: float32): void =
  cam.turnAbsolute(quatf(vec3f(0,0,1), angle))

proc lookAt*(cam: var WorldNode; pos: Vec3f; up: Vec3f = vec3f(0,0,1)): void =
  var f = normalize(cam.pos.xyz - pos)
  var s = normalize(cross(up, f))
  var u = cross(f, s)
  let m = mat3(s,u,f)
  cam.dir = quatf(m)

proc lookAt*(cam: var WorldNode; target: WorldNode; up: Vec3f = vec3f(0,0,1)): void =
  cam.lookAt(target.pos.xyz, up)

proc lookAtCamera*(eye, center, up: Vec3f) : WorldNode =
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

  var cam  = newWorldNode()
  cam.pos.xyz = eye
  cam.lookAt(center, up)

  let matB = cam.viewmat

  var matC = matA - matB
  for i in 0 .. 3:
    for j in 0 .. 3:
      assert matC[i][j] < 1e-6
