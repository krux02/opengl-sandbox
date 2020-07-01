type
  CameraControls* = object
    rotation*: Vec2f
    speed*: float32

proc cameraControlEventWatch*(userdata: pointer; event: ptr Event): cint {.cdecl.} =
  let control = cast[ptr CameraControls](userdata)
  if event.kind == MouseMotion:
    control.rotation.xy += vec2f(event[].motion.rel.yx) / -128.0
    control.rotation.x   = clamp(control.rotation.x , -Pi*0.5, Pi*0.5)

proc update*(node: var WorldNode, control: CameraControls): void =
  var movement = vec3f(0,0,0)
  var i: cint
  var state = getKeyboardState(i.addr)
  movement.z = (state[SCANCODE_D].float - state[SCANCODE_E].float) * control.speed
  movement.x = (state[SCANCODE_F].float - state[SCANCODE_S].float) * control.speed


  node.dir = quatf(0,0,0,1)
  node.turnRelativeX(control.rotation.x + Pi*0.5)
  node.turnAbsoluteZ(control.rotation.y)
  node.moveRelative(movement)
