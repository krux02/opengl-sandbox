type
  CameraControls* = object
    rotation: Vec2f
    speed*: float32

proc cameraControlEventWatch*(userdata: pointer; event: ptr Event): Bool32 {.cdecl.} =
  let control = cast[ptr CameraControls](userdata)
  if event.kind == MouseMotion:
    control.rotation.xy -= event[].motion.rel.yx.vec2f / 128.0
    control.rotation.x   = clamp(control.rotation.x , 0, PI)
   
proc update*(node: var WorldNode, control: CameraControls): void =
  var movement = vec3f(0,0,0)
  var state = getKeyboardState()
  movement.z = (state[SDL_SCANCODE_D.int].float - state[SDL_SCANCODE_E.int].float) * control.speed
  movement.x = (state[SDL_SCANCODE_F.int].float - state[SDL_SCANCODE_S.int].float) * control.speed

  node.dir = quatf(0,0,0,1)
  node.turnRelativeX(control.rotation.x)
  node.turnAbsoluteZ(control.rotation.y)
  node.moveRelative(movement)
