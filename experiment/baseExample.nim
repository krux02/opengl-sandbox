import renderMacro

# this file is mostly a scratchpad for testing. Nothing to learn from.

let (window, context) = defaultSetup(vec2i(640,480))

type
  MyVertexType = tuple
    position_os: Vec4f
    normal_os: Vec4f
    texCoord: Vec2f

  MyFragmentType = tuple
    color: Vec4f

  Light = object
    position_ws : Vec4f
    color : Vec4f

  #MyMesh = Mesh[MyVertexType]

genMeshType(MyMesh, MyVertexType)

## ...

var myTexture: Texture2D
var mesh: MyMesh
#var framebuffer: MyFramebuffer
var mvp: Mat4f
var M,V,P: Mat4f
var lights: array[10,Light]

## ...

render(mesh) do (v, gl):
  gl.Position     = (P * V * M) * v.position_os
  let position_cs = V*M*v.position_os
  let normal_cs   =
    inverse(transpose(V*M)) * v.normal_os

  ## rasterize

  var lighting: Vec4f
  for light in lights:
    let light_position_cs = V * light.position_ws
    let light_direction_cs =
      light_position_cs - position_cs
    let light_intensity =
      max(dot(light_direction_cs, normal_cs), 0)
    lighting += light_intensity * light.color

  let textureSample = texture(myTexture, v.texCoord)
  result.color = textureSample * lighting

## ...

proc basicTransform(position: Vec4f): Vec4f =
  # access to global shared variable
  mvp * position

proc basicLight(position_ws,normal_ws:Vec4f):Vec4f =
  # access to global shared variable
  for light in lights:
    let light_direction_ws =
      light.position_ws - position_ws
    let light_intensity =
      max(dot(light_direction_ws, normal_ws), 0)
    result += light_intensity * light.color


var runGame: bool = true

#let timer = newStopWatch(true)
#let aspect = window.aspectRatio.float32
#let projMat : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

while runGame:

  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_ESCAPE:
      runGame = false
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_F10:
      window.screenshot

  #let time = (frame / 100) * Pi * 2
  #let time = timer.time.float32

  # let viewMat = mat4f(1)
  #   .translate(0,1,5)            # position camera at position 0,1,5
  #   .rotateX(Pi * -0.05)         # look a bit down
  #   .inverse                     # the camera matrix needs to be inverted

  # let modelMat = mat4f(1)
  #   .rotateY(time)               # rotate the triangle
  #   .scale(3)                    # scale the triangle to be big enough on screen

  #let mvp = projMat * viewMat * modelMat

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  # triangleMesh.render do (vertex, gl):
  #   gl.Position = mvp * vertex.position
  #   ## rasterize
  #   result.color = vertex.color
  #   #result.color.rgb = vec3f(simplex((modelMat * vertex.position).xyz))

  renderDebug(mesh) do (v, gl):
    gl.Position  = basicTransform(v.position_os)
    let position_ws = M * v.position_os
    let normal_ws   = M * v.normal_os
    ## rasterize
    let a = basicLight(position_ws, normal_ws)
    let b = texture(myTexture, v.texCoord)
    result.color = a * b


  glSwapWindow(window)
