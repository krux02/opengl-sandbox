import renderMacro

import glm/noise

#let (window, context) = defaultSetup(vec2i(640,480))
let (window, context) = defaultSetup()

type
  MyFragmentType = object
    color: Vec4f # {. GL_RGB16F .}

  MyVertexType = tuple
    position: Vec4f
    color: Vec4f

  #MyMesh = Mesh[MyVertexType]

genMeshType(MyMesh, MyVertexType)

var triangleMesh: MyMesh
triangleMesh.vertexIndices.mode = GL_TRIANGLES
triangleMesh.vertexIndices.numVertices = 3

triangleMesh.buffers.position = arrayBuffer([
  vec4f(-1,-1, 0, 1),
  vec4f( 1,-1, 0, 1),
  vec4f( 0, 1, 0, 1)
])

triangleMesh.buffers.color = arrayBuffer([
  vec4f( 1, 0,0,1),
  vec4f(0, 1,0,1),
  vec4f(0,0,1,1)
])

var runGame: bool = true

let timer = newStopWatch(true)

let aspect = window.aspectRatio.float32
let projMat : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

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
  let time = timer.time.float32

  let viewMat = mat4f(1)
    .translate(0,1,5)            # position camera at position 0,1,5
    .rotateX(Pi * -0.05)         # look a bit down
    .inverse                     # the camera matrix needs to be inverted

  let modelMat = mat4f(1)
    .rotateY(time)               # rotate the triangle
    .scale(3)                    # scale the triangle to be big enough on screen

  let mvp = projMat * viewMat * modelMat

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  triangleMesh.render do (vertex, gl):
    gl.Position = mvp * vertex.position
    ## rasterize
    #result.color = vertex.color
    result.color.rgb = vec3f(simplex((modelMat * vertex.position).xyz))

  glSwapWindow(window)
