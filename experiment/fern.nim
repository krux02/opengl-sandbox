import renderMacro
import random
#import glm




proc nextPoint(arg: Vec2f; r : float32): Vec2f =
  if r < 0.01:
    result.x = 0
    result.y =  0.16 * arg.y
  elif r < 0.86:
    result.x =  0.85 * arg.x +  0.04 * arg.y
    result.y = -0.04 * arg.x +  0.85 * arg.y + 1.6
  elif r < 0.93:
    result.x = 0.20 * arg.x + -0.26 * arg.y
    result.y = 0.23 * arg.x +  0.22 * arg.y + 1.6
  else:
    result.x = -0.15 * arg.x +  0.28 * arg.y
    result.y =  0.26 * arg.x +  0.24 * arg.y + 0.44


proc genPointsBuffer(): ArrayBuffer[Vec2f] =
  var points = @[ vec2f(0) ]
  for i in 0 ..< 200000:
    points.add points[^1].nextPoint(rand(1.0f))

  result = points.arrayBuffer



let (window, context) = defaultSetup()

type
  MyFragmentType = object
    color: Vec4f # {. GL_RGB16F .}

  MyVertexType = tuple
    position: Vec2f

  #MyMesh = Mesh[MyVertexType
genMeshType(MyMesh, MyVertexType)

var triangleMesh: MyMesh
triangleMesh.vertexIndices.mode = GL_Points
triangleMesh.vertexIndices.numVertices = 200000

triangleMesh.buffers.position = genPointsBuffer()

var runGame: bool = true

let timer = newStopWatch(true)
let aspect = window.aspectRatio.float32
let projMat : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

glPointSize(2)

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
    .translate(0,6,5)            # position camera at position 0,1,5
    .rotateX(Pi * -0.05)         # look a bit down
    .inverse                     # the camera matrix needs to be inverted

  let modelMat = mat4f(1)
    #.translate(2,0,0)
    .rotateY(time)               # rotate the triangle
    .scale(1)                    # scale the triangle to be big enough on screen

  let mvp = projMat * viewMat * modelMat

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  triangleMesh.render do (vertex, gl):
    gl.Position = mvp * vec4f(vertex.position, 0, 1)
    ## rasterize
    result.color = vec4f(1)#vertex.color
    #result.color.rgb = vec3f(simplex((modelMat * vertex.position).xyz))

  glSwapWindow(window)


#void setup() {
#  size(600, 600);
#  background(51);
#}


#points.add points[^1].nextPoint(rand(1.0f))
