import renderMacro

import glm/noise

import teapot

type
  MyFragmentType = object
    color: Vec4f # {. GL_RGB16F .}

  MyVertexType = tuple
    position_os: Vec4f
    normal_os: Vec4f
    texCoord: Vec2f

  Light = object
    position_ws : Vec4f
    color : Vec4f

  #MyFramebuffer = Framebuffer2[MyFragmentType]

genMeshType(MyMesh, MyVertexType)

genMeshType(ControlPointMesh, tuple[position_os: Vec3f])

let (window, context) = defaultSetup()
discard setRelativeMouseMode(true)

let myTexture: Texture2D = loadTexture2DFromFile(getResourcePath("crate.png"))
let skyTexture: TextureCubeMap = loadTextureCubeMapFromFiles([
  getResourcePath("skyboxes/darkskies/darkskies_rt.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_lf.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_up.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_dn.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_bk.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_ft.tga"),
])

glEnable(GL_CULL_FACE)
glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS)
glEnable(GL_DEPTH_CLAMP)

var mesh1: MyMesh
mesh1.mode = GL_TRIANGLES
mesh1.numVertices = boxVertices.len
mesh1.buffers.position_os = arrayBuffer(boxVertices)
mesh1.buffers.normal_os   = arrayBuffer(boxNormals)
mesh1.buffers.texCoord    = arrayBuffer(boxTexCoords)

var tetraederArrayBuffer = createArrayBuffer[MyVertexType](tetraederVertices.len)
for i, vertex in tetraederArrayBuffer.wPairs:
  vertex.position_os = tetraederVertices[i]
  vertex.normal_os   = tetraederNormals[i]
  vertex.texCoord    = tetraederTexCoords[i]

var mesh2: MyMesh
mesh2.mode = GL_TRIANGLES
mesh2.numVertices = tetraederVertices.len
mesh2.buffers.position_os = tetraederArrayBuffer.view(position_os)
mesh2.buffers.normal_os   = tetraederArrayBuffer.view(normal_os)
mesh2.buffers.texCoord    = tetraederArrayBuffer.view(texCoord)

let P = perspective(45'f32, window.aspectRatio, 0.01, 100.0)

var mesh3: MyMesh

block initTeapot:
  echo " creating teapot "
  let timer = newStopWatch(true)
  var teapotData = teapot(11)
  echo "teapot created in ", timer.time, "s"
  # teapot  7 in 1.313054411s
  # teapot  7 in 0.312588133s (precalculate grid)
  # teapot  7 in 0.016067872s (precalculate binomials)
  # teapot 11 in 0.0349098s   (more details)
  # teapot 11 in 0.01723968s (precalculate bernstein polynom weights)
  # teapot 11 in 0.204542387s (added analytic normals)
  var teapotBuffer = arrayBuffer(teapotData.data)

  mesh3.mode = GL_TRIANGLE_STRIP
  mesh3.numVertices = teapotData.data.len
  mesh3.buffers.position_os = teapotBuffer.view(position_os)
  mesh3.buffers.normal_os   = teapotBuffer.view(normal_os)
  mesh3.buffers.texCoord    = teapotBuffer.view(texCoord)



var mesh4: MyMesh = mesh3
mesh4.mode = GL_POINTS # GL_TRIANGLE_STRIP



var lights: array[4,Light]
lights[0].color = vec4f(1,0,1,1)
lights[1].color = vec4f(1,0,0,1)
lights[2].color = vec4f(0,1,0,1)
lights[3].color = vec4f(0,0,1,1)

let timer = newStopWatch(true)
var currentMesh = mesh3

var objRot  = mat4f(1).rotateX(0.5).rotateY(0.75)
var viewRot = mat4f(1)
var toggleA, toggleB: bool


glPointSize(20)

var runGame = true
while runGame:
  for evt in events():
    case evt.kind:
    of QUIT:
      runGame = false
      break
    of KeyDown:
      case evt.key.keysym.scancode
      of SCANCODE_ESCAPE:
        runGame = false
      of SCANCODE_F10:
        window.screenshot
      of SCANCODE_1:
        currentMesh = mesh1
      of SCANCODE_2:
        currentMesh = mesh2
      of SCANCODE_3:
        currentMesh = mesh3
      of SCANCODE_4:
        currentMesh.mode = 0
        toggleB = false
      of SCANCODE_SPACE:
        toggleB = not toggleB

      else:
        discard
    of MouseWheel:
      let alpha = float32(evt.wheel.x + evt.wheel.y) * 0.05
      let rotMat = mat4f(1).rotateZ(alpha)

      if toggleA:
        viewRot = rotMat * viewRot
      else:
        objRot = rotMat * objRot
    of MouseMotion:
      let v = vec2(evt.motion.yrel.float32, evt.motion.xrel.float32)
      let alpha = v.length * 0.01
      let axis = vec3(v, 0)
      let rotMat = mat4f(1).rotate(alpha, axis)

      if toggleA:
        viewRot = rotMat * viewRot
      else:
        objRot = rotMat * objRot
    of MouseButtonDown:
      toggleA = not toggleA
    else:
      discard

  let M = objRot
  let V = mat4f(1).translate( 0, 0, -7) * viewRot

  let time = float32(timer.time)
  for i, light in lights.mpairs:
    let alpha = time + Pi * 0.5f * float32(i)
    light.position_ws.xy = vec2f(cos(alpha), sin(alpha)) * 4

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  glDisable(GL_DEPTH_TEST)
  glCullFace(GL_FRONT)

  render(mesh1) do (vertex, gl):
    var position_os = vec4(vertex.position_os.xyz, 0)
    let position_cs = V*position_os
    gl.Position = P * position_cs

    #let normal_cs   = inverse(transpose(V*M)) * vertex.normal_os

    ## rasterize

    #var textureSample = texture(myTexture, vertex.texCoord)
    var textureSample = texture(skyTexture, vertex.position_os.xyz)
    result.color = textureSample

  glEnable(GL_DEPTH_TEST)
  glCullFace(GL_BACK)

  if currentMesh.mode != 0:
    if toggleB:
      currentMesh.render do (vertex, gl):
        var position_os = vertex.position_os
        var tmp = 1 - fract(time * 2)  # + floor(float32(gl.VertexID) / 36.0f) * 1.235f
        tmp *= tmp
        tmp *= tmp
        tmp *= tmp
        position_os.xyz *= 1 + tmp * 0.125
        let position_ws = M*position_os
        let position_cs = V*position_ws
        gl.Position = P * position_cs
        let normal_cs   = inverse(transpose(V*M)) * vertex.normal_os

        ## rasterize

        var lighting: Vec4f = vec4f(0.2f)
        for light in lights:
          let light_position_cs = V * light.position_ws
          let light_direction_cs = light_position_cs-position_cs
          let light_distance = length(position_cs - light_position_cs)
          let light_intensity = max(dot(light_direction_cs, normal_cs), 0) * max((10 - light_distance) * 0.1f, 0)
          lighting += light_intensity * light.color

        var n: Vec2f
        n.x = simplex( position_ws.xyz * 7 + vec3(time, 0, 0))
        n.y = simplex(-position_ws.xyz * 7 + vec3(time, 0, 0))

        var textureSample = texture(myTexture, vertex.texCoord + n * 0.025f)
        #textureSample = mix(textureSample, textureSample.yzxw, n)

        result.color = textureSample * lighting
        #result.color = textureSample * lighting
        #result.color = vec4f(fract(vertex.texCoord), 0, 1)

    else:

      let invV = inverse(V)
      let cameraPos_ws =     invV * vec4f(0,0,0,1)

      currentMesh.render do (vertex, gl):
        var position_os = vertex.position_os
        var tmp = 1 - fract(time * 2)  # + floor(float32(gl.VertexID) / 36.0f) * 1.235f
        tmp *= tmp
        tmp *= tmp
        tmp *= tmp
        position_os.xyz *= 1 + tmp * 0.125
        let position_ws = M*position_os
        let position_cs = V*position_ws
        gl.Position = P * position_cs
        let normal_cs   = inverse(transpose(V*M)) * vertex.normal_os

        ## rasterize

        var textureSample = texture(myTexture, vertex.texCoord)
        var dir_cs = vec4(position_cs.xyz, 0)
        dir_cs.xyz = reflect(dir_cs.xyz, normal_cs.xyz)
        let dir_ws = invV * dir_cs
        var skySample = texture(skyTexture, dir_ws.xyz)

        let alpha = (textureSample.r + textureSample.g + textureSample.b) * 0.3333
        result.color = mix(skySample, textureSample, alpha)
  else:
    if toggleB:
      mesh3.renderDebug do (vertex, gl):
        var position_os = vec4f(vertex.position_os.xyz, 1)
        let position_ws = M*position_os
        let position_cs = V*position_ws
        gl.Position = P * position_cs

        var color: Vec4f

        color.r = float32((gl.VertexID and (3 shl 0)) shr 0) * 0.33f
        color.g = float32((gl.VertexID and (3 shl 2)) shr 2) * 0.33f
        color.b = float32((gl.VertexID and (3 shl 4)) shr 4) * 0.33f

        ## rasterize
        result.color = color

    else:
      mesh4.renderDebug do (vertex, gl):
        var position_os = vec4f(vertex.position_os.xyz, 1)
        let position_ws = M*position_os
        let position_cs = V*position_ws
        gl.Position = P * position_cs

        var color: Vec4f

        color.r = float32((gl.VertexID and (3 shl 0)) shr 0) * 0.33f
        color.g = float32((gl.VertexID and (3 shl 2)) shr 2) * 0.33f
        color.b = float32((gl.VertexID and (3 shl 4)) shr 4) * 0.33f

        ## rasterize
        #result.color = color
        result.color = vertex.normal_os

  glSwapWindow(window)
