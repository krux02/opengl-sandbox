import renderMacro

import glm/noise

type
  MyFragmentType = object
    color: Vec4f # {. GL_RGB16F .}

  MyVertexType = object
    position_os: Vec4f
    normal_os: Vec4f
    texCoord: Vec2f

  Light = object
    position_ws : Vec4f
    color : Vec4f

  MyMesh        = Mesh[MyVertexType]
  #MyFramebuffer = Framebuffer2[MyFragmentType]

proc generateCity(radius: int): seq[MyVertexType] =
  for x in -radius .. radius:
    for y in -radius .. radius:
      let pos = vec2f(x.float32,y.float32)

      for i in 0 ..< boxVertices.len:
        var vertex: MyVertexType
        vertex.position_os = boxVertices[i]
        vertex.normal_os   = boxNormals[i]
        vertex.texCoord    = boxTexCoords[i]
        vertex.position_os.x +=  pos.x*3
        vertex.position_os.z +=  pos.y*3
        result.add vertex

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

var myMeshArrayBuffer = createArrayBuffer[MyVertexType](boxVertices.len)
for i, vertex in myMeshArrayBuffer.wPairs:
  vertex.position_os = boxVertices[i]
  vertex.normal_os   = boxNormals[i]
  vertex.texCoord    = boxTexCoords[i]

#var myMeshArrayBuffer = arrayBuffer(generateCity(20))
var mesh: Mesh[MyVertexType]
#var framebuffer: MyFramebuffer = createFramebuffer[MyFragmentType](window.size, GL_)

var M,V,P: Mat4f

M = mat4f(1).rotateX(0.5).rotateY(0.75)
V = mat4f(1).translate( 0, 0, -7)
P = perspective(45'f32, window.aspectRatio, 0.01, 100.0)

var lights: array[4,Light]
lights[0].color = vec4f(1,0,1,1)
lights[1].color = vec4f(1,0,0,1)
lights[2].color = vec4f(0,1,0,1)
lights[3].color = vec4f(0,0,1,1)

let timer = newStopWatch(true)

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
      else:
        discard
    of MouseWheel:
      let alpha = float32(evt.wheel.x + evt.wheel.y) * 0.05
      let rotMat = mat4f(1).rotateZ(alpha)
      M = rotMat * M
    of MouseMotion:
      let v = vec2(evt.motion.yrel.float32, evt.motion.xrel.float32)
      let alpha = v.length * 0.01
      let axis = vec3(v, 0)
      let rotMat = mat4f(1).rotate(alpha, axis)
      M = rotMat * M
    else:
      discard

  let time = float32(timer.time)
  for i, light in lights.mpairs:
    let alpha = time + Pi * 0.5f * float32(i)
    light.position_ws.xy = vec2f(cos(alpha), sin(alpha)) * 4

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  glDisable(GL_DEPTH_TEST)
  glCullFace(GL_FRONT)

  render(mesh) do (vertex, gl):
    var position_os = vec4(vertex.position_os.xyz, 0)
    let position_ws = M*position_os
    let position_cs = V*position_ws
    gl.Position = P * position_cs

    #let normal_cs   = inverse(transpose(V*M)) * vertex.normal_os

    ## rasterize

    #var textureSample = texture(myTexture, vertex.texCoord)
    var textureSample = texture(skyTexture, vertex.position_os.xyz)
    result.color = textureSample


  glEnable(GL_DEPTH_TEST)
  glCullFace(GL_BACK)

  renderDebug(mesh) do (vertex, gl):
    var position_os = vertex.position_os
    var tmp = 1 - fract(time * 2 + floor(float32(gl.VertexID) / 36.0f) * 1.235f)
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

  glSwapWindow(window)
