
import os, strutils

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
#[
let skyTexture: TextureCubeMap = loadTextureCubeMapFromFiles([
  getResourcePath("skyboxes/darkskies/darkskies_rt.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_lf.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_up.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_dn.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_bk.tga"),
  getResourcePath("skyboxes/darkskies/darkskies_ft.tga"),
])
]#

var skyboxes : seq[TextureCubeMap] = @[]

for kind, path in walkDir(getResourcePath("skyboxes")):
  if kind == pcDir:
    var names: array[6,string]

    for kind, path in walkDir(path):
      if kind == pcFile:
        if path[^4] == '.':
          let suffix = toLowerASCII(path[^3 .. ^1])
          if suffix == "tga" or suffix == "jpg" or suffix == "png":
            let suffix = toLowerASCII(path[^6 .. ^5])

            let idx = find(["rt","lf","up","dn","bk","ft"], suffix)
            if idx >= 0:
              names[idx] = path
            else:
              echo "fail for ", suffix

    if find(names,"") == -1:
      skyboxes.add loadTextureCubeMapFromFiles(names)
    else:
      echo path, " (FAIL) ", names

echo " loaded ", skyboxes.len, " cube map textures."

var skyIndex = 0


var skyTexture = skyboxes[0]


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
var mesh4: MyMesh

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
  for vertex in teapotData.data.mitems:
    vertex.normal_os *= -1

  mesh3.mode = GL_TRIANGLE_STRIP
  mesh3.numVertices = teapotData.data.len
  mesh3.buffers.position_os = teapotBuffer.view(position_os)
  mesh3.buffers.normal_os   = teapotBuffer.view(normal_os)
  mesh3.buffers.texCoord    = teapotBuffer.view(texCoord)

  for vertex in teapotData.data.mitems:
    vertex.position_os += vertex.normal_os * 0.05

  teapotBuffer = arrayBuffer(teapotData.data)

  mesh4.mode = GL_TRIANGLE_STRIP
  mesh4.numVertices = teapotData.data.len
  mesh4.buffers.position_os = teapotBuffer.view(position_os)
  mesh4.buffers.normal_os   = teapotBuffer.view(normal_os)
  mesh4.buffers.texCoord    = teapotBuffer.view(texCoord)

var lights: array[4,Light]
lights[0].color = vec4f(1,0,1,1)
lights[1].color = vec4f(1,0,0,1)
lights[2].color = vec4f(0,1,0,1)
lights[3].color = vec4f(0,0,1,1)

let timer = newStopWatch(true)
var currentMesh = mesh1

var objRot  = mat4f(1).rotateX(0.5).rotateY(0.75)
var objScale = 1.0f
var viewRot = mat4f(1)
var toggleA, toggleB: bool

var renderMode: int = 0


glPointSize(20)


proc normalTransform(n,p:Vec4f, invMV: Mat4f): Vec4f =
  ## transforms normal vector in eye space
  let q = if p.w == 0: 0.0f else: dot(p.xyz, n.xyz) / -p.w

  result = vec4f(n.xyz, q) * invMV

  # rescaling
  let f = 1 / length(vec3f(invMV[0][2], invMV[1][2], invMV[2][2]))

  result.xyz *= f

  # normalization

  result.xyz /= length(result.xyz)


proc shear(arg: Mat4f; value: float32): Mat4f =
  var tmp = mat4f(1)
  tmp[0,1] = value
  result = tmp * arg

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
        currentMesh = mesh4

      of SCANCODE_F1:
        renderMode = 0
      of SCANCODE_F2:
        renderMode = 1
      of SCANCODE_F3:
        renderMode = 2
      of SCANCODE_F4:
        renderMode = 3
      of SCANCODE_F5:
        renderMode = 4

      of SCANCODE_SPACE:
        toggleB = not toggleB

      of SCANCODE_KP_PLUS:
        skyIndex += 1
        skyTexture = skyboxes[skyIndex mod skyboxes.len]

      of SCANCODE_KP_MINUS:
        skyIndex += skyboxes.high
        skyTexture = skyboxes[skyIndex mod skyboxes.len]


      else:
        discard
    of MouseWheel:
      let alpha = float32(evt.wheel.x + evt.wheel.y) * 0.05
      objScale *= (1 + alpha)

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

  let M = objRot.scale(objScale)
  let V = mat4f(1).translate( 0, 0, -7) * viewRot

  let time = float32(timer.time)
  for i, light in lights.mpairs:
    let alpha = time + Pi * 0.5f * float32(i)
    light.position_ws.xy = vec2f(cos(alpha), sin(alpha)) * 4

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  glDisable(GL_DEPTH_TEST)
  glCullFace(GL_FRONT)

  # render skybox
  render(mesh1) do (vertex, gl):
    var position_os = vec4(vertex.position_os.xyz, 0)
    let position_cs = V*position_os
    gl.Position = P * position_cs
    ## rasterize
    result.color = texture(skyTexture, vertex.position_os.xyz)

  glEnable(GL_DEPTH_TEST)
  glCullFace(GL_BACK)

  block rendering:
    let beat = floor(time * 2)
    let beatFract = fract(time * 2)
    let subBeat = floor(beat) * 0.25

    let M =
      if int(beat) mod 4 == 0:
        var shearMat = mat4f(1)
        let idx = int(subBeat) mod 6

        if   idx == 0:
          shearmat[0,1] = beatFract
        elif idx == 1:
          shearmat[0,2] = beatFract
        elif idx == 2:
          shearmat[1,0] = beatFract
        elif idx == 3:
          shearmat[1,2] = beatFract
        elif idx == 4:
          shearmat[2,0] = beatFract
        elif idx == 5:
          shearmat[2,1] = beatFract

        shearMat * M
      else:
        var tmp = 1 - beatFract  # + floor(float32(gl.VertexID) / 36.0f) * 1.235f
        tmp *= tmp
        tmp *= tmp
        tmp *= tmp
        tmp = 1 + tmp * 0.125
        M.scale(tmp)

    let oldMode = currentMesh.mode
    if toggleB:
      currentMesh.mode = GL_POINTS

    case renderMode
    of 0:
      currentMesh.render do (vertex, gl):
        var position_os = vertex.position_os
        let position_ws = M*position_os
        let position_cs = V*position_ws
        gl.Position = P * position_cs
        let normal_cs : Vec4f = normalTransform(
          vertex.normal_os,
          vertex.position_os,
          inverse(V*M)
        )


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

    of 1:

      let invV = inverse(V)
      let cameraPos_ws =     invV * vec4f(0,0,0,1)

      currentMesh.render do (vertex, gl):
        var position_os = vertex.position_os
        let position_ws = M*position_os
        let position_cs = V*position_ws
        gl.Position = P * position_cs


        var normal_cs : Vec4f = normalTransform(
          vertex.normal_os,
          vertex.position_os,
          inverse(V*M)
        )

        ## rasterize

        var textureSample = texture(myTexture, vertex.texCoord)
        var dir_cs = vec4(position_cs.xyz, 0)
        dir_cs.xyz = reflect(dir_cs.xyz, normal_cs.xyz)
        let dir_ws = invV * dir_cs
        var skySample = texture(skyTexture, dir_ws.xyz)

        let alpha = (textureSample.r + textureSample.g + textureSample.b) * 0.3333
        result.color = mix(skySample, textureSample, alpha)
    of 2:
      currentMesh.render do (vertex, gl):
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

    of 3:
      currentMesh.render do (vertex, gl):
        var position_os = vec4f(vertex.position_os.xyz, 1)
        let position_ws = M*position_os
        let position_cs = V*position_ws
        gl.Position = P * position_cs

        ## rasterize
        result.color = vertex.normal_os

    of 4:
      currentMesh.render do (vertex, gl):

        var position_os = vec4f(vertex.position_os.xyz, 1)
        let position_ws = M*position_os
        let position_cs = V*position_ws
        gl.Position = P * position_cs

        let normal_cs : Vec4f = normalTransform(
          vertex.normal_os,
          vertex.position_os,
          inverse(V*M)
        )

        ## rasterize

        result.color = normal_cs
    else:
      discard

    currentMesh.mode = oldMode

  glSwapWindow(window)
