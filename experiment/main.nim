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

  # MyFramebuffer = Framebuffer2[MyFragmentType]

genMeshType(MyMesh, MyVertexType)
genMeshType(ControlPointMesh, tuple[position_os: Vec3f])

# let (window, context) = defaultSetup(vec2i(640,480))
let (window, context) = defaultSetup()
discard setRelativeMouseMode(true)

let myTexture: Texture2D = loadTexture2DFromFile(getResourcePath("crate.png"))

type
  Skybox = object
    path: string
    texture: TextureCubeMap

var skyboxes: seq[Skybox]

block readSkyboxesDir:
  for kind, path in walkDir(getResourcePath("skyboxes")):
    if kind == pcDir:
      skyboxes.add Skybox(path: path)

proc loadTextureCubeMapFromDir(path: string): TextureCubeMap =
  let timer = newStopWatch(true)
  var names: array[6,string]
  for kind, path in walkDir(path):
    if kind == pcFile:
      if path[^4] == '.':
        let suffix = toLowerASCII(path[^3 .. ^1])
        if suffix == "tga" or suffix == "jpg" or suffix == "png":

          let suffix = toLowerASCII(path[^6 .. ^5])

          let idx = find(["ft","bk","up","dn","rt","lf"], suffix)
          if idx >= 0:
            names[idx] = path
          else:
            echo "can't do anything with ", path, "."
            echo "It does not end on ft/bk/up/dn/rt/lf, therefore ignored"

  if find(names,"") == -1:
    let path0 = names[0]
    let idx = rfind(path0, '/') + 1
    let name = path0[idx .. ^8]

    result = loadTextureCubeMapFromFiles(names)
  else:
    echo "failed to load texture cube map from dir: ", path

var skyIndex: int

block findInterstellar:
  for i, skybox in skyboxes:
    let path = skybox.path
    if path[rfind(path, '/')+1 ..< path.len] == "interstellar":
      skyIndex = i
      break findInterstellar

var skyTexture: TextureCubeMap

proc lazyLoadSkybox(idx: int): void =
  let path = skyboxes[idx].path
  if skyboxes[idx].texture.handle == 0:
    echo "loading ", path
    var timer = newStopWatch(true)
    skyboxes[idx].texture = loadTextureCubeMapFromDir(path)
    echo timer.time, "s"
  skyTexture = skyboxes[idx].texture
  window.title = path[rfind(path, '/')+1 ..< path.len]

proc nextSky(): void =
  skyIndex =   (skyIndex + 1) mod skyboxes.len
  lazyLoadSkybox(skyIndex)

proc prevSky(): void =
  skyIndex = (skyIndex + skyboxes.high) mod skyboxes.len
  lazyLoadSkybox(skyIndex)

lazyLoadSkybox(skyIndex)

glEnable(GL_CULL_FACE)
glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS)
glEnable(GL_DEPTH_CLAMP)

var mesh1: MyMesh
mesh1.vertexIndices.mode = GL_TRIANGLES
mesh1.vertexIndices.numVertices = boxVerticesCenterAtZero.len
mesh1.buffers.position_os = arrayBuffer(boxVerticesCenterAtZero)
mesh1.buffers.normal_os   = arrayBuffer(boxNormals)
mesh1.buffers.texCoord    = arrayBuffer(boxTexCoords)

var tetraederArrayBuffer = createArrayBuffer[MyVertexType](tetraederVertices.len)
for i, vertex in tetraederArrayBuffer.wPairs:
  vertex.position_os = tetraederVertices[i]
  vertex.normal_os   = tetraederNormals[i]
  vertex.texCoord    = tetraederTexCoords[i]

var mesh2: MyMesh
mesh2.vertexIndices.mode = GL_TRIANGLES
mesh2.vertexIndices.numVertices = tetraederVertices.len
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
  # teapot 11 in 0.060911777s (use precalculated weights also for normals)
  # teapot 11 in 0.074394525s (precalculated weights don't work as expected :[ )
  # teapot 11 in 0.066273446s (reuse one vertex)
  # teapot 11 in 0.058945103s (use precalc powers in normals)

  # fixing teapot normals, very hacky

  var teapotBuffer = arrayBuffer(teapotData.data)

  mesh3.buffers.position_os       = teapotBuffer.view(position_os)
  mesh3.buffers.normal_os         = teapotBuffer.view(normal_os)
  mesh3.buffers.texCoord          = teapotBuffer.view(texCoord)
  mesh3.vertexIndices             = elementArrayBuffer(teapotData.indices)
  mesh3.vertexIndices.mode        = GL_TRIANGLE_STRIP

  var displacedPositions = newSeq[Vec4f](teapotData.data.len)
  for i, vertex in teapotData.data.mpairs:
    displacedPositions[i] = vertex.position_os + vertex.normal_os * 0.05

  mesh4 = mesh3
  mesh4.buffers.position_os = arrayBuffer(displacedPositions)

var lights: array[4,Light]
lights[0].color = vec4f(1,0,1,1)
lights[1].color = vec4f(1,0,0,1)
lights[2].color = vec4f(0,1,0,1)
lights[3].color = vec4f(0,0,1,1)

proc calcBaseLighting(V: Mat4f, position_cs, normal_cs: Vec4f): Vec4f =
  result = vec4f(0.2f)
  for light in lights:
    let light_position_cs = V * light.position_ws
    let light_direction_cs = light_position_cs-position_cs
    let light_distance = length(position_cs - light_position_cs)
    let light_intensity = max(dot(light_direction_cs, normal_cs), 0) * max((10 - light_distance) * 0.1f, 0)
    result += light_intensity * light.color

let timer = newStopWatch(true)
var currentMesh = mesh1

var objRot  = mat4f(1).rotateX(0.5).rotateY(0.75)
var objScale = 1.0f
var viewRot = mat4f(1)
var toggleA, toggleB: bool

var renderMode: int = 0

proc loadAllSkyboxes() =
  for i in 0 ..< skyboxes.len:
    lazyLoadSkybox(i)

loadAllSkyboxes()

import algorithm

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

#[
# DO NOT MULTIPLY BY COS THETA
proc shadingSpecularGGX(N, V, L: Vec3f; roughness: float32; F0: Vec3f): Vec3f =
  # ported from material/shader/glow-material/material-ggx.glsl
  # see http://www.filmicworlds.com/2014/04/21/optimizing-ggx-shaders-with-dotlh/
  let H = normalize(V + L);

  let dotLH = max(dot(L, H), 0.0f);
  let dotNH = max(dot(N, H), 0.0f);
  let dotNL = max(dot(N, L), 0.0f);
  let dotNV = max(dot(N, V), 0.0f);

  let alpha = roughness * roughness;

  # D (GGX normal distribution)
  let alphaSqr = alpha * alpha;
  let denom = dotNH * dotNH * (alphaSqr - 1.0f) + 1.0f;
  let D = alphaSqr / (denom * denom);
  # no pi because BRDF -> lighting

  # F (Fresnel term)
  let F_a = 1.0f;
  let F_b = pow(1.0f - dotLH, 5.0f); # manually?
  let F = mix(vec3f(F_b), vec3f(F_a), F0);

  # G (remapped hotness, see Unreal Shading)
  let k = (alpha + 2 * roughness + 1) / 8.0;
  let G = dotNL / (mix(dotNL, 1, k) * mix(dotNV, 1, k));
  # '* dotNV' - canceled by normalization

  return D * F * G / 4.0;
]#

var runGame = true
while runGame:
  for evt in events():
    case evt.kind:
    of Quit:
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
      of SCANCODE_F6:
        renderMode = 5

      of SCANCODE_DELETE:
        echo "delete this: ", skyboxes[skyIndex].path

      of SCANCODE_SPACE:
        toggleB = not toggleB

      of SCANCODE_KP_PLUS:
        nextSky()

      of SCANCODE_KP_MINUS:
        prevSky()

      else:
        discard
    of MouseWheel:
      let alpha = float32(evt.wheel.x + evt.wheel.y) * 0.05
      objScale *= (1 + alpha)

    of MouseMotion:
      let v = vec2(evt.motion.yrel.float32, evt.motion.xrel.float32)
      let alpha = v.length * 0.01
      let axis = vec3(v, 0)
      let rotMat =
        if alpha > 0 and axis.length2 > 0:
          mat4f(1).rotate(alpha, axis)
        else:
          mat4f(1)

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
          shearMat[0,1] = beatFract
        elif idx == 1:
          shearMat[0,2] = beatFract
        elif idx == 2:
          shearMat[1,0] = beatFract
        elif idx == 3:
          shearMat[1,2] = beatFract
        elif idx == 4:
          shearMat[2,0] = beatFract
        elif idx == 5:
          shearMat[2,1] = beatFract

        shearMat * M
      else:
        var tmp = 1 - beatFract  # + floor(float32(gl.VertexID) / 36.0f) * 1.235f
        tmp *= tmp
        tmp *= tmp
        tmp *= tmp
        tmp = 1 + tmp * 0.125
        M.scale(tmp)

    let oldMode = currentMesh.vertexIndices.mode
    if toggleB:
      currentMesh.vertexIndices.mode = GL_POINTS

    case renderMode
    of 0:
      currentMesh.renderDebug do (vertex, gl):
        var position_os = vertex.position_os
        let position_ws = M*position_os
        let position_cs = V*position_ws
        gl.Position = P * position_cs
        let normal_cs : Vec4f = normalTransform(
          vertex.normal_os,
          vertex.position_os,
          inverse(V*M)
        )

        let lighting = calcBaseLighting(V, position_cs, normal_cs)

        ## rasterize

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

        let fresnel = max(normal_cs.z,0)

        ## rasterize

        var textureSample = texture(myTexture, vertex.texCoord)
        var dir_cs = vec4(position_cs.xyz, 0)
        dir_cs.xyz = reflect(dir_cs.xyz, normal_cs.xyz)
        let dir_ws = invV * dir_cs
        var skySample = texture(skyTexture, dir_ws.xyz)

        result.color = mix(skySample, vec4(vec3(fresnel), 1.0f), fresnel)
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

    currentMesh.vertexIndices.mode = oldMode

  glSwapWindow(window)

# Local Variables:
# compile-command: "cd experiment; nim c -r main.nim"
# End:
