import renderMacro

#import glm/noise

let (window, context) = defaultSetup(vec2i(640,480))
#let (window, context) = defaultSetup()

type
  MyFragmentType = object
    color: Vec4f # {. GL_RGB16F .}

  MyVertexType = tuple
    position: Vec4f
    color: Vec4f

  #MyMesh = Mesh[MyVertexType
genMeshType(MyMesh, MyVertexType)

var triangleMesh: MyMesh
triangleMesh.vertexIndices.mode = GL_TRIANGLES
triangleMesh.vertexIndices.numVertices = 3

triangleMesh.buffers.position = arrayBuffer([
  vec4f(-1,-1, 0, 1),
  vec4f( 3,-1, 0, 1),
  vec4f(-1, 3, 0, 1)
])

triangleMesh.buffers.color = arrayBuffer([
  vec4f( 1, 0,0,1),
  vec4f(0, 1,0,1),
  vec4f(0,0,1,1)
])

proc noiseTexture2D*(size: Vec2i): Texture2D =
  let surface = createRGBSurface(0, size.x, size.y, 32, 0,0,0,0)
  if surface.isNil:
    panic "SDL_CreateRGBSurface() failed: ", getError()
  defer:
    freeSurface(surface)

  let pixels = cast[ptr UncheckedArray[uint32]](surface.pixels)
  for i in 0 ..< size.x * size.y:
    pixels[i] = rand_u32()

  result = texture2D(surface)
  result.parameter(GL_TEXTURE_WRAP_S, GL_REPEAT)
  result.parameter(GL_TEXTURE_WRAP_T, GL_REPEAT)
  result.parameter(GL_TEXTURE_WRAP_R, GL_REPEAT)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  result.generateMipmap
  result.label = "RGBA noise"



let iChannel0 = noiseTexture2D(vec2i(256))
let iChannelResolution = [vec2f(256)]
var iMouse = vec4f(0)
let iResolution = vec2f(window.size)
var iTime = 0.0'f32

###################
### shader code ###
###################


# Remnant X
# by David Hoskins.
# Thanks to boxplorer and the folks at 'Fractalforums.com'
# HD Video:- https://www.youtube.com/watch?v=BjkK9fLXXo0

const STEREO = false

let sunDir = normalize( vec3f(0.35, 0.1,  0.3 ))
const sunColour = vec3f(1.0, 0.95, 0.8)


const SCALE = 2.8f
const MINRAD2 = 0.25f
const minRad2 = clamp(MINRAD2, 1.0e-9f, 1.0f);
const scale = (vec4f(SCALE, SCALE, SCALE, abs(SCALE)) / minRad2)
let absScalem1: float32 = abs(SCALE - 1.0);
let AbsScaleRaisedTo1mIters: float32 = pow(abs(SCALE), float32(1-10));
var surfaceColour1 = vec3f(0.8, 0.0, 0.0);
var surfaceColour2 = vec3f(0.4, 0.4, 0.5);
var surfaceColour3 = vec3f(0.5, 0.3, 0.00);
var fogCol = vec3f(0.4, 0.4, 0.4);
var gTime: float32;

#----------------------------------------------------------------------------------------
proc Noise(x: Vec3f): float32 =
  let p = floor(x)
  var f = fract(x)
  f = f*f*(3.0'f32 - 2.0'f32 * f)
  let uv = (p.xy+vec2f(37.0,17.0)*p.z) + f.xy;
  let rg: Vec2f = texture( iChannel0, (uv+ 0.5)/256.0, -99.0 ).yx;
  return mix( rg.x, rg.y, f.z )


#----------------------------------------------------------------------------------------

proc Map(pos: Vec3f): float32 =
  var p = vec4f(pos,1);
  let p0 = p;  # p.w is the distance estimate

  for i in 0'i32 ..< 9'i32:
    p.xyz = clamp(p.xyz, -1.0, 1.0) * 2.0 - p.xyz

    let r2 = dot(p.xyz, p.xyz)
    p *= clamp(max(minRad2/r2, minRad2), 0.0, 1.0)

    # scale, translate
    p = p * scale + p0

  return ((length(p.xyz) - absScalem1) / p.w - AbsScaleRaisedTo1mIters)

#----------------------------------------------------------------------------------------
proc Colour(pos: Vec3f, sphereR: float32): Vec3f =
  var p = pos
  let p0 = p
  var trap: float32 = 1.0;

  for i in 0 ..< 6:
    p.xyz = clamp(p.xyz, -1.0, 1.0) * 2.0 - p.xyz;
    let r2 = dot(p.xyz, p.xyz);
    p *= clamp(max(minRad2/r2, minRad2), 0.0, 1.0);

    p = p*scale.xyz + p0.xyz;
    trap = min(trap, r2);

  let c = clamp(vec2( 0.3333*ln(dot(p,p))-1.0, sqrt(trap) ), 0.0, 1.0)
  let t = floorMod(length(pos) - gTime*150'f32, 16'f32);
  surfaceColour1 = mix( surfaceColour1, vec3f(0.4, 3.0, 5.0), pow(smoothstep(0.0'f32, 0.3'f32, t) * smoothstep(0.6'f32, 0.3'f32, t), 10.0));
  return mix(mix(surfaceColour1, surfaceColour2, c.y), surfaceColour3, c.x);



#----------------------------------------------------------------------------------------

proc GetNormal(pos: Vec3f, distance: float32): Vec3f =
  let distance = distance * (0.001'f32 + 0.0001'f32);
  let eps = vec2f(distance, 0.0);
  let nor = vec3f(
    Map(pos+eps.xyy) - Map(pos-eps.xyy),
    Map(pos+eps.yxy) - Map(pos-eps.yxy),
    Map(pos+eps.yyx) - Map(pos-eps.yyx)
  )
  return normalize(nor)

#----------------------------------------------------------------------------------------

proc GetSky(pos: Vec3f): float32 =
  let pos = pos * 2.3;
  var t: float32 = Noise(pos);
  t += Noise(pos * 2.1) * 0.5;
  t += Noise(pos * 4.3) * 0.25;
  t += Noise(pos * 7.9) * 0.125;
  return t;

#----------------------------------------------------------------------------------------
proc BinarySubdivision(rO: Vec3f, rD: Vec3f, t: Vec2f): float32 =
  var t = t
  var halfwayT: float32

  for i in 0 ..< 6:
    halfwayT = dot(t, vec2f(0.5))
    let d = Map(rO + halfwayT*rD)
    #if (abs(d) < 0.001) break;
    t = mix(vec2(t.x, halfwayT), vec2(halfwayT, t.y), step(0.0005, d))

  return halfwayT

#----------------------------------------------------------------------------------------

proc Scene(rO: Vec3f, rD: Vec3f, fragCoord: Vec2f): Vec2f =

  var t: float32 = 0.05'f32 + 0.05'f32 * texture(iChannel0, fragCoord.xy / iChannelResolution[0].xy).y
  var p = vec3f(0.0)
  var oldT = 0.0'f32
  var hit = false
  var glow: float32 = 0.0'f32
  var dist: Vec2f
  for i in 0 ..< 100:
    if t > 12.0:
      break;
    p = rO + t*rD;

    let h = Map(p)

    if h < 0.0005:
      dist = vec2f(oldT, t)
      hit = true;
      break;

    glow += clamp(0.05'f32-h, 0.0, 0.4'f32);
    oldT = t;
    t +=  h + t*0.001;

  if not hit:
    t = 1000.0;
  else:
    t = BinarySubdivision(rO, rD, dist)
  return vec2f(t, clamp(glow * 0.25'f32, 0.0'f32, 1.0'f32))


#----------------------------------------------------------------------------------------
proc Hash(p: Vec2f): float32 =
  return fract(sin(dot(p, vec2f(12.9898'f32, 78.233'f32))) * 33758.5453'f32) - 0.5'f32

#----------------------------------------------------------------------------------------
proc PostEffects(rgb: Vec3f, xy: Vec2): Vec3f =
  # Gamma first...
  # Then...
  const CONTRAST = 1.08'f32
  const SATURATION = 1.5'f32
  const BRIGHTNESS = 1.5'f32
  var rgb = mix(vec3f(0.5'f32), mix(vec3f(dot(vec3f(0.2125'f32, 0.7154'f32, 0.0721'f32), rgb*BRIGHTNESS)), rgb*BRIGHTNESS, SATURATION), CONTRAST)
  # Noise...
  #rgb = clamp(rgb+Hash(xy*iTime)*.1, 0.0, 1.0);
  # Vignette...
  rgb *= 0.5'f32 + 0.5'f32 * pow(20.0*xy.x*xy.y*(1.0'f32-xy.x)*(1.0'f32-xy.y), 0.2'f32)
  rgb = pow(rgb, vec3f(0.47'f32))
  return rgb

#----------------------------------------------------------------------------------------
proc Shadow(ro: Vec3f; rd: Vec3f): float32 =
  var res = 1.0'f32
  var t = 0.05'f32
  var h: float32

  for i in 0 ..< 8:
    h = Map( ro + rd*t )
    res = min(6.0*h / t, res)
    t += h;

  return max(res, 0.0)


#----------------------------------------------------------------------------------------
proc RotationMatrix(axis: Vec3f; angle: float32): Mat3f =
  let axis = normalize(axis)
  let s: float32 = sin(angle)
  let c: float32 = cos(angle)
  let oc: float32 = 1.0 - c

  result[0] = vec3f(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s)
  result[1] = vec3f(oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s)
  result[2] = vec3f(oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c)

#----------------------------------------------------------------------------------------
proc LightSource(spotLight: Vec3f, dir: Vec3f, dis: float32): Vec3f =
  var g = 0.0'f32
  if length(spotLight) < dis:
    g = pow(max(dot(normalize(spotLight), dir), 0.0'f32), 500.0'f32)

  return vec3f(0.6'f32) * g

#----------------------------------------------------------------------------------------

proc CameraPath(t: float32): Vec3f =
  return vec3f(-0.78 + 3.0 * sin(2.14*t), 0.05+2.5 * sin(0.942*t+1.3),0.05 + 3.5 * cos(3.594*t))

#----------------------------------------------------------------------------------------
proc mainImage(fragColor: var Vec4f; fragCoord: Vec2f) =

  let m: float32 = (iMouse.x/iResolution.x)*300.0'f32;
  gTime = (iTime+m) * 0.01'f32 + 15.00'f32;
  let xy: Vec2f = fragCoord.xy / iResolution.xy;
  let uv: Vec2f = (-1.0'f32 + 2.0'f32 * xy) * vec2(iResolution.x/iResolution.y, 1.0'f32);

  when STEREO:
    let isRed = mod(fragCoord.x + mod(fragCoord.y, 2.0'f32),2.0'f32);

  let cameraPos: Vec3f  = CameraPath(gTime);
  let camTar: Vec3f    = CameraPath(gTime + 0.01'f32);

  let roll: float32 = 13.0'f32*sin(gTime*0.5'f32 + 0.4'f32);
  var cw: Vec3f = normalize(camTar-cameraPos);

  let cp: Vec3f = vec3f(sin(roll), cos(roll),0.0'f32);
  let cu: Vec3f = normalize(cross(cw,cp));

  let cv: Vec3f = normalize(cross(cu,cw));
  cw = RotationMatrix(cv, sin(-gTime*20.0'f32)*0.7'f32) * cw;
  let dir: Vec3f = normalize(uv.x*cu + uv.y*cv + 1.3'f32*cw);

  when STEREO:
    cameraPos += 0.008*cu*isRed; # move camera to the right

  let spotLight = CameraPath(gTime + 0.03'f32) + vec3f(sin(gTime*18.4'f32), cos(gTime*17.98'f32), sin(gTime * 22.53'f32))*0.2'f32;
  var col = vec3f(0.0);
  let sky = vec3f(0.03, 0.04, 0.05) * GetSky(dir);
  let ret = Scene(cameraPos, dir,fragCoord);

  if ret.x < 900.0'f32:
    let p = cameraPos + ret.x*dir;
    let nor = GetNormal(p, ret.x);

    var spot = spotLight - p;
    var atten = length(spot);

    spot /= atten;

    let shaSpot = Shadow(p, spot);
    let shaSun = Shadow(p, sunDir);

    let bri = max(dot(spot, nor), 0.0) / pow(atten, 1.5) * 0.15;
    let briSun = max(dot(sunDir, nor), 0.0) * 0.3;

    col = Colour(p, ret.x);
    col = (col * bri * shaSpot) + (col * briSun* shaSun);

    let refl = reflect(dir, nor);
    col += pow(max(dot(spot,  refl), 0.0'f32), 10.0'f32) * 2.0'f32 * shaSpot * bri;
    col += pow(max(dot(sunDir, refl), 0.0'f32), 10.0'f32) * 2.0'f32 * shaSun  * bri;

  col = mix(sky, col, min(exp(-ret.x+1.5'f32), 1.0'f32));
  col += vec3f(pow(abs(ret.y), 2.0)) * vec3f(0.02'f32, 0.04'f32, 0.1'f32);

  col += LightSource(spotLight-cameraPos, dir, ret.x);
  col = PostEffects(col, xy);

  when STEREO:
    col *= vec3f( isRed, 1.0'f32-isRed, 1.0'f32-isRed )

  fragColor=vec4f(col,1.0'f32)

#--------------------------------------------------------------------------

#######################
### shader code end ###
#######################

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

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  triangleMesh.render do (vertex, gl):
    gl.Position = vertex.position
    ## rasterize
    # result.color = vertex.color
    mainImage(result.color, gl.FragCoord.xy)
    #result.color.rgb = vec3f(simplex((modelMat * vertex.position).xyz))

  glSwapWindow(window)
