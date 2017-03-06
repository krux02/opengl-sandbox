import ../fancygl

let (window, context) = defaultSetup()
let windowsize = vec2f(window.size)

const
  numParticles   = 5000
  maxParticleAge = 16.0'f64

type ParticleRenderData = object
  pos: Vec2f
  col: Vec3f
  rot: float32


var cpuParticleRenderData = newSeq[ParticleRenderData](numParticles)

for particle in cpuParticleRenderData.mitems():
  particle.pos = vec2f(rand_f32(), rand_f32()) * windowsize
  particle.col = vec3f(rand_f32(), rand_f32(), rand_f32())
  particle.rot = randNormal().float32 + 2

var particleRenderData = cpuParticleRenderData.arrayBuffer(GL_STREAM_DRAW)

let
  posView = particleRenderData.view(pos)
  colView = particleRenderData.view(col)
  rotView = particleRenderData.view(rot)

type ParticleSimulationData = object
  vel: Vec2f
  birthday: float64

var particleSimulationData = newSeq[ParticleSimulationData](numParticles)

for particle in particleSimulationData.mitems():
  particle.vel.x = randNormal().float32 * 32
  particle.vel.y = randNormal().float32 * 32
  particle.birthday = - rand_f64() * maxParticleAge


type
  ParticleRef = object
    simulationData: ptr ParticleSimulationData
    renderData: ptr ParticleRenderData

proc pos(this: ParticleRef): var Vec2f =
  this.renderData.pos
proc col(this: ParticleRef): var Vec3f =
  this.renderData.col
proc rot(this: ParticleRef): var float32 =
  this.renderData.rot
proc vel(this: ParticleRef): var Vec2f =
  this.simulationData.vel
proc birthday(this: ParticleRef): var float64 =
  this.simulationData.birthday

var running = true
var gameTimer = newStopWatch(true)
var frameTimer = newStopWatch(true)

var mvp : Mat4f

mvp[0][0] = 2 / windowsize.x
mvp[1][1] = 2 / windowsize.y
mvp[3] = vec4f(-1, -1, 0, 1)

glDisable(GL_DEPTH_TEST)
glEnable(GL_BLEND)
glBlendFunc(GL_SRC_ALPHA, GL_ONE)

var mouseX, mouseY: int32
var pmouseX, pmouseY: int32

while running:
  let frameTime = frameTimer.reset
  let time = gameTimer.time

  pmouseX = mouseX
  pmouseY = mouseY
  discard getMouseState(mouseX.addr, mouseY.addr)
  mouseY = windowsize.y.int32 - mouseY

  let dirx = float32(cos(time * 5))
  let diry = float32(sin(time * 5))

  for i in 0 ..< numParticles:
    let particle = ParticleRef(simulationData: particleSimulationData[i].addr, renderData: cpuParticleRenderData[i].addr)

    particle.pos += particle.vel * frameTime.float32

    let flipX = 1'f32 - float32(windowsize.x < particle.pos.x or particle.pos.x < 0) * 2
    let flipY = 1'f32 - float32(windowsize.y < particle.pos.y or particle.pos.y < 0) * 2

    particle.pos() = clamp(particle.pos, vec2f(0), windowsize)

    particle.vel.x = particle.vel.x * flipX
    particle.vel.y = particle.vel.y * flipY

    if particle.birthday + maxParticleAge < time:
      particle.birthday() = time
      particle.vel.x = 128 * dirx + randNormal() * 16
      particle.vel.y = 128 * diry + randNormal() * 16
      particle.pos() = mix(vec2f(pmouseX.float32, pmouseY.float32), vec2f(mouseX.float32, mouseY.float32), rand_f32())

  particleRenderData.setData(cpuParticleRenderData)

  var evt = defaultEvent
  while evt.pollEvent:
    if evt.kind == QuitEvent:
      running = false
      break
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SDL_SCANCODE_ESCAPE:
        running = false
        break
      of SDL_SCANCODE_F10:
        window.screenshot
      of SDL_SCANCODE_PAUSE:
        gameTimer.toggle

      else:
        discard

  glClear(GL_COLOR_BUFFER_BIT)

  glEnable(GL_BLEND)

  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices = numParticles

    uniforms:
      time = float32(time+100)
      mvp
      particleSize = vec2f(16,4)

    attributes:
      a_pos = posView
      a_color = colView
      a_rot  = rotView

    vertexMain:
      """
      v_pos = a_pos;
      v_color = vec4(a_color,0.25);
      v_rot = a_rot;
      """
    vertexOut:
      "out vec4 v_color"
      "out vec2 v_pos"
      "out float v_rot"
      "flat out float v_id"
    geometryMain:
      "layout(triangle_strip, max_vertices=4) out"
      """
      g_color = v_color[0];

      float s = sin(time * v_rot[0]);
      float c = cos(time * v_rot[0]);

      mat2 r = mat2(vec2(c,s),vec2(-s,c));

      gl_Position = mvp * vec4(v_pos[0] + r * (particleSize * vec2(-1,-1)), 0, 1);
      EmitVertex();

      gl_Position = mvp * vec4(v_pos[0] + r * (particleSize * vec2( 1,-1)), 0, 1);
      EmitVertex();

      gl_Position = mvp * vec4(v_pos[0] + r * (particleSize * vec2(-1, 1)), 0, 1);
      EmitVertex();

      gl_Position = mvp * vec4(v_pos[0] + r * (particleSize * vec2( 1, 1)), 0, 1);
      EmitVertex();
      """
    geometryOut:
      "out vec4 g_color"
    fragmentMain:
      """
      color = g_color;
      """

  glDisable(GL_BLEND)

  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices = numParticles

    uniforms:
      time = float32(time+100)
      mvp
      particleSize = vec2f(16,4)

    attributes:
      a_pos = posView
      a_color = colView
      a_rot  = rotView

    vertexMain:
      """
      v_pos = a_pos;
      v_color = vec4(a_color,0.25);
      v_rot = a_rot;
      """
    vertexOut:
      "out vec4 v_color"
      "out vec2 v_pos"
      "out float v_rot"
      "flat out float v_id"
    geometryMain:
      "layout(line_strip, max_vertices=5) out"
      """
      g_color = v_color[0];

      float s = sin(time * v_rot[0]);
      float c = cos(time * v_rot[0]);

      mat2 r = mat2(vec2(c,s),vec2(-s,c));

      gl_Position = mvp * vec4(v_pos[0] + r * (particleSize * vec2(-1,-1)), 0, 1);
      EmitVertex();

      gl_Position = mvp * vec4(v_pos[0] + r * (particleSize * vec2( 1,-1)), 0, 1);
      EmitVertex();

      gl_Position = mvp * vec4(v_pos[0] + r * (particleSize * vec2( 1, 1)), 0, 1);
      EmitVertex();

      gl_Position = mvp * vec4(v_pos[0] + r * (particleSize * vec2(-1, 1)), 0, 1);
      EmitVertex();

      gl_Position = mvp * vec4(v_pos[0] + r * (particleSize * vec2(-1,-1)), 0, 1);
      EmitVertex();

      """
    geometryOut:
      "out vec4 g_color"
    fragmentMain:
      """
      color = g_color;
      """

  glSwapWindow(window)
