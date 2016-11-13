import ../fancygl, arnelib

var windowsize = vec2f(1024,768)
let (window, context) = defaultSetup(windowsize)

const
  numParticles   = 16000
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

proc modulo(x,y: float32): float32 =
  x - y * floor(x / y)

proc modulo(v1,v2: Vec2f): Vec2f =
  result.x = modulo(v1.x, v2.x)
  result.y = modulo(v1.y, v2.y)

while running:
  let frameTime = frameTimer.reset
  let time = gameTimer.time

  for i, renderData in cpuParticleRenderData.mpairs():
    template simData : untyped = particleSimulationData[i]
    
    renderData.pos += simData.vel * frameTime.float32

    renderData.pos = clamp(renderData.pos, vec2f(0), windowsize)

    let flipX = float32(windowsize.x < renderData.pos.x or renderData.pos.x < 0) * 2 - 1;
    let flipY = float32(windowsize.y < renderData.pos.y or renderData.pos.x < 0) * 2 - 1;

    simData.vel = simData.vel * vec2f(flipX, flipY)
    if i == 17:
      echo simData.birthday
      echo renderData.pos
      
    if simData.birthday + maxParticleAge < time:    
      simData.birthday = time
      simData.vel.x = 32
      simData.vel.y = 128
      renderData.pos = vec2f(windowsize.x / 2, 20)


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
      of SDL_SCANCODE_PAUSE:
        gameTimer.toggle

      else:
        discard

  glClear(GL_COLOR_BUFFER_BIT)

  shadingDsl(GL_POINTS):
    numVertices = numParticles

    uniforms:
      time = float32(time)
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

  glSwapWindow(window)

    
    

