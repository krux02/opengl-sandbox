import ../fancygl

let (window, context) = defaultSetup()
let windowsize = vec2f(window.size)

const
  numParticles   = 2000
  maxParticleAge = 16.0'f64

type
  ParticleData = object
    pos: Vec2f
    vel: Vec2f
    col: Vec3f
    rot: float32
    birthday: float64

var cpuParticleData = newSeq[ParticleData](numParticles)

for particle in cpuParticleData.mitems():
  particle.pos = vec2f(rand_f32(), rand_f32()) * windowsize
  particle.col = vec3f(rand_f32(), rand_f32(), rand_f32())
  particle.rot = randNormal().float32 + 2
  particle.vel.x = randNormal().float32 * 32
  particle.vel.y = randNormal().float32 * 32
  particle.birthday = - rand_f64() * maxParticleAge

var particleData           = cpuParticleData.arrayBuffer(GL_STREAM_DRAW)
var particleTarget         = cpuParticleData.arrayBuffer(GL_STREAM_DRAW)

let
  posView      = particleData.view(pos)
  colView      = particleData.view(col)
  rotView      = particleData.view(rot)
  velView      = particleData.view(vel)
  birthdayView = particleData.view(birthday)

  posTargetView = particleTarget.view(pos)
  colTargetView = particleTarget.view(col)
  rotTargetView = particleTarget.view(rot)
  velTargetView = particleTarget.view(vel)
  birthdayTargetView = particleTarget.view(birthday)

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

  for particle in cpuParticleData.mitems:
    
    particle.pos += particle.vel * frameTime.float32

    let flipX = 1'f32 - float32(windowsize.x < particle.pos.x or particle.pos.x < 0) * 2
    let flipY = 1'f32 - float32(windowsize.y < particle.pos.y or particle.pos.y < 0) * 2

    particle.pos = clamp(particle.pos, vec2f(0), windowsize)

    particle.vel.x = particle.vel.x * flipX
    particle.vel.y = particle.vel.y * flipY
      
    if particle.birthday + maxParticleAge < time:    
      particle.birthday = time
      particle.vel.x = 128 * dirx + randNormal() * 16
      particle.vel.y = 128 * diry + randNormal() * 16
      particle.pos   = mix(vec2f(pmouseX.float32, pmouseY.float32), vec2f(mouseX.float32, mouseY.float32), rand_f32())

  particleData.setData(cpuParticleData)

  #glBeginTransformFeedback(GL_POINTS)
  
  shadingDsl(GL_POINTS):
    debugResult
    numVertices = numParticles

    programIdent = tfProgram
    vaoIdent = tfVao

    uniforms:
      mouse = vec2f(mouseX.float32,mouseY.float32)
      pmouse = vec2f(pmouseX.float32,pmouseY.float32)
      frameTime = frameTime.float32
      windowsize = windowsize
      time
      maxParticleAge
      dir = vec2f(dirx,diry)

    attributes:
      a_pos = posView
      a_col = colView
      a_rot = rotView

      a_vel = velView
      a_birthday = birthdayView

    vertexMain:
      """
      v_pos = a_pos + a_vel * frameTime;

      //bool flipX = 1.0 - float(windowsize.x < v_pos.x or v_pos.x < 0) * 2.0;
      //bool flipY = 1.0 - float(windowsize.y < v_pos.y or v_pos.y < 0) * 2.0;
      vec2 flip = vec2(1) - vec2( notEqual(lessThan(windowsize, v_pos),lessThan(v_pos, vec2(0)))) * 2.0 ;

      v_pos = clamp(v_pos, vec2(0), windowsize);
      v_vel = a_vel * flip;

      if(a_birthday + maxParticleAge < time) {
        v_birthday = time;
        v_vel = 128 * dir;
        v_pos = mix( pmouse, mouse, 0.5 );
      } else {
        v_birthday = a_birthday;
        v_vel = a_vel;
      }
      """

    vertexOut:
      v_pos = posTargetView
      v_col = colTargetView
      v_rot = rotTargetView
      v_vel = velTargetView
      v_birthday = birthdayTargetView

    fragmentMain:
      """
      color = vec4(1);
      """

  
  #glEndTransformFeedback()

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

  glEnable(GL_BLEND)

  shadingDsl(GL_POINTS):
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

  shadingDsl(GL_POINTS):
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

    
    

