import ../fancygl


proc printf(frmt: cstring): cint {.importc: "printf", header: "<stdio.h>", nodecl, varargs.}

## Transform Feedback is not yet properly integrated

const
  numParticles   = 2000
  maxParticleAge = 16.0'f32

type
  ParticleDataA = object
    pos: Vec2f
    vel: Vec2f
    birthday: float32
    padding: float32

  ParticleDataB = object
    col: Vec3f
    rot: float32

let (window, _) = defaultSetup()
let windowsize = window.size.vec2f

var particleData, particleTarget: ArrayBuffer[ParticleDataA]
var particleDataB: ArrayBuffer[ParticleDataB]

block:
  var particlesSeq = newSeq[particleData.T](numParticles)

  for particle in particlesSeq.mitems:
    particle.pos = vec2f(rand_f32(), rand_f32()) * windowsize
    particle.vel.x = randNormal().float32
    particle.vel.y = randNormal().float32
    particle.birthday = -rand_f32() * maxParticleAge

  particleData = arrayBuffer(particlesSeq, GL_STATIC_COPY)
  particleTarget = arrayBuffer(particlesSeq, GL_STATIC_COPY)

  var particlesBSeq = newSeq[particleDataB.T](numParticles)
  for particle in particlesBSeq.mitems:
    particle.col = vec3f(rand_f32(), rand_f32(), rand_f32())
    particle.rot = randNormal().float32 + 2

  particleDataB = arrayBuffer(particlesBSeq, GL_STATIC_DRAW)

# this needs to be a template, because otherwise

template  posView:untyped      = particleData.view(pos)
template  velView:untyped      = particleData.view(vel)
template  birthdayView:untyped = particleData.view(birthday)
template  colView:untyped      = particleDataB.view(col)
template  rotView:untyped      = particleDataB.view(rot)

var running    = true
var gameTimer  = newStopWatch(true)

var mvp : Mat4f

mvp[0][0] = 2 / windowsize.x
mvp[1][1] = 2 / windowsize.y
mvp[3]    = vec4f(-1, -1, 0, 1)

glDisable(GL_DEPTH_TEST)
glEnable(GL_BLEND)
glBlendFunc(GL_SRC_ALPHA, GL_ONE)

var mouseX  , mouseY  : int32
var pmouseX , pmouseY : int32
var transformFeedback = createTransformFeedback[ParticleDataA]()

var pdir, dir: Vec2f

while running:
  let time = gameTimer.time.float32

  for evt in events():
    if evt.kind == QUIT:
      running = false
      break
    if evt.kind == KEY_DOWN:
      case evt.key.keysym.scancode
      of SCANCODE_ESCAPE:
        running = false
        break
      of SCANCODE_PAUSE:
        gameTimer.toggle

      else:
        discard

  pmouseX = mouseX
  pmouseY = mouseY
  discard getMouseState(mouseX.addr, mouseY.addr)
  mouseY = windowsize.y.int32 - mouseY

  pdir = dir
  dir.x = float32(cos(time * 5))
  dir.y = float32(sin(time * 5))

  glClear(GL_COLOR_BUFFER_BIT)

  transformFeedback.bufferBase(0, particleTarget)

  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices   = numParticles
    programIdent  = tfProgram
    vaoIdent      = tfVao

    uniforms:
      mouse      = vec2f(mouseX.float32,  mouseY.float32)
      pmouse     = vec2f(pmouseX.float32,  pmouseY.float32)
      windowsize = windowsize
      time
      maxParticleAge
      numParticles
      pdir
      dir

    attributes:
      a_pos = posView
      a_vel = velView
      a_birthday = birthdayView

      #a_color = colView
      #a_rot   = rotView


    afterSetup:
      discard

    beforeRender:

      glEnable(GL_RASTERIZER_DISCARD);
      glBindTransformFeedback(GL_TRANSFORM_FEEDBACK, transformFeedback.handle)
      glBeginTransformFeedback(GL_POINTS)

    afterRender:

      glEndTransformFeedback()
      glBindTransformFeedback(GL_TRANSFORM_FEEDBACK, 0)
      glDisable(GL_RASTERIZER_DISCARD);

    vertexSrc:
      """

layout(xfb_buffer = 0, xfb_stride = 24) out ParticleDataA {
  layout(xfb_offset = 0) vec2 pos;
  layout(xfb_offset = 8) vec2 vel;
  layout(xfb_offset = 16) float birthday;
  layout(xfb_offset = 20) float padding;
};

void main() {

  if(a_birthday + maxParticleAge < time) {
    float a = float(gl_VertexID) * (1.0 / float(numParticles));
    pos      = mix(pmouse,mouse, a);
    vel = mix(pdir, dir, a);
    birthday = time;
  } else {
    birthday = a_birthday;

    pos = a_pos + a_vel;

    vec2 flip;
    flip.x = float(windowsize.x < pos.x || pos.x < 0) * -2 + 1;
    flip.y = float(windowsize.y < pos.y || pos.y < 0) * -2 + 1;

    pos = clamp(pos, vec2(0), windowsize);
    vel = a_vel * flip;
  }
}
      """

  swap particleData, particleTarget

  glEnable(GL_BLEND)

  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices   = numParticles

    uniforms:
      mvp
      particleSize = vec2f(16,4)

    attributes:
      a_pos   = posView
      a_color = colView
      a_rot   = rotView

    vertexMain:
      """
      v_pos   = a_pos;
      v_color = vec4(a_color,0.25);
      v_rot   = a_rot;
      """
    vertexOut:
      "out vec4 v_color"
      "out vec2 v_pos"
      "out float v_rot"
      "flat out float v_id"
    geometryMain:
      "layout(triangle_strip, max_vertices=4) out"
      """
      g_color     = v_color[0];
      float s     = sin(v_rot[0]);
      float c     = cos(v_rot[0]);

      mat2 r      = mat2(vec2(c,s),vec2(-s,c));

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
      color       = g_color;
      """

  glDisable(GL_BLEND)

  shadingDsl:
    primitiveMode = GL_POINTS
    numVertices   = numParticles

    uniforms:
      mvp
      particleSize = vec2f(16,4)

    attributes:
      a_pos   = posView
      a_color = colView
      a_rot   = rotView

    vertexMain:
      """
      v_pos   = a_pos;
      v_color = vec4(a_color,0.25);
      v_rot   = a_rot;
      """
    vertexOut:
      "out vec4 v_color"
      "out vec2 v_pos"
      "out float v_rot"
      "flat out float v_id"
    geometryMain:
      "layout(line_strip, max_vertices=5) out"
      """
      g_color     = v_color[0];
      float s     = sin(v_rot[0]);
      float c     = cos(v_rot[0]);

      mat2 r      = mat2(vec2(c,s),vec2(-s,c));

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
