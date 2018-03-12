import ../fancygl

## Transform Feedback is net yet properly integrated yet.

const
  numParticles   = 2000
  maxParticleAge = 16.0'f32

type
  ParticleData = object
    pos: Vec2f
    vel: Vec2f
    col: Vec3f
    rot: float32
    birthday: float32

type
  TransformFeedback_ParticleData = object
    handle*: GLuint
    buffer*: ArrayBuffer[ParticleData]


let (window, _) = defaultSetup()
let windowsize = window.size.vec2f

var cpuParticleData = newSeq[ParticleData](numParticles)

for particle in cpuParticleData.mitems():
  particle.pos = vec2f(rand_f32(), rand_f32()) * windowsize
  particle.col = vec3f(rand_f32(), rand_f32(), rand_f32())
  particle.rot = randNormal().float32 + 2
  particle.vel.x = randNormal().float32
  particle.vel.y = randNormal().float32
  particle.birthday = - rand_f32() * maxParticleAge

var particleData           = cpuParticleData.arrayBuffer(GL_STREAM_DRAW)
var particleTarget         = cpuParticleData.arrayBuffer(GL_STREAM_DRAW)

# this needs to be a template, because otherwise
template  posView:untyped      = particleData.view(pos)
template  colView:untyped      = particleData.view(col)
template  rotView:untyped      = particleData.view(rot)
template  velView:untyped      = particleData.view(vel)
template  birthdayView:untyped = particleData.view(birthday)

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

echo offsetOf(ParticleData, pos)

var transformFeedback = newTransformFeedback[ParticleData]()

echo transformFeedback
echo transformFeedback.label

while running:
  let time = gameTimer.time.float32

  pmouseX = mouseX
  pmouseY = mouseY
  discard getMouseState(mouseX.addr, mouseY.addr)
  mouseY = windowsize.y.int32 - mouseY

  var dir: Vec2f
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
      dir

    attributes:
      a_pos = posView
      a_col = colView
      a_rot = rotView
      a_vel = velView
      a_birthday = birthdayView

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
layout(xfb_buffer = 0, xfb_stride = 36) out ParticleData {
  layout(xfb_offset = 0) vec2 pos;
  layout(xfb_offset = 8) vec2 vel;
  layout(xfb_offset = 16) vec3 col;
  layout(xfb_offset = 28) float rot;
  layout(xfb_offset = 32) float birthday;
};

void main() {
  col = a_col;
  rot = a_rot + length(a_vel) * 0.1;
  birthday = a_birthday;
  pos = a_pos + a_vel;

  vec2 flip;
  flip.x = float(windowsize.x < pos.x || pos.x < 0) * -2 + 1;
  flip.y = float(windowsize.y < pos.y || pos.y < 0) * -2 + 1;
  vel = a_vel * flip;

  pos = clamp(pos, vec2(0), windowsize);

  if(a_birthday + maxParticleAge < time) {
    birthday = time;
    pos      = mix(pmouse,mouse, float(gl_VertexID) * (1.0 / float(numParticles)));
    vel = dir;
  }
}
      """

  swap particleData, particleTarget

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


  glEnable(GL_BLEND)

  shadingDsl:
    #debug
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
    #debug
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
