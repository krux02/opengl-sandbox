import ../fancygl

## WARNING THIS CODE DOES NOT WORK ##

let (window, _) = defaultSetup()
let windowsize = window.size.vec2f

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

var cpuParticleData = newSeq[ParticleData](numParticles)

for particle in cpuParticleData.mitems():
  particle.pos = vec2f(rand_f32(), rand_f32()) * windowsize
  particle.col = vec3f(rand_f32(), rand_f32(), rand_f32())
  particle.rot = randNormal().float32 + 2
  particle.vel.x = randNormal().float32 * 32
  particle.vel.y = randNormal().float32 * 32
  particle.birthday = - rand_f32() * maxParticleAge

var particleData           = cpuParticleData.arrayBuffer(GL_STREAM_DRAW)
var particleTarget         = cpuParticleData.arrayBuffer(GL_STREAM_DRAW)

let
  posView      = particleData.view(pos)
  colView      = particleData.view(col)
  rotView      = particleData.view(rot)
  velView      = particleData.view(vel)
  birthdayView = particleData.view(birthday)

  posTargetView      = particleTarget.view(pos)
  colTargetView      = particleTarget.view(col)
  rotTargetView      = particleTarget.view(rot)
  velTargetView      = particleTarget.view(vel)
  birthdayTargetView = particleTarget.view(birthday)

var running    = true
var gameTimer  = newStopWatch(true)
var frameTimer = newStopWatch(true)

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

proc glslLayoutSpecification[T](t: typedesc[T], buffer: int): string =
  var tmp: T

  let stride = sizeof(T)
  let name = t.name

  let baseAddr = cast[uint](tmp.addr)
  result = s"layout(xfb_buffer = $buffer, xfb_stride = $stride) out $name {\n"
  for fieldName, field in tmp.fieldPairs:
    let offset = cast[uint](field.addr) - baseAddr



  result.add "};\n"

#[

layout(xfb_buffer = 0, xfb_stride = 36) out ParticleData {
  layout(xfb_offset = 0)  vec2 pos;
  layout(xfb_offset = 8)  vec2 vel;
  layout(xfb_offset = 16) vec3 col;
  layout(xfb_offset = 28) float rot;
  layout(xfb_offset = 32) float birthday;
};

  layout(xfb_offset = 0)  vec2 pos;
  layout(xfb_offset = 8)  vec2 vel;
  layout(xfb_offset = 16) vec3 col;
  layout(xfb_offset = 28) float rot;
  layout(xfb_offset = 32) float birthday;
};
]#


while running:
  let frameTime = frameTimer.reset
  let time = gameTimer.time.float32

  pmouseX = mouseX
  pmouseY = mouseY
  discard getMouseState(mouseX.addr, mouseY.addr)
  mouseY = windowsize.y.int32 - mouseY

  let dirx = float32(cos(time * 5))
  let diry = float32(sin(time * 5))

  glClear(GL_COLOR_BUFFER_BIT)

  transformFeedback.bufferBase(0, particleTarget)

  shadingDsl:
    #debug
    primitiveMode = GL_POINTS
    numVertices   = numParticles
    programIdent  = tfProgram
    vaoIdent      = tfVao

    uniforms:
      mouse      = vec2f(mouseX.float32,mouseY.float32)
      pmouse     = vec2f(pmouseX.float32,pmouseY.float32)
      frameTime  = frameTime.float32
      windowsize = windowsize
      time
      maxParticleAge
      dir        = vec2f(dirx,diry)

    attributes:
      a_pos = posView
      a_col = colView
      a_rot = rotView
      a_vel = velView
      a_birthday = birthdayView

    afterSetup:
      #tfProgram.transformFeedbackVaryings(["v_pos", "v_col", "v_rot", "v_vel", "v_birthday"], GL_INTERLEAVED_ATTRIBS)
      discard

    beforeRender:

      glEnable(GL_RASTERIZER_DISCARD);
      glBindTransformFeedback(GL_TRANSFORM_FEEDBACK, transformFeedback.handle)
      glBeginTransformFeedback(GL_POINTS)

    vertexSrc:
      """

layout(xfb_buffer = 0, xfb_stride = 36) out ParticleData {
  layout(xfb_offset = 0)  vec2 pos;
  layout(xfb_offset = 8)  vec2 vel;
  layout(xfb_offset = 16) vec3 col;
  layout(xfb_offset = 28) float rot;
  layout(xfb_offset = 32) float birthday;
};

void main() {
  //v_pos = a_pos + a_vel * frameTime;
  //v_pos = clamp(a_pos, vec2(0), windowsize);
  //v_vel = a_vel * flip;

  //if(a_birthday + maxParticleAge > time) {
  //  v_birthday = time;
  //  v_vel = 128 * dir;
  //  v_pos = mix( pmouse, mouse, 0.5 );
  //} else {
  //  v_birthday = a_birthday;
  //  v_vel = a_vel;
  //}

  pos = a_pos + a_vel * time;
  //vel.x = 0 <= pos.x && pos.x <= windowsize.x ? a_vel.x : -a_vel.x;
  //vel.y = 0 <= pos.y && pos.y <= windowsize.y ? a_vel.y : -a_vel.y;
  vel = vec2(0);
  pos = clamp(pos, vec2(0), windowsize);

  birthday = a_birthday;
  col = a_col;
  rot = a_rot * time;
}
      """

    vertexOut:
      #v_pos      = posTargetView
      #v_col      = colTargetView
      #v_rot      = rotTargetView
      #v_vel      = velTargetView
      #v_birthday = birthdayTargetView
      #transformFeedback
      discard

    afterRender:

      glEndTransformFeedback()
      glBindTransformFeedback(GL_TRANSFORM_FEEDBACK, 0)
      glDisable(GL_RASTERIZER_DISCARD);

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


  glEnable(GL_BLEND)

  shadingDsl:
    #debug
    primitiveMode = GL_POINTS
    numVertices   = numParticles

    uniforms:
      mvp
      particleSize = vec2f(16,4)

    attributes:
      a_pos   = posTargetView
      a_color = colTargetView
      a_rot   = rotTargetView

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
      a_pos   = posTargetView
      a_color = colTargetView
      a_rot   = rotTargetView

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
