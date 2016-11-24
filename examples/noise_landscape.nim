const glslnoise = """
vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 mod289(vec4 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 permute(vec4 x) {
     return mod289(((x*34.0)+1.0)*x);
}

vec4 taylorInvSqrt(vec4 r) {
  return 1.79284291400159 - 0.85373472095314 * r;
}

float snoise(vec3 v) { 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //   x0 = x0 - 0.0 + 0.0 * C.xxx;
  //   x1 = x0 - i1  + 1.0 * C.xxx;
  //   x2 = x0 - i2  + 2.0 * C.xxx;
  //   x3 = x0 - 1.0 + 3.0 * C.xxx;
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

// Permutations
  i = mod289(i); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients: 7x7 points over a square, mapped onto an octahedron.
// The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
  float n_ = 0.142857142857; // 1.0/7.0
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
}

float calcHeight(vec2 pos) {
  return snoise(vec3(pos * 0.123, time * 0.0123)) * 5.0;
}
"""


import ../fancygl, libarne

let heightmap = newHeightMap(128,128)

let (window, context) = defaultSetup()

let windowsize = window.size

let verts = arrayBuffer(heightmap.vertices)
let triangleStripIndices = elementArrayBuffer(heightmap.indicesTriangleStrip)
let quadIndices          = elementArrayBuffer(heightmap.indicesQuads)

let triangleStripIndicesLen = triangleStripIndices.len
let quadIndicesLen = quadIndices.len

var running = true

var projMat = perspective(45'f32, windowsize.x / windowsize.y, 0.9, 1000.0)

var gameTimer = newStopWatch(true)

#glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
#glEnable(GL_CULL_FACE)
discard setRelativeMouseMode(Bool32(true))

let layers = newTexture1D(512, GL_RGBA8)

block:
  var data = newSeq[Vec4u8](512)
  for color in data.mitems:
    color.x = rand_u8()
    color.y = rand_u8()
    color.z = rand_u8()
    color.w = 255
    
  layers.setData data
  layers.generateMipmap

glEnable(GL_PRIMITIVE_RESTART);
glPrimitiveRestartIndex(cast[GLuint](-1'i32))

var frame = 0
var camera = newCamera()

camera.moveAbolute(vec3f(heightmap.size.vec2f * 0.5f,0))
camera.turnRelativeX(radians(45.0f))
camera.moveRelative(vec3f(0,0,20))

while running:
  defer:
    frame += 1

  # handle events
  
  var evt = defaultEvent

  var rotation, movement : Vec3f

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      running = false
      break
    elif evt.kind == KeyDown:
      if evt.key.keysym.scancode == SDL_SCANCODE_ESCAPE:
        running = false
        break
      if evt.key.keysym.scancode == SDL_SCANCODE_F10:
        screenshot(window, "noise_landscape")
      else:
        discard
        
    elif evt.kind == MouseMotion:
      rotation.x = rotation.x - evt.motion.yrel.float / 128.0
      rotation.y = rotation.y - evt.motion.xrel.float / 128.0

    else:
      discard

  var state = getKeyboardState()

  movement.z = (state[SDL_SCANCODE_D.int].float - state[SDL_SCANCODE_E.int].float) * 0.4
  movement.x = (state[SDL_SCANCODE_F.int].float - state[SDL_SCANCODE_S.int].float) * 0.4

  camera.moveRelative(movement)
  camera.turnRelativeX(rotation.x)
  camera.turnAbsoluteZ(rotation.y)

  let time = gameTimer.time.float32

  # render stuff
      
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)


  let mvp = projMat * camera.viewMat

  #echo indices.handle

  if false:
    shadingDsl(GL_TRIANGLE_STRIP):
      numVertices = triangleStripIndicesLen

      uniforms:
        mvp
        viewMat = camera.viewMat
        time
        layers

      attributes:
        indices = triangleStripIndices
        pos = verts
      includes:
        glslNoise
        """
        vec4 desaturate(vec4 color, float desaturation) {
          vec3 grayXfer = vec3(0.3, 0.59, 0.11);
          vec3 gray = vec3(dot(grayXfer, color.rgb));
          return vec4(mix(color.rgb, gray, desaturation), color.a);
        }
        """
      vertexMain:
        """
        pos2d_ws = pos.xy;
        vec4 vertexPos = pos + vec4(0,0,calcHeight(pos2d_ws),0);
        gl_Position = mvp * vertexPos;
        distance = length(viewMat * vertexPos);
        """
      vertexOut:
        "out vec2 pos2d_ws"
        "out float distance"
      fragmentMain:
        """
        float height = calcHeight(pos2d_ws);
        float detail = snoise(vec3(pos2d_ws * 12.3, 0));

        color = texture(layers,height * 0.01 + detail * 0.003);

        float desaturation = clamp(distance / 128.0, 0, 1);

        color = desaturate(color, desaturation);


        //color = vec4(1);
        """

  elif(true):
    shadingDsl(GL_LINES_ADJACENCY):
      numVertices = quadIndicesLen

      uniforms:
        mvp
        viewMat = camera.viewMat
        time
        layers

      attributes:
        indices = quadIndices
        pos = verts
      includes:
        glslNoise
        """
        vec4 desaturate(vec4 color, float desaturation) {
          vec3 grayXfer = vec3(0.3, 0.59, 0.11);
          vec3 gray = vec3(dot(grayXfer, color.rgb));
          return vec4(mix(color.rgb, gray, desaturation), color.a);
        }
        """
      vertexMain:
        """
        v_pos2d_ws = pos.xy;
        v_height = calcHeight(v_pos2d_ws);
        vec4 vertexPos = pos + vec4(0,0,v_height,0);
        gl_Position = mvp * vertexPos;
        v_distance = length(viewMat * vertexPos);
        """
      vertexOut:
        "out float v_height"
        "out vec2 v_pos2d_ws"
        "out float v_distance"
      geometryMain:
        "layout(triangle_strip, max_vertices=4) out"
        """
        int indicesB[4] = int[4](0,1,2,3);
        int indicesA[4] = int[4](1,0,3,2);

        int indices[4] = abs(v_height[0] - v_height[3]) < abs(v_height[1] - v_height[2]) ? indicesA : indicesB;

        for(int i = 0; i < 4; ++i) {
          gl_Position = gl_in[indices[i]].gl_Position;
          pos2d_ws = v_pos2d_ws[indices[i]];
          distance = v_distance[indices[i]];
          EmitVertex();
        }


        """
      geometryOut:
        "out vec2 pos2d_ws"
        "out float distance"
      fragmentMain:
        """
        float height = calcHeight(pos2d_ws);
        float detail = snoise(vec3(pos2d_ws * 12.3, 0));

        color = texture(layers,height * 0.01 + detail * 0.003);

        //float desaturation = clamp(distance / 128.0, 0, 1);

        //color = desaturate(color, desaturation);


        //color = vec4(1);
        """

  else:
    discard
  
  
      

  glSwapWindow(window)

  

  
