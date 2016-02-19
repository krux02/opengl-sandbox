# OpenGL example using SDL2

import sdl2, opengl, math, glm, sequtils, ../fancygl

discard sdl2.init(INIT_EVERYTHING)

var windowsize = vec2f(640,480)
var viewport = vec4f(0,0,640,480)

let window = createWindow("SDL/OpenGL Skeleton", 100, 100, windowsize.x.cint, windowsize.y.cint, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE)
let context = window.glCreateContext()

# Initialize OpenGL
loadExtensions()

let
  vertex = boxVertices.arrayBuffer
  normal = boxNormals.arrayBuffer
  color = boxColors.arrayBuffer
  texcoord = boxTexCoords.arrayBuffer

let screenSpaceTriangleVerts = @[
  vec4f(-1,-1,1,1), vec4f(3,-1,1,1), vec4f(-1,3,1,1)
].arrayBuffer

let screenSpaceTriangleTexcoords = @[
  vec2f(0,0), vec2f(2,0), vec2f(0,2)
].arrayBuffer

let indices = toSeq( countup[int8,int8](0, int8(vertex.len-1)) ).elementArrayBuffer

let crateTexture = loadTexture2DFromFile("crate.png")

declareFramebuffer(Fb1FramebufferType):
  depth = createEmptyDepthTexture2D(windowsize)
  color = createEmptyTexture2D(windowsize)

if 0 != glSetSwapInterval(-1):
  echo "glSetSwapInterval -1 not supported"
  echo sdl2.getError()
  if 0 != glSetSwapInterval(1):
    echo "but glSetSwapInterval 1 is ok"
  else:
    echo "even 1 is not ok"
    echo sdl2.getError()

glClearColor(0.0, 0.0, 0.0, 1.0)                  # Set background color to black and opaque
glClearDepth(1.0)                                 # Set background depth to farthest
glEnable(GL_DEPTH_TEST)                           # Enable depth testing for z-culling
glDepthFunc(GL_LEQUAL)                            # Set the type of depth-test
glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST) # Nice perspective corrections

glEnable(GL_CULL_FACE)
glCullFace(GL_BACK)

const glslCode = """
vec4 mymix(vec4 color, float alpha) {
  float a = 3*(alpha/3 - floor(alpha/3));

  float x = 1 - min(1, min(a, 3-a));
  float y = 1 - min(1, abs(a - 1));
  float z = 1 - min(1, abs(a - 2));

  float r = dot(vec4(x,y,z,0), color);
  float g = dot(vec4(y,z,x,0), color);
  float b = dot(vec4(z,x,y,0), color);

  return vec4(r,g,b, color.a);
}
"""

var projection_mat : Mat4x4[float]

proc reshape() =
  viewport.x = 0
  viewport.y = 0
  viewport.z = windowsize.x
  viewport.w = windowsize.y
  # Set the viewport to cover the new window
  glViewport(viewport.x.GLint, viewport.y.GLint, windowsize.x.GLint, windowsize.y.GLint)
  projection_mat = perspective(45.0, windowsize.x / windowsize.y, 0.1, 100.0)

var
  mouseX, mouseY: int32
  simulationTime = 0.0
  frameCounter = 0

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  let mouse = vec2f(mouseX.float32, windowsize.y - mouseY.float32)
  #let mouseX_Norm = (mouseX.float32 / screenWidth.float32)
  #let mouseY_Norm = (mouseY.float32 / screenHeight.float32)
  let mousePosNorm = (mouse - viewport.xy) / viewport.zw

  #for i in 0..<5:
  block:
    let time = simulationTime

    var modelview_mat = I4()
    modelview_mat = modelview_mat.translate( vec3[float](sin(time)*2, cos(time)*2, -7) )
    modelview_mat = modelview_mat.rotate( vec3[float](0,0,1), time )
    modelview_mat = modelview_mat.rotate( vec3[float](0,1,0), time )
    modelview_mat = modelview_mat.rotate( vec3[float](1,0,0), time )

    bindFramebuffer(fb1, Fb1FramebufferType):

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

      shadingDsl(GL_TRIANGLES):
        numVertices = vertex.len.GLsizei

        uniforms:
          modelview = modelview_mat
          projection = projection_mat
          time
          mousePosNorm
          crateTexture

        attributes:
          pos = vertex
          col = color
          texcoord
          normal
          indices

        includes:
          glslCode

        vertexMain:
          """
          gl_Position = projection * modelview * vec4(pos, 1);
          v_eyepos = modelview * vec4(pos,1);
          v_eyenormal = modelview * vec4(normal, 0);
          v_col = vec4(col,1);
          v_texcoord = texcoord;
          """

        vertexOut:
          "out vec4 v_col"
          "out vec2 v_texcoord"
          "out vec4 v_eyepos"
          "out vec4 v_eyenormal"

        fragmentMain:
          """
          vec4 t_col = texture(crateTexture, v_texcoord);
          vec2 offset = gl_FragCoord.xy / 32 + mousePosNorm * 10;
          vec4 mix_col = mymix(t_col, time + dot( vec2(cos(time),sin(time)), offset ));
          color = v_col * mix_col;
          //color = t_col;
          //color = (v_eyenormal + vec4(1)) * 0.5;
          //color.rg = vec2(float(int(gl_FragCoord.x) % 256) / 255.0, float(int(gl_FragCoord.y) % 256) / 255.0);
          """

    fb1.color.generateMipmap

    shadingDsl(GL_TRIANGLES):
      numVertices = 3

      uniforms:
        mouse
        tex = fb1.color
        depth = fb1.depth
        time
        viewport
        texSize = fb1.color.size

      attributes:
        pos = screenSpaceTriangleVerts
        texcoord = screenSpaceTriangleTexcoords

      vertexMain:
        """
        gl_Position = pos;
        v_texcoord = texcoord;
        """

      vertexOut:
        "out vec2 v_texcoord"

      fragmentMain:
        """
        vec2 offset = vec2(sin(time * 5 + gl_FragCoord.y / 8) * 0.01, 0);
        vec2 texcoord = (v_texcoord * viewport.zw ) / texSize;
        vec4 t_col = texture(tex, texcoord + offset);
        gl_FragDepth = texture(depth, texcoord + offset).x;
        color = t_col;
        """

    # render face normals using the geometry shader
    shadingDsl(GL_TRIANGLES):
      numVertices = vertex.len.GLsizei

      uniforms:
        modelview = modelview_mat
        projection = projection_mat
      attributes:
        pos = vertex
        normal

      vertexMain:
        """
        gl_Position = modelview * vec4(pos, 1);
        v_eyepos = modelview * vec4(pos,1);
        """

      vertexOut:
        "out vec4 v_eyepos"

      geometryMain:
        "layout(line_strip, max_vertices=2) out"
        """
        vec4 center = v_eyepos[0] + v_eyepos[1] + v_eyepos[2];

        vec3 v1 = (v_eyepos[1] - v_eyepos[0]).xyz;
        vec3 v2 = (v_eyepos[2] - v_eyepos[0]).xyz;
        vec4 normal = vec4(cross(v1,v2),0);

        gl_Position = projection * center;
        g_color = abs(normal);
        EmitVertex();
        gl_Position = projection * (center + normal);
        g_color = abs(normal);
        EmitVertex();
        """
      geometryOut:
        "out vec4 g_color"
      fragmentMain:
        """
        color = g_color;
        """

  frameCounter += 1
  glSwapWindow(window) # Swap the front and back frame buffers (double buffering)

# Main loop

var
  evt = sdl2.defaultEvent
  runGame = true
  gamePaused = false
  simulationTimeOffset = 0.0
  fpsFrameCounter = 0
  fpsFrameCounterStartTime = 0.0

reshape() # Set up initial viewport and projection

while runGame:
  let time = float64( getTicks() ) / 1000.0

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == WindowEvent:
      let windowEvent = cast[WindowEventPtr](addr(evt))
      if windowEvent.event == WindowEvent_Resized:
        windowsize.x = windowEvent.data1.float32
        windowsize.y = windowEvent.data2.float32
        reshape()
    if evt.kind == KeyDown:
      let keyboardEvent = cast[KeyboardEventPtr](addr(evt))
      if keyboardEvent.keysym.scancode == SDL_SCANCODE_ESCAPE:
        runGame = false
        break
      if keyboardEvent.keysym.scancode == SDL_SCANCODE_PAUSE:
        if gamePaused:
          gamePaused = false
          simulationTimeOffset = time - simulationTime
        else:
          gamePaused = true

    if evt.kind == MouseMotion:
      let mouseEvent = cast[MouseMotionEventPtr](addr(evt))
      mouseX = mouseEvent.x
      mouseY = mouseEvent.y


  if not gamePaused:
    simulationTime = time - simulationTimeOffset

  if time - fpsFrameCounterStartTime >= 1:
    echo "FPS: ", fpsFrameCounter
    fpsFrameCounter = 0
    fpsFrameCounterStartTime = time

  render()
  fpsframeCounter += 1

  #limitFrameRate()
