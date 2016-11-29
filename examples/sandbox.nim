# OpenGL example using SDL2

import sequtils, ../fancygl

let windowsize = vec2i(640, 480)
let (window, context) = defaultSetup(windowsize)

let crateTexture = loadTexture2DFromFile("crate.png")

type
  VertexStruct = object
    pos:      Vec4f
    normal:   Vec4f
    color:    Vec4f
    texcoord: Vec2f

var boxBuffer = newArrayBuffer[VertexStruct](boxVertices.len)

for i, vertex in boxBuffer.mpairs:
  vertex.pos      = boxVertices[i]
  vertex.normal   = boxNormals[i]
  vertex.color    = boxColors[i]
  vertex.texcoord = boxTexCoords[i]

let
  vertex = boxBuffer.view(pos)
  normal = boxBuffer.view(normal)
  color =  boxBuffer.view(color)
  texcoord = boxBuffer.view(texcoord)
  
let indices = toSeq( countup[int8,int8](0, int8(high(boxvertices)))).elementArrayBuffer

declareFramebuffer(Fb1FramebufferType):
  depth = newDepthTexture2D(windowsize)
  color = newTexture2D(windowsize)

let fb1 = newFb1FramebufferType()

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

var
  projection_mat : Mat4x4[float32]
  viewport: Vec4f

proc setViewportAndProjection() =
  viewport.x = 0
  viewport.y = 0
  viewport.z = windowsize.x.float32
  viewport.w = windowsize.y.float32
  # Set the viewport to cover the new window
  glViewport(viewport.x.GLint, viewport.y.GLint, windowsize.x.GLint, windowsize.y.GLint)
  projection_mat = perspective(45'f32, windowsize.x / windowsize.y, 0.1, 100.0)
  # TODO this is not fixed
  # fb1.resize(windowsize.xy)

setViewportAndProjection() # Set up initial viewport and projection
  
var
  mouseX, mouseY: int32
  gameTimer    = newStopWatch(true)
  frameCounter = 0

const
  constVec = vec4f(1,2,3,4)
  
proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  let mouse = vec2f(mouseX.float32, float32(windowsize.y - mouseY))
  #let mouseX_Norm = (mouseX.float32 / screenWidth.float32)
  #let mouseY_Norm = (mouseY.float32 / screenHeight.float32)
  let mousePosNorm = (mouse - viewport.xy) / viewport.zw

  #for i in 0..<5:
  block writeToFramebufferBlock:
    let time = gameTimer.time

    var modelview_mat = I4f
    modelview_mat = modelview_mat.translate( vec3f(sin(time)*2, cos(time)*2, -7) )
    modelview_mat = modelview_mat.rotate( vec3f(0,0,1), time )
    modelview_mat = modelview_mat.rotate( vec3f(0,1,0), time )
    modelview_mat = modelview_mat.rotate( vec3f(1,0,0), time )

    bindFramebuffer(fb1):
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
      
      shadingDsl(GL_TRIANGLES):
        numVertices = vertex.len

        uniforms:
          modelview = modelview_mat
          projection = projection_mat
          time
          mousePosNorm
          crateTexture
          constVec

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
          gl_Position = projection * modelview * pos;
          v_eyepos = modelview * pos;
          v_eyenormal = modelview * normal;
          v_col = col;
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
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
    
    shadingDsl(GL_TRIANGLES):
      numVertices = 3

      uniforms:
        mouse
        tex = fb1.color
        depth = fb1.depth
        time
        viewport
        texSize = fb1.color.size.vec2f

      fragmentMain:
        """
        vec2 offset = vec2(sin(time * 5 + gl_FragCoord.y / 8) * 0.01, 0);
        vec2 texcoord = (texCoord * viewport.zw ) / texSize;
        vec4 t_col = texture(tex, texcoord + offset);
        gl_FragDepth = texture(depth, texcoord + offset).x;
        color = t_col;
        """

    # render face normals using the geometry shader
    shadingDsl(GL_TRIANGLES):
      numVertices = vertex.len

      uniforms:
        modelview = modelview_mat
        projection = projection_mat
      attributes:
        pos = vertex
        normal

      vertexMain:
        """
        gl_Position = modelview * pos;
        v_eyepos = modelview * pos;
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

var
  evt: Event = defaultEvent
  runGame = true
  fpsTimer = newStopWatch(true)
  fpsFrameCounter = 0

while runGame:

  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    #if evt.kind == WindowEvent:
    #  #if evt.window.event == WindowEvent_Resized:
    #  #  windowsize.x = evt.window.data1.float32
    #  #  windowsize.y = evt.window.data2.float32
    #  #  etViewportAndProjection()
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SDL_SCANCODE_ESCAPE:
        runGame = false
      of SDL_SCANCODE_PAUSE:
        gameTimer.toggle
      of SDL_SCANCODE_F10:
        window.screenshot("sandbox")
      else:
        discard


    if evt.kind == MouseMotion:
      mouseX = evt.motion.x
      mouseY = evt.motion.y
      
  if fpsTimer.time >= 1:
    echo "FPS: ", fpsFrameCounter
    fpsFrameCounter = 0
    fpsTimer.reset

  render()
  fpsframeCounter += 1
