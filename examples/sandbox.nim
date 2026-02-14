# OpenGL example using SDL2

import ../fancygl

#let (window, context) = defaultSetup(vec2i(640,480))
let (window, context) = defaultSetup()

let crateTexture = loadTexture2DFromFile(getResourcePath("crate.png"))

type
  VertexStruct = object
    pos:      Vec4f
    normal:   Vec4f
    color:    Vec4f
    texcoord: Vec2f

var boxBuffer = createArrayBuffer[VertexStruct](boxVerticesCenterAtZero.len)

for i, vertex in boxBuffer.wPairs:
  vertex.pos      = boxVerticesCenterAtZero[i]
  vertex.normal   = boxNormals[i]
  vertex.color    = boxColors[i]
  vertex.texcoord = boxTexCoords[i]

let
  vertex = boxBuffer.view(pos)
  normal = boxBuffer.view(normal)
  color =  boxBuffer.view(color)
  texcoord = boxBuffer.view(texcoord)

let indices = iotaSeq[int8](boxverticesCenterAtZero.len).elementArrayBuffer

declareFramebuffer(Fb1FramebufferType):
  depth = newDepthTexture2D(window.size)
  color = newTexture2D(window.size)

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
  projection_mat : Mat4f = perspective(45'f32, window.aspectratio, 0.1, 100.0)
  mouseX, mouseY: int32
  gameTimer    = newStopWatch(true)
  fps          = 0
  frameCounter = 0

glClearColor(0.4,0.1,0.2,1.0)
glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA)

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  let mousePos  = vec2i(mouseX,mouseY)
  let mousePosf = vec2f(mousePos)
  let mousePosNorm = mousePosf / vec2f(window.size)

  #for i in 0..<5:
  block writeToFramebufferBlock:
    let time = gameTimer.time.float32

    var modelview_mat = mat4f()
    modelview_mat = modelview_mat.translate( vec3f(sin(time)*2, cos(time)*2, -7) )
    modelview_mat = modelview_mat.rotateZ(time)
    modelview_mat = modelview_mat.rotateY(time)
    modelview_mat = modelview_mat.rotateX(time)

    blockBindFramebuffer(fb1):
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

      shadingDsl:
        primitiveMode = GL_TRIANGLES
        numVertices = vertex.len
        indices = indices

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

    shadingDsl:

      uniforms:
        tex = fb1.color
        depth = fb1.depth
        time
        windowsize = vec2f(window.size)
        texSize = fb1.color.size.vec2f

      fragmentMain:
        """
        vec2 offset = vec2(sin(time * 5 + gl_FragCoord.y * 60 / windowsize.y) * 0.01, 0);
        vec2 texcoord = (texCoord * windowsize ) / texSize;
        vec4 t_col = texture(tex, texcoord + offset);
        gl_FragDepth = texture(depth, texcoord + offset).x;
        color = t_col;
        """

    # render face normals using the geometry shader
    shadingDsl:
      primitiveMode = GL_TRIANGLES
      numVertices = vertex.len

      uniforms:
        modelview = modelview_mat
        projection = projection_mat
      attributes:
        pos = vertex

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

  glEnable(GL_BLEND)
  renderText(s"FPS: $fps", vec2i(11) )
  glDisable(GL_BLEND)

  frameCounter += 1
  glSwapWindow(window) # Swap the front and back frame buffers (double buffering)

var
  runGame = true
  fpsTimer = newStopWatch(true)
  fpsFrameCounter = 0

while runGame:

  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KeyDown:
      case evt.key.keysym.scancode
      of SCANCODE_ESCAPE:
        runGame = false
      of SCANCODE_PAUSE:
        gameTimer.toggle
      of SCANCODE_F10:
        screenshot(window)
      else:
        discard

  if fpsTimer.time >= 1:
    fps = fpsFrameCounter
    fpsFrameCounter = 0
    fpsTimer.reset

  discard getMouseState(mouseX.addr, mouseY.addr)

  render()
  fpsframeCounter += 1

# Local Variables:
# compile-command: "cd examples; nim c -r sandbox.nim"
# End:
