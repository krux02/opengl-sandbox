# OpenGL example using SDL2

import sdl2, opengl, math, glm, fancygl, sequtils, macros, macroutils

var vertex = @[
  vec3f(+1, +1, -1), vec3f(-1, +1, -1), vec3f(-1, +1, +1),
  vec3f(+1, +1, +1), vec3f(+1, +1, -1), vec3f(-1, +1, +1),
  vec3f(+1, -1, +1), vec3f(-1, -1, +1), vec3f(-1, -1, -1),
  vec3f(+1, -1, -1), vec3f(+1, -1, +1), vec3f(-1, -1, -1),
  vec3f(+1, +1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, +1),
  vec3f(+1, -1, +1), vec3f(+1, +1, +1), vec3f(-1, -1, +1),
  vec3f(+1, -1, -1), vec3f(-1, -1, -1), vec3f(-1, +1, -1),
  vec3f(+1, +1, -1), vec3f(+1, -1, -1), vec3f(-1, +1, -1),
  vec3f(-1, +1, +1), vec3f(-1, +1, -1), vec3f(-1, -1, -1),
  vec3f(-1, -1, +1), vec3f(-1, +1, +1), vec3f(-1, -1, -1),
  vec3f(+1, +1, -1), vec3f(+1, +1, +1), vec3f(+1, -1, +1),
  vec3f(+1, -1, -1), vec3f(+1, +1, -1), vec3f(+1, -1, +1)
]

var normal = @[
  vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
  vec3f( 0, +1,  0), vec3f( 0, +1,  0), vec3f( 0, +1,  0),
  vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
  vec3f( 0, -1,  0), vec3f( 0, -1,  0), vec3f( 0, -1,  0),
  vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
  vec3f( 0,  0, +1), vec3f( 0,  0, +1), vec3f( 0,  0, +1),
  vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
  vec3f( 0,  0, -1), vec3f( 0,  0, -1), vec3f( 0,  0, -1),
  vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
  vec3f(-1,  0,  0), vec3f(-1,  0,  0), vec3f(-1,  0,  0),
  vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0),
  vec3f(+1,  0,  0), vec3f(+1,  0,  0), vec3f(+1,  0,  0)
]

var color = @[
  vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
  vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0), vec3f(0.0, 1.0, 0.0),
  vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
  vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0), vec3f(1.0, 0.5, 0.0),
  vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
  vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0), vec3f(1.0, 0.0, 0.0),
  vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
  vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0), vec3f(1.0, 1.0, 0.0),
  vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
  vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0), vec3f(0.0, 0.0, 1.0),
  vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0),
  vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0), vec3f(1.0, 0.0, 1.0)
]

var texcoord = @[
  vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
  vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),
  vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
  vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),
  vec2f(1, 1), vec2f(0, 1), vec2f(0, 0),
  vec2f(1, 0), vec2f(1, 1), vec2f(0, 0),
  vec2f(1, 0), vec2f(0, 0), vec2f(0, 1),
  vec2f(1, 1), vec2f(1, 0), vec2f(0, 1),
  vec2f(1, 1), vec2f(1, 0), vec2f(0, 0),
  vec2f(0, 1), vec2f(1, 1), vec2f(0, 0),
  vec2f(1, 0), vec2f(1, 1), vec2f(0, 1),
  vec2f(0, 0), vec2f(1, 0), vec2f(0, 1)
]

var screenSpaceTriangleVerts = @[
  vec4f(-1,-1,1,1), vec4f(3,-1,1,1), vec4f(-1,3,1,1)
]

var screenSpaceTriangleTexcoords = @[
  vec2f(0,0), vec2f(2,0), vec2f(0,2)
]

discard sdl2.init(INIT_EVERYTHING)

var windowsize = vec2f(640,480)
var viewport = vec4f(0,0,640,480)

let window = createWindow("SDL/OpenGL Skeleton", 100, 100, windowsize.x.cint, windowsize.y.cint, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE)
let context = window.glCreateContext()

# Initialize OpenGL
loadExtensions()

let crateTexture = loadAndBindTexture2DFromFile("crate.png")
let crateTextureRect = loadAndBindTextureRectangleFromFile("crate.png")

let framebufferName = createFrameBuffer()
framebufferName.bindIt

let depthrenderbuffer = createAndBindDepthRenderBuffer( windowsize )
let renderedTexture = createAndBindEmptyTexture2D( windowsize )

glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depthrenderbuffer.GLuint )
glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, renderedTexture.GLuint, 0)

drawBuffers( GL_COLOR_ATTACHMENT0.GLenum )

macro framebuffertest(arg:untyped) : stmt =
  result = newStmtList()

  var fragmentOutputs = newSeq[string]()

  var depthType:NimNode = nil
  var depthCreateExpr:NimNode = nil

  for asgn in arg:
    asgn.expectKind nnkAsgn

    let lhs = asgn[0]
    let rhs = asgn[1]

    if lhs.ident == !"depth":
        echo rhs.treerepr
        if rhs.kind == nnkCall and rhs[0].ident == !"newRenderbuffer":
          depthType = bindSym"DepthRenderbuffer"
          depthCreateExpr = newCall(bindSym"createAndBindDepthRenderBuffer", rhs[1])

    else:
      fragmentOutputs.add($asgn[0])

  result.add newConstStmt(!!"fragmentOutputs", fragmentOutputs.toConstExpr)

  let recList = newNimNode(nnkRecList)
  recList.add( newExpIdentDef(!"glname", bindSym"FrameBuffer") )
  recList.add( newExpIdentDef(!"depth", depthType) )

  for fragOut in fragmentOutputs:
    recList.add( newExpIdentDef(!fragOut, bindSym"Texture2D") )

  result.add newObjectTy(!"FramebufferType", recList)

  result.add(newNimNode2(nnkVarSection, newNimNode2(nnkIdentDefs,
    !!"currentFrameBuffer",
    !!"FramebufferType",
    newEmptyNode()
  )))

  result.add(newAssignment(newDotExpr(!!"currentFrameBuffer", !!"glname"),
    newCall(bindSym"createFrameBuffer")
  ))

  result.add(newDotExpr(!!"currentFrameBuffer", !!"glname", !!"bindIt"))
  result.add(newAssignment(newDotExpr(!!"currentFrameBuffer", !!"depth"),
    depthCreateExpr
  ))
  result.add(newCall(bindSym"glFramebufferRenderbuffer", bindSym"GL_FRAMEBUFFER",
    bindSym"GL_DEPTH_ATTACHMENT", bindSym"GL_RENDERBUFFER",
    newDotExpr(!!"currentFrameBuffer", !!"glname", bindSym"GLuint")
  ))

  let drawBuffersCall = newCall(bindSym"drawBuffers")

  for i,name in fragmentOutputs:
    result.add(newAssignment( newDotExpr( !!"currentFrameBuffer", !! name ),
      newCall( bindSym"createAndBindEmptyTexture2D", !!"windowsize" ),
    ))
    result.add(newCall(bindSym"glFramebufferTexture", bindSym"GL_FRAMEBUFFER", !!("GL_COLOR_ATTACHMENT" & $i),
      newDotExpr(!!"currentFrameBuffer", !! name, bindSym"GLuint"), newLit(0)
    ))
    drawBuffersCall.add( newCall(bindSym"GLenum", !!("GL_COLOR_ATTACHMENT" & $i)) )

  result.add( drawBuffersCall )

  echo result.repr


dumpTree:
  type FramebufferType = object
    glname*: FrameBuffer
    depth*: DepthRenderbuffer
    color*: Texture2D

  var currentFrameBuffer: FramebufferType
  currentFrameBuffer.glname = createFrameBuffer()
  currentFrameBuffer.glname.bindIt
  currentFrameBuffer.depth = createAndBindDepthRenderBuffer( windowsize )
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, currentFrameBuffer.glname.GLuint )
  currentFrameBuffer.color = createAndBindEmptyTexture2D( windowsize )
  glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, currentFrameBuffer.color.GLuint, 0)
  drawBuffers(GLenum(GL_COLOR_ATTACHMENT0))



framebuffertest:
  depth = newRenderbuffer(windowsize)
  color = newTexture(windowsize)

glBindFramebuffer(GL_FRAMEBUFFER, 0)

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
    #let time = simulationTime * ( 1.0 + i.float32 / 10 )

    var modelview_mat = I4()
    modelview_mat = modelview_mat.translate( vec3[float](sin(time)*2, cos(time)*2, -7) )
    modelview_mat = modelview_mat.rotate( vec3[float](0,0,1), time )
    modelview_mat = modelview_mat.rotate( vec3[float](0,1,0), time )
    modelview_mat = modelview_mat.rotate( vec3[float](1,0,0), time )
    #modelview_mat = modelview_mat.scale( vec3[float](50,50,50) )

    #let mvp : Mat4x4[float32] =  modelview_mat * projection_mat;

    block: # render to framebuffer
      framebufferName.bindIt
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

      shadingDsl(GL_TRIANGLES, vertex.len.GLsizei):
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

    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    renderedTexture.bindIt
    glGenerateMipmap(GL_TEXTURE_2D)

    shadingDsl(GL_TRIANGLES, 3):
      uniforms:
        mouse
        tex = renderedTexture
        time
        viewport
        texSize = renderedTexture.size

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
        color = t_col;
        """

    # render face normals using the geometry shader
    shadingDsl(GL_TRIANGLES, vertex.len.GLsizei):
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
