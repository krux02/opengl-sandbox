# OpenGL example using SDL2

import sdl2, opengl, math, sequtils, strutils, glm, typetraits, macros, fancygl


var vertex: seq[Vec3[float]] = @[
  vec3(+1.0, +1.0, -1.0), vec3(-1.0, +1.0, -1.0), vec3(-1.0, +1.0, +1.0),
  vec3(+1.0, +1.0, +1.0), vec3(+1.0, +1.0, -1.0), vec3(-1.0, +1.0, +1.0),
  vec3(+1.0, -1.0, +1.0), vec3(-1.0, -1.0, +1.0), vec3(-1.0, -1.0, -1.0),
  vec3(+1.0, -1.0, -1.0), vec3(+1.0, -1.0, +1.0), vec3(-1.0, -1.0, -1.0),
  vec3(+1.0, +1.0, +1.0), vec3(-1.0, +1.0, +1.0), vec3(-1.0, -1.0, +1.0),
  vec3(+1.0, -1.0, +1.0), vec3(+1.0, +1.0, +1.0), vec3(-1.0, -1.0, +1.0),
  vec3(+1.0, -1.0, -1.0), vec3(-1.0, -1.0, -1.0), vec3(-1.0, +1.0, -1.0),
  vec3(+1.0, +1.0, -1.0), vec3(+1.0, -1.0, -1.0), vec3(-1.0, +1.0, -1.0),
  vec3(-1.0, +1.0, +1.0), vec3(-1.0, +1.0, -1.0), vec3(-1.0, -1.0, -1.0),
  vec3(-1.0, -1.0, +1.0), vec3(-1.0, +1.0, +1.0), vec3(-1.0, -1.0, -1.0),
  vec3(+1.0, +1.0, -1.0), vec3(+1.0, +1.0, +1.0), vec3(+1.0, -1.0, +1.0),
  vec3(+1.0, -1.0, -1.0), vec3(+1.0, +1.0, -1.0), vec3(+1.0, -1.0, +1.0)
]

var color: seq[Vec3[float]] = @[
  vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0),
  vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0),
  vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0),
  vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0),
  vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0),
  vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0),
  vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0),
  vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0),
  vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0),
  vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0),
  vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0),
  vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0)
]


discard sdl2.init(INIT_EVERYTHING)

var screenWidth: cint = 640
var screenHeight: cint = 480

var time = 0.0
var window = createWindow("SDL/OpenGL Skeleton", 100, 100, screenWidth, screenHeight, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE)
var context = window.glCreateContext()

# Initialize OpenGL
loadExtensions()

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
glShadeModel(GL_SMOOTH)                           # Enable smooth shading
glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST) # Nice perspective corrections


let glslCode = """
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

var projection_mat : Mat4x4[float64]

proc reshape(newWidth: cint, newHeight: cint) =
  glViewport(0, 0, newWidth, newHeight)   # Set the viewport to cover the new window
  glMatrixMode(GL_PROJECTION)             # To operate on the projection matrix

  # Enable perspective projection with fovy, aspect, zNear and zFar
  projection_mat = perspective(45.0, newWidth / newHeight, 0.1, 100.0)

  glLoadMatrixd(cast[ptr GLdouble](projection_mat.addr))

template attribSize(t: type Vec3[float64]) : GLint = 3
template attribType(t: type Vec3[float64]) : GLenum = cGL_DOUBLE
template attribNormalized(t: type Vec3[float64]) : bool = false

proc makeAndBindBuffer[T](buffer: var ArrayBuffer[T], index: GLuint, value: var seq[T], usage: GLenum) =
  buffer = newArrayBuffer[T]()
  buffer.bindIt
  glBufferData(GL_ARRAY_BUFFER, GLsizeiptr(value.len * sizeof(T)), value[0].addr, usage)
  glVertexAttribPointer(index, attribSize(T), attribType(T), attribNormalized(T), 0, nil)

template renderBlockTemplate(globalsBlock, sequenceInitBlock,
               bufferCreationBlock, setUniformsBlock: expr): stmt {. dirty .} =
  block:
    var vao {.global.}: VertexArrayObject
    var glProgram {.global.}: GLuint  = 0

    globalsBlock

    if glProgram == 0:

      sequenceInitBlock

      gl_program = linkShader(
        compileShader(GL_VERTEX_SHADER,   genShaderSource(uniforms, true, attributes, true, varyings, includes, vertexSrc)),
        compileShader(GL_FRAGMENT_SHADER, genShaderSource(uniforms, true, varyings, false, fragOut, includes, fragmentSrc)),
      )

      glUseProgram(gl_program)
      vao = newVertexArrayObject()
      bindIt(vao)

      bufferCreationBlock

      glBindBuffer(GL_ARRAY_BUFFER, 0)
      bindIt(nil_vao)
      glUseProgram(0)

    glUseProgram(gl_program)

    bindIt(vao)

    setUniformsBlock

    glDrawArrays(GL_TRIANGLES, 0, GLsizei(len(vertex)))

    bindIt(nil_vao)
    glUseProgram(0);


macro macro_test(statement: expr) : stmt =

  let lhsName = "projection"
  let rhsName = "projection_mat"

  let attributesSection = newNimNode(nnkBracket)
  let uniformsSection = newNimNode(nnkBracket)
  let varyingsSection = newNimNode(nnkBracket)
  let fragOutSection = newNimNode(nnkBracket)
  let includesSection = newNimNode(nnkBracket)

  let globalsBlock = newStmtList()
  let bufferCreationBlock = newStmtList()
  let setUniformsBlock = newStmtList()

  var attribCount = 0;
  proc addAttrib(lhsIdent, rhsIdent: NimNode): void =
    let lhsStrLit = newLit($lhsIdent)
    let bufferIdentNode = newIdentNode($lhsIdent & "Buffer")

    let shaderParam = quote do:
      (`lhsStrLit`, glslAttribType(type(`rhsIdent`)))

    attributesSection.add(shaderParam)

    template foobarTemplate( lhs, rhs : expr ) : stmt{.dirty.} =
      var lhs {.global.}: ArrayBuffer[rhs[0].type]


    let line = getAst(foobarTemplate( bufferIdentNode, rhsIdent ))

    globalsBlock.add line
    bufferCreationBlock.add(newCall("glEnableVertexAttribArray", newLit(attribCount)))
    bufferCreationBlock.add(newCall("makeAndBindBuffer",
        bufferIdentNode,
        newLit(attribCount),
        rhsIdent,
        newIdentNode(!"GL_STATIC_DRAW")
    ))

    attribCount += 1

  var uniformCount = 0
  proc addUniform(lhsName, rhsName: string): void =
    let shaderParam = "(\"" & lhsName & "\", glslUniformType(type(" & rhsName & ")))"
    uniformsSection.add(parseExpr(shaderParam))

    setUniformsBlock.add newCall("uniform", newLit(uniformCount), newIdentNode(rhsName))

    uniformCount += 1

  var varyingCount = 0
  proc addVarying(name, typ: string): void =
    let shaderParam = newPar( newLit(name), newLit(typ) )
    varyingsSection.add shaderParam

    varyingCount += 1

  var fragOutCount = 0
  proc addFragOut(name, typ: string): void =
    let  shaderParam = newPar( newLit(name), newLit(typ) )
    fragOutSection.add shaderParam

    fragOutCount += 1

  var vertexSourceNode = newLit("")
  var fragmentSourceNode = newLit("")

  #### BEGIN PARSE TREE ####

  for section in statement.items:
    section.expectKind nnkCall
    let ident = section[0]
    ident.expectKind nnkIdent
    let stmtList = section[1]
    stmtList.expectKind nnkStmtList
    if $ident.ident == "uniforms":
      for capture in stmtList.items:
        capture.expectKind({nnkAsgn, nnkIdent})

        if capture.kind == nnkAsgn:
          capture.expectLen 2
          capture[0].expectKind nnkIdent
          capture[1].expectKind nnkIdent
          addUniform($capture[0], $capture[1])
        elif capture.kind == nnkIdent:
          addUniform($capture, $capture)

    elif $ident.ident == "attributes":
      for capture in stmtList.items:
        capture.expectKind({nnkAsgn, nnkIdent})

        if capture.kind == nnkAsgn:
          capture.expectLen 2
          capture[0].expectKind nnkIdent
          capture[1].expectKind nnkIdent
          echo "addAttrib(", capture[0],",", capture[1], ")"
          addAttrib(capture[0], capture[1])
        elif capture.kind == nnkIdent:
          addAttrib(capture, capture)

    elif $ident.ident == "varyings":
      warning("yay got varyings with StmtList")
      for varSec in stmtList.items:
        varSec.expectKind nnkVarSection
        for def in varSec:
          def.expectKind nnkIdentDefs
          echo " varying "
          def[0].expectKind nnkIdent
          def[1].expectKind nnkIdent
          addVarying( $def[0] , $def[1] )

    elif $ident.ident == "frag_out":
      warning("yay got frag_out with StmtList")
      for varSec in stmtList.items:
        varSec.expectKind nnkVarSection
        for def in varSec:
          def.expectKind nnkIdentDefs
          def.expectKind nnkIdentDefs
          echo " varying "
          def[0].expectKind nnkIdent
          def[1].expectKind nnkIdent
          addFragOut( $def[0] , $def[1] )

    elif $ident.ident == "vertex_prg":
      stmtList.expectLen(1)
      stmtList[0].expectKind({nnkTripleStrLit, nnkStrLit})
      vertexSourceNode = stmtList[0]

    elif $ident.ident ==  "fragment_prg":
      stmtList.expectLen(1)
      stmtList[0].expectKind({ nnkTripleStrLit, nnkStrLit })
      fragmentSourceNode = stmtList[0]

    elif $ident == "includes":
      for statement in stmtList:
        statement.expectKind( nnkIdent )
        includesSection.add statement

    else:
      error("unknown section " & $ident.ident)

  #### END PARSE TREE ####

  let sequenceInitBlock = newStmtList()

  var statement:NimNode

  statement = parseStmt(" let attributes: seq[ShaderParam] = @[] ")
  statement[0][0][2][1] = attributesSection
  sequenceInitBlock.add statement

  statement = parseStmt(" let uniforms: seq[ShaderParam] = @[] ")
  statement[0][0][2][1] = uniformsSection
  sequenceInitBlock.add statement

  statement = parseStmt(" let varyings: seq[ShaderParam] = @[] ")
  statement[0][0][2][1] = varyingsSection
  sequenceInitBlock.add statement

  statement = parseStmt(" let fragOut: seq[ShaderParam] = @[] ")
  statement[0][0][2][1] = fragOutSection
  sequenceInitBlock.add statement

  statement = parseStmt(" let includes: seq[string] = @[] ")
  statement[0][0][2][1] = includesSection
  sequenceInitBlock.add statement

  sequenceInitBlock.add newLetStmt(newIdentNode("vertexSrc"), vertexSourceNode)
  sequenceInitBlock.add newLetStmt(newIdentNode("fragmentSrc"), fragmentSourceNode)

  result = getAst( renderBlockTemplate(globalsBlock, sequenceInitBlock,
                                       bufferCreationBlock, setUniformsBlock))

var mouseX, mouseY: int32

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  var modelview_mat: Mat4x4[float]  = I4()
  modelview_mat = modelview_mat.transform( vec3(2*sin(time), 2*cos(time), -7.0) )
  modelview_mat = modelview_mat.rotate( vec3[float](0,0,1), time*1.1 )
  modelview_mat = modelview_mat.rotate( vec3[float](0,1,0), time*1.2 )
  modelview_mat = modelview_mat.rotate( vec3[float](1,0,0), time*1.3 )

  let mouseX_Norm = (mouseX.float32 / screenWidth.float32)
  let mouseY_Norm = (mouseY.float32 / screenHeight.float32)
  let mousePosNorm = vec2(mouseX_Norm, mouseY_Norm)
  let mvp =  modelview_mat * projection_mat;

  macro_test:
    uniforms:
      mvp
      time
      mousePosNorm
    attributes:
      pos = vertex
      col = color
    varyings:
      var v_col : vec4
    frag_out:
      var color : vec4
    includes:
      glslCode
    vertex_prg:
      """
      gl_Position = mvp * vec4(pos,1);
      v_col = vec4(col,1);
      """
    fragment_prg:
      """
      vec2 offset = gl_FragCoord.xy / 32 + mousePosNorm * 10;
      color = mymix(v_col, time + dot( vec2(cos(time),sin(time)), offset ));
      """

  glSwapWindow(window) # Swap the front and back frame buffers (double buffering)

# Main loop

var
  evt = sdl2.defaultEvent
  runGame = true
  frameCounter = 0
  frameCounterStartTime = 0.0

reshape(screenWidth, screenHeight) # Set up initial viewport and projection

while runGame:
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == WindowEvent:
      let windowEvent = cast[WindowEventPtr](addr(evt))
      if windowEvent.event == WindowEvent_Resized:
        let newWidth = windowEvent.data1
        let newHeight = windowEvent.data2
        reshape(newWidth, newHeight)
    if evt.kind == KeyDown:
      let keyboardEvent = cast[KeyboardEventPtr](addr(evt))
      if keyboardEvent.keysym.scancode == SDL_SCANCODE_ESCAPE:
        runGame = false
        break
    if evt.kind == MouseMotion:
      let mouseEvent = cast[MouseMotionEventPtr](addr(evt))
      mouseX = mouseEvent.x
      mouseY = mouseEvent.y

  time = float64( getTicks() ) / 1000.0
  if time - frameCounterStartTime >= 1:
    echo "FPS: ", frameCounter
    frameCounter = 0
    frameCounterStartTime = time

  render()
  frameCounter += 1



  #limitFrameRate()

