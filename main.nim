# OpenGL example using SDL2

import sdl2
import opengl
import glu
import math
import sequtils
import strutils
import nre
import glm
import typetraits

discard sdl2.init(INIT_EVERYTHING)

var screenWidth: cint = 640
var screenHeight: cint = 480

var time = 0.0
var window = createWindow("SDL/OpenGL Skeleton", 100, 100, screenWidth, screenHeight, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE)
var context = window.glCreateContext()

# Initialize OpenGL
loadExtensions()
glClearColor(0.0, 0.0, 0.0, 1.0)                  # Set background color to black and opaque
glClearDepth(1.0)                                 # Set background depth to farthest
glEnable(GL_DEPTH_TEST)                           # Enable depth testing for z-culling
glDepthFunc(GL_LEQUAL)                            # Set the type of depth-test
glShadeModel(GL_SMOOTH)                           # Enable smooth shading
glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST) # Nice perspective corrections

proc uniformType(t : Vec4): string = "vec4"
proc uniformType(t : Vec3): string = "vec3"
proc uniformType(t : Vec2): string = "vec2"
proc uniformType(t : Mat4x4): string = "mat4"
proc uniformType(t : Mat3x3): string = "mat3"
proc uniformType(t : Mat2x2): string = "mat2"

proc glslType(t : seq[Vec4]): string = "vec4"
proc glslType(t : seq[Vec3]): string = "vec3"
proc glslType(t : seq[Vec2]): string = "vec2"
proc glslType(t : seq[Mat4x4]): string = "mat4"
proc glslType(t : seq[Mat3x3]): string = "mat3"
proc glslType(t : seq[Mat2x2]): string = "mat2"


type ArrayBuffer[T]        = distinct GLuint
type ElementArrayBuffer[T] = distinct GLuint
type UniformBuffer[T]      = distinct GLuint

proc newArrayBuffer[T](): ArrayBuffer[T] =
  glGenBuffers(1, cast[ptr GLuint](result.addr))

proc newElementArrayBuffer[T](): ElementArrayBuffer[T] =
  glGenBuffers(1, cast[ptr GLuint](result.addr))

proc newUniformBuffer[T](): UniformBuffer[T] =
  glGenBuffers(1, cast[ptr GLuint](result.addr))


proc currentArrayBuffer[T](): ArrayBuffer[T] =
  glGetIntegerv(GL_ARRAY_BUFFER_BINDING, cast[ptr GLint](result.addr))

proc currentElementArrayBuffer[T](): ElementArrayBuffer[T] =
  glGetIntegerv(GL_ELEMENT_ARRAY_BUFFER_BINDING, cast[ptr GLint](result.addr))

proc currentUniformBuffer[T](): UniformBuffer[T] =
  glGetIntegerv(GL_UNIFORM_BUFFER_BINDING, cast[ptr GLint](result.addr))


proc bindBuffer[T](buffer: ArrayBuffer[T]) =
  glBindBuffer(GL_ARRAY_BUFFER, GLuint(buffer))

proc bindBuffer[T](buffer: ElementArrayBuffer[T]) =
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, GLuint(buffer))

proc bindBuffer[T](buffer: UniformBuffer[T]) =
  glBindBuffer(GL_UNIFORM_BUFFER, GLuint(buffer))


proc `data=`[T](buffer: ArrayBuffer[T], data: seq[T]) =
  glBufferData(GL_ARRAY_BUFFER, data.len * sizeof(T), data[0].addr, GL_STATIC_DRAW)

proc `data=`[T](buffer: ElementArrayBuffer[T], data: seq[T]) =
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, data.len * sizeof(T), data[0].addr, GL_STATIC_DRAW)

proc `data=`[T](buffer: UniformBuffer[T], data: T) =
  glBufferData(GL_ARRAY_BUFFER, sizeof(T), data.addr, GL_STATIC_DRAW)


#proc swap[T](buffer: ArrayBuffer[T]): ArrayBuffer =
#  result = currentArrayBuffer[T]()
#  buffer.bindBuffer
#
#proc swap[T](buffer: ElementArrayBuffer[T]): ElementArrayBuffer =
#  result = currentElementArrayBuffer[T]()
#  buffer.bindBuffer
#
#proc swap[T](buffer: UniformBuffer[T]): UniformBuffer =
#  result = currentUniformBuffer()
#  buffer.bindBuffer


template withBuffer(buffer: expr, body: stmt): stmt {.immediate.} =
  var buf = buffer
  buf.bindBuffer
  body
  buf = 0
  buf.bindBuffer

proc I4() : Mat4x4[float] = mat4x4(
  vec4(1.0, 0, 0, 0),
  vec4(0.0, 1, 0, 0),
  vec4(0.0, 0, 1, 0),
  vec4(0.0, 0, 0, 1)
)

var vertex: seq[Vec3[float]] = @[
  vec3(+1.0, +1.0, -1.0), vec3(-1.0, +1.0, -1.0), vec3(-1.0, +1.0, +1.0), vec3(+1.0, +1.0, +1.0), vec3(+1.0, +1.0, -1.0), vec3(-1.0, +1.0, +1.0),
  vec3(+1.0, -1.0, +1.0), vec3(-1.0, -1.0, +1.0), vec3(-1.0, -1.0, -1.0), vec3(+1.0, -1.0, -1.0), vec3(+1.0, -1.0, +1.0), vec3(-1.0, -1.0, -1.0),
  vec3(+1.0, +1.0, +1.0), vec3(-1.0, +1.0, +1.0), vec3(-1.0, -1.0, +1.0), vec3(+1.0, -1.0, +1.0), vec3(+1.0, +1.0, +1.0), vec3(-1.0, -1.0, +1.0),
  vec3(+1.0, -1.0, -1.0), vec3(-1.0, -1.0, -1.0), vec3(-1.0, +1.0, -1.0), vec3(+1.0, +1.0, -1.0), vec3(+1.0, -1.0, -1.0), vec3(-1.0, +1.0, -1.0),
  vec3(-1.0, +1.0, +1.0), vec3(-1.0, +1.0, -1.0), vec3(-1.0, -1.0, -1.0), vec3(-1.0, -1.0, +1.0), vec3(-1.0, +1.0, +1.0), vec3(-1.0, -1.0, -1.0),
  vec3(+1.0, +1.0, -1.0), vec3(+1.0, +1.0, +1.0), vec3(+1.0, -1.0, +1.0), vec3(+1.0, -1.0, -1.0), vec3(+1.0, +1.0, -1.0), vec3(+1.0, -1.0, +1.0)
]

var color: seq[Vec3[float]] = @[
  vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0),
  vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0), vec3(1.0, 0.5, 0.0),
  vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0),
  vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0),
  vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0),
  vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0)
]

type Attribute =
  tuple[name: string, gl_type: string, location: int]

type Uniform =
  tuple[name: string, gl_type: string, location: int]

proc attribute[T](name:string, t : seq[T], location: int = -1) : Attribute = (name, glslType(t), location)
proc uniform[T](name:string, t : seq[T], location: int = -1) : Uniform = (name, glslType(t), location)

type ShaderParam =
  tuple[name: string, gl_type: string]

type Program =
  ref object
    uniforms: seq[ShaderParam]
    attributes: seq[ShaderParam]
    varyings: seq[ShaderParam]
    frag_out: seq[ShaderParam]
    vertex_prg: string
    fragment_prg: string

proc vertexSource(prg: Program): string =
  result = "#version 330\n#extension GL_ARB_explicit_uniform_location : enable\n"
  for i, u in prg.uniforms:
    result.add("layout(location = $3) uniform $2 $1;\n" % [u.name, u.gl_type, $(i)])
  for i, a in prg.attributes:
    result.add("layout(location = $3) in $2 $1;\n" % [a.name, a.gl_type, $(i)])
  for v in prg.varyings:
    result.add("out $2 $1;\n" % [v.name, v.gl_type])
  result.add("void main() {\n")
  result.add(prg.vertex_prg)
  result.add("\n}")

proc fragmentSource(prg: Program): string =
  result = "#version 330\n#extension GL_ARB_explicit_uniform_location : enable\n"
  for i, u in prg.uniforms:
    result.add("layout(location = $3) uniform $2 $1;\n" % [u.name, u.gl_type, $(i)])
  for v in prg.varyings:
    result.add("in $2 $1;\n" % [v.name, v.gl_type])
  for v in prg.frag_out:
    result.add("out $2 $1;\n" % [v.name, v.gl_type])
  result.add("void main() {\n")
  result.add(prg.fragment_prg)
  result.add("\n}\n")

proc shaderSource(shader: GLuint, source: string) =
  var source_array: array[1, string] = [source]
  var c_source_array = allocCStringArray(source_array)
  defer: deallocCStringArray(c_source_array)
  glShaderSource(shader, 1, c_source_array, nil)

proc compileStatus(shader:GLuint): bool =
  var status: GLint
  glGetShaderiv(shader, GL_COMPILE_STATUS, status.addr)
  status != 0

proc linkStatus(program:GLuint): bool =
  var status: GLint
  glGetProgramiv(program, GL_LINK_STATUS, status.addr)
  status != 0

proc shaderInfoLog(shader: GLuint): string =
  var length: GLint = 0
  glGetShaderiv(shader, GL_INFO_LOG_LENGTH, length.addr)
  result = newString(length.int)
  glGetShaderInfoLog(shader, length, nil, result)

proc showError(log: string, source: string): void =
  let lines = source.splitLines
  for match in log.findIter(re"(\d+)\((\d+)\).*"):
    let line_nr = match.captures[1].parseInt;
    echo lines[line_nr - 1]
    echo match.match

proc programInfoLog(program: GLuint): string =
  var length: GLint = 0
  glGetProgramiv(program, GL_INFO_LOG_LENGTH, length.addr);
  result = newString(length.int)
  glGetProgramInfoLog(program, length, nil, result);

proc reshape(newWidth: cint, newHeight: cint) =
  glViewport(0, 0, newWidth, newHeight)   # Set the viewport to cover the new window
  glMatrixMode(GL_PROJECTION)             # To operate on the projection matrix
  glLoadIdentity()                        # Reset
  gluPerspective(45.0, newWidth / newHeight, 0.1, 100.0)  # Enable perspective projection with fovy, aspect, zNear and zFar

proc createShader(shaderType: GLenum, source: string): GLuint =
  result = glCreateShader(shaderType)
  result.shaderSource(source)
  glCompileShader(result)
  if not result.compileStatus:
    echo "==== start Shader Problems ======================================="
    echo source
    echo "------------------------------------------------------------------"
    showError(result.shaderInfoLog, source)
    echo "==== end Shader Problems ========================================="

proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers
  glMatrixMode(GL_MODELVIEW)                          # To operate on model-view matrix
  var mvp: Mat4x4[float]  = I4()
  mvp = mvp.transform( vec3(2*sin(time), 2*cos(time), -7.0) )
  mvp = mvp.rotate( vec3[float](0,0,1), time*1.1 )
  mvp = mvp.rotate( vec3[float](0,1,0), time*1.2 )
  mvp = mvp.rotate( vec3[float](1,0,0), time*1.3 )
  glLoadMatrixd(cast[ptr GLDouble](mvp.addr))

  #glCallList(index)
  glPushClientAttrib(GLbitfield(GL_CLIENT_ALL_ATTRIB_BITS))
  glEnableClientState(GL_COLOR_ARRAY)
  glEnableClientState(GL_VERTEX_ARRAY)

  glVertexPointer(3, cGL_DOUBLE, 0, vertex[0].addr)
  glColorPointer(3, cGL_DOUBLE, 0, color[0].addr)

  glDrawArrays(GL_TRIANGLES, 0, GLsizei(vertex.len) )
  glPopClientAttrib()

  var gl_program {.global.} : GLuint  = 0;
  if gl_program == 0:
    var myprog = Program(
      uniforms: @[ ("mvp", uniformType(mvp) ) ],
      attributes: @[ ("pos", glslType(vertex) ), ("col", glslType(color)) ],
      varyings: @[ ("v_col", "vec4") ],
      frag_out: @[ ("color", "vec4") ],
      vertex_prg: "gl_Position = mvp * vec4(pos,1); v_col = vec4(col,1);",
      fragment_prg: "color = v_col;"
    )

    let vertexShader = createShader(GL_VERTEX_SHADER, myprog.vertexSource)
    defer: glDeleteShader(vertexShader)
    let fragmentShader = createShader(GL_FRAGMENT_SHADER, myprog.fragmentSource)
    defer: glDeleteShader(fragmentShader)

    gl_program = glCreateProgram()
    glAttachShader(gl_program, vertexShader)
    glAttachShader(gl_program, fragmentShader)
    glLinkProgram(gl_program)

    if not gl_program.linkStatus:
      echo "Log: ", gl_program.programInfoLog

  glUseProgram(gl_program)
  glVertexAttribPointer(0, 3, cGL_DOUBLE, false, 0, vertex[0].addr)
  glVertexAttribPointer(1, 3, cGL_DOUBLE, false, 0, color[0].addr)

  var mvp_float : Mat4x4[float32] = mat4x4[float32](mvp)
  glUniformMatrix4fv(0, 1, true, cast[ptr GLfloat](mvp_float.addr))

  glUseProgram(0);
  window.glSwapWindow # Swap the front and back frame buffers (double buffering)

# Frame rate limiter

let targetFramePeriod: uint32 = 20 # 20 milliseconds corresponds to 50 fps
var frameTime: uint32 = 0

proc limitFrameRate() =
  let now = getTicks()
  if frameTime > now:
    delay(frameTime - now) # Delay to maintain steady frame rate
  frameTime += targetFramePeriod

# Main loop

var
  evt = sdl2.defaultEvent
  runGame = true

reshape(screenWidth, screenHeight) # Set up initial viewport and projection

while runGame:
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    if evt.kind == WindowEvent:
      var windowEvent = cast[WindowEventPtr](addr(evt))
      if windowEvent.event == WindowEvent_Resized:
        let newWidth = windowEvent.data1
        let newHeight = windowEvent.data2
        reshape(newWidth, newHeight)
    if evt.kind == KeyDown:
      var keyboardEvent = cast[KeyboardEventPtr](addr(evt))
      if keyboardEvent.keysym.scancode == SDL_SCANCODE_ESCAPE:
        runGame = false
        break

  time = float64( getTicks() ) / 1000.0

  render()

  limitFrameRate()
