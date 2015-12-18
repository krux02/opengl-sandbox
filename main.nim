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
import macros

type Vec4f = Vec4[float32]
type Vec3f = Vec4[float32]
type Vec2f = Vec4[float32]
type Vec4d = Vec4[float64]
type Vec3d = Vec4[float64]
type Vec2d = Vec4[float64]

template glslUniformType(t : type Vec4): string = "vec4"
template glslUniformType(t : type Vec3): string = "vec3"
template glslUniformType(t : type Vec2): string = "vec2"
template glslUniformType(t : type Mat4x4): string = "mat4"
template glslUniformType(t : type Mat3x3): string = "mat3"
template glslUniformType(t : type Mat2x2): string = "mat2"
template glslUniformType(t : type float): string = "float"
template glslUniformType(t : type float32): string = "float"
template glslUniformType(t : type float64): string = "float"

template glslAttribType(t : type seq[Vec4]): string = "vec4"
template glslAttribType(t : type seq[Vec3]): string = "vec3"
template glslAttribType(t : type seq[Vec2]): string = "vec2"
template glslAttribType(t : type seq[Mat4x4]): string = "mat4"
template glslAttribType(t : type seq[Mat3x3]): string = "mat3"
template glslAttribType(t : type seq[Mat2x2]): string = "mat2"

type VertexArrayObject = distinct GLuint

proc newVertexArrayObject() : VertexArrayObject =
  glGenVertexArrays(1, cast[ptr GLuint](result.addr))

const nil_vao = VertexArrayObject(0)

proc bindIt(vao: VertexArrayObject) =
  glBindVertexArray(GLuint(vao))

proc delete(vao: VertexArrayObject) =
  var raw_vao = GLuint(vao)
  glDeleteVertexArrays(1, raw_vao.addr)

template blockBind(vao: VertexArrayObject, blk: stmt) : stmt =
  vao.bindIt
  blk
  nil_vao.bindIt

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


proc bindIt[T](buffer: ArrayBuffer[T]) =
  glBindBuffer(GL_ARRAY_BUFFER, GLuint(buffer))

proc bindIt[T](buffer: ElementArrayBuffer[T]) =
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, GLuint(buffer))

proc bindIt[T](buffer: UniformBuffer[T]) =
  glBindBuffer(GL_UNIFORM_BUFFER, GLuint(buffer))


proc bufferData[T](buffer: ArrayBuffer[T], data: var seq[T]) =
  glBufferData(GL_ARRAY_BUFFER, GLsizeiptr(data.len * sizeof(T)), data[0].addr, GL_STATIC_DRAW)

proc bufferData[T](buffer: ElementArrayBuffer[T], data: seq[T]) =
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, GLsizeiptr(data.len * sizeof(T)), data[0].addr, GL_STATIC_DRAW)

proc bufferData[T](buffer: UniformBuffer[T], data: T) =
  glBufferData(GL_ARRAY_BUFFER, GLsizeiptr(sizeof(T)), data.addr, GL_STATIC_DRAW)

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

proc mat4f(mat: Mat4x4[float64]): Mat4x4[float32] =
  for i in 0..<4:
   for j in 0..<4:
     result[i][j] = mat[i][j]

proc uniform(location: GLint, mat: Mat4x4[float64]) =
  var mat_float32 = mat4f(mat)
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat_float32.addr))

proc uniform(location: GLint, mat: var Mat4x4[float32]) =
  glUniformMatrix4fv(location, 1, false, cast[ptr GLfloat](mat.addr))

proc uniform(location: GLint, value: float32) =
  glUniform1f(location, value)

proc uniform(location: GLint, value: float64) =
  glUniform1f(location, value)

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

#type Attribute =
#  tuple[name: string, gl_type: string, location: int]

#type Uniform =
#  tuple[name: string, gl_type: string, location: int]

#proc attribute[T](name:string, t : seq[T], location: int = -1) : Attribute =
#  (name, glslType(t), location)
#
#proc uniform[T](name:string, t : seq[T], location: int = -1) : Uniform =
#  (name, glslType(t), location)

type ShaderParam =
  tuple[name: string, gl_type: string]

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

let sourceHeader = """
#version 330
#extension GL_ARB_explicit_uniform_location : enable
#define M_PI 3.1415926535897932384626433832795

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

proc genShaderSource(
    uniforms : openArray[ShaderParam], uniformLocations : bool,
    inParams : openArray[ShaderParam], inLocations : bool,
    outParams: openArray[ShaderParam], mainSrc: string): string =
  result = sourceHeader
  for i, u in uniforms:
    if uniformLocations:
      result.add format("layout(location = $3) uniform $2 $1;\n", u.name, u.gl_type, i)
    else:
      result.add format("uniform $2 $1;\n", u.name, u.gl_type)
  for i, a in inParams:
    if inLocations:
      result.add format("layout(location = $3) in $2 $1;\n", a.name, a.gl_type, i)
    else:
      result.add format("in $2 $1;\n", a.name, a.gl_type, i)
  for v in outParams:
    result.add format("out $2 $1;\n", v.name, v.gl_type)
  result.add("void main() {\n")
  result.add(mainSrc)
  result.add("\n}")

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

proc compileShader(shaderType: GLenum, source: string): GLuint =
  result = glCreateShader(shaderType)
  result.shaderSource(source)
  glCompileShader(result)
  if not result.compileStatus:
    echo "==== start Shader Problems ======================================="
    echo source
    echo "------------------------------------------------------------------"
    showError(result.shaderInfoLog, source)
    echo "==== end Shader Problems ========================================="

proc linkShader(shaders: varargs[GLuint]): GLuint =
  result = glCreateProgram()

  for shader in shaders:
    glAttachShader(result, shader)
    glDeleteShader(shader)
  glLinkProgram(result)

  if not result.linkStatus:
    echo "Log: ", result.programInfoLog
    glDeleteProgram(result)
    result = 0

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

macro macro_test(statement: expr) : stmt =
  #echo treeRepr(statement)
  #echo repr(statement)
  discard


proc render() =
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT) # Clear color and depth buffers

  var modelview_mat: Mat4x4[float]  = I4()
  modelview_mat = modelview_mat.transform( vec3(2*sin(time), 2*cos(time), -7.0) )
  modelview_mat = modelview_mat.rotate( vec3[float](0,0,1), time*1.1 )
  modelview_mat = modelview_mat.rotate( vec3[float](0,1,0), time*1.2 )
  modelview_mat = modelview_mat.rotate( vec3[float](1,0,0), time*1.3 )

  macro_test:
    uniforms:
      projection = projection_mat
      modelview = modelview_mat
      time
    attributes:
      pos = vertex
      col = color
    varyings:
      var v_col : vec4
    frag_out:
      var color : vec4
    vertex_prg:
      """
      gl_Position = projection * modelview * vec4(pos,1)
      v_col = vec4(col,1)
      """
    fragment_prg:
      """
      color = mymix(v_col, time)
      """

  dumptree:
    var vao {.global.}: VertexArrayObject
    var gl_program {.global.}: GLuint  = 0
    var pos_buffer {.global.}: ArrayBuffer[vertex[0].type]
    var col_buffer {.global.}: ArrayBuffer[color[0].type]

    if gl_program == 0:
      let uniforms: seq[ShaderParam] = @[
          ("projection", glslUniformType(type(projection_mat))),
          ("modelview", glslUniformType(type(modelview_mat))),
          ("time", glslUniformType(type(time))),
      ]
      let attributes: seq[ShaderParam] = @[
          ("pos", glslAttribType(type(vertex))),
          ("col", glslAttribType(type(color))),
      ]
      let varyings: seq[ShaderParam] = @[ ("v_col", "vec4") ]
      let fragOut: seq[ShaderParam] = @[ ("color", "vec4") ]
      let vertexSrc: string =
        """
        gl_Position = projection * modelview * vec4(pos,1);
        v_col = vec4(col,1);
        """
      let fragmentSrc: string =
        """
        color = mymix(v_col, time);
        """

      gl_program = linkShader(
        compileShader(GL_VERTEX_SHADER,   genShaderSource(uniforms, true, attributes, true, varyings, vertexSrc)),
        compileShader(GL_FRAGMENT_SHADER, genShaderSource(uniforms, true, varyings, false, fragOut, fragmentSrc)),
      )

      glUseProgram(gl_program)
      vao = newVertexArrayObject()
      bindIt(vao)

      glEnableVertexAttribArray(0)
      makeAndBindBuffer(pos_buffer, 0, vertex, GL_STATIC_DRAW)
      glEnableVertexAttribArray(1)
      makeAndBindBuffer(col_buffer, 1, color, GL_STATIC_DRAW)

      glBindBuffer(GL_ARRAY_BUFFER, 0)
      bindIt(nil_vao)
      glUseProgram(0)

    glUseProgram(gl_program)

    bindIt(vao)

    uniform(0, projection_mat)
    uniform(1, modelview_mat)
    uniform(2, time)

    glDrawArrays(GL_TRIANGLES, 0, GLsizei(len(vertex)))

    bindIt(nil_vao)
    glUseProgram(0);

  glSwapWindow(window) # Swap the front and back frame buffers (double buffering)

# Main loop

var
  evt = sdl2.defaultEvent
  runGame = true

reshape(screenWidth, screenHeight) # Set up initial viewport and projection

var frameCounter = 0
var frameCounterStartTime = 0.0

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
  if time - frameCounterStartTime >= 1:
    echo "FPS: ", frameCounter
    frameCounter = 0
    frameCounterStartTime = time

  render()
  frameCounter += 1



  #limitFrameRate()

