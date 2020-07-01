import renderMacro

import glm/noise
import macros

let (window, context) = defaultSetup(vec2i(640,480))
# let (window, context) = defaultSetup()

type
  MyFragmentType = object
    color: Vec4f # {. GL_RGB16F .}

  MyVertexType = tuple
    position: Vec4f
    color: Vec4f

  #MyMesh = Mesh[MyVertexType
genMeshType(MyMesh, MyVertexType)

var triangleMesh: MyMesh
triangleMesh.vertexIndices.mode = GL_TRIANGLES
triangleMesh.vertexIndices.numVertices = 3

triangleMesh.buffers.position = arrayBuffer([
  vec4f(-1,-1, 0, 1),
  vec4f( 1,-1, 0, 1),
  vec4f( 0, 1, 0, 1)
])

triangleMesh.buffers.color = arrayBuffer([
  vec4f( 1, 0,0,1),
  vec4f(0, 1,0,1),
  vec4f(0,0,1,1)
])

var runGame: bool = true

let timer = newStopWatch(true)

let aspect = window.aspectRatio.float32
let projMat : Mat4f = frustum(-aspect * 0.01f, aspect * 0.01f, -0.01f, 0.01f, 0.01f, 100.0)

while runGame:

  for evt in events():
    if evt.kind == QUIT:
      runGame = false
      break
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_ESCAPE:
      runGame = false
    if evt.kind == KEY_DOWN and evt.key.keysym.scancode == SCANCODE_F10:
      window.screenshot

  #let time = (frame / 100) * Pi * 2
  let time = timer.time.float32

  let viewMat = mat4f(1)
    .translate(0,1,5)            # position camera at position 0,1,5
    .rotateX(Pi * -0.05)         # look a bit down
    .inverse                     # the camera matrix needs to be inverted

  let modelMat = mat4f(1)
    .rotateY(time)               # rotate the triangle
    .scale(3)                    # scale the triangle to be big enough on screen

  let mvp = projMat * viewMat * modelMat

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)


  type
    UniformBufType705242 = tuple[mvp: type(mvp)]
  type
    Pipeline705240 = tuple[program: Program, vao: VertexArrayObject,
                         uniformBufferHandle: GLuint, uniformBufferData: pointer,
                         uniformBufferSize: GLint]
  var p705241 {.global.}: Pipeline705240
  if p705241.program.handle ==
      0:
    p705241.program.handle = glCreateProgram()
    p705241.program.attachAndDeleteShader(compileShader(GL_VERTEX_SHADER, """#version 450
  // types section
  // uniforms section
  layout(std140, binding=0) uniform dynamic_shader_data {
  mat4 mvp;
  };
  // all attributes
  in layout(location=0) vec4 in_vertex_position;
  in layout(location=1) vec4 in_vertex_color;
  // all varyings
  out layout(location=0) vec4 out_vertex_color;
  void main() {
    // convert used attributes to local variables (because reasons)
    vec4 vertex_position = in_vertex_position;
    // glsl translation of main body
    gl_Position = (mvp * vertex_position);
    // forward attributes that are used in the fragment shader
    out_vertex_color = in_vertex_color;
    // forward other varyings
  }
  """, LineInfo(
        filename: "/home/arne/proj/nim/fancygl/experiment/hello_triangle.nim",
        line: 70, column: 4)))
    p705241.program.attachAndDeleteShader(compileShader(GL_FRAGMENT_SHADER, """#version 450
  // types section
  // uniforms section
  layout(std140, binding=0) uniform dynamic_shader_data {
  mat4 mvp;
  };
  // fragment output symbols
  out layout(location=0) vec4 result_color;
  // all varyings
  in layout(location=0) vec4 in_vertex_color;
  void main() {
  // convert varyings to local variables (because reasons)
  vec4 vertex_color = in_vertex_color;
  /// rasterize
  ;
  result_color = vertex_color;
  }
  """, LineInfo(
        filename: "/home/arne/proj/nim/fancygl/experiment/hello_triangle.nim",
        line: 70, column: 4)))
    p705241.program.linkOrDelete
    glCreateVertexArrays(1, p705241.vao.handle.addr)
    let uniformBlockIndex705245 = glGetUniformBlockIndex(p705241.program.handle,
        "dynamic_shader_data")
    doAssert uniformBlockIndex705245 != GL_INVALID_INDEX
    var blockSize705244: GLint
    glGetActiveUniformBlockiv(p705241.program.handle, uniformBlockIndex705245,
                              GL_UNIFORM_BLOCK_DATA_SIZE, blockSize705244.addr)
    assert blockSize705244 > 0
    p705241.uniformBufferSize = blockSize705244
    glCreateBuffers(1, p705241.uniformBufferHandle.addr)
    glNamedBufferStorage(p705241.uniformBufferHandle, GLsizei(blockSize705244), nil,
                         GL_DYNAMIC_STORAGE_BIT)
    p705241.uniformBufferData = alloc(blockSize705244)
    glEnableVertexArrayAttrib(p705241.vao.handle, 0'u32)
    glVertexArrayBindingDivisor(p705241.vao.handle, 0'u32, 0)
    setFormat(p705241.vao, 0'u32, triangleMesh.buffers.position)
    glVertexArrayAttribBinding(p705241.vao.handle, 0'u32, 0'u32)
    glEnableVertexArrayAttrib(p705241.vao.handle, 1'u32)
    glVertexArrayBindingDivisor(p705241.vao.handle, 1'u32, 0)
    setFormat(p705241.vao, 1'u32, triangleMesh.buffers.color)
    glVertexArrayAttribBinding(p705241.vao.handle, 1'u32, 1'u32)
  glVertexArrayElementBuffer(p705241.vao.handle, triangleMesh.vertexIndices.handle)
  glUseProgram(p705241.program.handle)
  glBindVertexArray(p705241.vao.handle)
  var uniformObject705243: UniformBufType705242
  uniformObject705243.mvp = mvp
  discard std140AlignedWrite(p705241.uniformBufferData, 0, uniformObject705243)
  glNamedBufferSubData(p705241.uniformBufferHandle, 0, p705241.uniformBufferSize,
                       p705241.uniformBufferData)
  glBindBufferBase(GL_UNIFORM_BUFFER, 0, p705241.uniformBufferHandle)
  bindTextures(0)
  setBuffers(p705241.vao, 0, triangleMesh.buffers.position,
             triangleMesh.buffers.color)
  let numVertices705247 = GLsizei(triangleMesh.len)
  if triangleMesh.vertexIndices.handle ==
      0:
    glDrawArrays(triangleMesh.vertexIndices.mode, 0, numVertices705247)
  else:
    glDrawElementsBaseVertex(triangleMesh.vertexIndices.mode, numVertices705247,
                             triangleMesh.vertexIndices.typ, cast[pointer](triangleMesh.vertexIndices.byteOffset),
                             GLint(triangleMesh.vertexIndices.baseVertex))


  # triangleMesh.renderDebug do (vertex, gl):
  #   gl.Position = mvp * vertex.position
  #   ## rasterize
  #   result.color = vertex.color
  #   #result.color.rgb = vec3f(simplex((modelMat * vertex.position).xyz))

  glSwapWindow(window)
