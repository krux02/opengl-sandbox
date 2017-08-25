
#shadingDsl

 * uniforms

  Each uniform to the shader is represented here as an
  assignment. The identifier on the left side of each assignment
  should define how the uniform can be used from the shader. The
  expression on the right of the assignment should be a c++ symbol
  that sets the value and the type of the uniform. There has to be
  done some type mapping. In this case modelViewMat is of type
  glm::mat4, in GLSL modelView should be of type mat4, the GLSL
  mapping of that type.

 * attributes

  Each attribute to the shader is represented here as an
  assignment. The identifier on the left side of each assignment
  should define how the attribute can be used from the shader. The
  expression on the right of the assignment should be a c++ symbol
  that sets the value and the type of the attribute. There should be
  done some type mapping. In this case vertices is of type
  ArrayBuffer<glm::vec4>, in GLSL a_vertex should be of type vec4, the
  GLSL attribute mapping of that type.

 * vertexMain:

  This is the code that will be inserted in the main function of the
  vertex shader. As you can see, shader code does not need to be
  complicated, when there is no complicated shading happening.

 * vertexOut:

  This is simply the GLSL code that would be written in the shader, to
  declare the output types of the vertex shader. The only reason to
  put it in a special block, is to reuse that code block in the
  fragment shader as inputs.

 * fragmentMain:

  This is the code for the fragment shader. In this case it simply
  forwards the input color from the rasterization stage as output
  color. Inputs are generated from the "vertexOut" section, but with
  "out" replaced by "in".
