
# minimal rendering prototyper

[DEMO](https://www.youtube.com/watch?v=JO0iqGDgFqA) (warning slow
speaking)

This project is a toolbox project that wants to make the
utilization of the GPU for rasterization and computation as easy as
possible.

## motivation

In computer graphics to my experience the most time consuming task to
utilize the GPU is to actually fiddle with the interface (OpenGL)
between code that runs on the CPU (mostly C++) and code that runs on
the GPU (GLSL). When the goal of the rendering pipeline is not to have
photo realistic graphics the logic that needs to be executed within
the shader is in many cases trivial, but getting the data on the GPU,
that the GPU code can actually understand it correctly, and knitting
together the data of the buffers with the attributes in the shader is
not only tedious but also very error prone and without good tools hard
to debug. With nothing more than the code editor and an OpenGL context
to play with the programmer often sits in front of a black screen with
no feedback which of the 100 necessary steps have a wrong parameter.

Unlike many other attempts, I do not think that providing a default
gouraud or phong shader that can be configured with several variables
or options helps at all to solve this problem. The code needed to
implement a phong shader is trivial if you know the math, it literally
is a single line of code.  But setting up a shader (compiling
linking), and connecting it with correct data is not.  Providing
default shaders where we do not need to write any GPU code, just puts
us back to the time when OpenGL had its fixed function pipeline, and
we get all it's inflexibility with it. As soon as we do not want the
default rendering, things become much more complicated and we try to
avoid them. But I would really like to encourage unconventional ways
of rendering, because only when you try out the unknown you can
eventually find something that looks interesting. And for
that reason I was looking for a solution that really gets rid of all
the Boilerplate code.  Writing a shader should be as easy as writing a
C++11 lambda expression.

## Technology (Why Nim?)

I tried to do 3D OpenGL rendering in a lot of programming languages
already, C++, Scala and Go.  All of them great languages with their
benefits and drawbacks. But in none of the above languages I felt able
to implement what I was looking forward to implement. First of all I
need a language that supports an array of struct. A simple data
structure that puts all its elements sequentially in memory, so that I
could pass data as OpenGL buffers forward and backward between GPU and
CPU without transformation.  Even though this seems trivial it already
disqualifies Scala, because on the Java virtual machine (Scala runs on
it) this can simply not be done.

When I want to be able to capture local variables into the scope of a
shader, I need to map this type to a representation that GLSL
understands. While I can do a lot in C++ with *templates*, *constexpr*,
*sizeof*, *offsetof* one simple thing I probably never will be able to do
is iterating a fields of a struct. Scala and Go can do this, it is
called reflection.  One more reason I do not want to develop in C++ is
the experience I had with very long compilation time (more than 30
minutes). I already invested many hours optimizing build times and
making headers as small as possible, just to see the next day that the
new project member doesn't know or care and includes a lot of headers
in other header files.  Every time I see myself thinking of how I
should structure my project, so that it builds fast, feels like a huge
waste of time that could otherwise go to develop the actual engine
code.

The overall experience to develop in Go was much nicer. No complicated
CMake project files or sbt project files (Scala), just the engine code
and it compiles.  But the programming language just feels too stupid
to make anything as smart as this project needs to have. Go doesn't
even have static dispatch on functions or generics.
[In my project](https://github.com/krux02/turnt-octo-wallhack) I felt
like I had to duplicate a lot of code every time I wanted to render
some slightly different data.

Eventually I decided, that being able to do transformations on the
*AST* (abstract syntax tree) of the programming language would greatly
help to develop this toolbox. I tried out Rust, but I quickly found
out that their macros are too
weak
[to even implement println](https://github.com/rust-lang/rust/blob/master/src/libstd/macros.rs#L222),
and I had to look somewhere else. (At this point in time Rust also has
procedural macros, but they still don't appear to be powerful enough
to do the things I need to do for this project). Eventually with Nim I
finally found a language that suited my needs. It has AST based
macros, arbitrary code execution at compile time, static typing,
reflection and a lot more.

## Concepts

### the c++11 lambda like

As mentioned earlier, creating code that renders things from data
stored in local variables should be as easy/complicated as writing a
c++11 style lambda expression.  While I cannot provide the compact
syntax for c++11 lambdas here in an *embedded DSL* (embeded domain
specific language), I am very much able to have the same
functionality. A c++ lambda has three bracket pairs, the capture, the
arguments and the body or code block.
``[capture1, capture2](A arg1, B arg2){ foo(arg1); bar(arg2); }``. The
captures are variables that are copied into the lambda object, and
then they can be accessed from within the body of the lambda
expression. That is what I wanted to have for my shaders, too. I
wanted to be able to write a shader inline in my render function, and
from there I wanted to be able to access all local variables with
their name from the actual shader code.


    shadingDsl:
      primitiveMode = GL_TRIANGLES
      numVertices = mesh.numVertices

      uniforms:
        lightDir
        mvp = projection * modelview
        tex = mesh.texture
      attributes:
        a_position = mesh.vertexPositionBuffer
        a_normal   = mesh.vertexNormalBuffer
        a_texCoord = mesh.vertexTexCoordBuffer
      vertexMain:
        """
        gl_Position = mvp * a_position;
        v_normal    = mvp * a_normal;
        v_texCoord  = a_texCoord
        """
      vertexOut:
        "out vec4 v_normal"
        "out vec2 v_texCoord"
      fragmentMain:
        """
        color = texture(tex, v_texCoord) * max(dot(ligthDir, v_normal), 0);
        """

The code block that starts with ``shadingDsl:`` is the actual DSL that
gets parsed to generate the appropriate OpenGL code.  The first thing
to notice are the assignments to *primitiveMode* and *numVertices*.
Thise are some named arguments for the dsl. These named arguments do
not need to be passed to the DSL is cases where they can be inferred
from other parameters. The primitive mode and the numVertices are
forwarded to the OpenGL draw call.

The uniforms section is the first part that is inspired by the c++11
lambda. ``lightDir`` is a capture that is captured with an OpenGL
uniform. for lightDir each shader gets a line at the beginning
``uniform vec4 lightDir;``. For `mvp` the line ``uniform mat4 mvp;``
is generated. For the CPU side code each uniform gets a uniform
location, a call to query this uniform location, and a call to set
this uniform to the value of the local variable. The type *vec4* is
inferred from the type of ``lightDir`` and *mat4* is inferred from the
type of the expression ``projection * modelview``. The names for the
uniforms in the shader code are the identifiers *lightDir* and
*mvp*. The assignment syntax was added to be able to give arbitrary
names to uniforms but it turned out that supporting arbitrary
expressions on the right side of the assignment was just trivial.

The attributes section works analog to the uniforms section, it just
works with per vertex attributes.  Attributes are stored in
ArrayBuffers. So in order to get an attribute ``in vec4 a_position;``
the type of ``mesh.vertexPositionBuffer`` needs to be
``ArrayBuffer[Vec4f]``. An *ArrayBuffer* is like an array, just that
it's data is stored inside of the GPU, and for convenience reasons
they can be converted to and from the *seq* type, and in iterator is
supported that uses memory mapping internally to read all the
data. But simple indexing into the buffer is not possible (if it would
be, it shouldn't be used for performance reasons).

The *vertexMain* section will be inserted into the shader between
``void main {`` and ``}`` the strings in the *vertexOut* section get
inserted to the vertex shader, and with the "out" replaced by "in"
inserted in the next shader stage. In this case it is the fragment
shader.

The fragment shader is the next part that gets a bit interesting
again. As you can see I can write into color. This is a name I have
chosen for the case, rendering is done to the screen.  But in case a
framebuffer is bound, and rendering is done to textures, the names of
the output variables are inferred from the targets of the currently
active framebuffer. More on that in the future (or read the code).


## Examples

from here on are just some onld examples and screenshots. They are not all up to date.

![sandbox](https://media.giphy.com/media/l1J9s65SWjHiyaJeo/giphy.gif)

```
< CubeVertices | cubeVertexShader | someFragmentShader > framebuffer1
< framebuffer1  |  postProcessFragmentShader > screen
CubeVertices | forwardVertexShader | faceNormalGeometryShader | forwardFragmentShader >> screen
```

This renders a simple cube with a different colors on each side. The
depth and the color is rendered into a frame buffer. The frame buffer
is then used to put a post process on it that simply moves each line
of the texture horizontally, simply to show that post processing is
possible. Then the cube is rendered again, but this time the geometry
shader transforms each face into a normal, so that you can see each
face normal as a line. Interesting to mention here, is that every call
of `shadingDsl` has a block to pass variables from the current context
to the shader. It is comparable to the c++11 lambda expressions, with
explicit closure. The difference here is that 'uniforms' expects
values that actually can be passed as a uniform to a shader, and
attributes expects a sequence type or a typed opengl-buffer, where the
elements are visible in the fragment shader.

```Nim
shadingDsl:
  primitiveMode = GL_TRIANGLES
  numVertices = vertex.len
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
```

![Imgur2](http://i.imgur.com/eF45VTy.png)

```
framebufferdeclaration depth color normal
heightmapVertices | vertexShader | fragmentShader > framebuffer
update lightPositions
< lightPositions | vertexShader | fragmentShader > screen
```

Here you see a test with 500 moving light sources being rendered via
instancing. The process is called deferred shading. So first only the
scene without lighting is rendered, and then in a second step the
effect of each light source is rendered and added into the final
image. Interesting to note here is in the framebuffer code:

```Nim
declareFramebuffer(FirstFramebuffer):
  depth = newDepthTexture2D(windowsize)
  color = newTexture2D(windowsize, GL_RGBA8)
  normal = newTexture2D(windowsize, GL_RGBA16F)
```

While depth is currently an attribute every framebuffer needs, normal
is a completely free identifier. It could be named anything. The
effect is visible in the fragment shader code from the contex, where
the framebuffer is bound. Here the fragmentshader automatically get's
to know the output variable normal.

```
  fragmentMain:
    """
    color = texture(crateTexture, g_texcoord);
    normal.rgb = g_normal_cs;
    """
```

![Imgur3](http://i.imgur.com/Z1OCZip.jpg)

The light sources from a closer perspective. The light source texture
is rendered again via instancing. The texture itself is alpha centauri
captured from the hubble space telescope, in case you care.

![Imgur4](http://i.imgur.com/ZoEAQEe.png)

This is a simple bone animation loaded from an iqm file. Iqm files
have horrible documentation, but it is a nice binary format for 3D
models with bone animations. On each bone also the name from the iqm
file is rendered.

![Imgur5](http://i.imgur.com/YENpsbQ.png)

Same mesh from a different perspective

![Imgur6](http://i.imgur.com/70TDgF0.png)

Again the same mesh, same program, but this time a different shader
pipeline is used with a geometry shader that renders vertex normals
and tangents as colored lines.

# supported compiler switches

#### -d:noregexp

When for some reason the module `nre` does not work (`pcre` not found)
this switch disables it. Regular expressions are used to parse shader
compiler errors.  Without them the compiler error message is still
printed, but properly parsed messages are nicer for development.

# requirements (important for owners of computers from Apple)

All examples are written to require OpenGL 3.3 core profile, and the
following extensions:

 * ``GL_ARB_direct_state_access``

Direct state access is a feature that got introduced into core opengl
version 4.5, mostly to eliminate a lot of state binding, and allow
opengl objects to behave much more like objects known from object
oriented languages.  This feature does not require any special kind of
hardware, therefore ARB_direct_state_access was created, that allows
to use this 4.5 feature in 2.0 context. The only thing required is a
modern GPU driver. As I recently learned, requirering a decent driver
excludes support for Mac OS. As far as I know Apple restricts its
customers to only use their driver that they bundle with their system
updates, and then they just don't provide OpengGL updates. Please
correct me, if I am wrong but as far as I know the highest Version of
OpenGL usable on Mac OS is 4.1. That version is from 2010, and since
then a lot of problems have been resolved and extensions have been
released, to make the live of developers easier, just not for
people of Apple computers.

I will not try to put some dirty hacks into this project to make it
work on Mac OS, it would greatly reduce the readability of the code,
and then it might happen that at the same time I am done, Apple just
releases a driver and all the work was for nothing. What you can do,
is you try to find a hack that lets you allow to install a newer
driver into your operating system, or you can find a way to hack
another operating on your machine that allows you to use modern
drivers. There hardware is not the limitation. Sorry for the
inconvenience.

# ideas for other names

The name is not fix yet, these are valid naming ideas:

 * BlitzNim
 * RenderMyData
