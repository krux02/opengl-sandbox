# opengl-sandbox
A sandbox to test nim with opengl.

These examples are nothing spelcial from what they are actually doing, the interesting part is, when you look at the code and see how simle these things can actually be implemented, when you have the help of the macros provided in this package.

![Imgur1](http://i.imgur.com/zHnLCqd.png)
This renders a simple cube with a different colors on each side. The depth and the color is rendered into a frame buffer. The frame buffer is then used to put a post process on it that simply moves each line of the texture horizontally, simply to show that post processing is possible. Then the cube is rendered again, but this time the geometry shader transforms each face into a normal, so that you can see each face normal as a line.

![Imgur2](http://i.imgur.com/eF45VTy.png)
Here you see a test with 500 moving light sources being rendered via instancing. The process is called deferred shading. So first only the scene without lighting is rendered, and then in a second step the effect of each light source is rendered and added into the final image.

![Imgur3](http://i.imgur.com/Z1OCZip.jpg)
The light sources from a closer perspective. The light source texture is rendered again via instancing. The texture itself is alpha centauri captured from the hubble space telescope, in case you care.

![Imgur4](http://i.imgur.com/ZoEAQEe.png)
This is a simple bone animation loaded from an iqm file. Iqm files have horrible documentation, but it is a nice binary format for 3D models with bone animations. On each bone also the name from the iqm file is rendered.

![Imgur5](http://i.imgur.com/YENpsbQ.png)
Same mesh from a different perspective

![Imgur6](http://i.imgur.com/70TDgF0.png)
Again the same mesh, same program, but this time a different shader pipeline is used with a geometry shader that renders vertex normals and tangents as colored lines.

