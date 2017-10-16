#[ Package ]#

version       = "0.2.1"
author        = "Arne Döring"
description   = "nice way of handling render code"
license       = "MIT"

bin           = @[
  #"examples/audiotest",
  "examples/console",
  "examples/deferred_shading",
  "examples/font_rendering",
  "examples/forward_vertex_shader",
  "examples/hello_shapes",
  "examples/hello_triangle",
  "examples/mandelbrot",
  "examples/mesh_loading_tiny",
  "examples/neuralnetwork",
  "examples/noise_landscape",
  "examples/particles",
  "examples/particles_transform_feedback",
  "examples/player_controls",
  "examples/retro_tiling",
  "examples/sandbox",
  "examples/tetris",
  "examples/waves"
]

#kdtree
#octree
#openmesh
#test_animation_code.nim

skipDirs = @["tests"]

#[ Dependencies ]#

requires @[
  "nim         >= 0.17.3",
  "sdl2_nim    >= 2.0.5.0",
  "opengl      >= 1.1.0",
  "glm         >= 0.1.1",
  "AntTweakBar >= 1.0.0"
  #"fftw3       >= 0.1.0", # add this if you want audiotest to work
]
