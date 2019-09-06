#[ Package ]#

version       = "0.3.2"
author        = "Arne Döring"
description   = "nice way of handling render code"
license       = "MIT"

bin           = @[
  "examples/hello_triangle",
  "examples/forward_vertex_shader",
  "examples/deferred_shading",
  "examples/hello_shapes",
  "examples/mandelbrot",
  "examples/iqm_mesh_loading",
  "examples/neuralnetwork",
  "examples/noise_landscape",
  "examples/particles",
  "examples/particles_transform_feedback",
  "examples/player_controls",
  "examples/retro_tiling",
  "examples/sandbox",
  "examples/tetris",
  "examples/waves",
  "experiment/main",
  "examples/console",
]

skipDirs = @["unittests", "experiment"]

#[ Dependencies ]#

requires @[
  "nim                  >= 0.19.9  ",
  "AntTweakBar          >= 1.0.2   ",
  "sdl2_nim             >= 2.0.6.1 ",
  "glm                  >= 1.1.1   ",
  "ast_pattern_matching >= 1.0.0   ",
  #"fftw3               >= 0.1.0   ", # add this if you want audiotest to work
  "OpenMesh              >= 0.1.0  ",
]

task run, "run all examples":
  for binary in bin:
    exec(binary)
