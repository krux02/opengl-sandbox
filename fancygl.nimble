#[ Package ]#

version       = "0.3.1"
author        = "Arne Döring"
description   = "nice way of handling render code"
license       = "MIT"

bin           = @[
  #"examples/audiotest",
  "examples/deferred_shading",
  "examples/forward_vertex_shader",
  "examples/hello_shapes",
  "examples/hello_triangle",
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
  "examples/console",
]

skipDirs = @["tests"]

#[ Dependencies ]#

requires @[
  "nim         >= 0.18.1",
  "AntTweakBar >= 1.0.0",
  "sdl2_nim    >= 2.0.6.1",
  "glm         >= 1.0.1"
  #"fftw3       >= 0.1.0", # add this if you want audiotest to work
]

task run, "run all examples":
  for binary in bin:
    exec(binary)
