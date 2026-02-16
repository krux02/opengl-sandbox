import ../fancygl

import hello_triangle
import shooty
import openmesh
import forward_vertex_shader
import neuralnetwork
import hello_shapes
import skew_box
import mandelbrot
import particles
# import particles_transform_feedback
import fractalworld
# import waves
# import craftingmine
# import tetris
# import noise_landscape
# import octree
# import player_controls
# import iqm_mesh_loading
# import sandbox
# import retro_tiling
# import kdtree
# import deferred_shading
# import console

let (window, context) = defaultSetup()

proc resetGlState(): void =
  glEnable(GL_DEPTH_TEST)
  glViewport(0,0, window.size.x, window.size.y)
  glClearColor(0,0,0,0)

resetGlState()
hello_triangle.main(window)
resetGlState()
shooty.main(window)
resetGlState()
openmesh.main(window)
resetGlState()
forward_vertex_shader.main(window)
resetGlState()
neuralnetwork.main(window)
resetGlState()
hello_shapes.main(window)
resetGlState()
skew_box.main(window)
resetGlState()
mandelbrot.main(window)
resetGlState()
particles.main(window)
# resetGlState()
# particles_transform_feedback.main(window) # this is bugged, can't be integrated right now
resetGlState()
fractalworld.main(window)
# waves.main(window)
# craftingmine.main(window)
# tetris.main(window)
# noise_landscape.main(window)
# octree.main(window)
# player_controls.main(window)
# iqm_mesh_loading.main(window)
# sandbox.main(window)
# retro_tiling.main(window)
# kdtree.main(window)
# deferred_shading.main(window)
# console.main(window)
