import ../fancygl

import shooty
import openmesh
import forward_vertex_shader
import neuralnetwork
import hello_shapes
import skew_box
# import test_animation_code
# import mandelbrot
# import particles_transform_feedback
# import fractalworld
# import hello_triangle
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
# import particles
# import deferred_shading
# import console

let (window, context) = defaultSetup()

shooty.main(window)
glViewport(0,0, window.size.x, window.size.y)
openmesh.main(window)
glClearColor(0,0,0,0)
forward_vertex_shader.main(window)
neuralnetwork.main(window)
glEnable(GL_DEPTH_TEST)
hello_shapes.main(window)
skew_box.main(window)
# test_animation_code.main(window)
# mandelbrot.main(window)
# particles_transform_feedback.main(window)
# fractalworld.main(window)
# hello_triangle.main(window)
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
# particles.main(window)
# deferred_shading.main(window)
# console.main(window)

