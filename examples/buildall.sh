#!/usr/bin/env bash

for FILE in *.nim;do
  nim c $FILE
done

./shooty
./openmesh
./forward_vertex_shader
./audiotest
./neuralnetwork
./hello_shapes
./skew_box
./test_animation_code
./mandelbrot
./particles_transform_feedback
./fractalworld
./hello_triangle
./waves
./craftingmine
./tetris
./noise_landscape
./octree
./player_controls
./iqm_mesh_loading
./sandbox
./retro_tiling
./kdtree
./particles
./deferred_shading
./console
