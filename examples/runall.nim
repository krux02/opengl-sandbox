import ../fancygl
import macros

let (window, context) = defaultSetup()

proc resetState(): void =
  glEnable(GL_DEPTH_TEST)
  glViewport(0,0, window.size.x, window.size.y)
  glClearColor(0,0,0,0)
  glCullFace(GL_BACK)
  glDepthFunc(GL_LESS)
  glDisable(GL_CULL_FACE)
  glDisable(GL_BLEND)
  glDisable(GL_STENCIL_TEST)
  glDisable(GL_PROGRAM_POINT_SIZE)
  
  discard setRelativeMouseMode(false)

macro makeRunExamples(args: varargs[untyped]): untyped =
  let imports = nnkImportStmt.newTree()
  let runExamplesBody = newStmtList()

  for ident in args:
    imports.add ident
    runExamplesBody.add quote do:
      resetState()
      `ident`.main(window)
  
  result = quote do:
    `imports`
    proc runExamples(): void =
      `runExamplesBody`

    runExamples()
        
makeRunExamples(
  hello_triangle, 
  shooty, 
  openmesh, 
  forward_vertex_shader, 
  neuralnetwork, 
  hello_shapes, 
  skew_box, 
  mandelbrot, 
  particles, 
  # particles_transform_feedback,  # this is bugged, can't be integrated right now
  fractalworld, 
  craftingmine, 
  # waves,  TODO this causes sometimes segfaults for some reason
  tetris,
  noise_landscape, 
  # octree, 
  # player_controls, 
  # iqm_mesh_loading, 
  # sandbox, 
  # retro_tiling, 
  # kdtree, 
  # deferred_shading, 
  # console, 

)
  
