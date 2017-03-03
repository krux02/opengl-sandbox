########################################################################
############################### fancy gl ###############################
########################################################################
import opengl, glm, math, random, strutils, macros,
       includes/macroutils, sdl2, sdl2/image, sdl2/ttf, os, terminal, includes/basic_random

proc panic*(message: varargs[string, `$`]): void {. noreturn .} =
  ## nobody cares about exception classes
  var msg = ""
  for msgFract in message:
    msg.add msgFract
  raise newException(Exception, msg)

include includes/etc, includes/glm_additions, includes/stopwatch, includes/default_setup, includes/shapes, includes/typeinfo, includes/samplers, includes/samplertypeinfo,
       includes/framebuffer, includes/glwrapper, includes/heightmap, includes/iqm, includes/camera, includes/sdladditions, includes/cameraControls, includes/shadingDsl

include includes/text

export opengl, glm, sdl2, basic_random, macroutils.s
export math.arctan2
