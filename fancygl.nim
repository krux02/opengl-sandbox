########################################################################
############################### fancy gl ###############################
########################################################################
import opengl, glm, math, random, strutils, macros,
       macroutils, sdl2, sdl2/image, sdl2/ttf, os, terminal, basic_random

proc panic*(message: varargs[string, `$`]): void {. noreturn .} =
  ## nobody cares about exception classes
  var msg = ""
  for msgFract in message:
    msg.add msgFract
  raise newException(Exception, msg)

include etc, glm_additions, stopwatch, default_setup, shapes, typeinfo, samplers, samplertypeinfo,
       framebuffer, glwrapper, heightmap, iqm, camera, sdladditions, cameraControls, shadingDsl

import text
export text

export opengl, glm, sdl2, basic_random, macroutils.s
export math.arctan2
