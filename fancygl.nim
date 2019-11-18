########################################################################
############################### fancy gl ###############################
########################################################################
import glad/gl, glm, math, random, strutils, macros, ast_pattern_matching,
       sdl2/sdl, sdl2/sdl_image as img, sdl2/sdl_ttf as ttf, os, terminal

const
  GL_VERTEX_ARRAY: GLenum = GLenum(32884)

import fancygl/[
  macroutils,
  basic_random,
  normalizeType,
  typeinfo
]

proc panic*(message: varargs[string, `$`]): void {. noreturn .} =
  ## nobody cares about exception classes
  var msg = ""
  for msgFract in message:
    msg.add msgFract
  raise newException(Exception, msg)

include "fancygl/etc.nim"
include "fancygl/glm_additions.nim"
include "fancygl/stopwatch.nim"
include "fancygl/default_setup.nim"
include "fancygl/shapes.nim"
#include "fancygl/typeinfo.nim"
include "fancygl/samplers.nim"
include "fancygl/samplertypeinfo.nim"
include "fancygl/framebuffer.nim"
include "fancygl/glwrapper.nim"
include "fancygl/heightmap.nim"
include "fancygl/iqm.nim"
include "fancygl/camera.nim"
include "fancygl/sdladditions.nim"
include "fancygl/cameraControls.nim"
include "fancygl/std140AlignedWrite.nim"
include "fancygl/nimgen.nim"
include "fancygl/shadingDsl.nim"
include "fancygl/text.nim"

export gl, glm, sdl, basic_random, macroutils.s
export math.arctan2
