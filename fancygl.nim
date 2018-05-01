########################################################################
############################### fancy gl ###############################
########################################################################
import glad/gl, glm, math, random, strutils, macros,
       sdl2/sdl, sdl2/sdl_image as img, sdl2/sdl_ttf as ttf, os, terminal

const
  GL_VERTEX_ARRAY: GLenum = GLenum(32884)

import fancygl/[
  macroutils,
  basic_random
]

proc panic*(message: varargs[string, `$`]): void {. noreturn .} =
  ## nobody cares about exception classes
  var msg = ""
  for msgFract in message:
    msg.add msgFract
  raise newException(Exception, msg)

include fancygl/etc
include fancygl/glm_additions
include fancygl/stopwatch
include fancygl/default_setup
include fancygl/shapes
include fancygl/typeinfo
include fancygl/samplers
include fancygl/samplertypeinfo
include fancygl/framebuffer
include fancygl/glwrapper
include fancygl/heightmap
include fancygl/iqm
include fancygl/camera
include fancygl/sdladditions
include fancygl/cameraControls
include fancygl/shadingDsl
include fancygl/text

export gl, glm, sdl, basic_random, macroutils.s
export math.arctan2

when not defined(release) and not defined(windows) and not defined(nogdbsection):
  include fancygl/debug
