########################################################################
############################### fancy gl ###############################
########################################################################
import opengl, glm, math, random, strutils, macros,
       macroutils, sdl2, sdl2/image, sdl2/ttf, os, terminal, basic_random

include etc, glm_additions, stopwatch, default_setup, shapes, typeinfo, samplers, samplertypeinfo,
       framebuffer, glwrapper, heightmap, iqm, camera, sdladditions, cameraControls, shadingDsl, text

export opengl, glm, sdl2, basic_random, macroutils.s
export math.arctan2  
