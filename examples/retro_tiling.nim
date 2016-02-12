import math, sequtils, sdl2, opengl, ../fancygl, glm

type TileMap = object
  w,h: int
  dataseq: seq[int16]

proc `[]`*(hm: var TileMap, x,y: int): int16 {. inline .} =
  let
    nx = x and (hm.w - 1)
    ny = y and (hm.h - 1)

  hm.dataseq[nx + hm.w * ny]

proc `[]=`*(hm: var TileMap, x,y: int; value: int16)  {. inline .} =
  let
    nx = x and (hm.w - 1)
    ny = y and (hm.h - 1)

  hm.dataseq[nx + hm.w * ny] = value
