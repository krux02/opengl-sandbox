import macros, opengl, sdl2
import glm

include "typeinfo.nim"

# TODO continue here

type TransformFeedback[T] = object
  handle: cint

type
  MyVertexData = object
    data1: float32
    data2: float64
    data3: int32
    data4: int64
    a,b,c: char

  ParticleData = object
    pos: Vec2f
    vel: Vec2f
    col: Vec3f
    rot: float32
    birthday: float32

const offsetof = """
import glm

template offsetof*(typ, field: untyped): int =
  var dummy: typ
  cast[system.int](addr(dummy.field)) - cast[system.int](addr(dummy))

"""


proc offsetsString(name: string, typeImpl: NimNode): string {.compileTime.} =
  var str = offsetof

  for s in ["type ", name, " = ", typeImpl.repr, "\n"]:
    str.add s

  for identDefs in typeImpl[2]:
    for i in 0 ..< identDefs.len - 2:
      let memberSym = identDefs[i]
      #str.add "echo \"" & $memberSym & " \", offsetof(" & $tpe & ", " & $memberSym & ")\n"
      str.add "stdout.write offsetof(" & name & ", " & $memberSym & "), ' '\n"

  let tempfileName = "/tmp/offsets.nim"

  writeFile(tempfileName, str)
  let offsets = staticExec("nim c --verbosity:0 --hint[Processing]:off -r  " & tempfileName, cache = typeImpl.lispRepr)

  result = offsets

import strutils

proc head(node: NimNode): NimNode {.compileTime.} = node[0]

macro stringIdent(arg: static[string]): string =
  result = newLit(arg)


static:
  echo glslTypeRepr(Vec2f)

macro memberTypeNames[T](t: typedesc[T]): string =
  #result = head quote do:
  #  (var `ident`: `t`; "")

  #let typeSym =

  var typeSym = t.getTypeInst


  # yea sometimes I do not get a bracket expression but just a symbol, yaya inconsistency!!! yeaaaa!!!
  if typeSym.kind == nnkBracketExpr:
    typeSym = typeSym[1]

  echo typeSym.treeRepr

  typeSym.expectKind(nnkSym)

  var res = newLit("")

  let typeImpl = typeSym.getTypeImpl
  for identDefs in typeImpl[2]:
    let memberType = identDefs[^2]
    for i in 0 ..< identDefs.len - 2:
      #let memberSym = identDefs[i]
      res = head quote do:
        `res` & glslTypeRepr(type(`memberType`)) & " "

  result = newCall(bindSym"stringIdent", res)
      #str.add "echo \"" & $memberSym & " \", offsetof(" & $tpe & ", " & $memberSym & ")\n"
      #str.add "stdout.write offsetof(" & name & ", " & $memberSym & "), ' '\n"

# echo memberTypeNames(ParticleData)


dumpTree:
            getAst(type(MyData))

macro glslOutSection[T](self: TransformFeedback[T]): string =


  let typeSym = self.getTypeInst[1]
  typeSym.expectKind nnkSym

  let typeImpl = typeSym.getTypeImpl
  let offsets  = offsetsString($typeSym, typeImpl).split

  #let typenames = getAst(memberTypeNames(typeSym))

  var res = newLit("")
  var i = 0
  for identDefs in typeImpl[2]:
    let memberType = identDefs[^2]
    for k in 0 ..< identDefs.len - 2:
      let member = $identDefs[k]
      let offset = offsets[i]

      let leftStr  = newLit("  layout(xfb_offset = " & offset & ") ")
      let rightStr = newLit(" " & member & ";\n")

      let getAstArg = newCall(ident"type", memberType)
      echo getAst(glslTypeRepr(getAstArg))

      #glslTypeRepr(type(`memberType`))

      i += 1

  result = newLit("")

var
  tf1 : TransformFeedback[MyVertexData]
  tf2 : TransformFeedback[ParticleData]

let section1 = glslOutSection(tf1)
let section2 = glslOutSection(tf2)

#echo glslTypeRepr(ParticleData.memberType(col))
#import strutils
#for line in offsets.splitLines:
#  let words = line.split


#  for word in line.split:
#    echo word
    #echo toHex(parseInt(line), 2)

#[
macro transformFeedbackOutSection(self: TransformFeedback): string =
  var res = newLit("""
#extension GL_ARB_enhanced_layouts : enable
layout(xfb_buffer = 0, xfb_stride = 36) out bananas {
""")

  let tpe = self.getTypeInst[1]
  let typeImpl = tpe.getTypeImpl
  typeImpl.expectKind(nnkObjectTy)
  for identDef in typeImpl[2]:
    for i in 0 ..< identDef.len-2:
      let sym = identDef[i]
      let symName = newLit($sym)

      # glslTypeRepr
      res = head quote do:
        `res` & "layout(xfb_offset = " & $offsetOf(`tpe`, `sym`) & ") vec4 " & `symName` & ";"

  let name = self.repr
  result = res
]#
