import macros, strutils

#counter that will be used to generate unique intermediate macro name
#and avoid name collision
var macroCount {.compileTime.} = 0

#this proc is exported because of the NLBFunc macro expansion
#occured on bindFunction caller module
proc bindFuncImpl*(arg: openArray[NimNode]): NimNode {.compileTime.} =
  result = newNimNode(nnkStmtList)
  for n in arg:
    if n.kind == nnkSym:
      echo getImpl(n.symbol).repr #see what the symbol actually is
      echo getType(n)
      #from here you can use getType/getImpl combination
    else:
      #overloaded symbol
      for k in children(n):
        echo getImpl(k.symbol).repr #see what the symbol actually is

  #here you can put your glue code generator

macro bindFunction*(arg: varargs[untyped]): stmt =
  result = newNimNode(nnkStmtList)

  #generate intermediate macro to utilize
  #bindSym that can only accept string literal
  let macroName = "NLBFunc" & $macroCount
  var nlb = "macro " & macroName & "(): stmt =\n"
  nlb.add "  let procList = [\n"

  var i = 0
  for k in children(arg):
    let comma = if i < arg.len-1: "," else: ""
    nlb.add "    bindSym\"$1\"$2\n" % [$k, comma]
    inc i

  nlb.add "  ]\n"
  nlb.add "  result = bindFuncImpl(procList)\n"
  nlb.add macroName & "()\n"  #don't forget to call the intermediate macro

  result.add parseStmt(nlb)
  echo nlb #inspect the generated code, you can remove it safely
  inc macroCount


proc mulv(a, b: int): int = discard
proc mulv(a, b: float): float = discard
proc abc(a: string): string = discard

var i : int


bindFunction(mulv, abc, i)

#you can do something like this if you modified bindFunction properly
#bindFunction:
#  mulv -> "newname"
#  abc
