import macros, strutils




macro isInt(x: typed): stmt =
  if macros.sameType( getType(int), x.getType):
    echo "yea"
  else:
    echo "nope"
  discard

let i = 0

isInt(i)  # yea

type MyVec3[T] = tuple[x,y,z:T]

macro isMyVec3(x: typed): stmt =
  if macros.sameType( getType(MyVec3[float])[1], x.getType):
    echo "yea"
  else:
    echo "nope"
  discard

let v : MyVec3[float] = (1.0,2.0,3.0)

isMyVec3(v) # nope


const banana = join(["bana", "na"])


proc foo(arg:string): int = 0

macro foobar(arg: typed): stmt =
  # it's not the root node I am working on.
  let subNode = arg[1]      #
  echo subNode.treeRepr     # Sym "banana"
  echo subNode.getType.repr # string
  #echo subNode.strVal       # Error: field 'strVal' cannot be found

  let sym = subNode.symbol
  echo sym.repr             # banana
  #echo sym.kind             # Error: type mismatch: got (NimSym), expected macros.kind(n: NimNode)
  #echo sym.strVal           # Error: type mismatch: got (NimSym), expected macros.strVal(n: NimNode)
  echo sym.getImpl.strVal


foobar(foo(banana))

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

bindFunction(mulv, abc, i)

#you can do something like this if you modified bindFunction properly
#bindFunction:
#  mulv -> "newname"
#  abc
