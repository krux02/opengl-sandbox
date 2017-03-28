## not a graphical demo here, just a console in development that might get integrated with rendering and stuff.

import rdstdin, strutils, sequtils, parseutils, macros, typetraits

proc parseArg[T](arg: string): tuple[couldParse: bool, value: T] =
  when T is int:
    let processedChars = parseutils.parseInt(arg, result.value)
    if processedChars == arg.len:
      result.couldParse = true

macro parseArgument(argsIdent: untyped; argIdent: untyped; typ: typed; argId: static[int]): untyped =
  let idLit      = newLit(argId)
  let typeStrLit = newLit(typ.repr)
  result = quote do:
    let (parseOk, `argIdent`) = parseArg[`typ`](`argsIdent`[`idLit`])
    if not parseOk:
      stderr.writeLine("argument ", `idLit`, " (", `argsIdent`[`idLit`], ") cannot be parsed as type ", `typeStrLit`)
      return

type CommandProc = proc(args: openarray[string]): void

var registeredCommands = newSeq[tuple[name: string, callback: CommandProc]]()

proc registerCommand(name: string; callback: CommandProc): void =
  registeredCommands.add((name: name, callback: callback))

proc callCommand(cmdname: string; arguments: openarray[string]): void =
  var cmdFound = false
  for name, callback in registeredCommands.items:
    if name == cmdname:
      callback(arguments)
      cmdFound = true
      break

  if not cmdFound:
    stderr.writeLine("could not find command: ", cmdname)


proc stripPrefix(arg, prefix: string): string =
  if arg.startsWith prefix:
    result = arg.substr(prefix.len, arg.len - 1)
  else:
    result = arg


macro genCommandFacade(arg: typed): untyped =
  let impl = arg.symbol.getImpl
  let name = impl[0].repr
  let commandNameLit = newLit(name.stripPrefix("command_"))
  var paramTypes    = newSeq[NimNode](0)
  let params = impl[3]
  let argsIdent = genSym(nskParam, "args")

  for i in 1 ..< params.len:
    let identDefs = params[i]
    for j in 0 ..< identDefs.len - 2:
      paramTypes.add identDefs[identDefs.len - 2]

  let parseArgumentCalls = newStmtList()

  let commandCall = newCall(arg)
  for i, paramType in paramTypes:
    let paramIdent = genSym(nskLet, "arg" & $(i+1))
    parseArgumentCalls.add newCall(bindSym"parseArgument", argsIdent, paramIdent, paramType, newLit(i+1))
    commandCall.add paramIdent


  let numParamsLit = newLit(paramTypes.len)
  let facadeSym = genSym(nskProc, name & "_facade")
  result = quote do:
    proc `facadeSym`(`argsIdent`: openarray[string]): void =
      if `argsIdent`.len != `numParamsLit` + 1:
        stderr.writeLine("expect ", `numParamsLit`, " arguments, got ", `argsIdent`.len - 1, " arguments")
        return

      `parseArgumentCalls`

      `commandCall`

    registerCommand(`commandNameLit`, `facadeSym`)

  echo result.repr



macro interpreterProcs(arg: untyped): untyped =
  result = arg

  for procDef in arg:
    if procDef.kind == nnkProcDef:
      result.add newCall(bindSym"genCommandFacade", procDef[0])

  echo arg.treeRepr

interpreterProcs:
  proc command_add(arg1: int, arg2: int): void =
    let res = arg1 + arg2
    echo arg1, " + ", arg2, " = ", res

  proc command_add3(arg1,arg2,arg3: int): void =
    let res = arg1 + arg2 + arg3
    echo arg1, " + ", arg2, " + ", arg3, " = ", res

  proc command_mult(arg1,arg2: int): void =
    echo arg1, " * ", arg2, " = ", arg1 * arg2

block main:
  var line: string = ""

  # if readPasswordFromStdin("gimme your password> ", line):
  #   echo "got your password, it is: ", line
  # else:
  #   echo "got no password, quitting now"
  #   break main

  while readLineFromStdin("> ", line):
    #echo "got line: ", line

    let arguments = toSeq(split(line))
    if arguments.len > 0:
      let command = arguments[0]
      if command.len > 0:
        callCommand(command, arguments)
