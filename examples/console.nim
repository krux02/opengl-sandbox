## not a graphical demo here, just a console in development that might get integrated with rendering and stuff.

import rdstdin, strutils, sequtils, parseutils, macros, typetraits

proc command_add(arg1: int, arg2: int): void =
  let res = arg1 + arg2
  echo arg1, " + ", arg2, " = ", res


proc parseArg[T](arg: string): tuple[couldParse: bool, value: T] =
  when T is int:
    let processedChars = parseutils.parseInt(arg, result.value)
    if processedChars == arg.len:
      result.couldParse = true


##  TODO report this bug, this should not be None

# macro foobar(typ: typedesc): untyped =
#   echo name(typ)
# foobar(int)
# foobar(float)
  
macro parseArgument(typ: typed, argId: static[int]): untyped =
  let argIdent   = ident("arg" & $argId)
  let idLit      = newLit(argId)
  let typeStrLit = newLit(typ.repr)
  result = quote do:
    let (parseOk, `argIdent`) = parseArg[`typ`](args[`idLit`])
    if not parseOk:
      stderr.writeLine("argument ", `idLit`, " (", args[`idLit`], ") cannot be parsed as type ", `typeStrLit`)
      return

  #echo result.repr

proc command_add_facade(args: openarray[string]): void =
  if args.len != 3:
    stderr.writeLine("expect 2 arguments, got ", args.len - 1, " arguments")
    return

  parseArgument(int,1)
  parseArgument(int,2)
    
  command_add(arg1, arg2)
  
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

  
registerCommand("add", command_add_facade)




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
      

  
