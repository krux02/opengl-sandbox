## not a graphical demo here, just a console in development that might get integrated with rendering and stuff.
import rdstdin, strutils, sequtils, parseutils, macros, typetraits

proc parseArg[T](arg: string): tuple[couldParse: bool, value: T] =
  when T is int:
    let processedChars = parseutils.parseInt(arg, result.value)
    if processedChars == arg.len:
      result.couldParse = true
  elif T is string:
    result.couldParse = true
    result.value = arg

macro parseArgument(argsIdent: untyped; argIdent: untyped; typ: typed; argId: static[int]): untyped =
  let idLit      = newLit(argId)
  let typeStrLit = newLit(typ.repr)
  result = quote do:
    let (parseOk, `argIdent`) = parseArg[`typ`](`argsIdent`[`idLit`])
    if not parseOk:
      stderr.writeLine("argument ", `idLit`, " (", `argsIdent`[`idLit`], ") cannot be parsed as type ", `typeStrLit`)
      return

macro parseVarargs(argsIdent: untyped; argIdent; typ: typed; argId: static[int]): untyped =
  let idLit      = newLit(argId)
  let typeStrLit = newLit(typ.repr)
  result = quote do:
    var `argIdent` = newSeq[`typ`](0)
    for i in `argId` ..< `argsIdent`.len:
      let (parseOk, value) = parseArg[`typ`](`argsIdent`[i])
      if not parseOk:
        stderr.writeLine("argument ", `idLit`, " (", `argsIdent`[`idLit`], ") cannot be parsed as type ", `typeStrLit`)
        return
      `argIdent`.add value

type CommandProc = proc(args: openarray[string]): void

var registeredCommands = newSeq[tuple[name: string, callback: CommandProc, comment: string]]()

proc registerCommand(name: string; callback: CommandProc, comment: string): void =
  registeredCommands.add((name: name, callback: callback, comment: comment))

proc callCommand(cmdname: string; arguments: openarray[string]): void =
  var cmdFound = false
  for name, callback, _ in registeredCommands.items:
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

macro interpreterCommand(impl: typed): untyped =
  ## Inteded to be called as a pragma on a procedure.
  ## Generates the wrapper code so that the procedure can be called from the interpreter
  let comment =
    if impl.body.kind == nnkStmtList and impl.body[0].kind == nnkCommentStmt:
      $impl.body[0]
    else:
      "<no comment>"

  let commentLit = newLit(comment)
  let arg = impl[0]
  let name = $impl[0]

  let commandNameLit = newLit(name.stripPrefix("command_"))
  var paramTypes    = newSeq[NimNode](0)
  let params = impl[3]
  let argsIdent = genSym(nskParam, "args")
  var hasVarargs = newLit(false)

  for i in 1 ..< params.len:
    let identDefs = params[i]
    for j in 0 ..< identDefs.len - 2:
      paramTypes.add identDefs[identDefs.len - 2]

  let parseArgumentCalls = newStmtList()

  let commandCall = newCall(arg)

  for i, paramType in paramTypes:
    # varargs
    if paramType.kind == nnkBracketExpr and paramType[0] == bindSym"varargs":
      if i != paramTypes.high:
        error("varargs needs to be last", paramType)
      #echo "hoohoo"
      let paramIdent = genSym(nskVar, "arg" & $(i+1))
      parseArgumentCalls.add newCall(bindSym"parseVarargs", argsIdent, paramIdent, paramType[1], newLit(i+1))
      commandCall.add paramIdent
      hasVarargs = newLit(true)
    else:
      echo paramType.lispRepr
      let paramIdent = genSym(nskLet, "arg" & $(i+1))
      parseArgumentCalls.add newCall(bindSym"parseArgument", argsIdent, paramIdent, paramType, newLit(i+1))
      commandCall.add paramIdent

  let numParamsLit = newLit(paramTypes.len)
  let facadeSym = genSym(nskProc, name & "_facade")
  result = quote do:
    proc `facadeSym`(`argsIdent`: openarray[string]): void =
      when `hasVarargs`:
        if `argsIdent`.len - 1  < `numParamsLit` - 1:
          stderr.writeLine("expect at least ", `numParamsLit` - 1, " arguments, got ", `argsIdent`.len - 1, " arguments")
          return
      else:
        if `argsIdent`.len - 1 != `numParamsLit`:
          stderr.writeLine("expect ", `numParamsLit`, " arguments, got ", `argsIdent`.len - 1, " arguments")
          return

      `parseArgumentCalls`
      `commandCall`

    registerCommand(`commandNameLit`, `facadeSym`, `commentLit`)

  echo result.repr

proc add(arg1: int; arg2: int): void {.interpreterCommand.} =
  ## adds two numbers
  let res = arg1 + arg2
  echo arg1, " + ", arg2, " = ", res

proc add3(arg1,arg2,arg3: int): void {.interpreterCommand.} =
  ## adds three numbers
  let res = arg1 + arg2 + arg3
  echo arg1, " + ", arg2, " + ", arg3, " = ", res

proc mult(arg1,arg2: int): void {.interpreterCommand.} =
  ## multiplies two numbers
  echo arg1, " * ", arg2, " = ", arg1 * arg2

# if the last argument is varargs, it also works in the interpreter
proc sum(arg1: int; args: varargs[int]): void {.interpreterCommand.} =
  ## sum up all arguments, at least one
  var accum: int = arg1
  for arg in args:
    accum += arg
  echo "sum: ", accum

# if the last argument is varargs, it also works in the interpreter
proc prod(args: varargs[int]): void {.interpreterCommand.} =
  ## multiply up all arguments
  var accum: int = 1
  for arg in args:
    accum *= arg
  echo "prod: ", accum

proc commands(): void {.interpreterCommand.} =
  ## list all functions
  for name, _, comment in registeredCommands.items:
    echo name, "\t", comment

proc help(arg: string): void {.interpreterCommand.} =
  ## prints documentation of a single function
  for name, _, comment in registeredCommands.items:
    if name == arg:
      echo comment
      return
  echo "ERROR: no such function found"

block main:
  echo "this is the command interpreter, to get a list of possible commands, type \"commands\""
  echo "to get help for a specific command, type \"help <command>\""
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
