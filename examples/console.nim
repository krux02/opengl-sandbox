## A non-graphical demo here. It is just a command interpreter that
## might get integrated at some point.

import rdstdin, strutils, parseutils, macros, typetraits
import osproc

proc parseArg[T](arg: string): tuple[couldParse: bool, value: T] =
  when T is int:
    let processedChars = parseutils.parseInt(arg, result.value)
    if processedChars == arg.len:
      result.couldParse = true
  elif T is string:
    result.couldParse = true
    result.value = arg

macro parseArgument(argsIdent: untyped; argIdent: untyped; typ: typed;
                    argId: static[int]): untyped =
  let idLit      = newLit(argId)
  let typeStrLit = newLit(typ.repr)
  result = quote do:
    let (parseOk, `argIdent`) = parseArg[`typ`](`argsIdent`[`idLit`])
    if not parseOk:
      stderr.writeLine(
        "argument ", `idLit`, " (", `argsIdent`[`idLit`],
        ") cannot be parsed as type ", `typeStrLit`
      )
      return

macro parseVarargs(argsIdent, argIdent: untyped;
                   typ: typed; argId: static[int]): untyped =
  let idLit      = newLit(argId)
  let typeStrLit = newLit(typ.repr)
  result = quote do:
    var `argIdent` = newSeq[`typ`](0)
    for i in `argId` ..< `argsIdent`.len:
      let (parseOk, value) = parseArg[`typ`](`argsIdent`[i])
      if not parseOk:
        stderr.writeLine(
          "argument ", `idLit`, " (", `argsIdent`[`idLit`],
          ") cannot be parsed as type ", `typeStrLit`
        )
        return
      `argIdent`.add value

type CommandProc = proc(args: openarray[string]): void

var registeredCommands = newSeq[tuple[
  name: string,
  callback: CommandProc,
  comment: string,
  lineinfo: LineInfo
]]()

proc registerCommand(
    name: string;
    callback: CommandProc,
    comment: string,
    lineinfo: LineInfo): void =

  registeredCommands.add((
    name: name,
    callback: callback,
    comment: comment,
    lineinfo: lineinfo
  ))

proc callCommand(
    cmdname: string;
    arguments: openarray[string]): void =

  var cmdFound = false
  for name, callback, _, _ in registeredCommands.items:
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
  ## Inteded to be called as a pragma on a procedure.  Generates the
  ## wrapper code so that the procedure can be called from the
  ## interpreter
  let comment =
    if impl.body.kind == nnkStmtList and
       impl.body[0].kind == nnkCommentStmt:
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
    if paramType.kind == nnkBracketExpr and
       paramType[0] == bindSym"varargs":
      if i != paramTypes.high:
        error("varargs needs to be last", paramType)
      let paramIdent = genSym(nskVar, "arg" & $(i+1))
      parseArgumentCalls.add newCall(
        bindSym"parseVarargs", argsIdent,
        paramIdent, paramType[1], newLit(i+1)
      )
      commandCall.add paramIdent
      hasVarargs = newLit(true)
    else:
      let paramIdent = genSym(nskLet, "arg" & $(i+1))
      parseArgumentCalls.add newCall(
        bindSym"parseArgument", argsIdent,
        paramIdent, paramType, newLit(i+1)
      )
      commandCall.add paramIdent

  let numParamsLit = newLit(paramTypes.len)
  let facadeSym = genSym(nskProc, name & "_facade")
  let lineInfoLit = newLit(impl.lineinfoObj)
  result = quote do:
    proc `facadeSym`(`argsIdent`: openarray[string]): void =
      when `hasVarargs`:
        if `argsIdent`.len - 1  < `numParamsLit` - 1:
          stderr.writeLine(
            "expect at least ", `numParamsLit` - 1,
            " arguments, got ", `argsIdent`.len - 1,
            " arguments"
          )
          return
      else:
        if `argsIdent`.len - 1 != `numParamsLit`:
          stderr.writeLine(
            "expect ", `numParamsLit`, " arguments, got ",
            `argsIdent`.len - 1, " arguments"
          )
          return

      `parseArgumentCalls`
      `commandCall`

    registerCommand(
      `commandNameLit`, `facadeSym`,
      `commentLit`, `lineInfoLit`
    )

  #echo result.repr

proc add(arg1: int; arg2: int): void {.interpreterCommand.} =
  ## adds two numbers
  let res = arg1 + arg2
  echo arg1, " + ", arg2, " = ", res

proc mult(arg1,arg2: int): void {.interpreterCommand.} =
  ## multiplies two numbers
  echo arg1, " * ", arg2, " = ", arg1 * arg2

# if the last argument is varargs, it also works in the interpreter
proc sum(args: varargs[int]): void {.interpreterCommand.} =
  ## sum up all arguments, at least one
  var accum: int
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

proc exit(): void {.interpreterCommand.} =
  ## exit command interpreter (alias to quit)
  echo "Bye!"
  system.quit()

proc quit(): void {.interpreterCommand.} =
  ## quit command interpreter (alias to exit)
  echo "Ciao!"
  system.quit()

proc commands(): void {.interpreterCommand.} =
  ## list all functions
  for name, _, comment, _ in registeredCommands.items:
    echo name, "\t", comment

proc help(arg: string): void {.interpreterCommand.} =
  ## prints documentation of a single function
  for name, _, comment, lineinfo in registeredCommands.items:
    if name == arg:
      echo "location: ", lineinfo
      echo comment
      return
  echo "ERROR: no such function found"

proc ecedit(arg: string): void {.interpreterCommand.} =
  ## Edit file in emacsclient
  let processOptions = {poStdErrToStdOut, poUsePath}
  for name, _, _, lineinfo in registeredCommands.items:
    if name == arg:
      let location = "+" & $lineinfo.line & ":" & $lineinfo.column
      var filename: string = lineinfo.filename

      try:
        # if you don't know how emacsclient works, it connects to an
        # already running instance of emacs that is tagged as a
        # server. Then it brings this editor to the foreground and
        # blocks until the user says from the editor that he/she is
        # done editing the file. Then the emacsclient process
        # terminates. For other editors the integration would of
        # course look a bit different but similar.
        echo ["emacsclient", location, filename].join(" ")
        let process = startProcess(
          "emacsclient",
          args = [location, filename],
          options = processOptions,
        )
        discard process.waitForExit
        # from here on we know, that editing from emacs has been
        # done. In theory one could recompile and reload the nim
        # functions for the interpreter now. But that is not supported
        # yet.
      except OSError:
        let msg = getCurrentExceptionMsg()
        echo "OSError: ", msg
      return
  echo "Error: could not find command ", arg

block main:
  echo(
    "This is the command interpreter, to get a list of possible " &
    "commands, type \"commands\". To get help for a specific " &
    "command, type \"help <command>\"."
  )
  var line: string = ""

  # if readPasswordFromStdin("gimme your password> ", line):
  #   echo "got your password, it is: ", line
  # else:
  #   echo "got no password, quitting now"
  #   break main

  while readLineFromStdin("> ", line):
    #echo "got line: ", line
    var arguments = newSeq[string](0)
    for arg in split(line):
      if arg != "":
        arguments.add arg

    if arguments.len > 0:
      let command = arguments[0]
      if command.len > 0:
        callCommand(command, arguments)
