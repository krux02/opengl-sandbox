import strutils

proc myescape(s: string): string {.noSideEffect.} =
  ## strutils.escape fails with \x expressions
  result = newStringOfCap(s.len + s.len shr 2)
  result.add "\""
  var i = 0
  while i < s.len:
    if s[i] == char(13) and s[i+1] == char(10):
      result.add("\\n")
      i += 1
    elif s[i] == char(10):
      result.add("\\n")
    elif s[i] == char(9):
      result.add("\\t")
    elif s[i] == '\\':
      result.add("\\\\")
    elif s[i] == '\'':
      result.add("\\'")
    elif s[i] == '\"':
      result.add("\\\"")
    elif s[i].int in 32..127:
      result.add(s[i])
    else:
      debugecho "unsupported character in string ", s, " at index ", i
      return nil
    i += 1

  result.add "\""

static:
  let gdbScript = "slurped-gdb-script\n" & slurp("nim-gdb.py")
  let asmCode = """
  .pushsection ".debug_gdb_scripts", "MS",@progbits,1
  .byte 4
  .ascii $1
  .byte 0
  .popsection
  """ % [myescape(gdbScript)]

  let asmCall = "asm($1);" % [myescape(asmCode)]

{.emit: asmCall.}
