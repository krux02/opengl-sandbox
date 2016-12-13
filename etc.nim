# included from fancygl.nim

proc iotaSeq*[T: SomeNumber](length: T) : seq[T] =
  result.newSeq length.int
  for i in 0 ..< length.int:
    result[i] = T(i)
 
macro debugResult(arg: typed) : untyped =
  for str in arg.repr.split("""\x0A"""):
    echo str
  arg

proc mkString*[T](v : T, prefix, sep, postfix : string) : string =
  result = prefix
  var first = true
  for x in v:
    if not first:
      result.add(sep)
    result.add($x)
    first = false

  result.add(postfix)

proc mkString*[T](v : T, sep : string = ", ") : string =
  mkString(v, "", sep, "")


proc back*[T](data: seq[T]): T = data[high(data)]
proc back*[T](data: openarray[T]): T = data[high(data)]

proc head*[T](data: seq[T]): T = data[0]
proc head*[T](data: openarray[T]): T = data[0]

macro namedEcho*(x: typed, xs: varargs[typed]): untyped =
  let lit = newLit(x.repr & "=")
  let sepLit = newLit(" ")
  result = newCall(ident"echo", lit, x)

  for x in xs:
    let lit = newLit(x.repr & "=")
    result.add sepLit, lit, x

