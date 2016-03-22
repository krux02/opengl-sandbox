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
