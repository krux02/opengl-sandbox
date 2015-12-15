import typetraits

template value(T:type int): string = "int"
template value(T:type float): string = "float"
template value(T:type string): string = "string"

let a = 12
let b = 12.0
let c = "12"


proc foobar[T](v: T) =
  echo v, ": ", v.type.value


foobar(a)
foobar(b)
foobar(c)


template woot(
