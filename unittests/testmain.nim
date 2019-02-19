include ../fancygl

proc main() =
  type TestType = object
    a,b,c,d: float32

  var ab = ArrayBuffer[TestType](handle : 7)
  var v: ArrayBufferView[float32]
  v.handle = ab.handle
  v.stride = 16
  v.absoluteoffset = 0

  v.relativeoffset = 0
  assert ab.view(a) == v
  v.relativeoffset = 4
  assert ab.view(b) == v
  v.relativeoffset = 8
  assert ab.view(c) == v
  v.relativeoffset = 12
  assert ab.view(d) == v


when isMainModule:
  main()
