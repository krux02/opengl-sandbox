import typetraits
import streams
import macros

template value(T:type int): string = "int"
template value(T:type float): string = "float"
template value(T:type string): string = "string"


proc mySource(): string =
  result = ""
  var fs = newFileStream("tent.nim", fmRead)
  if not isNil(fs):
    var line = ""
    while fs.readLine(line):
      result.add(line)
    fs.close()


#macro derive(f: expr, v: expr): expr =
#  dumpTree(f)


dumpTree:
  uniforms:
    projection = projection_mat
    modelview = modelview_mat
    time = time
  attributes:
    pos = vertex
    col = color
  varyings:
    var v_col : vec4
  frag_out:
    var color : vec4
  vertex_prg:
    gl_Position = projection * modelview * vec4(pos,1)
    v_col = vec4(col,1)
  fragment_prg:
    color = mymix(v_col, time)



var ss = newStringStream("hallo welt")

ss.writeLine("peter")
ss.writeLine("morphose")

echo "here it comes"


echo "that was it"
