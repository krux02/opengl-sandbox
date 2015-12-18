


template templateTest( expr: openArray[expr] ) =


  macro debug(n: varargs[expr]): stmt =
  # `n` is a Nim AST that contains a list of expressions;
  # this macro returns a list of statements:
  result = newNimNode(nnkStmtList, n)
  # iterate over any argument that is passed to this macro:
  for i in 0..n.len-1:
    # add a call to the statement list that writes the expression;
    # `toStrLit` converts an AST to its string representation:
    result.add(newCall("write", newIdentNode("stdout"), toStrLit(n[i])))
    # add a call to the statement list that writes ": "
    result.add(newCall("write", newIdentNode("stdout"), newStrLitNode(": ")))
    # add a call to the statement list that writes the expressions value:
    result.add(newCall("writeLine", newIdentNode("stdout"), n[i]))



