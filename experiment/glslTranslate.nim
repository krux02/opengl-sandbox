## this file is for all functions that are related to translating Nim
## directly to glsl.

import normalizeType, glm, ast_pattern_matching, macros, algorithm, boring_stuff

proc symKind(arg: NimNode): NimSymKind =
  if arg.kind == nnkHiddenDeref:
    symKind(arg[0])
  else:
    macros.symKind(arg)

proc isSampler*(arg: NimNode): bool =
  arg.getTypeInst.normalizeType.matchAst:
  of ident"Texture1D":
    return true
  of ident"Texture2D":
    return true
  of ident"Texture3D":
    return true
  of ident"TextureCubeMap":
    return true
  of ident"Texture2DShadow":
    return true
  of ident"TextureCubeShadow":
    return true
  of ident"Texture2DArray":
    return true
  of ident"Texture2DArrayShadow":
    return true
  else:
    return false

proc expectIntIn(arg: NimNode; slice: Slice[int]): void =
  if arg.intVal notin slice:
    error("expect integer literal in range: " & $slice.a & " .. " & $slice.b & " but got " & $arg.intVal, arg)

proc glslType*(arg: NimNode): string {.compileTime.} =
  let arg = arg.normalizeType

  arg.matchAst:
  of nnkBracketExpr( ident"array", nnkInfix(ident"..", 0, `highLit`), `innerType`):
    result = glslType(innerType)
    result.add "["
    result.add(highLit.intVal+1)
    result.add "]"
    return
  else:
    discard

  arg.matchAst:
  of ident"float32":
    return "float"
  of ident"float64":
    return "double"
  of ident"int32":
    return "int"
  of ident"bool":
    return "bool"
  of ident"Texture1D":
    return "sampler1D"
  of ident"Texture2D":
    return "sampler2D"
  of ident"Texture3D":
    return "sampler3D"
  of ident"TextureCubeMap":
    return "samplerCube"
  of ident"Texture2DShadow":
    return "sampler2DShadow"
  of ident"TextureCubeShadow":
    return "samplerCubeShadow"
  of ident"Texture2DArray":
    return "sampler2DArray"
  of ident"Texture2DArrayShadow":
    return "sampler2DArrayShadow"

  of nnkBracketExpr(ident"Vec", `sizeLit`, `Tsym`):

    Tsym.matchAst:
    of ident"float32":
      result = "vec"
    of ident"float64":
      result = "dvec"
    of ident"int32":
      result = "ivec"
    of ident"bool":
      result = "bvec"

    sizeLit.expectIntIn 2..4
    result.add sizeLit.intVal

  of nnkBracketExpr(ident"Mat", `sizeLit1`, `sizeLit2`, `Tsym`):

    Tsym.matchAst:
    of ident"float32":
      result = "mat"
    of ident"float64":
      result = "dmat"
    of ident"int32":
      result = "imat"
    of ident"bool":
      result = "bmat"

    sizeLit1.expectIntIn 2..4
    sizeLit2.expectIntIn 2..4

    let intVal1 = sizeLit1.intVal
    let intVal2 = sizeLit2.intVal

    result.add intVal1
    if intVal2 != intVal1:
      result.add "x"
      result.add intVal2

  else:
    ## well this is definitively wrong
    ## the type needs to be translated to glsl.
    result = arg.repr


when isMainModule:
  macro testGlslType(arg: typed): untyped =
    var expected: string
    for varSection in arg:
      if varSection.kind == nnkVarSection:
        let identDefs = varSection[0]
        let sym = identDefs[0]

        let glslType = sym.getTypeInst.glslType
        echo glslType
        assert glslType == expected
      else:
        expected = varSection.strVal

  testGlslType:
    ## vec4
    var a: Vec4f
    ## vec4
    var b: Vec4[float32]
    ## vec4
    var c: Vec[4,float32]
    ## mat4
    var d: Mat4f
    ## mat4
    var e: Mat4[float32]
    ## mat4
    var f: Mat[4,4,float32]
    ## float
    var g: float32
    ## mat3x4
    var h: Mat[3,4, float32]
    ## mat4x2
    var i: Mat[4,2, float32]



## too many local variables, and the compiler goes boom, therefore here is a totally non semanitc split of compileToGlsl.


proc compileToGlsl*(result: var string; arg: NimNode): void

var buffer: string

proc compileToGlslA*(result: var string; arg: NimNode): void =
  arg.matchAst(errorSym):
  of {nnkFloat32Lit,nnkFloat64Lit,nnkFloatLit}:
    result.add arg.floatVal
  of {nnkInt32Lit, nnkInt64Lit, nnkIntLit}:
    result.add arg.intVal
  of nnkEmpty:
    result.add "/* empty */"
  of nnkCommentStmt:
    result.add "/// "
    result.add arg.strVal
    result.add "\n"
  of nnkIfExpr(  nnkElifExpr(`cond`, `body`), nnkElseExpr( `elseBody` )):
    result.add "("
    result.compileToGlsl(cond)
    result.add " ? "
    result.compileToGlsl(body)
    result.add " : "
    result.compileToGlsl(elseBody)
    result.add ")"
  of nnkStmtList:
    for stmt in arg:
      result.compileToGlsl(stmt)
      result.add ";\n"
  of nnkStmtListExpr( nnkCommentStmt, `expr`):
    result.compileToGlsl expr
  of nnkProcDef(`nameSym`, nnkEmpty, nnkEmpty, `params` @ nnkFormalParams, nnkEmpty, nnkEmpty, `body`, `resultSym`):
    result.add glslType(params[0]), " "
    result.compileToGlsl(nameSym)
    result.add "("
    for memberSym, typeSym in params.fields:
      result.add glslType(typeSym), " "
      result.compileToGlsl(memberSym)
      result.add ", "
    result[^2] = ')'
    result.add "{\n"
    result.compileToGlsl(body)
    result.add ";\n}\n"
  of {nnkIdent, nnkSym}:
    buffer = ""
    for c in arg.repr:
      if c != '_': # underscore is my personal separator
        buffer.add c
    if glslKeywords.binarySearch(buffer) < 0:
      result.add buffer
    else:
      result.add buffer
      result.add "_XXX"
  of nnkDotExpr(`lhs`, `rhs`):
    # I am pretty sure this is a big hack
    let symKind = lhs.symKind
    result.compileToGlsl(lhs)
    if symKind in {nskParam, nskResult}:
      result.add '_'
    else:
      result.add '.'
    result.compileToGlsl(rhs)
  of nnkConv(`typ`, `expr`):
    result.add typ.glslType, '('
    result.compileToGlsl(expr)
    result.add ')'

proc compileToGlslB*(result: var string; arg: NimNode): void =
  arg.matchAst(errorSym):
  of nnkAsgn(`lhs`, `rhs`):
    result.compileToGlsl(lhs)
    result.add " = "
    result.compileToGlsl(rhs)
  of {nnkHiddenDeref, nnkHiddenAddr}(`sym`):
    result.compileToGlsl(sym)
  of nnkHiddenStdConv(nnkEmpty, `sym`):
    result.compileToGlsl(sym)
  of nnkPrefix(`op`, `arg`):
    result.add op.strVal
    result.add '('
    result.compileToGlsl(arg)
    result.add ')'

  of nnkInfix(`op`, `lhs`, `rhs`):
    result.add "("
    result.compileToGlsl(lhs)
    result.add " "
    result.add op.strVal
    result.add " "
    result.compileToGlsl(rhs)
    result.add ")"
  of {nnkLetSection, nnkVarSection}:
    for memberSym, value in arg.fieldValuePairs:
      let typeStr = memberSym.getTypeInst.glslType
      result.add typeStr
      result.add ' '
      result.compileToGlsl(memberSym)
      result.add " = "
      if value.kind != nnkEmpty:
        result.compileToGlsl(value)
      else:
        result.add typeStr
        result.add "(0)"
      result.add ";\n"
    result.setLen(result.len-2)

  of nnkCall:
    arg[0].expectKind nnkSym

    let funcName = arg[0].strVal


    if funcName == "[]":
      arg.expectLen(3)
      result.compileToGlsl(arg[1])
      result.add('[')
      result.compileToGlsl(arg[2])
      result.add(']')
    elif funcName.isSwizzle:
      if funcName[^1] == '=':
        result.compileToGlsl(arg[1])
        result.add ".", funcName, " "
        result.compileToGlsl(arg[2])
      else:
        arg.expectLen(2)
        result.compileToGlsl(arg[1])
        result.add ".", funcName
    else:
      if funcName in glslConvProc:
        # TODO, mention something about this in the documentation
        result.add arg.getTypeInst.glslType
      elif funcName == "modulo":
        result.add "mod"
      else:
        result.add funcName
      result.add "("
      for arg in arg.args:
        result.compileToGlsl(arg)
        result.add ", "

      result[^2] = ')'

  of nnkBracketExpr(`syma`, `symb`):
    result.compileToGlsl(syma)
    result.add '['
    result.compileToGlsl(symb)
    result.add ']'

  of nnkReturnStmt(nnkAsgn( _, `expr`)):
    result.add "return "
    result.compileToGlsl expr
  of nnkBlockStmt(
    `sym1` @ nnkSym,
    nnkStmtList(
      nnkVarSection(
        nnkIdentDefs( `loopVar` @ nnkSym, nnkEmpty, nnkEmpty)
      ),
      nnkStmtList(
        nnkCommentStmt,
        nnkVarSection( nnkIdentDefs( `loopIndex` @ nnkSym, nnkEmpty, 0)),
        nnkIfStmt(
          nnkElifBranch(
            nnkInfix( ident"<=", _ #[ `loopIndex` ]#, `upperBound` @ nnkIntLit),
            nnkBlockStmt(
              `blockSym2`,
              nnkWhileStmt(
                1,
                nnkStmtList(
                  nnkStmtList(
                    nnkFastAsgn(_ #[`loopVar`]#, nnkBracketExpr(`collectionSym`,_ #[`loopIndex`]#)),
                    `body`
                  ),
                  nnkIfStmt(
                    nnkElifBranch(
                      nnkStmtListExpr(
                        nnkCommentStmt,
                        nnkInfix(ident"<=", _ #[`upperBound`]#, _ #[`loopIndex`]#)
                      ),
                      nnkBreakStmt( _ #[`sym1`]#)
                    ),
                  ),
                  nnkCall( ident"inc", _ #[`loopIndex`]# , 1 )
                )
              )
            )
          )
        )
      )
    )
  ):
    let loopIndexTrue = genSym(nskVar, "i")
    let irepr = loopIndexTrue.repr
    result.add "for(int "
    result.add irepr
    result.add " = 0; "
    result.add irepr
    result.add " < "
    result.add(upperBound.intVal+1)
    result.add "; ++"
    result.add irepr
    result.add ") {\n"

    # TODO this is actually correct, but for now I cheat around fixing it
    # result.add loopVar.getTypeInst.glslType
    result.add loopVar.getTypeInst.strVal
    result.add ' '
    result.compileToGlsl(loopVar)
    result.add " = "
    result.compileToGlsl(collectionSym)
    result.add "[", irepr, "];\n{\n"
    result.compileToGlsl body

    result.add "\n}}"

const AKinds = {
  nnkFloat32Lit,
  nnkFloat64Lit,
  nnkFloatLit,
  nnkInt32Lit,
  nnkInt64Lit,
  nnkIntLit,
  nnkEmpty,
  nnkCommentStmt,
  nnkIfExpr,
  nnkStmtList,
  nnkStmtListExpr,
  nnkProcDef,
  nnkIdent,
  nnkSym,
  nnkDotExpr,
  nnkConv
}

proc compileToGlsl*(result: var string; arg: NimNode): void =
  if arg.kind in AKinds:
    result.compileToGlslA(arg)
  else:
    result.compileToGlslB(arg)
