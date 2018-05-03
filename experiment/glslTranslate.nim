## this file is for all functions that are related to translating Nim
## directly to glsl.

import normalizeType, glm, ast_pattern_matching


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
  of ident"TextureCube":
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
