import macros

type Pipeline* = object
  program*: Program
  vao*: VertexArrayObject
  uniformBufferHandle*: GLuint
  uniformBufferData*: pointer
  uniformBufferSize*: GLint

const persistentMappedBuffers = false

proc nimGenUniforms*(pSym, typeSection, initCode, drawCode: NimNode; uniformRest, uniformSamplers: seq[(Nimnode,NimNode)]) =
  ## Generate code necessary to pass uniforms to a shader.

  if uniformRest.len > 0:
    let empty = newEmptyNode()
    let uniformObjectSym = genSym(nskVar, "uniformObject")
    let blockSize = genSym(nskVar, "blockSize")
    let uniformRecList = nnkTupleTy.newTree()
    let uniformBufTypeSym = genSym(nskType, "UniformBufType")

    typeSection.add nnkTypeDef.newTree(
      uniformBufTypeSym,
      empty,
      uniformRecList
    )

    initCode.add quote do:
      # this is not the binding index
      let uniformBlockIndex = glGetUniformBlockIndex(`pSym`.program.handle, "dynamic_shader_data");
      doAssert uniformBlockIndex != GL_INVALID_INDEX
      var `blockSize`: GLint
      glGetActiveUniformBlockiv(`pSym`.program.handle, uniformBlockIndex, GL_UNIFORM_BLOCK_DATA_SIZE, `blockSize`.addr)
      assert `blockSize` > 0
      `pSym`.uniformBufferSize = `blockSize`

      glCreateBuffers(1, `pSym`.uniformBufferHandle.addr)

    if persistentMappedBuffers:
      initCode.add quote do:
        glNamedBufferStorage(
          `pSym`.uniformBufferHandle, GLsizei(`blockSize`), nil,
          GL_MAP_WRITE_BIT or GL_MAP_PERSISTENT_BIT
        )
        `pSym`.uniformBufferData = glMapNamedBufferRange(
          `pSym`.uniformBufferHandle, 0, `blockSize`,
          GL_MAP_WRITE_BIT or GL_MAP_PERSISTENT_BIT or GL_MAP_FLUSH_EXPLICIT_BIT
        )
    else:
      initCode.add quote do:
        glNamedBufferStorage(`pSym`.uniformBufferHandle, GLsizei(`blockSize`), nil, GL_DYNAMIC_STORAGE_BIT)
        `pSym`.uniformBufferData = alloc(`blockSize`)

    drawCode.add quote do:
      var `uniformObjectSym`: `uniformBufTypeSym`

    for (uniformName, uniformExpr)  in uniformRest.items:
      let uniformIdent = ident(uniformName.strVal)

      #let typeExprA = uniform.getTypeInst
      let typeExprB = uniformExpr.getTypeInst

      uniformRecList.add nnkIdentDefs.newTree(
        uniformIdent,
        typeExprB,
        empty,
      )

      drawCode.add quote do:
        `uniformObjectSym`.`uniformIdent` = `uniformExpr`

    drawCode.add quote do:
      discard std140AlignedWrite(`pSym`.uniformBufferData, 0, `uniformObjectSym`)

    if persistentMappedBuffers:
      drawCode.add quote do:
        glFlushMappedNamedBufferRange(`pSym`.uniformBufferHandle, 0, `pSym`.uniformBufferSize)
    else:
      drawCode.add quote do:
        glNamedBufferSubData(`pSym`.uniformBufferHandle, 0, `pSym`.uniformBufferSize, `pSym`.uniformBufferData)
    drawCode.add quote do:
      glBindBufferBase(GL_UNIFORM_BUFFER, 0, `pSym`.uniformBufferHandle)

  if uniformSamplers.len > 0:
    let bindTexturesCall = newCall(bindSym"bindTextures", newLit(0) )
    for (uniformName, uniformExpr) in uniformSamplers.items:
      bindTexturesCall.add uniformExpr

    drawCode.add bindTexturesCall
