type DepthRenderbuffer* = distinct GLuint

proc bindIt*(drb: DepthRenderbuffer): void =
  glBindRenderbuffer(GL_RENDERBUFFER, drb.GLuint)

proc createDepthRenderBuffer*(size: Vec2f) : DepthRenderbuffer =
  glGenRenderbuffers(1, cast[ptr GLuint](result.addr))
  glNamedRenderbufferStorageEXT(result.GLuint, GL_DEPTH_COMPONENT, size.x.GLsizei, size.y.GLsizei)

type FrameBuffer* = distinct GLuint

proc bindIt*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_FRAMEBUFFER, fb.GLuint)

proc bindDraw*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fb.GLuint)

proc bindRead*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_READ_FRAMEBUFFER, fb.GLuint)

proc createFrameBuffer*(): FrameBuffer =
  when false:
    glGenFramebuffers(1, cast[ptr GLuint](result.addr))
  else:
    glCreateFramebuffers(1, cast[ptr GLuint](result.addr));

proc setRenderbuffer*(fb: FrameBuffer, attachment, renderbuffertarget: GLenum, renderbuffer: GLuint) =
  when false:
    glNamedFramebufferRenderbufferEXT(fb.GLuint, attachment, renderbuffertarget, renderbuffer)
  else:
    glNamedFramebufferRenderbuffer(fb.GLuint, attachment, renderbuffertarget, renderbuffer)

proc setTexture*(fb: FrameBuffer, attachment: GLenum, texture: Texture2D, level: GLint = 0) =
  when false:
    glNamedFramebufferTextureEXT(fb.GLuint, attachment, texture.GLuint, level);
  else:
    glNamedFramebufferTexture(fb.GLuint, attachment, texture.GLuint, level);

proc drawBuffers*(fb: FrameBuffer, args : varargs[GLenum]) =
  var tmp = newSeq[GLenum](args.len)
  for i, arg in args:
    tmp[i] = arg

  if tmp.len > 0:
    when false:
      glFramebufferDrawBuffersEXT(fb.GLuint, tmp.len.GLsizei, tmp[0].addr)
    else:
      glNamedFramebufferDrawBuffers(fb.GLuint, tmp.len.GLsizei, tmp[0].addr)

const currentFramebuffer* = 0

# default fragment Outputs
const fragmentOutputs* = ["color"]

macro declareFramebuffer*(typename,arg:untyped) : untyped =
  typename.expectKind nnkIdent

  result = newStmtList()

  var fragmentOutputs = newSeq[string]()

  var depthType:NimNode = nil
  var depthCreateExpr:NimNode = nil
  var useDepthRenderbuffer = true

  for asgn in arg:
    asgn.expectKind nnkAsgn

    let lhs = asgn[0]
    let rhs = asgn[1]

    if lhs.ident == !"depth":
        rhs.expectKind(nnkCall)
        depthCreateExpr = rhs;

        if rhs[0].ident == !"createDepthRenderBuffer":
          depthType = bindSym"DepthRenderbuffer"
          useDepthRenderbuffer = true
        elif rhs[0].ident == !"createEmptyDepthTexture2D":
          depthType = bindSym"Texture2D"
          useDepthRenderbuffer = false
        else:
          error "expected call to either createDepthRenderBuffer or createEmptyDepthTexture2D"

    else:
      fragmentOutputs.add($asgn[0])

  let recList = newNimNode(nnkRecList)
  recList.add( newExpIdentDef(!"glname", bindSym"FrameBuffer") )
  recList.add( newExpIdentDef(!"depth", depthType) )

  for fragOut in fragmentOutputs:
    recList.add( newExpIdentDef(!fragOut, bindSym"Texture2D") )

  result.add newObjectTy(typename, recList)


  result.add(
    newNimNode2(nnkTemplateDef,
      !!"fragmentOutputSeq",
      newEmptyNode(),
      newEmptyNode(),
      newNimNode2(nnkFormalParams,
        newNimNode2(nnkBracketExpr, bindSym"seq", bindSym"string"),
        newNimNode2(nnkIdentDefs,
          !!"t",
          newNimnode2(nnkBracketExpr,
            bindSym"typedesc",
            typename),
          newEmptyNode()
        )
      ),
      newEmptyNode(),
      newEmptyNode(),
      newNimNode2(nnkStmtList,
        fragmentOutputs.toConstExpr
      )
    )
  )

  #result.add newConstStmt(!!"fragmentOutputs", fragmentOutputs.toConstExpr)

  let branchStmtList = newStmtList()

  branchStmtList.add(newAssignment(newDotExpr(!!"result", !!"glname"),
    newCall(bindSym"createFrameBuffer")
  ))

  branchStmtList.add(newAssignment(newDotExpr(!!"result", !!"depth"),
    depthCreateExpr
  ))

  if useDepthRenderbuffer:
    branchStmtList.add(newCall(bindSym"glNamedFramebufferRenderbufferEXT",
      newDotExpr(!!"result", !!"glname", bindSym"GLuint"), bindSym"GL_DEPTH_ATTACHMENT", bindSym"GL_RENDERBUFFER",
      newDotExpr(!!"result", !!"depth", bindSym"GLuint")
    ))
  else:
    branchStmtList.add(newCall(bindSym"setTexture",
      newDotExpr(!!"result", !!"glname"), bindSym"GL_DEPTH_ATTACHMENT",
      newDotExpr(!!"result", !!"depth"), newLit(0)
    ))

  let drawBuffersCall = newCall(bindSym"drawBuffers", newDotExpr(!!"result", !!"glname"))

  var i = 0
  for asgn in arg:
    let lhs = asgn[0]
    let rhs = asgn[1]

    if lhs.ident != !"depth":
      let name = $lhs
      branchStmtList.add(newAssignment( newDotExpr( !!"result", !! name ), rhs))

      branchStmtList.add(newCall(bindSym"setTexture",
                                 newDotExpr(!!"result", !!"glname"),
                                 !!("GL_COLOR_ATTACHMENT" & $i), 
                                 newDotExpr(!!"result", !!name), 
                                 newLit(0)
      ))

      drawBuffersCall.add( newCall(bindSym"GLenum", !!("GL_COLOR_ATTACHMENT" & $i)) )
      i += 1

  branchStmtList.add( drawBuffersCall )

  result.add(
    newNimNode2( nnkProcDef,
      !!( join(["create",$typename]) ),
      newEmptyNode(),
      newEmptyNode(),
      newNimNode2( nnkFormalParams,
        typename
      ),
      newEmptyNode(),
      newEmptyNode(),
      branchStmtList
    )
  )

  when false:
    let resizeStmtList = newStmtList()
    resizeStmtList.add( newCall(bindSym"resize", newDotExpr(!!"fb", !!"depth"), !!"newsize") )
    for fragOut in fragmentOutputs:
      resizeStmtList.add( newCall(bindSym"resize", newDotExpr(!!"fb", !!fragOut), !!"newsize") )
    
    result.add quote do:
      newNimNode2( nnkProcDef,
        !!"resize",
         newEmptyNode(),
        newEmptyNode(),
        newNimNode2( nnkFormalParams,
          bindSym"void",
          newNimNode2(nnkIdentDefs,
            !!"fb",
             typename,
            newEmptyNode()
          ),
          newNimNode2(nnkIdentDefs,
            !!"newsize",
            bindSym"Vec2f",
            newEmptyNode()
          )
        ),
        newEmptyNode(),
        newEmptyNode(),
        resizeStmtList
      )

  result = newCall( bindSym"debugResult", result )

template bindFramebuffer*(name, blok: untyped): untyped =
  var drawfb, readfb: GLint
  glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, drawfb.addr)
  glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, readfb.addr)

  name.glname.bindIt
  block:
    let currentFramebuffer {. inject .} = name
    const fragmentOutputs {.inject.} = name.type.fragmentOutputSeq
    blok

  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, drawfb.GLuint)
  glBindFramebuffer(GL_READ_FRAMEBUFFER, readfb.GLuint)
