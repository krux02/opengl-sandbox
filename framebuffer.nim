# included from fancygl.nim

type
  DepthRenderbuffer* = distinct GLuint
  FrameBuffer* = distinct GLuint

proc bindIt*(drb: DepthRenderbuffer): void =
  glBindRenderbuffer(GL_RENDERBUFFER, drb.GLuint)

proc newDepthRenderBuffer*(size: Vec2i) : DepthRenderbuffer =
  glGenRenderbuffers(1, cast[ptr GLuint](result.addr))
  glNamedRenderbufferStorageEXT(result.GLuint, GL_DEPTH_COMPONENT, size.x.GLsizei, size.y.GLsizei)

proc bindIt*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_FRAMEBUFFER, fb.GLuint)

proc bindDraw*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fb.GLuint)

proc bindRead*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_READ_FRAMEBUFFER, fb.GLuint)

proc newFrameBuffer*(): FrameBuffer =
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
    glNamedFramebufferTextureEXT(fb.GLuint, attachment, texture.handle, level);
  else:
    glNamedFramebufferTexture(fb.GLuint, attachment, texture.handle, level);

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
  var wrapWithDebugResult = false

  for asgn in arg:
    if asgn.kind == nnkIdent:
      if asgn == ident("debugResult"):
        wrapWithDebugResult = true
        continue
      else:
        error("unknow identifier: " & asgn.repr & " did you mean debugResult?")
    
    asgn.expectKind nnkAsgn

    let lhs = asgn[0]
    let rhs = asgn[1]

    if lhs.ident == !"depth":
        rhs.expectKind(nnkCall)
        depthCreateExpr = rhs;

        if rhs[0].ident == !"newDepthRenderBuffer":
          depthType = bindSym"DepthRenderbuffer"
          useDepthRenderbuffer = true
        elif rhs[0].ident == !"newDepthTexture2D":
          depthType = bindSym"Texture2D"
          useDepthRenderbuffer = false
        else:
          error "expected call to either newDepthRenderBuffer or newDepthTexture2D", rhs

    else:
      fragmentOutputs.add($asgn[0])

  let recList = newNimNode(nnkRecList)
  recList.add( newExpIdentDef(!"handle", bindSym"FrameBuffer") )
  recList.add( newExpIdentDef(!"depth", depthType) )

  for fragOut in fragmentOutputs:
    recList.add( newExpIdentDef(!fragOut, bindSym"Texture2D") )

  result.add newObjectTy(typename, recList)

  let fragmentOutputsSeqNode = fragmentOutputs.toConstExpr
  result.add quote do:
    template fragmentOutputSeq(t: typedesc[`typename`]): seq[string] =
      `fragmentOutputsSeqNode`

  #result.add newConstStmt(ident"fragmentOutputs", fragmentOutputs.toConstExpr)

  let branchStmtList = newStmtList()

  let resultIdent = ident"result"

  branchStmtList.add quote do:
    `resultIdent`.handle = newFrameBuffer()
    `resultIdent`.depth  = `depthCreateExpr`
    
  #branchStmtList.add(newAssignment(newDotExpr(ident"result", ident"handle"),
  #  newCall(bindSym"newFrameBuffer")
  #))
  #branchStmtList.add(newAssignment(newDotExpr(ident"result", ident"depth"),
  #  depthCreateExpr
  #))

  if useDepthRenderbuffer:
    branchStmtList.add quote do:
      glNamedFramebufferRenderbuffer( `resultIdent`.handle.GLuint, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, `resultIdent`.depth.GLuint )
    #newDotExpr(ident"result", ident"handle", bindSym"GLuint"), bindSym"GL_DEPTH_ATTACHMENT", bindSym"GL_RENDERBUFFER",
    #newDotExpr(ident"result", ident"depth", bindSym"GLuint")
    #))
  else:
    branchStmtList.add quote do:
      `resultIdent`.handle.setTexture(GL_DEPTH_ATTACHMENT, `resultIdent`.depth, 0)
    #branchStmtList.add(newCall(bindSym"setTexture",
    #  newDotExpr(ident"result", ident"handle"), bindSym"GL_DEPTH_ATTACHMENT",
    #  newDotExpr(ident"result", ident"depth"), newLit(0)
    #))

  let drawBuffersCall = newCall(bindSym"drawBuffers", newDotExpr(ident"result", ident"handle"))

  var i = 0
  for asgn in arg:
    let lhs = asgn[0]
    let rhs = asgn[1]

    if lhs.ident != !"depth":
      let name = $lhs
      branchStmtList.add(newAssignment( newDotExpr( ident"result", ident(name) ), rhs))

      branchStmtList.add(newCall(bindSym"setTexture",
                                 newDotExpr(ident"result", ident"handle"),
                                 ident("GL_COLOR_ATTACHMENT" & $i), 
                                 newDotExpr(ident"result", ident(name)), 
                                 newLit(0)
      ))

      drawBuffersCall.add( newCall(bindSym"GLenum", ident("GL_COLOR_ATTACHMENT" & $i)) )
      i += 1

  branchStmtList.add( drawBuffersCall )

  result.add(
    newTree( nnkProcDef,
      ident("new" & $typename),
      newEmptyNode(),
      newEmptyNode(),
      newTree( nnkFormalParams,
        typename
      ),
      newEmptyNode(),
      newEmptyNode(),
      branchStmtList
    )
  )

  when false:
    let resizeStmtList = newStmtList()
    resizeStmtList.add( newCall(bindSym"resize", newDotExpr(ident"fb", ident"depth"), ident"newsize") )
    for fragOut in fragmentOutputs:
      resizeStmtList.add( newCall(bindSym"resize", newDotExpr(ident"fb", ident(fragOut)), ident"newsize") )
    
    result.add quote do:
      newTree( nnkProcDef,
        ident"resize",
         newEmptyNode(),
        newEmptyNode(),
        newTree( nnkFormalParams,
          bindSym"void",
          newTree(nnkIdentDefs,
            ident"fb",
             typename,
            newEmptyNode()
          ),
          newTree(nnkIdentDefs,
            ident"newsize",
            bindSym"Vec2f",
            newEmptyNode()
          )
        ),
        newEmptyNode(),
        newEmptyNode(),
        resizeStmtList
      )

  if wrapWithDebugResult:
    result = newCall( bindSym"debugResult", result )

template blockBindFramebuffer*(name, blok: untyped): untyped =
  var drawfb, readfb: GLint
  glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, drawfb.addr)
  glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, readfb.addr)

  name.handle.bindIt
  block:
    let currentFramebuffer {. inject .} = name
    const fragmentOutputs {.inject.} = name.type.fragmentOutputSeq
    blok

  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, drawfb.GLuint)
  glBindFramebuffer(GL_READ_FRAMEBUFFER, readfb.GLuint)
