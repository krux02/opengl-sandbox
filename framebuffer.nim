# included from fancygl.nim

when isMainModule:
  import glm, opengl, macros

type
  DepthRenderbuffer* = object
    handle*: GLuint
  FrameBuffer* = object
    handle*: GLuint

proc bindIt*(drb: DepthRenderbuffer): void =
  glBindRenderbuffer(GL_RENDERBUFFER, drb.handle)

proc newDepthRenderBuffer*(size: Vec2i) : DepthRenderbuffer =
  glGenRenderbuffers(1, cast[ptr GLuint](result.addr))
  glNamedRenderbufferStorageEXT(result.handle, GL_DEPTH_COMPONENT, size.x.GLsizei, size.y.GLsizei)

proc bindIt*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_FRAMEBUFFER, fb.handle)

proc bindDraw*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fb.handle)

proc bindRead*(fb: FrameBuffer): void =
  glBindFramebuffer(GL_READ_FRAMEBUFFER, fb.handle)

proc newFrameBuffer*(): FrameBuffer =
  when false:
    glGenFramebuffers(1, cast[ptr GLuint](result.addr))
  else:
    glCreateFramebuffers(1, cast[ptr GLuint](result.addr));

proc setRenderbuffer*(fb: FrameBuffer, attachment, renderbuffertarget: GLenum, renderbuffer: GLuint) =
  when false:
    glNamedFramebufferRenderbufferEXT(fb.GLuint, attachment, renderbuffertarget, renderbuffer)
  else:
    glNamedFramebufferRenderbuffer(fb.handle, attachment, renderbuffertarget, renderbuffer)

proc setTexture*(fb: FrameBuffer, attachment: GLenum, texture: Texture2D, level: GLint = 0) =
  when false:
    glNamedFramebufferTextureEXT(fb.GLuint, attachment, texture.handle, level);
  else:
    glNamedFramebufferTexture(fb.handle, attachment, texture.handle, level);

proc drawBuffers*(fb: FrameBuffer, args : varargs[GLenum]) =
  var tmp = newSeq[GLenum](args.len)
  for i, arg in args:
    tmp[i] = arg

  if tmp.len > 0:
    when false:
      glFramebufferDrawBuffersEXT(fb.handle, tmp.len.GLsizei, tmp[0].addr)
    else:
      glNamedFramebufferDrawBuffers(fb.handle, tmp.len.GLsizei, tmp[0].addr)

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
        error("unknow identifier: " & asgn.repr & " did you mean debugResult?", asgn)
    
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

  result.add head quote do:
    type
      `typename` = object
        handle*: FrameBuffer
        depth*: `depthType`

  for fragOut in fragmentOutputs:
    
    result.back[0][2][2].add newExpIdentDef(!fragOut, bindSym"Texture2D")

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
      glNamedFramebufferRenderbuffer( `resultIdent`.handle.handle, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, `resultIdent`.depth.handle )
  else:
    branchStmtList.add quote do:
      `resultIdent`.handle.setTexture(GL_DEPTH_ATTACHMENT, `resultIdent`.depth, 0)

  let drawBuffersCall = head quote do:
    drawBuffers( `resultIdent`.handle )

  var i = 0
  for asgn in arg:
    let lhs = asgn[0]
    let rhs = asgn[1]

    if lhs.ident != !"depth":
      let nameIdent = ident($lhs)
      let attachmentLit = ident("GL_COLOR_ATTACHMENT" & $i)
      branchStmtList.add quote do:
        `resultIdent`.`nameIdent` = `rhs`
        setTexture( `resultIdent`.handle, `attachmentLit`, `resultIdent`.`nameIdent`, 0)
        
      drawBuffersCall.add head quote do:
        GLenum(`attachmentLit`)
        
      i += 1

  branchStmtList.add( drawBuffersCall )

  let constructorIdent = ident("new" & $typename)
  result.add quote do:
    proc `constructorIdent`(): `typename` =
      `branchStmtList`

  when false:
    let resizeStmtList = newStmtList()
    resizeStmtList.add( newCall(bindSym"resize",
                                newDotExpr(ident"fb", ident"depth"), ident"newsize") )
    for fragOut in fragmentOutputs:
      resizeStmtList.add( newCall(bindSym"resize",
                                  newDotExpr(ident"fb", ident(fragOut)), ident"newsize") )
    
    result.add quote do:
      proc resize(newsize: Vec2i): void =
        `resizeStmtList`

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
