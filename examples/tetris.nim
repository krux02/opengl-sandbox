import ../fancygl, sdl2/mixer, os, strutils

import sequtils, algorithm

let (window, _) = defaultSetup()
let windowsize = window.size

var animationLength = 0.125

let projection_mat : Mat4f = perspective(45'f32, windowsize.x / windowsize.y, 0.1, 100.0)

type
  SimpleMesh = object
    vertices,normals,colors: ArrayBuffer[Vec4f]
    indices: ElementArrayBuffer[int16]
    indicesLen: int

const
  NumRows = 24
  NumCols = 10

  startBlockPos = vec2i(NumCols div 2, NumRows - 1)
  nextBlockPos  = vec2i(NumCols + 5, NumRows - 5)

var camera = newWorldNode()
camera.pos.xyz = vec3f(NumCols div 2, NumRows div 2 - 13, 20)
camera.turnRelativeX(0.5f)
echo camera
#camera.lookAt(vec3f(0,-22,0), vec3f(0,1,0))

#camera.pos.x += float32(NumCols div 2)
#camera.pos.y += float32(NumRows div 2)

const colorsArray = [
  vec4f(1,0,0,1),
  vec4f(0,1,0,1),
  vec4f(0,0,1,1),
  vec4f(1,1,0,1),
  vec4f(0,1,1,1),
  vec4f(1,0,1,1),
  vec4f(0.5)
]

#
#    ()
#  ()XX()
#
#  ()
#  ()xx()
#
#      ()
#  ()xx()
#
#  ()XX
#    ()()
#
#    XX()
#  ()()
#
#    XX()
#    ()()
#
#    ()
#    XX
#    ()
#    ()
#

const piecesArray = [
  [vec2i(-1, 0), vec2i( 1, 0), vec2i( 0, 1)],
  [vec2i(-1, 0), vec2i( 1, 0), vec2i(-1, 1)],
  [vec2i(-1, 0), vec2i( 1, 0), vec2i( 1, 1)],
  [vec2i(-1, 0), vec2i( 0,-1), vec2i( 1,-1)],
  [vec2i( 1, 0), vec2i( 0,-1), vec2i(-1,-1)],
  [vec2i( 1, 0), vec2i( 0,-1), vec2i( 1,-1)],
  [vec2i( 0, 1), vec2i( 0,-1), vec2i( 0,-2)]
]

# BUG: why does this not work with const?
let rotationsArray = [
  mat2i(vec2i( 1, 0), vec2i( 0, 1)),
  mat2i(vec2i( 0, 1), vec2i(-1, 0)),
  mat2i(vec2i(-1, 0), vec2i( 0,-1)),
  mat2i(vec2i( 0,-1), vec2i( 1, 0))
]

proc rotationMat2(angle: float32): Mat2f =
  let s = sin(angle)
  let c = cos(angle)
  result[0,0] =  c
  result[0,1] =  s
  result[1,0] = -s
  result[1,1] =  c

#proc rotationMat(angle: float32): Mat2f =

var icosphereMesh, boxMesh: SimpleMesh

block init:

  let isNumVerts = icosphereIndicesTriangles.len
  var unrolledVertices = newSeqOfCap[Vec4f](isNumVerts)
  var unrolledColors = newSeqOfCap[Vec4f](isNumVerts)
  var unrolledNormals = newSeqOfCap[Vec4f](isNumVerts)

  for i in countup(0, icosphereIndicesTriangles.len-1, 3):
    var normal : Vec4f
    for j in 0 ..< 3:
      let idx = icosphereIndicesTriangles[i+j]
      let v = icosphereVertices[idx]
      unrolledVertices.add v
      normal += v

    # averageing vertex positions of a face, to get face normals,
    # really only works for spherical meshes, where the xyz components
    # of the normal and the point, is equal.
    normal.w = 0
    normal = normalize(normal)
    unrolledNormals.add([normal,normal,normal])

    let color = vec4f(rand_f32(), rand_f32(), rand_f32(), 1'f32)
    unrolledColors.add([color,color,color])

  for v in unrolledVertices.mitems:
    v.xyz = (v.xyz * 0.65 * 0.5f) + 0.5f


  icosphereMesh.vertices = arrayBuffer(unrolledVertices)
  icosphereMesh.colors   = arrayBuffer(unrolledColors)
  icosphereMesh.normals  = arrayBuffer(unrolledNormals)
  icosphereMesh.indices  = elementArrayBuffer(iotaSeq[int16](unrolledVertices.len.int16))
  icosphereMesh.indicesLen = icosphereMesh.indices.len

  var newBoxVertices = boxVertices
  for v in newBoxVertices.mitems:
    v.xyz = (v.xyz * 0.5f) + 0.5f

  boxMesh.vertices = arrayBuffer(newBoxVertices)
  boxMesh.colors = arrayBuffer(boxColors)
  boxMesh.normals = arrayBuffer(boxNormals)
  boxMesh.indices = elementArrayBuffer(iotaSeq[int16](boxVertices.len.int16))
  boxMesh.indicesLen = boxMesh.indices.len

  glDisable(GL_DEPTH_CLAMP)

var planeVertices = arrayBuffer([
  vec4f(0,0,0,1), vec4f( 100, 0,0,1), vec4f( 0, 100,0,1),
  vec4f(0,0,0,1), vec4f( 0, 100,0,1), vec4f(-100, 0,0,1),
  vec4f(0,0,0,1), vec4f(-100, 0,0,1), vec4f( 0,-100,0,1),
  vec4f(0,0,0,1), vec4f( 0,-100,0,1), vec4f( 100, 0,0,1)
])

var planeNode = newWorldNode()

var evt: Event = defaultEvent
var runGame: bool = true

var frame = 0

var fieldRows: array[NumRows, array[NumCols, int]]

proc fieldRead(pos: Vec2i): int =
  if 0 <= pos.x and 0 <= pos.y and pos.x < NumCols:
    if pos.y < NumRows:
      fieldRows[pos.y][pos.x]
    else:
      -1
  else:
    0x7fffffff

for row in fieldRows.mitems:
  row.fill(-1)

let positionsBuffer = newArrayBuffer[Vec4f](length = NumRows*NumCols, usage = GL_STREAM_DRAW, label = "positions" )
let colorsBuffer    = newArrayBuffer[Vec4f](length = NumRows*NumCols, usage = GL_STREAM_DRAW, label = "colors")

var framePositionsBuffer : ArrayBuffer[Vec4f]
var frameColorsBuffer    : ArrayBuffer[Vec4f]
var framePositionsLen: int

block:
  var framePositionsSeq = newSeq[Vec4f]()
  var frameColorsSeq    = newSeq[Vec4f]()

  for i in 0 ..< NumCols:
    framePositionsSeq.add vec4f(float32(i), -1.0f,   0, 1)
    #framePositionsSeq.add vec4f(float32(i), NumRows, 0, 1)

  for i in 0 ..< NumRows:
    framePositionsSeq.add vec4f(-1.0f,   float32(i), 0, 1)
    framePositionsSeq.add vec4f(NumCols, float32(i), 0, 1)

  framePositionsSeq.add vec4f(-1,-1,0,1)
  framePositionsSeq.add vec4f(NumCols, -1,0,1)

  for i in -3'i32 ..< 3'i32:
    framePositionsSeq.add vec4f(vec2f(nextBlockPos + vec2i( i,-3)), 0, 1)
    framePositionsSeq.add vec4f(vec2f(nextBlockPos + vec2i(-i, 3)), 0, 1)
    framePositionsSeq.add vec4f(vec2f(nextBlockPos + vec2i(-3,-i)), 0, 1)
    framePositionsSeq.add vec4f(vec2f(nextBlockPos + vec2i( 3, i)), 0, 1)

  frameColorsSeq.setLen(framePositionsSeq.len)
  frameColorsSeq.fill(vec4f(vec3f(0.5f), 1))



  framePositionsBuffer = arrayBuffer(framePositionsSeq)
  frameColorsBuffer    = arrayBuffer(frameColorsSeq)
  framePositionsLen = framePositionsSeq.len

var previewBlockType = 0
var previewBlockRot  = 0


####################
# animation system #
####################

type
  AnimationState = object
    position: Vec2f
    rotation: float32

  Animation = object
    startState, startDerivative, endState, endDerivative: AnimationState
    startTime, endTime: float64


var playerAnimation: Animation
var insertAnimation: Animation

# template cubicTemplate = ((v1 * s + v2) * s + v3) * s + v4
# proc cubic*[N,T](v1,v2,v3,v4: Vec[N,T]; s: T): Vec[N,T] =
#   ## return a point from a cubic curve
#   return cubicTemplate
# proc cubic*[T : SomeNumber](v1,v2,v3,v4,s: T): T =
#   ## return a point from a cubic curve
#   return cubicTemplate

var blockPos: Vec2i
var blockRot: int
var lastBlockType, blockType: int

proc animate(animation: Animation; gameTime: float64): tuple[state,derivative: AnimationState] =

  if gameTime < animation.endTime:
    # does animation

    # before entering this block of code, all animations have the
    # state of animations for lastGameTime. The state is known for how
    # the state should be at visualBlockAnimationEndTime. So the state
    # that should be atgame Time can be interpolated.

    # f (x) = a⋅1 + b⋅x + c⋅x⋅x + d⋅x⋅x⋅x = [1 x xx xxx] ⋅ [a b c d]
    # f'(x) =   0 +   b + 2⋅c⋅x + 3⋅d⋅x⋅x = [0 1 2x 3xx] ⋅ [a b c d]

    # f (lastGameTime)                = visualBlockRot
    # f'(lastGameTime)                = visualBlockRotPrime
    # f (visualBlockAnimationEndTime) = blockRotf
    # f'(visualBlockAnimationEndTime) = 0

    proc calcMatCol(x: float32): Vec4f =
      result[0] = 1
      result[1] = x
      result[2] = x * x
      result[3] = result[2] * x

    proc calcMatColPrime(x: float32): Vec4f =
      result[0] = 0
      result[1] = 1
      result[2] = 2 * x
      result[3] = 3 * x * x

    let m = transpose(mat4f(
      calcMatCol(0),
      calcMatColPrime(0),
      calcMatCol(animation.endTime - animation.startTime),
      calcMatColPrime(animation.endTime - animation.startTime)
    ))

    var v,w : Vec4f

    let m_inv = inverse(m)

    w = calcMatCol(gameTime - animation.startTime)
    # solve for animation parameters [a b c d] (stored in v)
    v = m_inv * vec4f(animation.startState.rotation, animation.startDerivative.rotation, animation.endState.rotation, animation.endDerivative.rotation)
    result.state.rotation = dot(v, w)
    v = m_inv * vec4f(animation.startState.position.x, animation.startDerivative.position.x, animation.endState.position.x, animation.endDerivative.position.x)
    result.state.position.x = dot(v, w)
    v = m_inv * vec4f(animation.startState.position.y, animation.startDerivative.position.y, animation.endState.position.y, animation.endDerivative.position.y)
    result.state.position.y = dot(v, w)

    w = calcMatColPrime(gameTime - animation.startTime)
    # solve for animation parameters [a b c d] (stored in v)
    v = m_inv * vec4f(animation.startState.rotation, animation.startDerivative.rotation, animation.endState.rotation, animation.endDerivative.rotation)
    result.derivative.rotation = dot(v, w)
    v = m_inv * vec4f(animation.startState.position.x, animation.startDerivative.position.x, animation.endState.position.x, animation.endDerivative.position.x)
    result.derivative.position.x = dot(v, w)
    v = m_inv * vec4f(animation.startState.position.y, animation.startDerivative.position.y, animation.endState.position.y, animation.endDerivative.position.y)
    result.derivative.position.y = dot(v, w)

    # f (x) = calcMatCol(x) ⋅ v  // v is only true for visualBlockRot
    # f'(x) = calcMatColPrime(x) ⋅ v

    #echo "f (lastGameTime)                = visualBlockRot"
    # echo dot(v, calcMatCol(lastGameTime)), " = ", visualBlockRot
    # echo "f'(lastGameTime)                = visualBlockRotPrime"
    # echo dot(v, calcMatColPrime(lastGameTime)), " = ", visualBlockRotPrime
    # echo "f (visualBlockAnimationEndTime) = blockRotf"
    # echo dot(v, calcMatCol(visualBlockAnimationEndTime)), " = ", blockRotf
    # echo "f'(visualBlockAnimationEndTime) = 0"
    # echo dot(v, calcMatColPrime(visualBlockAnimationEndTime)), " = ", 0
    #echo visualBlockRot, " ", visualBlockRotPrime
    #visualBlockRot = mix(visualBlockRot, blockRotf, a)
    #visualBlockPos = mix(visualBlockPos, vec2f(blockPos), a)
  else:
    result.state      = animation.endState
    result.derivative = animation.endDerivative

proc stickAnimation(animation: Animation; gameTime: float64, endState, endDerivative: AnimationState): Animation =
  ## creates a new animation, that sticks perfectly to the time point of another animation:
  ## used to make transitions between two animations smooth

  let (state, derivative) = animation.animate(gameTime)
  result.startState      = state
  result.startDerivative = derivative
  result.endState = endState
  result.endDerivative = endDerivative

  result.startTime = gameTime
  result.endTime = gameTime + animationLength

proc currentBlockStateAsAnimationState(): AnimationState =
  result.position = vec2f(blockPos)
  result.rotation = float32(blockRot) * 0.5f * float32(Pi)

########################
# end animation system #
########################

const RowsPerLevel = 10

var level = 0
var clearedRows = 0
var rowsToGo = RowsPerLevel  # rows to go before entering the next level
var score = 0

var downTimer = newStopWatch(true)
var gameTimer = newStopWatch(true)

var gameTime, lastGameTime: float64

proc callNextBlock(): void =
  lastBlockType = blockType

  blockPos  = startBlockPos
  blockType = previewBlockType
  blockRot  = previewBlockRot

  # cancel player animations
  playerAnimation.endTime = 0
  playerAnimation.endState = currentBlockStateAsAnimationState()

  previewBlockType = rand_i32() mod piecesArray.len
  previewBlockRot  = rand_i32() mod 3

callNextBlock()
callNextBlock()

iterator blockPositions(pos: Vec2i, rot, typ: int): Vec2i =
  yield pos
  for rawOffset in piecesArray[typ]:
    yield pos + rotationsArray[rot and 3] * rawOffset

iterator blockPositions(pos: Vec2f; rot: float32; typ: int): Vec2f =
  yield pos
  for rawOffset in piecesArray[typ]:
    yield pos + rotationMat2(rot) * vec2f(rawOffset)

proc validBlockPos(pos: Vec2i, rot,typ: int): bool =
  for npos in blockPositions(pos,rot,typ):
    if fieldRead(npos) > -1:
      return false

  return true

proc lose(): void =
  echo "YOU LOSE"
  echo "cleared rows: ", clearedRows
  echo "score:        ", score

  echo "*** HIGHSCORE ***"

  var highscore = newSeq[int](0)

  let filename = getAppDir() / "highscore"
  if fileExists(filename):
    for line in lines(filename):
      highscore.add parseInt(line)

  highscore.sort(cmp, SortOrder.Descending)

  var index = 0
  while index < highscore.len and highscore[index] >= score:
    index += 1

  highscore.insert(score, index)

  for i, score in highscore:
    if i == index:
      echo " +++ ", i+1, ".\t", score, " +++ "
    else:
      echo "     ", i+1, ".\t", score

  let outfile = open(filename, fmWrite)
  defer:
    outfile.close
  for score in highscore:
    outfile.writeLine $score

  runGame = false

proc insertBlock(): void =
  var ok = true
  for pos in blockPositions(blockPos, blockRot, blockType):
    if NumRows <= pos.y or fieldRead(pos) > -1:
       ok = false
       break
  if ok:
    score += 1
    for pos in blockPositions(blockPos, blockRot, blockType):
      fieldRows[pos.y][pos.x] = blockType
    callNextBlock()
    if not validBlockPos(blockPos, blockRot, blockType):
      lose()
  else:
    lose()

proc downStep(): void =
  downTimer.reset()
  let offset = vec2i(0,-1)
  if validBlockPos(blockPos + offset, blockRot, blockType):
    blockPos += offset
    playerAnimation = stickAnimation(playerAnimation, gameTime, currentBlockStateAsAnimationState(), AnimationState())
  else:
    insertBlock()

###############
# sound state #
###############

var shouldPlayAudio = true
var canPlayAudio = true

if mixer.openAudio(MIX_DEFAULT_FREQUENCY * 2, MIX_DEFAULT_FORMAT, 2, 1024) == -1:
  stderr.write "Could not open sdl2 mixer: ", getError(), "\n"
  canPlayAudio = false

# defer: mixer.closeAudio()

var currentTrack = -1
var music = newSeq[ptr Music](0)
var music_names = newSeq[string](0)

if canPlayAudio:
  for kind, path in walkDir(getAppDir() / "resources" / "music"):
    if kind == pcFile:
      let track = loadMUS(path)
      if track.isNil:
        stderr.write "Could not open music: ", getError(), "\n"
      else:
        music.add track
        let a = path.rfind('/') + 1
        let b = path.rfind('.') - 1
        let name = path[a .. b]
        echo name
        musicNames.add name

if music.len == 0:
  canPlayAudio = false

#defer:
#  for track in music:
#    freeMusic(track)

proc nextTrack(): void =
  currentTrack = (currentTrack + 1) mod music.len
  if shouldPlayAudio and canPlayAudio:
    discard fadeOutMusic(250)
    if playMusic(music[currentTrack], -1) != 0:
      echo "problems with: ", musicNames[currentTrack]
      echo "error: ", getError()
    else:
      echo "now playing: ", musicNames[currentTrack]

proc toggleAudio(): void =
  shouldPlayAudio = not shouldPlayAudio
  if shouldPlayAudio:
    if playMusic(music[currentTrack], -1) != 0:
      echo "problems with: ", musicNames[currentTrack]
      echo "error: ", getError()
    else:
      echo "now playing: ", musicNames[currentTrack]
  else:
    discard fadeOutMusic(250)

nextTrack()

var menuActive = true

proc action_start(): void =
  menuActive = false
  discard

proc action_highscore(): void =
  echo "action highscore"
  discard

proc action_quit(): void =
  runGame = false

const
  mainMenuEntries = ["start", "highscore", "quit"]
  mainMenuActions = [action_start, action_highscore, action_quit]
var mainMenuSelection : 0..mainMenuEntries.high

proc mainMenu(): void =
  for i, entry in mainMenuEntries:
    let y = int32(i * 20 + 20)
    let x = int32(40)
    if i == mainMenuSelection:
      renderText("> ", vec2i(x - 20, y))
    renderText(entry, vec2i(x,y))


proc main(): void =

  while runGame:
    frame += 1

    lastGameTime = gameTime
    gameTime = gameTimer.time

    ####################
    # Input Processing #
    ####################

    while pollEvent(evt):
      if evt.kind == QuitEvent:
        lose()
        break

      if evt.kind == KeyDown:
        if menuActive:
          case evt.key.keysym.scancode
          of SDL_SCANCODE_UP:
            mainMenuSelection = (mainMenuSelection + mainMenuEntries.high) mod mainMenuEntries.len
          of SDL_SCANCODE_DOWN:
            mainMenuSelection = (mainMenuSelection + 1) mod mainMenuEntries.len

          of SDL_SCANCODE_SPACE, SDL_SCANCODE_RETURN:
            mainMenuActions[mainMenuSelection]()

          else:
            discard


        else:
          case evt.key.keysym.scancode
          of SDL_SCANCODE_ESCAPE:
            lose()
            break

          of SDL_SCANCODE_J, SDL_SCANCODE_S, SDL_SCANCODE_KP_4, SDL_SCANCODE_LEFT:
            # step left
            let offset = vec2i(-1,0)
            if validBlockPos(blockPos + offset, blockRot, blockType):
              blockPos += offset
              playerAnimation = stickAnimation(playerAnimation, gameTime, currentBlockStateAsAnimationState(), AnimationState())


          of SDL_SCANCODE_K, SDL_SCANCODE_D, SDL_SCANCODE_KP_5, SDL_SCANCODE_DOWN:
            # step down
            downStep()

          of SDL_SCANCODE_L, SDL_SCANCODE_F, SDL_SCANCODE_KP_6, SDL_SCANCODE_RIGHT:
            # step right
            let offset = vec2i(1,0)
            if validBlockPos(blockPos + offset, blockRot, blockType):
              blockPos += offset
              playerAnimation = stickAnimation(playerAnimation, gameTime,
                                               currentBlockStateAsAnimationState(), AnimationState())

          of SDL_SCANCODE_U, SDL_SCANCODE_W, SDL_SCANCODE_KP_7:
            # rotate left
            let nBlockRot = blockRot + 1
            for offset in [vec2i(0,0), vec2i(-1,0), vec2i(1,0)]:
              if validBlockPos(blockPos + offset, nBlockRot, blockType):
                blockRot = nBlockRot
                blockPos += offset
                playerAnimation = stickAnimation(playerAnimation, gameTime,
                                                 currentBlockStateAsAnimationState(), AnimationState())
                break

          of SDL_SCANCODE_O, SDL_SCANCODE_R, SDL_SCANCODE_KP_9, SDL_SCANCODE_UP:
            # rotate right
            let nBlockRot = blockRot - 1
            for offset in [vec2i(0,0), vec2i(-1,0), vec2i(1,0)]:
              if validBlockPos(blockPos + offset, nBlockRot, blockType):
                blockRot = nBlockRot
                blockPos += offset

                playerAnimation = stickAnimation(playerAnimation, gameTime,
                                                 currentBlockStateAsAnimationState(), AnimationState())
                break

          of SDL_SCANCODE_I, SDL_SCANCODE_E, SDL_SCANCODE_KP_8, SDL_SCANCODE_SPACE:
            # drop the brick
            let offset = vec2i(0, -1)
            while validBlockPos(blockPos + offset, blockRot, blockType):
              blockPos += offset


            let aes = AnimationState(position: vec2f(0,-20))
            insertAnimation = stickAnimation(playerAnimation, gameTime, currentBlockStateAsAnimationState(), aes)


            insertBlock()

          of SDL_SCANCODE_F2:
            nextTrack()

          of SDL_SCANCODE_KP_PLUS:
            animationLength *= 2.0
          of SDL_SCANCODE_KP_MINUS:
            animationLength *= 0.5

          of SDL_SCANCODE_F10:
            window.screenshot

          else:
            discard

    if menuActive:
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

      mainMenu()
    else:
      ####################
      # apply game logic #
      ####################

      if downTimer.time > pow(0.8, level.float64):
        downStep()

      # remove full lines

      var clearedRowsLocal = 0
      for y in 0 ..< NumRows:
        while fieldRows[y].find(-1) == -1:
          fieldRows[y].fill(-1)
          for y2 in y+1 ..< NumRows:
            swap(fieldRows[y2-1], fieldRows[y2])
          clearedRowsLocal += 1

      clearedRows += clearedRowsLocal
      rowsToGo -= clearedRowsLocal

      while rowsToGo <= 0:
        rowsToGo += RowsPerLevel
        level += 1
        nextTrack()

      case clearedRowsLocal
      of 0:
        discard
      of 1:
        score +=   40 * (level + 1)
      of 2:
        score +=  100 * (level + 1)
      of 3:
        score +=  300 * (level + 1)
      of 4:
        score += 1200 * (level + 1)
      else:
        echo "WTF cleared ", clearedRowsLocal, " at once"
        echo "no idea what to do with that"
        echo "just give you a billion points"
        score += 1000000000

      #####################
      # animation updates #
      #####################

      ###############
      # Render Code #
      ###############

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

      #renderText("score: " & $score,       vec2i(20, 20))
      #renderText("level: " & $level,       vec2i(20, 40))
      #renderText("lines: " & $clearedRows, vec2i(20, 60))
      iterator allBlockPositions(): tuple[pos:Vec2f; typ: int] =
        for y, row in fieldRows:
          for x, value in row:
            let tile = fieldRows[y][x]
            if tile > -1:
              let pos = vec2f(float32(x),float32(y))
              yield((pos: pos, typ: tile))

        let visualBlockState = playerAnimation.animate(gameTime)
        for pos in blockPositions(visualBlockState.state.position, visualBlockState.state.rotation, blockType):
          yield((pos: pos, typ: blockType))

        if score > 0 and gameTime < insertAnimation.endTime:
          let visualBlockState = insertAnimation.animate(gameTime)
          for pos in blockPositions(visualBlockState.state.position, visualBlockState.state.rotation, lastBlockType):
            yield((pos: pos, typ: lastBlockType))

        for pos in blockPositions(nextBlockPos, previewBlockRot, previewBlockType):
          yield((pos: vec2f(pos), typ: previewBlockType))

      var numPositions = 0
      positionsBuffer.mapWriteBlock:
        colorsBuffer.mapWriteBlock:
          for pos, typ in allBlockPositions():
            let posx = float32(pos.x)
            let posy = float32(pos.y)
            positionsBuffer[numPositions] = vec4f(posx, posy, 0, 1)
            colorsBuffer[numPositions] = colorsArray[typ]
            numPositions += 1

      proc renderMeshInstanced(mesh: SimpleMesh; objectPos, objectColor: ArrayBuffer[Vec4f], numInstances: int ): void =
        shadingDsl:
          primitiveMode = GL_TRIANGLES
          numVertices   = mesh.indicesLen
          indices       = mesh.indices
          numInstances  = numInstances

          uniforms:
            proj = projection_mat
            modelView = camera.viewMat

          attributes:
            a_vertex   = mesh.vertices
            a_normal   = mesh.normals
            a_texCoord = mesh.colors
            objectPos   {.divisor: 1.}
            objectColor {.divisor: 1.}

          vertexMain:
            """
            gl_Position = proj * modelView * vec4(a_vertex.xyz + objectPos.xyz, 1);
            v_normal = modelView * a_normal;
            v_Color = vec4(a_texCoord.x + a_texCoord.y) * objectColor;
            """

          vertexOut:
            "out vec4 v_normal"
            "out vec4 v_Color"

          fragmentMain:
            """
            // cheap fake lighting from camera direction
            color = v_Color * v_normal.z;
            """

      renderMeshInstanced(icosphereMesh, positionsBuffer, colorsBuffer, numPositions)
      renderMeshInstanced(boxMesh, framePositionsBuffer, frameColorsBuffer, framePositionsBuffer.len)

      let modelViewProj = projection_mat * camera.viewMat * planeNode.modelMat

      shadingDsl:
        primitiveMode = GL_TRIANGLES
        numVertices = 12
        uniforms:
          modelViewProj

        attributes:
          a_vertex   = planeVertices

        vertexMain:
          """
          gl_Position = modelViewProj * a_vertex;
          v_pos_os = a_vertex;
          """
        vertexOut:
          "out vec4 v_pos_os"
        fragmentMain:
          """
          color = vec4(fract(v_pos_os.xy) * 0.1, 0, 1);
          """

    glSwapWindow(window)

main()
