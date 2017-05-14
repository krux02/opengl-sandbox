import glm, strutils

let blockRot = 1

var lastGameTime = 0.0
var gameTime = 0.0
let visualBlockAnimationEndTime = 1.0

var visualBlockRot = 0.0f
var visualBlockRotPrime = 0.0f

for i in 0 .. 100:
  lastGameTime = gameTime
  gameTime = lastGameTime + 0.01

  if gameTime < visualBlockAnimationEndTime:

    # before entering this block of code, all animations have the
    # state of animations for lastGameTime. The state is known for how
    # the state should be at visualBlockAnimationEndTime. So the state
    # that should be atgame Time can be interpolated.

    let blockRotf = float32(blockRot) * 0.5f * float32(Pi)

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
      calcMatCol(lastGameTime),
      calcMatColPrime(lastGameTime),
      calcMatCol(visualBlockAnimationEndTime),
      calcMatColPrime(visualBlockAnimationEndTime)
    ))

    # [a b c d]
    let v = inverse(m) * vec4f(visualBlockRot, visualBlockRotPrime, blockRotf, 0)

    # f (x) = calcMatCol(x) ⋅ v  // v is only true for visualBlockRot
    # f'(x) = calcMatColPrime(x) ⋅ v

    #echo "f (lastGameTime)                = visualBlockRot"
    #echo dot(v, calcMatCol(lastGameTime)), " = ", visualBlockRot
    #echo "f'(lastGameTime)                = visualBlockRotPrime"
    #echo dot(v, calcMatColPrime(lastGameTime)), " = ", visualBlockRotPrime
    #echo "f (visualBlockAnimationEndTime) = blockRotf"
    #echo dot(v, calcMatCol(visualBlockAnimationEndTime)), " = ", blockRotf
    #echo "f'(visualBlockAnimationEndTime) = 0"
    #echo dot(v, calcMatColPrime(visualBlockAnimationEndTime)), " = ", 0

    visualBlockRot      = dot(v, calcMatCol(gameTime))
    visualBlockRotPrime = dot(v, calcMatColPrime(gameTime))

    echo "f : ", repeat("*", int(round(100 * dot(v, calcMatCol(gameTime)))))
    echo inverse(m)

    # for i in 0 .. 100:
    #   let gameTime = i / 100
    #   echo "f': ", repeat("*", int(round(100 * dot(v, calcMatColPrime(gameTime)))))


    #echo visualBlockRot, " ", visualBlockRotPrime
