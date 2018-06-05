import sugar, algorithm, macros, strutils, tables, sequtils
# packages
import ../fancygl, ast_pattern_matching
# local stuff
import normalizeType, glslTranslate, boring_stuff, renderMacro

let _,_ = defaultSetup()

let vertexShaderSrc : string = readFile("experiment/uniformBufferTest.vert")
let fragmentShaderSrc : string = readFile("experiment/uniformBufferTest.frag")

var lineinfo: Lineinfo

let program = linkShader(
  compileShader(GL_VERTEX_SHADER, vertexShaderSrc, lineinfo),
  compileShader(GL_FRAGMENT_SHADER, fragmentShaderSrc, lineinfo)
)

for i in 0 .. 20:
  echo "\nbuffer ", i
  program.debugUniformBlock(GLuint(i))
