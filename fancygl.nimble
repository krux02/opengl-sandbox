# Package

version       = "0.2.1"
author        = "Arne Döring"
description   = "nice way of handling render code"
license       = "MIT"

bin           = @["fancygl"]

skipDirs = @["examples", "tests"]

# Dependencies

requires @["nim >= 0.16.1", "sdl2 >= 1.1.0", "opengl >= 1.1.0", "glm >= 0.1.1", "fftw3 >= 0.1.0"]
