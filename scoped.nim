import opengl

proc glSetBool( name : GLenum, value : bool ) : void =
  if value:
    glEnable(name)
  else:
    glDisable(name)

template glScopedSet*(name : GLenum, value : bool) =
  var old_value = glIsEnabled(name)
  glSetBool(name, value)
  defer: glSetBool(name, old_value != 0)

template glScopedCullFace*(mode : GLenum) =
  var old_value : GLint
  glGetIntegerv(GL_CULL_FACE_MODE, addr old_value)
  glCullFace(mode)
  defer: glCullFace(old_value.GLenum)

template glScopedDepthFunc*(mode : GLenum) =
  var old_value : GLint
  glGetIntegerv(GL_DEPTH_FUNC, addr old_value)
  glDepthFunc(mode)
  defer: glDepthFunc(old_value.GLenum)

template glScopedPolygonMode*(face, mode : GLenum) =
  var old_value : array[2, GLint]
  glGetIntegerv(GL_POLYGON_MODE, addr old_value[0])
  glPolygonMode(face, mode)
  defer: glPolygonMode(old_value[0].GLenum, old_value[1].GLenum)

template glScopedDepthMask*(value : bool) =
  var old_value : GLint
  glGetIntegerv(GL_DEPTH_WRITEMASK, addr old_value)
  glDepthMask(value)
  defer: glDepthMask(old_value != 0)

template glScopedStencilMask*(value : uint32) =
  var old_back_mask, old_front_mask : GLuint
  glGetIntegerv(GL_STENCIL_WRITEMASK, addr old_front_mask)
  glGetIntegerv(GL_STENCIL_BACK_WRITMASK, addr old_back_mask)
  glStencilMask(value)
  defer:
    glStencilMaskSeparate(GL_FRONT, old_front_mask)
    glStencilMaskSeparate(GL_BACK, old_back_mask)

template glScopedStencilMaskFront*(value : uint32) =
  var old_mask : GLuint
  glGetIntegerv(GL_STENCIL_WRITEMASK, addr old_mask)
  GLStencilMaskSeparate(GL_FRONT, value)
  defer: glStencilMaskSeparate(GL_FRONT, old_mask)

template glScopedStencilMaskBack*(value : uint32) =
  var old_mask : GLuint
  glGetIntegerv(GL_STENCIL_BACK_WRITEMASK, addr old_mask)
  GLStencilMaskSeparate(GL_BACK, value)
  defer: glStencilMaskSeparate(GL_BACK, old_mask)    
    
template glScopedColorMask*(r,g,b,a : bool) =
  var old_values : array[4,GLint]
  glGetIntegerv(GL_COLOR_WRITEMASK, addr old_values[0])
  glColorMask(r,g,b,a)
  defer: glColorMask(old_values[0], old_values[1], old_values[2], old_values[3])


template glScopedBlendFunc*(blend_src, blend_dst : GLenum) =
  var old_blend_src, old_blend_dst : GLint
  glGetIntegerv(GL_BLEND_SRC, addr old_blend_src)
  glGetIntegerv(GL_BLEND_DST, addr old_blend_dst)
  glBlendFunc(blend_src, blend_dst)
  defer: glBlendFunc(old_blend_src.GLenum, old_blend_dst.GLenum)
  
template glScopedDepthRange*(near, far: float32) =
  var old_value : array[2, float32]
  glGetFloatv(GL_DEPTH_RANGE, addr old_value[0])
  glDepthRangef(near, far)
  defer: glDepthRangef(old_value[0], old_value[1])

template glScopedDepthRange*(near, far: float64) =
  var old_value : array[2, float64]
  glGetDoublev(GL_DEPTH_RANGE, addr old_value[0])
  glDepthRange(near, far)
  defer: glDepthRange(old_value[0], old_value[1])

template glScopedBlendColor*(red,green,blue,alpha : float32) =
  var old_color : array[4, float32]
  glGetFloatv(GL_BLEND_COLOR, addr old_color[0])
  glBlendColor(red,green,blue,alpha)
  defer: glBlendColor(old_color[0], old_color[1], old_color[2], old_color[3])


  
