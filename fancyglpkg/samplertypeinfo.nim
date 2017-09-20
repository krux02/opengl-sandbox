template glslTypeRepr(t: typedesc[Texture1D]): string = "sampler1D"
template glslTypeRepr(t: typedesc[Texture2D]): string = "sampler2D"
template glslTypeRepr(t: typedesc[Texture3D]): string = "sampler3D"
template glslTypeRepr(t: typedesc[TextureRectangle]): string = "sampler2DRect"
template glslTypeRepr(t: typedesc[Texture2DArray]): string = "sampler2DArray"
template glslTypeRepr(t: typedesc[Texture1DArray]): string = "sampler1DArray"

template glslIsSampler(t: typedesc[Texture1D]): bool = true
template glslIsSampler(t: typedesc[Texture2D]): bool = true
template glslIsSampler(t: typedesc[Texture3D]): bool = true
template glslIsSampler(t: typedesc[TextureRectangle]): bool = true
template glslIsSampler(t: typedesc[Texture2DArray]): bool = true
template glslIsSampler(t: typedesc[Texture2DArrayShadow]): bool = true
