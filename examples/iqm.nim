const
  IQM_MAGIC* = "INTERQUAKEMODEL"
  IQM_VERSION* = 2

type
  iqmheader* = object
    magic*: array[16, char]
    version*: cuint
    filesize*: cuint
    flags*: cuint
    num_text*: cuint
    ofs_text*: cuint
    num_meshes*: cuint
    ofs_meshes*: cuint
    num_vertexarrays*: cuint
    num_vertexes*: cuint
    ofs_vertexarrays*: cuint
    num_triangles*: cuint
    ofs_triangles*: cuint
    ofs_adjacency*: cuint
    num_joints*: cuint
    ofs_joints*: cuint
    num_poses*: cuint
    ofs_poses*: cuint
    num_anims*: cuint
    ofs_anims*: cuint
    num_frames*: cuint
    num_framechannels*: cuint
    ofs_frames*: cuint
    ofs_bounds*: cuint
    num_comment*: cuint
    ofs_comment*: cuint
    num_extensions*: cuint
    ofs_extensions*: cuint

  iqmmesh* = object
    name*: cuint
    material*: cuint
    first_vertex*: cuint
    num_vertexes*: cuint
    first_triangle*: cuint
    num_triangles*: cuint


const
  IQM_POSITION* = 0
  IQM_TEXCOORD* = 1
  IQM_NORMAL* = 2
  IQM_TANGENT* = 3
  IQM_BLENDINDEXES* = 4
  IQM_BLENDWEIGHTS* = 5
  IQM_COLOR* = 6
  IQM_CUSTOM* = 0x00000010

const
  IQM_BYTE* = 0
  IQM_UBYTE* = 1
  IQM_SHORT* = 2
  IQM_USHORT* = 3
  IQM_INT* = 4
  IQM_UINT* = 5
  IQM_HALF* = 6
  IQM_FLOAT* = 7
  IQM_DOUBLE* = 8

type
  iqmtriangle* = object
    vertex*: array[3, cuint]

  iqmadjacency* = object
    triangle*: array[3, cuint]

  iqmjoint* = object
    name*: cuint
    parent*: cint
    translate*: array[3, cfloat]
    rotate*: array[4, cfloat]
    scale*: array[3, cfloat]

  iqmpose* = object
    parent*: cint
    mask*: cuint
    channeloffset*: array[10, cfloat]
    channelscale*: array[10, cfloat]

  iqmanim* = object
    name*: cuint
    first_frame*: cuint
    num_frames*: cuint
    framerate*: cfloat
    flags*: cuint


const
  IQM_LOOP* = 1 shl 0

type
  iqmvertexarray* = object
    `type`*: cuint
    flags*: cuint
    format*: cuint
    size*: cuint
    offset*: cuint

  iqmbounds* = object
    bbmin*: array[3, cfloat]
    bbmax*: array[3, cfloat]
    xyradius*: cfloat
    radius*: cfloat
