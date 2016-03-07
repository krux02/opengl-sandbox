import memfiles, glm, ../fancygl, sdl2, opengl, strutils

include iqm

proc iqmFormatString(format : cuint) : string =
  case format
  of IQM_BYTE: "byte"
  of IQM_UBYTE: "ubyte"
  of IQM_SHORT: "short"
  of IQM_USHORT: "ushort"
  of IQM_INT: "int"
  of IQM_UINT: "uint"
  of IQM_HALF: "half"
  of IQM_FLOAT: "float"
  of IQM_DOUBLE: "double"
  else: "INVALID IQM FORMAT TAG: " & $format


proc iqmTypeString(t : cuint) : string =
  case t
  of IQM_POSITION:     "position"
  of IQM_TEXCOORD:     "texcoord"
  of IQM_NORMAL:       "normal"
  of IQM_TANGENT:      "tangent"
  of IQM_BLENDINDEXES: "blendindexes"
  of IQM_BLENDWEIGHTS: "blendweights"
  of IQM_COLOR:        "color"
  of IQM_CUSTOM:       "custom"
  else:                "INVALID IQM TYPE TAG: " & $t


type
  MeshData = object
    position : DataView[Vec3f]
    texcoord : DataView[Vec2f]
    normal   : DataView[Vec3f]
    tangent  : DataView[Vec4f]
    blendindexes : DataView[Vec4[uint8]]
    blendweights : DataView[Vec4[uint8]]

  Mesh = object
    data : ptr MeshData
    firstVertex : int
    numVertices : int

proc memptr[T](file:MemFile, offset: cuint) : ptr T = cast[ptr T](cast[int](file.mem) + offset.int)
proc memptr[T](file:MemFile, offset: cuint, num_elements: cuint) : DataView[T] =
  dataView[T]( cast[pointer](cast[int](file.mem) + offset.int), num_elements.int )

proc mkString[T](v : T, before,sep,after : string) : string =
  result = before
  var i = 0
  let last_i = v.len
  for x in v:
    result.add($x)
    if i != last_i:
      result.add(sep)
      i += 1

  result.add(after)

proc mkString[T](v : T, sep : string = ", ") : string =
  mkString(v, "", sep, "")

proc main() =
  #let window = createWindow("SDL/OpenGL Skeleton", 100, 100, 640, 480, SDL_WINDOW_OPENGL) # SDL_WINDOW_MOUSE_CAPTURE
  #let context = window.glCreateContext()
  # Initialize OpenGL
  #loadExtensions()

  var file = memfiles.open("mrfixit.iqm")
  defer:
    close(file)

  let hdr = memptr[iqmheader](file, 0)
  echo hdr.version

  let vertexArrays = memptr[iqmvertexarray](file, hdr.ofs_vertexarrays, hdr.num_vertexarrays)

  var meshData : MeshData

  echo "num vertex arrays: ", vertexArrays.len
  for va in vertexArrays:
    #echo "---- vertex array ----"
    #echo "type:   ", va.`type`.iqmTypeString
    #echo "format: ", va.format.iqmFormatString
    #echo "size:   ", va.size

    if va.`type` == IQM_POSITION and va.format == IQM_FLOAT and va.size == 3:
      echo "got positions"
      meshData.position = memptr[Vec3f](file, va.offset, hdr.num_vertexes)

    if va.`type` == IQM_TEXCOORD and va.format == IQM_FLOAT and va.size == 2:
      echo "got texcoords"
      meshData.texcoord = memptr[Vec2f](file, va.offset, hdr.num_vertexes )

    if va.`type` == IQM_NORMAL and va.format == IQM_FLOAT and va.size == 3:
      echo "got normals"
      meshData.normal = memptr[Vec3f](file, va.offset, hdr.num_vertexes )

    if va.`type` == IQM_TANGENT and va.format == IQM_FLOAT and va.size == 4:
      echo "got tangents"
      meshData.tangent = memptr[Vec4f](file, va.offset, hdr.num_vertexes )

    if va.`type` == IQM_BLENDINDEXES and va.format == IQM_UBYTE and va.size == 4:
      echo "got blend indices"
      meshData.blendindexes = memptr[Vec4[uint8]](file, va.offset, hdr.num_vertexes )

    if va.`type` == IQM_BLENDWEIGHTS and va.format == IQM_UBYTE and va.size == 4:
      echo "got blend weights"
      meshData.blendweights = memptr[Vec4[uint8]](file, va.offset, hdr.num_vertexes )

  echo "====================================="
  let triangles = memptr[iqmtriangle](file, hdr.ofs_triangles, hdr.num_triangles)
  echo "triangles: ", triangles.len
  for tri in triangles.take(10):
    echo tri.vertex[0], ", ", tri.vertex[1], ", ", tri.vertex[2]

  echo "====================================="
  let adjacencies = memptr[iqmadjacency](file, hdr.ofs_adjacency, hdr.num_triangles)
  echo "adjacencies: ", adjacencies.len
  for adj in adjacencies.take(10):
    echo adj.triangle[0], ", ", adj.triangle[1], ", ", adj.triangle[2]

  echo "====================================="
  let meshes = memptr[iqmmesh](file, hdr.ofs_meshes, hdr.num_meshes)
  echo "meshes: ", meshes.len
  for mesh in meshes:
    echo "got iqm mesh:"
    echo "  name:           ", mesh.name
    echo "  material:       ", mesh.material
    echo "  first_vertex:   ", mesh.first_vertex
    echo "  num_vertexes:   ", mesh.num_vertexes
    echo "  first_triangle: ", mesh.first_triangle
    echo "  num_triangles:  ", mesh.num_triangles

  echo "====================================="
  let joints = memptr[iqmjoint](file, hdr.ofs_joints, hdr.num_joints)
  echo "joints: ", joints.len
  for joint in joints.take(10):
    echo "name:      ", joint.name
    echo "parent:    ", joint.parent
    echo "translate: ", joint.translate.Vec3f
    echo "rotate:    ", joint.rotate.Vec4f
    echo "scale:     ", joint.scale.Vec3f

  echo "====================================="

  let poses = memptr[iqmpose](file, hdr.ofs_poses, hdr.num_poses)
  echo "poses: ", poses.len
  for pose in poses.take(10):
    echo "parent:        ", pose.parent
    echo "mask:          ", pose.mask.int.toHex(8)
    echo "channeloffset: ", pose.channeloffset.mkString()
    echo "channelscale:  ", pose.channelscale.mkString()

  echo "====================================="

  let anims = memptr[iqmanim](file, hdr.ofs_anims, hdr.num_anims)
  echo "anims: ", anims.len
  for anim in anims.take(10):
    echo "  name:        ", anim.name
    echo "  first_frame: ", anim.first_frame
    echo "  num_frames:  ", anim.num_frames
    echo "  framerate:   ", anim.framerate
    echo "  flags:       ", anim.flags.int.toHex(8)

  echo "====================================="

  #end for
#end main

main()






