import macros

type Vec4f = distinct array[4, float32]

type Buffer[T] = distinct int
type MyType = distinct tuple[a,b:int]
#proc elementType[T](b: Buffer[T]) : typedesc = T

macro typeString( t : typeDesc[int] ): string = "int"
macro typeString( t : typeDesc[float] ): string = "float"
macro typeString( t : typeDesc[Vec4f] ): string = "vec4f"
macro typeString( t : typeDesc[Buffer[int]] ): string = "buffer int"
macro typeString( t : typeDesc[Buffer[float]] ): string = "buffer float"
macro typeString( t : typeDesc[Buffer[Vec4f]] ): string = "buffer vec4f"
macro typeString( t : typeDesc[MyType] ): string = "mytype"

macro typeStringOuter(a:typed): string =
  result = getAst( typeString(a) )

echo typeString( Buffer[int] )
echo typeString( Buffer[float] )
echo typeString( Buffer[Vec4f] )
echo typeString( int )
echo typeString( float )
echo typeString( Vec4f )
echo typeString( MyType )

echo typeStringOuter( Buffer[int] )
echo typeStringOuter( Buffer[float] )
echo typeStringOuter( Buffer[Vec4f] )
echo typeStringOuter( int )
echo typeStringOuter( float )
echo typeStringOuter( Vec4f )
echo typeStringOuter( MyType )



import macros

macro inner( t : typeDesc[int] ): string = "int"
macro inner( t : typeDesc[float] ): string = "float"

macro outer(a:typed): string =
  result = getAst( inner(a) )

echo "inner(int): ", inner(int)
echo "outer(int): ", outer(int)
echo "inner(float): ", inner(float)
echo "outer(float): ", outer(float)

type Person = object of RootObj
  name : string
  age : int

type Student  = object of Person
  id : int



