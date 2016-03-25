module LinearAlgebra.Vector4 (..) where

{-| A linear algebra library using pure Elm. Geared towards 3D graphics and use
with `Graphics.WebGL`. All vectors are immutable.

# Create
@docs Vec4, vec4

# Get and Set
The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, getZ, getW, setX, setY, setZ, setW

# Operations
@docs add, sub, negate, scale, dot, normalize, direction,
      length, lengthSquared, distance, distanceSquared

# Conversions
@docs toTuple, fromTuple, toRecord, fromRecord
-}


type Vec4
  = Vec4 Float Float Float Float


vec4 : Float -> Float -> Float -> Float -> Vec4
vec4 =
  Vec4


getX : Vec4 -> Float
getX (Vec4 x _ _ _) =
  x


getY : Vec4 -> Float
getY (Vec4 _ y _ _) =
  y

getZ : Vec4 -> Float
getZ (Vec4 _ _ z _) =
  z

getW : Vec4 -> Float
getW (Vec4 _ _ _ w) =
  w


setX : Vec4 -> Float -> Vec4
setX (Vec4 _ y z w) x =
  Vec4 x y z w


setY : Vec4 -> Float -> Vec4
setY (Vec4 x _ z w) y =
  Vec4 x y z w


setZ (Vec4 x y _ w) z =
  Vec4 x y z w


setW (Vec4 x y z _) w =
  Vec4 x y z w


toTuple : Vec4 -> ( Float, Float, Float, Float )
toTuple (Vec4 x y z w) =
  ( x, y, z, w )


toRecord : Vec4 -> { x : Float, y : Float, z : Float, w : Float }
toRecord (Vec4 x y z w) =
  { x = x, y = y, z = z, w = w }


fromTuple : ( Float, Float, Float, Float ) -> Vec4
fromTuple ( x, y, z, w ) =
  Vec4 x y z w


fromRecord : { a | x : Float, y : Float, z : Float, w : Float } -> Vec4
fromRecord { x, y, z, w } =
  Vec4 x y z w


add : Vec4 -> Vec4 -> Vec4
add (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) =
  Vec4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)


negate : Vec4 -> Vec4
negate (Vec4 x y z w) =
  Vec4 (-x) (-y) (-z) (-w)


sub : Vec4 -> Vec4 -> Vec4
sub v1 v2 =
  add v1 (negate v2)


length : Vec4 -> Float
length v =
  sqrt (lengthSquared v)


lengthSquared : Vec4 -> Float
lengthSquared (Vec4 x y z w) =
  (x * x) + (y * y) + (z * z) + (w * w)


scale : Float -> Vec4 -> Vec4
scale k (Vec4 x y z w) =
  Vec4 (x * k) (y * k) (z * k) (w * k)


direction : Vec4 -> Vec4 -> Vec4
direction v1 v2 =
  normalize (sub v1 v2)


distanceSquared : Vec4 -> Vec4 -> Float
distanceSquared (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) =
  let
    ( dx, dy, dz, dw ) =
      ( (x1 - x2), (y1 - y2), (z1 - z2), (w1 - w2) )
  in
    dx * dx + dy * dy + dz * dz + dw * dw


distance : Vec4 -> Vec4 -> Float
distance v1 v2 =
  sqrt (distanceSquared v1 v2)


normalize : Vec4 -> Vec4
normalize v =
  scale (1 / length v) v


dot : Vec4 -> Vec4 -> Float
dot (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) =
  x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2
