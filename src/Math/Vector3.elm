module Math.Vector3 (..) where

{-| A linear algebra library using pure Elm. Geared towards 3D graphics and use
with `Graphics.WebGL`. All vectors are immutable.

# Create
@docs Vec3, vec3

# Get and Set
The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, getZ, setX, setY, setZ

# Operations
@docs add, sub, negate, scale, dot, normalize, direction,
      length, lengthSquared, distance, distanceSquared

# Conversions
@docs toTuple, fromTuple, toRecord, fromRecord
-}


type Vec3
  = Vec3 Float Float Float


vec3 : Float -> Float -> Float -> Vec3
vec3 =
  Vec3


getX : Vec3 -> Float
getX (Vec3 x _ _) =
  x


getY : Vec3 -> Float
getY (Vec3 _ y _) =
  y


getZ (Vec3 _ _ z) =
  z


setX : Vec3 -> Float -> Vec3
setX (Vec3 _ y z) x =
  Vec3 x y z


setY : Vec3 -> Float -> Vec3
setY (Vec3 x _ z) y =
  Vec3 x y z


setZ (Vec3 x y _) z =
  Vec3 x y z


toTuple : Vec3 -> ( Float, Float, Float )
toTuple (Vec3 x y z) =
  ( x, y, z )


toRecord : Vec3 -> { x : Float, y : Float, z : Float }
toRecord (Vec3 x y z) =
  { x = x, y = y, z = z }


fromTuple : ( Float, Float, Float ) -> Vec3
fromTuple ( x, y, z ) =
  Vec3 x y z


fromRecord : { a | x : Float, y : Float, z : Float } -> Vec3
fromRecord { x, y, z } =
  Vec3 x y z


add : Vec3 -> Vec3 -> Vec3
add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3 (x1 + x2) (y1 + y2) (z1 + z2)


negate : Vec3 -> Vec3
negate (Vec3 x y z) =
  Vec3 (-x) (-y) (-z)


sub : Vec3 -> Vec3 -> Vec3
sub v1 v2 =
  add v1 (negate v2)


length : Vec3 -> Float
length v =
  sqrt (lengthSquared v)


lengthSquared : Vec3 -> Float
lengthSquared (Vec3 x y z) =
  (x * x) + (y * y) + (z * z)


scale : Float -> Vec3 -> Vec3
scale k (Vec3 x y z) =
  Vec3 (x * k) (y * k) (z * k)


direction : Vec3 -> Vec3 -> Vec3
direction v1 v2 =
  normalize (sub v1 v2)


distanceSquared : Vec3 -> Vec3 -> Float
distanceSquared (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  let
    ( dx, dy, dz ) =
      ( (x1 - x2), (y1 - y2), (z1 - z2) )
  in
    dx * dx + dy * dy + dz * dz


distance : Vec3 -> Vec3 -> Float
distance v1 v2 =
  sqrt (distanceSquared v1 v2)


normalize : Vec3 -> Vec3
normalize v =
  scale (1 / length v) v


dot : Vec3 -> Vec3 -> Float
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  x1 * x2 + y1 * y2 + z1 * z2


cross : Vec3 -> Vec3 -> Vec3
cross (Vec3 a0 a1 a2) (Vec3 b0 b1 b2) =
  let
    r0 =
      a1 * b2 - a2 * b1

    r1 =
      a2 * b0 - a0 * b2

    r2 =
      a0 * b1 - a1 * b0
  in
    Vec3 r0 r1 r2
