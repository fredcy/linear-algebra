module LinearAlgebra.Vector2 (..) where

{-| A linear algebra library using pure Elm. Geared towards 3D graphics and use
with `Graphics.WebGL`. All vectors are immutable.

# Create
@docs Vec2, vec2

# Get and Set
The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, setX, setY

# Operations
@docs add, sub, negate, scale, dot, normalize, direction,
      length, lengthSquared, distance, distanceSquared

# Conversions
@docs toTuple, fromTuple, toRecord, fromRecord
-}


type Vec2
  = Vec2 Float Float


vec2 : Float -> Float -> Vec2
vec2 =
  Vec2


getX : Vec2 -> Float
getX (Vec2 x _) =
  x


getY : Vec2 -> Float
getY (Vec2 _ y) =
  y


setX : Vec2 -> Float -> Vec2
setX (Vec2 _ y) x =
  Vec2 x y


setY : Vec2 -> Float -> Vec2
setY (Vec2 x _) y =
  Vec2 x y


toTuple : Vec2 -> ( Float, Float )
toTuple (Vec2 x y) =
  ( x, y )


toRecord : Vec2 -> { x : Float, y : Float }
toRecord (Vec2 x y) =
  { x = x, y = y }


fromTuple : ( Float, Float ) -> Vec2
fromTuple ( x, y ) =
  Vec2 x y


fromRecord : { a | x : Float, y : Float } -> Vec2
fromRecord { x, y } =
  Vec2 x y


add : Vec2 -> Vec2 -> Vec2
add (Vec2 x1 y1) (Vec2 x2 y2) =
  Vec2 (x1 + x2) (y1 + y2)


negate : Vec2 -> Vec2
negate (Vec2 x y) =
  Vec2 (-x) (-y)


sub : Vec2 -> Vec2 -> Vec2
sub vec1 vec2 =
  add vec1 (negate vec2)


length : Vec2 -> Float
length v =
  sqrt (lengthSquared v)


lengthSquared : Vec2 -> Float
lengthSquared (Vec2 x y) =
  (x * x) + (y * y)


scale : Float -> Vec2 -> Vec2
scale k (Vec2 x y) =
  Vec2 (x * k) (y * k)


direction : Vec2 -> Vec2 -> Vec2
direction v1 v2 =
  normalize (sub v1 v2)


distanceSquared : Vec2 -> Vec2 -> Float
distanceSquared (Vec2 x1 y1) (Vec2 x2 y2) =
  let
    ( dx, dy ) =
      ( (x1 - x2), (y1 - y2) )
  in
    dx * dx + dy * dy


distance : Vec2 -> Vec2 -> Float
distance v1 v2 =
  sqrt (distanceSquared v1 v2)


normalize : Vec2 -> Vec2
normalize v =
  scale (1 / length v) v


dot : Vec2 -> Vec2 -> Float
dot (Vec2 x1 y1) (Vec2 x2 y2) =
  (x1 * x2) + (y1 * y2)
