module LinearAlgebra.Matrix4 exposing (..)

{-| Matrix math

This is based on Math/Matrix4.elm in https://github.com/elm-community/elm-linear-algebra.

It currently implements only the functions needed for the elm-webgl "crate.elm" example.

# Create

@docs Mat4, mat4, identity

# Modify

@docs transform, makeRotate, mul, makeTranslate, makePerspective, makeLookAt, makeFrustum

-}

import LinearAlgebra.Vector3 as V3 exposing (Vec3, vec3)


{-| 4x4 matrix type
-}
type Mat4
  = Mat4 Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float


{-| Create a 4x4 matrix loading values by rows.
-}
mat4 : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Mat4
mat4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44 =
  Mat4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44


{-| identity
-}
identity : Mat4
identity =
  Mat4 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1


{-| Multiply a vector by a matrix: m * v
-}
transform : Mat4 -> Vec3 -> Vec3
transform (Mat4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44) v =
  let
    c1 =
      vec3 m11 m21 m31

    c2 =
      vec3 m12 m22 m32

    c3 =
      vec3 m13 m23 m33

    c4 =
      vec3 m14 m24 m34

    w =
      V3.dot v c4 + m44

    r1 =
      (V3.dot v c1 + m41) / w

    r2 =
      (V3.dot v c2 + m42) / w

    r3 =
      (V3.dot v c3 + m43) / w
  in
    vec3 r1 r2 r3


{-| Creates a transformation matrix for rotation in radians about the 3-element
vector axis.
-}
makeRotate : Float -> Vec3 -> Mat4
makeRotate angle axis =
  let
    ( x, y, z ) =
      V3.normalize axis |> V3.toTuple

    c =
      cos angle

    c1 =
      1 - c

    s =
      sin angle

    m11 =
      x * x * c1 + c

    m12 =
      y * x * c1 + z * s

    m13 =
      z * x * c1 - y * s

    m21 =
      x * y * c1 - z * s

    m22 =
      y * y * c1 + c

    m23 =
      y * z * c1 + x * s

    m31 =
      x * z * c1 + y * s

    m32 =
      y * z * c1 - x * s

    m33 =
      z * z * c1 + c
  in
    mat4 m11 m12 m13 0 m21 m22 m23 0 m31 m32 m33 0 0 0 0 1


{-| Matrix multiplication: m1 * m2
-}
mul : Mat4 -> Mat4 -> Mat4
mul (Mat4 a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44) (Mat4 b11 b12 b13 b14 b21 b22 b23 b24 b31 b32 b33 b34 b41 b42 b43 b44) =
  let
    r0 =
      a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41

    r1 =
      a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41

    r2 =
      a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41

    r3 =
      a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41

    r4 =
      a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42

    r5 =
      a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42

    r6 =
      a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42

    r7 =
      a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42

    r8 =
      a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43

    r9 =
      a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43

    r10 =
      a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43

    r11 =
      a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43

    r12 =
      a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44

    r13 =
      a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44

    r14 =
      a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44

    r15 =
      a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44
  in
    Mat4 r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15


{-| Creates a transformation matrix for translating each of the x, y, and z axes
by the amount given in the corresponding element of the 3-element vector.
-}
makeTranslate : Vec3 -> Mat4
makeTranslate v =
  let
    ( x, y, z ) =
      V3.toTuple v
  in
    mat4 1 0 0 0 0 1 0 0 0 0 1 0 x y z 1


{-| Creates a matrix for a perspective projection with the given parameters.
Parameters:
 * fovy - field of view in the y axis, in degrees
 * aspect - aspect ratio
 * znear - the near z distance of the projection
 * zfar - the far z distance of the projection
-}
makePerspective : Float -> Float -> Float -> Float -> Mat4
makePerspective fovy aspect znear zfar =
  let
    ymax =
      znear * tan (fovy * pi / 360.0)

    ymin =
      -ymax

    xmin =
      ymin * aspect

    xmax =
      ymax * aspect
  in
    makeFrustum xmin xmax ymin ymax znear zfar


{-| -}
makeFrustum : Float -> Float -> Float -> Float -> Float -> Float -> Mat4
makeFrustum left right bottom top znear zfar =
  let
    m11 =
      2 * znear / (right - left)

    m22 =
      2 * znear / (top - bottom)

    m31 =
      (right + left) / (right - left)

    m32 =
      (top + bottom) / (top - bottom)

    m33 =
      -1 * (zfar + znear) / (zfar - znear)

    m43 =
      -2 * zfar * znear / (zfar - znear)
  in
    Mat4 m11 0 0 0 0 m22 0 0 m31 m32 m33 -1 0 0 m43 0


{-|
Creates a transformation matrix for a camera.
Parameters:
 * eye - The location of the camera
 * center - The location of the focused object
 * up - The "up" direction according to the camera
-}
makeLookAt : Vec3 -> Vec3 -> Vec3 -> Mat4
makeLookAt eye center up =
  let
    z =
      V3.direction eye center

    x =
      V3.normalize (V3.cross up z)

    y =
      V3.normalize (V3.cross z x)

    ( ( x0, x1, x2 ), ( y0, y1, y2 ), ( z0, z1, z2 ) ) =
      ( V3.toTuple x, V3.toTuple y, V3.toTuple z )

    ( eye0, eye1, eye2 ) =
      V3.toTuple eye

    tm1 =
      mat4 x0 y0 z0 0 x1 y1 z1 0 x2 y2 z2 0 0 0 0 1

    tm2 =
      mat4 1 0 0 0 0 1 0 0 0 0 1 0 -eye0 -eye1 -eye2 1
  in
    mul tm1 tm2
