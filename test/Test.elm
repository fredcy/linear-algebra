module Main (..) where

import Vector2 as V2 exposing (vec2)
import Vector3 as V3 exposing (vec3)
import ElmTest exposing (..)


epsilon =
  1.0e-15


assertClose f1 f2 =
  let
    d =
      abs (f1 - f2)

    _ =
      Debug.log "d" d
  in
    assert <| d < epsilon


assertCloseVec2 v1 v2 =
  let
    d =
      V2.distance v1 v2

    _ =
      Debug.log "distance" ( v1, v2, d )
  in
    assert <| d < epsilon


vector2Suite =
  suite
    "Vector2"
    [ test "length" <| assertEqual 5 <| V2.length (vec2 3 4)
    , test "distance" <| assertEqual 5 <| V2.distance (vec2 10 20) (vec2 13 16)
    , test "normalize" <| assertCloseVec2 (vec2 0.6 0.8) <| V2.normalize (vec2 3 4)
    , test "dot" <| assertEqual 41 <| V2.dot (vec2 2 5) (vec2 3 7)
    ]


vector3Suite =
  let
    dist345 =
      sqrt (3 * 3 + 4 * 4 + 5 * 5)
  in
    suite
      "Vector3"
      [ test "length" <| assertClose dist345 <| V3.length (vec3 3 4 5)
      , test "distance" <| assertClose dist345
          <| V3.distance (vec3 10 20 30) (vec3 13 16 35)
      , test "dot" <| assertEqual 121 <| V3.dot (vec3 2 5 10) (vec3 3 7 8)
      ]


main =
  elementRunner
    <| suite
        "all tests"
        [ vector2Suite
        , vector3Suite
        ]
