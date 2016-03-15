module Main (..) where

import Vector2 exposing (..)
import ElmTest exposing (..)


epsilon =
  1.0e-15


assertClose v1 v2 =
  let
    d = distance v1 v2
    _ = Debug.log "distance" (v1, v2, d)
  in
    assert <| d < epsilon


main =
  elementRunner
    <| suite
        "all tests"
        [ test "length" <| assertEqual 5 <| length (Vec2 3 4)
        , test "distance" <| assertEqual 5 <| distance (vec2 10 20) (vec2 13 16)
        , test "normalize" <| assertClose (Vec2 0.6 0.8) <| normalize (Vec2 3 4)
        ]
