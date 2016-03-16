module Main (..) where

import Graphics.Element exposing (show)
import Signal
import Benchmark
import Task exposing (Task, andThen)
import Text
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Matrix4 as M4


main : Signal Graphics.Element.Element
main =
  Signal.map (Graphics.Element.leftAligned << Text.fromString) results.signal


vec3Suite =
  Benchmark.Suite
    "Vector3 suite"
    [ Benchmark.bench2 "V3.cross" V3.cross (V3.Vec3 2 3 4) (V3.Vec3 5 6 7)
    , Benchmark.bench2 "V3.distance" V3.distance (V3.Vec3 2 3 4) (V3.Vec3 5 6 7)
    ]


mat4Suite =
  let
    m1 =
      M4.Mat4 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16

    m2 =
      M4.Mat4 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
  in
    Benchmark.Suite
      "Matrix4 suite"
      [ Benchmark.bench2 "M4.mul" M4.mul m1 m2 ]


results : Signal.Mailbox String
results =
  Signal.mailbox "Benchmark loading"


port benchResults : Task Benchmark.Never ()
port benchResults =
  Benchmark.runWithProgress (Just results) mat4Suite `andThen` (\_ -> Task.succeed ())
