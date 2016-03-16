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
    [ Benchmark.bench2 "V3.cross" V3.cross (V3.vec3 2 3 4) (V3.vec3 5 6 7)
    , Benchmark.bench2 "V3.distance" V3.distance (V3.vec3 2 3 4) (V3.vec3 5 6 7)
    ]


mat4Suite =
  let
    driver _ =
      let
        m1 =
          M4.makePerspective 10 20 30 40

        m2 =
          M4.makeFrustum 100 200 300 400 50 60
      in
        M4.mul m1 m2
  in
    Benchmark.Suite
      "Matrix4 suite"
      [ Benchmark.bench1 "M4.mul" driver () ]


results : Signal.Mailbox String
results =
  Signal.mailbox "Benchmark loading"


port benchResults : Task Benchmark.Never ()
port benchResults =
  Benchmark.runWithProgress (Just results) mat4Suite `andThen` (\_ -> Task.succeed ())
