module Main (..) where

import Graphics.Element exposing (show)
import Signal
import Benchmark
import Task exposing (Task, andThen)
import Text
import Math.Vector2 as V2 exposing (Vec2, vec2)
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


dotSuite =
  let
    dotTuple ( x1, y1 ) ( x2, y2 ) =
      (x1 * x2) + (y1 * y2)

    dotSimple x1 y1 x2 y2 =
      (x1 * x2) + (y1 * y2)

    driverTuple _ =
      dotTuple ( 10, 20 ) ( 30, 40 )

    driverSimple _ =
      dotSimple 10 20 30 40

    driver _ =
      V2.dot (V2.Vec2 10 20) (V2.Vec2 30 40)
  in
    Benchmark.Suite
      "Dot suite"
      [ Benchmark.bench1 "tuple dot" driverTuple ()
      , Benchmark.bench1 "simple dot" driverSimple ()
      , Benchmark.bench1 "V2.dot" driver ()
      ]


results : Signal.Mailbox String
results =
  Signal.mailbox "Benchmark loading"


port benchResults : Task Benchmark.Never ()
port benchResults =
  Benchmark.runWithProgress (Just results) dotSuite `andThen` (\_ -> Task.succeed ())
