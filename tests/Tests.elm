module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Intervals
import Time


suite : Test
suite =
  describe "Intervals"
    [ test "Can make nice ints 1" <| \_ ->
        let result =
              Intervals.ints (Intervals.around 10) (Intervals.Range 0 100)

            expected =
              [0,10,20,30,40,50,60,70,80,90,100]
        in
        Expect.equal expected result
    , test "Can make nice ints 2" <| \_ ->
        let result =
              Intervals.ints (Intervals.around 15) (Intervals.Range 0 100)

            expected =
              [0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100]
        in
        Expect.equal expected result
    , test "Can make nice ints 3" <| \_ ->
        let result =
              Intervals.ints (Intervals.around 3) (Intervals.Range 0 10)

            expected =
              [0,2,4,6,8,10]
        in
        Expect.equal expected result
    , test "Can make nice ints 4" <| \_ ->
        let result =
              Intervals.ints (Intervals.around 2) (Intervals.Range 0 10)

            expected =
              [0,5,10]
        in
        Expect.equal expected result
    , test "Can make nice ints 5" <| \_ ->
        let result =
              Intervals.ints (Intervals.around 6) (Intervals.Range 0 2000)

            expected =
              [0,250,500,750,1000,1250,1500,1750,2000]
        in
        Expect.equal expected result
    , test "Can make nice floats 1" <| \_ ->
        let result =
              Intervals.floats (Intervals.around 5) (Intervals.Range 0 1)

            expected =
              [0,0.2,0.4,0.6,0.8,1]
        in
        Expect.equal expected result
    , test "Can make nice floats 2" <| \_ ->
        let result =
              Intervals.floats (Intervals.around 5) (Intervals.Range 0 10)

            expected =
              [0,2,4,6,8,10]
        in
        Expect.equal expected result
    , test "Can make nice floats 3" <| \_ ->
        let result =
              Intervals.floats (Intervals.around 5) (Intervals.Range -5 10)

            expected =
              [-5,-2.5,0,2.5,5,7.5,10]
        in
        Expect.equal expected result
    , test "Can make nice floats 4" <| \_ ->
        let result =
              Intervals.floats (Intervals.around 30) (Intervals.Range -5 5)

            expected =
              [-5,-4.75,-4.5,-4.25,-4,-3.75,-3.5,-3.25,-3,-2.75,-2.5,-2.25,-2,-1.75,-1.5,-1.25,-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5]
        in
        Expect.equal expected result
    , test "Can make nice custom intervals 1" <| \_ ->
        let result =
              Intervals.custom 45 10 (Intervals.Range 25 100)

            expected =
              [ 25, 35, 45, 55, 65, 75, 85, 95 ]
        in
        Expect.equal expected result

    , test "Can make nice custom intervals 2" <| \_ ->
        let result =
              Intervals.custom 30 20 (Intervals.Range 25 100)

            expected =
              [ 30, 50, 70, 90 ]
        in
        Expect.equal expected result
    ]
