module TestTimes exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Values exposing (..)
import Time
import Time.Extra as T


suite : Test
suite =
  describe "values"
   [ test "1" <| \_ ->
        let result =
              values Time.utc 3
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Apr 1 0 0 0 0))
                -- Diff 0 3 89 2113 126721 7603201 7603200001
                -- F M A

            expected =
              ( Month, 1 )
        in
        Expect.equal expected result
    , test "2" <| \_ ->
        let result =
              values Time.utc 3
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Jan 10 0 0 0 0))
                -- 4 5 6 7 8 9 10

            expected =
              ( Day, 3 )
        in
        Expect.equal expected result

    , test "3" <| \_ ->
        let result =
              values Time.utc 7
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Jan 10 0 0 0 0))
                -- 4 5 6 7 8 9 10

            expected =
              ( Day, 1 )
        in
        Expect.equal expected result


    , test "4" <| \_ ->
        let result =
              values Time.utc 18
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2024 Time.Jan 10 0 0 0 0))

            expected =
              ( Month, 3 )
        in
        Expect.equal expected result

    , test "4-2" <| \_ ->
        let result =
              values Time.utc 5
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2024 Time.Jan 10 0 0 0 0))

            expected =
              ( Year, 1 )
        in
        Expect.equal expected result


    , test "4-3" <| \_ ->
        let result =
              values Time.utc 105
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2024 Time.Jan 10 0 0 0 0))

            expected =
              ( Day, 14 )
        in
        Expect.equal expected result

    , test "4-A" <| \_ ->
        let result =
              getNumOfTicks Time.utc Day 14
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2024 Time.Jan 10 0 0 0 0))

            expected =
              105
        in
        Expect.equal expected result

    , test "5-1" <| \_ ->
        let result =
              values Time.utc 210
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2124 Time.Jan 10 0 0 0 0))

            expected =
              ( Month, 6 )
        in
        Expect.equal expected result

    , test "5-A" <| \_ ->
        let result =
              getNumOfTicks Time.utc Month 6
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2124 Time.Jan 10 0 0 0 0))

            expected =
              208
        in
        Expect.equal expected result


    , test "6-1" <| \_ ->
        let result =
              values Time.utc 2
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2224 Time.Jan 10 0 0 0 0))
                -- 2100  2200

            expected =
              ( Year, 100 )
        in
        Expect.equal expected result


    , test "6-2" <| \_ ->
        let result =
              values Time.utc 2
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 4224 Time.Jan 10 0 0 0 0))

            expected =
              ( Year, 1000 )
        in
        Expect.equal expected result

    , test "6-A" <| \_ ->
        let result =
              getNumOfTicks Time.utc Year 100
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2224 Time.Jan 10 0 0 0 0))

            expected =
              2
        in
        Expect.equal expected result


    , test "7-1" <| \_ ->
        let result =
              values Time.utc 210
                (toPosix (T.Parts 2020 Time.Feb 1 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Feb 1 16 0 0 0))

            expected =
              ( Minute, 5 )
        in
        Expect.equal expected result


    , test "7-A" <| \_ ->
        let result =
              getNumOfTicks Time.utc Minute 5
                (toPosix (T.Parts 2020 Time.Feb 1 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Feb 1 16 0 0 0))

            expected =
              193
        in
        Expect.equal expected result

    , test "8-1" <| \_ ->
        let result =
              values Time.utc 4
                (toPosix (T.Parts 2020 Time.Jan 1 0 15 0 0))
                (toPosix (T.Parts 2020 Time.Jan 1 2 0 0 0))
                -- 0:30 1 1:30 2

            expected =
              ( Minute, 30 )
        in
        Expect.equal expected result

    , test "8-2" <| \_ ->
        let result =
              values Time.utc 10
                (toPosix (T.Parts 2020 Time.Jan 1 0 15 0 0))
                (toPosix (T.Parts 2020 Time.Jan 1 2 0 0 0))
                -- 0:15 0:30 0:45 1 0:15 1:30 1:45 2

            expected =
              ( Minute, 15 )
        in
        Expect.equal expected result


    , test "8-3" <| \_ ->
        let result =
              values Time.utc 10
                (toPosix (T.Parts 2020 Time.Jan 1 0 15 0 0))
                (toPosix (T.Parts 2020 Time.Jan 1 2 0 0 0))
                -- 0:20 0:30 0:40 0:50 1 0:10 0:20 1:30 0:40 1:50 2

            expected =
              ( Minute, 15 )
        in
        Expect.equal expected result


    , test "8-4" <| \_ ->
        let result =
              values Time.utc 22
                (toPosix (T.Parts 2020 Time.Jan 1 0 15 0 0))
                (toPosix (T.Parts 2020 Time.Jan 1 2 0 0 0))

            expected =
              ( Minute, 5 )
        in
        Expect.equal expected result


    , test "8-A" <| \_ ->
        let result =
              getNumOfTicks Time.utc Minute 5
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Jan 1 2 0 0 0))

            expected =
              25
        in
        Expect.equal expected result

    , test "9-1" <| \_ ->
        let result =
              values Time.utc 4
                (toPosix (T.Parts 2020 Time.Jan 1 0 4 0 0))
                (toPosix (T.Parts 2020 Time.Jan 1 0 49 0 0))
                -- 0:15 0:30 0:45

            expected =
              ( Minute, 10 )
        in
        Expect.equal expected result


    , test "9-2" <| \_ ->
        let result =
              values Time.utc 7
                (toPosix (T.Parts 2020 Time.Jan 1 0 4 0 0))
                (toPosix (T.Parts 2020 Time.Jan 1 0 49 0 0))
                -- 0:10 0:20 0:30 0:40

            expected =
              ( Minute, 10 )
        in
        Expect.equal expected result

    , test "9-3" <| \_ ->
        let result =
              values Time.utc 10
                (toPosix (T.Parts 2020 Time.Jan 1 0 4 0 0))
                (toPosix (T.Parts 2020 Time.Jan 1 0 49 0 0))
                -- 0:15 0:10 0:15 0:20 0:25 0:30 0:35 0:40 0:45

            expected =
              ( Minute, 5 )
        in
        Expect.equal expected result


    , test "9-A" <| \_ ->
        let result =
              getNumOfTicks Time.utc Minute 5
                (toPosix (T.Parts 2020 Time.Jan 1 0 4 0 0))
                (toPosix (T.Parts 2020 Time.Jan 1 0 49 0 0))

            expected =
              9
        in
        Expect.equal expected result


    , test "10-1" <| \_ ->
        let result =
              values Time.utc 4
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 4 0))
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 49 0))

            expected =
              ( Second, 10 )
        in
        Expect.equal expected result

    , test "10-A" <| \_ ->
        let result =
              getNumOfTicks Time.utc Second 15
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 4 0))
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 49 0))

            expected =
              3
        in
        Expect.equal expected result

    , test "11-1" <| \_ ->
        let result =
              values Time.utc 4
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 256))
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 940))

            expected =
              ( Millisecond, 200 )
        in
        Expect.equal expected result

    , test "11-A" <| \_ ->
        let result =
              getNumOfTicks Time.utc Millisecond 200
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 256))
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 940))

            expected =
              3
        in
        Expect.equal expected result
    ]


getNumOfTicksSuite : Test
getNumOfTicksSuite =
  describe "getNumOfTicks"
    [ test "1" <| \_ ->
        let result =
              getNumOfTicks Time.utc Month 1
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Apr 1 0 0 0 0))

            expected =
              3
        in
        Expect.equal expected result
    , test "2" <| \_ ->
        let result =
              getNumOfTicks Time.utc Month 1
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2021 Time.Apr 1 0 0 0 0))

            expected =
              15
        in
        Expect.equal expected result

    , test "3" <| \_ ->
        let result =
              getNumOfTicks Time.utc Day 1
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Jan 9 0 0 0 0))

            expected =
              6
        in
        Expect.equal expected result

    , test "9" <| \_ ->
        let result =
              getNumOfTicks Time.utc Day 1
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Jan 10 0 0 0 0))

            expected =
              7
        in
        Expect.equal expected result

    , test "4" <| \_ ->
        let result =
              getNumOfTicks Time.utc Millisecond 1
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 1))
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 3))

            expected =
              3
        in
        Expect.equal expected result

    , test "5" <| \_ ->
        let result =
              getNumOfTicks Time.utc Millisecond 1
                (toPosix (T.Parts 2020 Time.Jan 1 1 1 1 1))
                (toPosix (T.Parts 2020 Time.Jan 1 1 1 1 1))

            expected =
              1
        in
        Expect.equal expected result
    ]



helperSuite : Test
helperSuite =
  describe "Diff"
    [ test "getDiff 1" <| \_ ->
        let result =
              getDiff Time.utc
                (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Apr 1 0 0 0 0))

            expected =
              Diff 0 2 27 0 0 0 0
        in
        Expect.equal expected result

    , test "getDiff 2" <| \_ ->
        let result =
              getDiff Time.utc
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0))
                (toPosix (T.Parts 2021 Time.Jan 1 0 0 0 0))

            expected =
              Diff 1 0 0 0 0 0 0
        in
        Expect.equal expected result

    , test "getDiff 3" <| \_ ->
        let result =
              getDiff Time.utc
                (toPosix (T.Parts 2020 Time.Feb 2 0 0 0 0))
                (toPosix (T.Parts 2022 Time.Jan 1 0 0 0 0))

            expected =
              Diff 1 10 30 0 0 0 0
        in
        Expect.equal expected result

    , test "getDiff 4" <| \_ ->
        let result =
              getDiff Time.utc
                (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0))
                (toPosix (T.Parts 2020 Time.Mar 1 0 0 0 0))

            expected =
              Diff 0 2 0 0 0 0 0
        in
        Expect.equal expected result

    , test "getDiff 5" <| \_ ->
        let result =
              getDiff Time.utc
                (toPosix (T.Parts 2020 Time.Jan 1 16 0 0 0))
                (toPosix (T.Parts 2020 Time.Jan 3 14 0 0 0))

            expected =
              Diff 0 0 1 22 0 0 0
        in
        Expect.equal expected result

    , test "getDiff 6" <| \_ ->
        let result =
              getDiff Time.utc
                (toPosix (T.Parts 2020 Time.Jan 1 16 45 0 0))
                (toPosix (T.Parts 2020 Time.Jan 3 14 30 0 0))

            expected =
              Diff 0 0 1 21 45 0 0
        in
        Expect.equal expected result

    , test "getDiff 7" <| \_ ->
        let result =
              getDiff Time.utc
                (toPosix (T.Parts 2020 Time.Jan 1 16 45 45 0))
                (toPosix (T.Parts 2020 Time.Jan 3 14 30 30 0))

            expected =
              Diff 0 0 1 21 44 45 0
        in
        Expect.equal expected result

    , test "getDiff 8" <| \_ ->
        let result =
              getDiff Time.utc
                (toPosix (T.Parts 2020 Time.Feb 1 0 0 0 0))
                (toPosix (T.Parts 2021 Time.Jan 3 0 0 0 0))

            expected =
              Diff 0 11 2 0 0 0 0
        in
        Expect.equal expected result

    , test "getDiff 9" <| \_ ->
        let result =
              getDiff Time.utc
                (toPosix (T.Parts 2020 Time.Feb 1 16 45 45 500))
                (toPosix (T.Parts 2021 Time.Jan 3 14 30 30 200))

            expected =
              Diff 0 11 1 21 44 44 700
        in
        Expect.equal expected result
    ]



ceilingSuit : Test
ceilingSuit =
  describe "ceilingX"
    [ describe "ceilingYear"
        [ test "1" <| \_ ->
            let result =
                  ceilingYear Time.utc 100
                    (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))

                expected =
                  toPosix (T.Parts 2100 Time.Jan 1 0 0 0 0)
            in
            Expect.equal expected result

        , test "2" <| \_ ->
            let result =
                  ceilingYear Time.utc 10
                    (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2030 Time.Jan 1 0 0 0 0
            in
            Expect.equal expected result

        , test "3" <| \_ ->
            let result =
                  ceilingYear Time.utc 10
                    (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0))

                expected =
                  toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0)
            in
            Expect.equal expected result

        , test "4" <| \_ ->
            let result =
                  ceilingYear Time.utc 200
                    (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0))

                expected =
                  toPosix (T.Parts 2200 Time.Jan 1 0 0 0 0)
            in
            Expect.equal expected result

        , test "5" <| \_ ->
            let result =
                  ceilingYear Time.utc 1000
                    (toPosix (T.Parts 2020 Time.Dec 1 0 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 3000 Time.Jan 1 0 0 0 0
            in
            Expect.equal expected result
        ]

    , describe "ceilingMonth"
        [ test "1" <| \_ ->
            let result =
                  ceilingMonth Time.utc 2
                    (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Mar 1 0 0 0 0
            in
            Expect.equal expected result

        , test "2" <| \_ ->
            let result =
                  ceilingMonth Time.utc 2
                    (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 1 0 0 0 0
            in
            Expect.equal expected result

        , test "3" <| \_ ->
            let result =
                  ceilingMonth Time.utc 3
                    (toPosix (T.Parts 2020 Time.May 1 0 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jul 1 0 0 0 0
            in
            Expect.equal expected result

        , test "4" <| \_ ->
            let result =
                  ceilingMonth Time.utc 3
                    (toPosix (T.Parts 2020 Time.Dec 1 0 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2021 Time.Jan 1 0 0 0 0
            in
            Expect.equal expected result

        , test "5" <| \_ ->
            let result =
                  ceilingMonth Time.utc 6
                    (toPosix (T.Parts 2020 Time.Dec 1 0 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2021 Time.Jan 1 0 0 0 0
            in
            Expect.equal expected result


        ]

    , describe "ceilingDay"
        [ test "1" <| \_ ->
            let result =
                  ceilingDay Time.utc 2
                    (toPosix (T.Parts 2020 Time.Jan 4 2 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 5 0 0 0 0
            in
            Expect.equal expected result

        , test "2" <| \_ ->
            let result =
                  ceilingDay Time.utc 2
                    (toPosix (T.Parts 2020 Time.Dec 31 2 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2021 Time.Jan 1 0 0 0 0
            in
            Expect.equal expected result

        , test "3" <| \_ ->
            let result =
                  ceilingDay Time.utc 7
                    (toPosix (T.Parts 2020 Time.Dec 31 2 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2021 Time.Jan 4 0 0 0 0
            in
            Expect.equal expected result

        , test "4" <| \_ ->
            let result =
                  ceilingDay Time.utc 1
                    (toPosix (T.Parts 2020 Time.Mar 31 2 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Apr 1 0 0 0 0
            in
            Expect.equal expected result
        ]

    , describe "ceilingHour"
        [ test "1" <| \_ ->
            let result =
                  ceilingHour Time.utc 2
                    (toPosix (T.Parts 2020 Time.Jan 4 3 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 4 0 0 0
            in
            Expect.equal expected result

        , test "2" <| \_ ->
            let result =
                  ceilingHour Time.utc 2
                    (toPosix (T.Parts 2020 Time.Jan 1 23 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 2 0 0 0 0
            in
            Expect.equal expected result

        , test "3" <| \_ ->
            let result =
                  ceilingHour Time.utc 3
                    (toPosix (T.Parts 2020 Time.Jan 1 23 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 2 0 0 0 0
            in
            Expect.equal expected result

        , test "4" <| \_ ->
            let result =
                  ceilingHour Time.utc 3
                    (toPosix (T.Parts 2020 Time.Jan 1 17 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 1 18 0 0 0
            in
            Expect.equal expected result

        , test "5" <| \_ ->
            let result =
                  ceilingHour Time.utc 1
                    (toPosix (T.Parts 2020 Time.Jan 4 3 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 0 0 0
            in
            Expect.equal expected result
        ]

    , describe "ceilingMinute"
        [ test "1" <| \_ ->
            let result =
                  ceilingMinute Time.utc 15
                    (toPosix (T.Parts 2020 Time.Jan 4 3 2 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 15 0 0
            in
            Expect.equal expected result

        , test "2" <| \_ ->
            let result =
                  ceilingMinute Time.utc 15
                    (toPosix (T.Parts 2020 Time.Jan 4 3 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 0 0 0
            in
            Expect.equal expected result

        , test "3" <| \_ ->
            let result =
                  ceilingMinute Time.utc 2
                    (toPosix (T.Parts 2020 Time.Jan 4 3 1 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 2 0 0
            in
            Expect.equal expected result
        ]

    , describe "ceilingSecond"
        [ test "1" <| \_ ->
            let result =
                  ceilingSecond Time.utc 15
                    (toPosix (T.Parts 2020 Time.Jan 4 3 0 2 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 0 15 0
            in
            Expect.equal expected result

        , test "2" <| \_ ->
            let result =
                  ceilingSecond Time.utc 15
                    (toPosix (T.Parts 2020 Time.Jan 4 3 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 0 0 0
            in
            Expect.equal expected result

        , test "3" <| \_ ->
            let result =
                  ceilingSecond Time.utc 2
                    (toPosix (T.Parts 2020 Time.Jan 4 3 0 1 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 0 2 0
            in
            Expect.equal expected result
        ]

    , describe "ceilingMs"
        [ test "1" <| \_ ->
            let result =
                  ceilingMs Time.utc 200
                    (toPosix (T.Parts 2020 Time.Jan 4 3 0 0 2))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 0 0 200
            in
            Expect.equal expected result

        , test "2" <| \_ ->
            let result =
                  ceilingMs Time.utc 300
                    (toPosix (T.Parts 2020 Time.Jan 4 3 0 0 0))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 0 0 0
            in
            Expect.equal expected result

        , test "3" <| \_ ->
            let result =
                  ceilingMs Time.utc 200
                    (toPosix (T.Parts 2020 Time.Jan 4 3 0 0 1))
                    |> fromPosix

                expected =
                  T.Parts 2020 Time.Jan 4 3 0 0 200
            in
            Expect.equal expected result
        ]
    ]




toPosix : T.Parts -> Time.Posix
toPosix =
  T.partsToPosix Time.utc


fromPosix : Time.Posix -> T.Parts
fromPosix =
  T.posixToParts Time.utc