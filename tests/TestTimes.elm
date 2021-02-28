module TestTimes exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Values exposing (..)
import Time
import Time.Extra as T



suite : Test
suite =
  describe "values:"
    [ testValues "a year with six ticks" 6
        (T.Parts 2020 Time.Jan 1 0 0 0 0)
        (T.Parts 2021 Time.Jan 1 0 0 0 0)
        [ Time (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0)) Time.utc True Month 3 Year
        , Time (toPosix (T.Parts 2020 Time.Apr 1 0 0 0 0)) Time.utc False Month 3 Month
        , Time (toPosix (T.Parts 2020 Time.Jul 1 0 0 0 0)) Time.utc False Month 3 Month
        , Time (toPosix (T.Parts 2020 Time.Oct 1 0 0 0 0)) Time.utc False Month 3 Month
        , Time (toPosix (T.Parts 2021 Time.Jan 1 0 0 0 0)) Time.utc False Month 3 Year
        ]

    , testValues "a offset year with six ticks" 6
        (T.Parts 2019 Time.Nov 1 0 0 0 0)
        (T.Parts 2021 Time.Jan 1 0 0 0 0)
        [ Time (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0)) Time.utc True Month 3 Year
        , Time (toPosix (T.Parts 2020 Time.Apr 1 0 0 0 0)) Time.utc False Month 3 Month
        , Time (toPosix (T.Parts 2020 Time.Jul 1 0 0 0 0)) Time.utc False Month 3 Month
        , Time (toPosix (T.Parts 2020 Time.Oct 1 0 0 0 0)) Time.utc False Month 3 Month
        , Time (toPosix (T.Parts 2021 Time.Jan 1 0 0 0 0)) Time.utc False Month 3 Year
        ]

    , testValues "a more offset year with six ticks" 6
        (T.Parts 2019 Time.Oct 1 0 0 0 0)
        (T.Parts 2021 Time.Jan 1 0 0 0 0)
        [ Time (toPosix (T.Parts 2019 Time.Oct 1 0 0 0 0)) Time.utc True Month 3 Month
        , Time (toPosix (T.Parts 2020 Time.Jan 1 0 0 0 0)) Time.utc False Month 3 Year
        , Time (toPosix (T.Parts 2020 Time.Apr 1 0 0 0 0)) Time.utc False Month 3 Month
        , Time (toPosix (T.Parts 2020 Time.Jul 1 0 0 0 0)) Time.utc False Month 3 Month
        , Time (toPosix (T.Parts 2020 Time.Oct 1 0 0 0 0)) Time.utc False Month 3 Month
        , Time (toPosix (T.Parts 2021 Time.Jan 1 0 0 0 0)) Time.utc False Month 3 Year
        ]

    , testValues "a week with seven ticks" 10
        (T.Parts 2020 Time.Jan 3 0 0 0 0)
        (T.Parts 2020 Time.Jan 12 0 0 0 0)
        [ Time (toPosix (T.Parts 2020 Time.Jan 3 0 0 0 0)) Time.utc True Day 1 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0)) Time.utc False Day 1 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 5 0 0 0 0)) Time.utc False Day 1 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 6 0 0 0 0)) Time.utc False Day 1 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 7 0 0 0 0)) Time.utc False Day 1 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 8 0 0 0 0)) Time.utc False Day 1 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 9 0 0 0 0)) Time.utc False Day 1 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 10 0 0 0 0)) Time.utc False Day 1 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 11 0 0 0 0)) Time.utc False Day 1 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 12 0 0 0 0)) Time.utc False Day 1 Day
        ]

    , testValues "offset two days with 24 ticks" 8
        (T.Parts 2020 Time.Jan 2 13 0 0 0)
        (T.Parts 2020 Time.Jan 4 4 0 0 0)
        [ Time (toPosix (T.Parts 2020 Time.Jan 2 18 0 0 0)) Time.utc True Hour 6 Hour
        , Time (toPosix (T.Parts 2020 Time.Jan 3 0 0 0 0)) Time.utc False Hour 6 Day
        , Time (toPosix (T.Parts 2020 Time.Jan 3 6 0 0 0)) Time.utc False Hour 6 Hour
        , Time (toPosix (T.Parts 2020 Time.Jan 3 12 0 0 0)) Time.utc False Hour 6 Hour
        , Time (toPosix (T.Parts 2020 Time.Jan 3 18 0 0 0)) Time.utc False Hour 6 Hour
        , Time (toPosix (T.Parts 2020 Time.Jan 4 0 0 0 0)) Time.utc False Hour 6 Day
        ]
    ]


testValues : String -> Int -> T.Parts -> T.Parts -> List Time -> Test
testValues name amount a b expected =
  test name <| \_ ->
    let result =
          values Time.utc amount (toPosix a) (toPosix b)
    in
    Expect.equal expected result


bestUnitSuite : Test
bestUnitSuite =
  describe "best unit for"
   [ testBestUnit "several months with month interval" 3
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2020 Time.Apr 1 0 0 0 0)
        ( Month, 1 )

    , testBestUnit "several day interval" 3
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2020 Time.Jan 10 0 0 0 0)
        ( Day, 3 )

    , testBestUnit "a week with single day interval" 7
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2020 Time.Jan 10 0 0 0 0)
        ( Day, 1 )

    , testBestUnit "some years with quarter interval" 18
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2024 Time.Jan 10 0 0 0 0)
        ( Month, 3 )

    , testBestUnit "some years with single year interval" 5
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2024 Time.Jan 10 0 0 0 0)
        ( Year, 1 )

    , testBestUnit "many years with day interval" 105
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2024 Time.Jan 10 0 0 0 0)
        ( Day, 14 )

    , testBestUnit "many years with some ticks" 210
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2124 Time.Jan 10 0 0 0 0)
        ( Month, 6 )

    , testBestUnit "many years with few ticks" 2
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2224 Time.Jan 10 0 0 0 0)
        ( Year, 100 )

    , testBestUnit "many many years with few ticks" 2
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 4224 Time.Jan 10 0 0 0 0)
        ( Year, 1000 )

    , testBestUnit "hours with many many ticks" 210
        (T.Parts 2020 Time.Feb 1 0 0 0 0)
        (T.Parts 2020 Time.Feb 1 16 0 0 0)
        ( Minute, 5 )

    , testBestUnit "minutes crossing hours with few ticks" 4
        (T.Parts 2020 Time.Jan 1 0 15 0 0)
        (T.Parts 2020 Time.Jan 1 2 0 0 0)
        ( Minute, 30 )

    , testBestUnit "minutes crossing hours with some ticks" 10
        (T.Parts 2020 Time.Jan 1 0 15 0 0)
        (T.Parts 2020 Time.Jan 1 2 0 0 0)
        ( Minute, 15 )

    , testBestUnit "minutes crossing hours with many ticks" 22
        (T.Parts 2020 Time.Jan 1 0 15 0 0)
        (T.Parts 2020 Time.Jan 1 2 0 0 0)
        ( Minute, 5 )

    , testBestUnit "minutes diff" 4
        (T.Parts 2020 Time.Jan 1 0 4 0 0)
        (T.Parts 2020 Time.Jan 1 0 49 0 0)
        ( Minute, 10 )

    , testBestUnit "minutes diff with more ticks" 7
        (T.Parts 2020 Time.Jan 1 0 4 0 0)
        (T.Parts 2020 Time.Jan 1 0 49 0 0)
        ( Minute, 10 )

    , testBestUnit "seconds diff" 10
        (T.Parts 2020 Time.Jan 1 0 4 0 0)
        (T.Parts 2020 Time.Jan 1 0 49 0 0)
        ( Minute, 5 )

    , testBestUnit "seconds diff with less ticks" 4
        (T.Parts 2020 Time.Jan 1 0 0 4 0)
        (T.Parts 2020 Time.Jan 1 0 0 49 0)
        ( Second, 10 )

    , testBestUnit "milliseconds diff" 4
        (T.Parts 2020 Time.Jan 1 0 0 0 256)
        (T.Parts 2020 Time.Jan 1 0 0 0 940)
        ( Millisecond, 200 )
    ]


testBestUnit : String -> Int -> T.Parts -> T.Parts -> ( Unit, Int ) -> Test
testBestUnit name amount a b expected =
  let ( unit, mult ) =
        toBestUnit Time.utc amount (toPosix a) (toPosix b)
  in
  describe name
    [ test "best unit" <| \_ ->
        Expect.equal expected ( unit, mult )
    , test "best unit is less than or equal to upper limit" <| \_ ->
        let numOfTicks =
              getNumOfTicks Time.utc unit mult (toPosix a) (toPosix b)
        in
        Expect.atMost amount numOfTicks
    ]



numOfTicksSuite : Test
numOfTicksSuite =
  describe "getNumOfTicks"
    [ testNumOfTicks "when small months diff" Month 1
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2020 Time.Apr 1 0 0 0 0)
        3

    , testNumOfTicks "when crossing months diff" Month 1
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2021 Time.Apr 1 0 0 0 0)
        15

    , testNumOfTicks "when small day diff" Day 1
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2020 Time.Jan 9 0 0 0 0)
        6

    , testNumOfTicks "when other small day diff" Day 1
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2020 Time.Jan 10 0 0 0 0)
        7

    , testNumOfTicks "when small milliseconds diff" Millisecond 1
        (T.Parts 2020 Time.Jan 1 0 0 0 1)
        (T.Parts 2020 Time.Jan 1 0 0 0 3)
        3

    , testNumOfTicks "when no difference" Millisecond 1
        (T.Parts 2020 Time.Jan 1 1 1 1 1)
        (T.Parts 2020 Time.Jan 1 1 1 1 1)
        1
    ]


testNumOfTicks : String -> Unit -> Int -> T.Parts -> T.Parts -> Int -> Test
testNumOfTicks name unit mult a b expected =
  test name <| \_ ->
    let result =
          getNumOfTicks Time.utc unit mult (toPosix a) (toPosix b)
    in
    Expect.equal expected result


diffDateSuite : Test
diffDateSuite =
  describe "difference between two dates"
    [ testDiff "offset months"
        (T.Parts 2020 Time.Jan 4 0 0 0 0)
        (T.Parts 2020 Time.Apr 1 0 0 0 0)
        (Diff 0 2 27 0 0 0 0)

    , testDiff "exactly on year"
        (T.Parts 2020 Time.Jan 1 0 0 0 0)
        (T.Parts 2021 Time.Jan 1 0 0 0 0)
        (Diff 1 0 0 0 0 0 0)

    , testDiff "crossing months"
        (T.Parts 2020 Time.Jan 1 0 0 0 0)
        (T.Parts 2020 Time.Mar 1 0 0 0 0)
        (Diff 0 2 0 0 0 0 0)

    , testDiff "crossing hours"
        (T.Parts 2020 Time.Jan 1 16 0 0 0)
        (T.Parts 2020 Time.Jan 3 14 0 0 0)
        (Diff 0 0 1 22 0 0 0)

    , testDiff "crossing minutes"
        (T.Parts 2020 Time.Jan 1 16 45 0 0)
        (T.Parts 2020 Time.Jan 3 14 30 0 0)
        (Diff 0 0 1 21 45 0 0)

    , testDiff "crossing seconds"
        (T.Parts 2020 Time.Jan 1 16 45 45 0)
        (T.Parts 2020 Time.Jan 3 14 30 30 0)
        (Diff 0 0 1 21 44 45 0)

    , testDiff "crossing years"
        (T.Parts 2020 Time.Feb 1 0 0 0 0)
        (T.Parts 2021 Time.Jan 3 0 0 0 0)
        (Diff 0 11 2 0 0 0 0)

    , testDiff "crossing years a different way"
        (T.Parts 2020 Time.Feb 2 0 0 0 0)
        (T.Parts 2022 Time.Jan 1 0 0 0 0)
        (Diff 1 10 30 0 0 0 0)

    , testDiff "milliseconds"
        (T.Parts 2020 Time.Feb 1 16 45 45 500)
        (T.Parts 2021 Time.Jan 3 14 30 30 200)
        (Diff 0 11 1 21 44 44 700)
    ]



testDiff : String -> T.Parts -> T.Parts -> Diff -> Test
testDiff name a b expected =
  test name <| \_ ->
    let result =
          getDiff Time.utc (toPosix a) (toPosix b)
    in
    Expect.equal expected result



ceilingSuit : Test
ceilingSuit =
  describe "ceilingX"
    [ describe "years"
        [ testCeiling "ceil correctly" Year 100
            (T.Parts 2020 Time.Jan 4 0 0 0 0)
            (T.Parts 2100 Time.Jan 1 0 0 0 0)

        , testCeiling "ceil correctly with 10s" Year 10
            (T.Parts 2020 Time.Jan 4 0 0 0 0)
            (T.Parts 2030 Time.Jan 1 0 0 0 0)

        , testCeiling "ceil correctly with 25s" Year 25
            (T.Parts 2020 Time.Jan 5 0 0 0 0)
            (T.Parts 2025 Time.Jan 1 0 0 0 0)

        , testCeiling "ceil correctly with 200s" Year 200
            (T.Parts 2020 Time.Jan 1 0 0 0 0)
            (T.Parts 2200 Time.Jan 1 0 0 0 0)

        , testCeiling "ceil correctly with 1000s" Year 1000
            (T.Parts 2020 Time.Dec 1 0 0 0 0)
            (T.Parts 3000 Time.Jan 1 0 0 0 0)

        , testCeiling "ceil correctly when already correct" Year 10
            (T.Parts 2020 Time.Jan 1 0 0 0 0)
            (T.Parts 2020 Time.Jan 1 0 0 0 0)
        ]

    , describe "months"
        [ testCeiling "ceil correctly" Month 2
            (T.Parts 2020 Time.Jan 4 0 0 0 0)
            (T.Parts 2020 Time.Mar 1 0 0 0 0)

        , testCeiling "ceil correctly with quarters" Month 3
            (T.Parts 2020 Time.May 1 0 0 0 0)
            (T.Parts 2020 Time.Jul 1 0 0 0 0)

        , testCeiling "ceil correctly with half years" Month 6
            (T.Parts 2020 Time.May 1 0 0 0 0)
            (T.Parts 2020 Time.Jul 1 0 0 0 0)

        , testCeiling "ceil correctly when already correct" Month 6
            (T.Parts 2020 Time.Jan 1 0 0 0 0)
            (T.Parts 2020 Time.Jan 1 0 0 0 0)

        , testCeiling "ceil correctly when overflowing" Month 3
            (T.Parts 2020 Time.Dec 1 0 0 0 0)
            (T.Parts 2021 Time.Jan 1 0 0 0 0)
        ]

    , describe "days"
        [ testCeiling "ceil correctly" Day 2
            (T.Parts 2020 Time.Jan 4 2 0 0 0)
            (T.Parts 2020 Time.Jan 5 0 0 0 0)

        , testCeiling "ceil correctly with weeks" Day 7
            (T.Parts 2020 Time.Dec 31 2 0 0 0)
            (T.Parts 2021 Time.Jan 4 0 0 0 0)

        , testCeiling "ceil correctly when already correct" Day 2
            (T.Parts 2020 Time.Jan 4 0 0 0 0)
            (T.Parts 2020 Time.Jan 4 0 0 0 0)

        , testCeiling "ceil correctly when overflowing" Day 2
            (T.Parts 2020 Time.Dec 31 2 0 0 0)
            (T.Parts 2021 Time.Jan 1 0 0 0 0)
        ]

    , describe "hours"
        [ testCeiling "ceil correctly" Hour 2
            (T.Parts 2020 Time.Jan 4 3 0 0 0)
            (T.Parts 2020 Time.Jan 4 4 0 0 0)

        , testCeiling "ceil correctly with thirds" Hour 3
            (T.Parts 2020 Time.Jan 4 4 0 0 0)
            (T.Parts 2020 Time.Jan 4 6 0 0 0)

        , testCeiling "ceil correctly with larger number" Hour 12
            (T.Parts 2020 Time.Jan 4 3 0 0 0)
            (T.Parts 2020 Time.Jan 4 12 0 0 0)

        , testCeiling "ceil correctly when already correct" Hour 6
            (T.Parts 2020 Time.Jan 4 0 0 0 0)
            (T.Parts 2020 Time.Jan 4 0 0 0 0)

        , testCeiling "ceil correctly when overflowing" Hour 6
            (T.Parts 2020 Time.Dec 31 23 0 0 0)
            (T.Parts 2021 Time.Jan 1 0 0 0 0)
        ]

    , describe "minutes"
        [ testCeiling "ceil correctly" Minute 2
            (T.Parts 2020 Time.Jan 4 3 1 0 0)
            (T.Parts 2020 Time.Jan 4 3 2 0 0)

        , testCeiling "ceil correctly with larger number" Minute 15
            (T.Parts 2020 Time.Jan 4 3 2 0 0)
            (T.Parts 2020 Time.Jan 4 3 15 0 0)

        , testCeiling "ceil correctly when already correct" Minute 15
            (T.Parts 2020 Time.Jan 4 3 0 0 0)
            (T.Parts 2020 Time.Jan 4 3 0 0 0)

        , testCeiling "ceil correctly when overflowing" Minute 15
            (T.Parts 2020 Time.Dec 31 23 59 0 0)
            (T.Parts 2021 Time.Jan 1 0 0 0 0)
        ]

    , describe "seconds"
        [ testCeiling "ceil correctly" Second 2
            (T.Parts 2020 Time.Jan 4 3 0 1 0)
            (T.Parts 2020 Time.Jan 4 3 0 2 0)

        , testCeiling "ceil correctly with larger number" Second 15
            (T.Parts 2020 Time.Jan 4 3 0 2 0)
            (T.Parts 2020 Time.Jan 4 3 0 15 0)

        , testCeiling "ceil correctly when already correct" Second 15
            (T.Parts 2020 Time.Jan 4 3 0 0 0)
            (T.Parts 2020 Time.Jan 4 3 0 0 0)

        , testCeiling "ceil correctly when overflowing" Second 15
            (T.Parts 2020 Time.Dec 31 23 59 58 0)
            (T.Parts 2021 Time.Jan 1 0 0 0 0)
        ]

    , describe "Milliseconds"
        [ testCeiling "ceil correctly" Millisecond 200
            (T.Parts 2020 Time.Jan 4 3 0 0 1)
            (T.Parts 2020 Time.Jan 4 3 0 0 200)

        , testCeiling "ceil correctly when overflowing" Millisecond 200
            (T.Parts 2020 Time.Dec 31 23 59 59 987)
            (T.Parts 2021 Time.Jan 1 0 0 0 0)

        , testCeiling "ceil correctly when already correct" Millisecond 300
            (T.Parts 2020 Time.Jan 4 3 0 0 0)
            (T.Parts 2020 Time.Jan 4 3 0 0 0)
        ]
    ]



testCeiling : String -> Unit -> Int -> T.Parts -> T.Parts -> Test
testCeiling name unit mult parts expected =
  test name <| \_ ->
    let result =
          ceilingUnit Time.utc unit mult (toPosix parts)
            |> fromPosix
    in
    Expect.equal expected result


toPosix : T.Parts -> Time.Posix
toPosix =
  T.partsToPosix Time.utc


fromPosix : Time.Posix -> T.Parts
fromPosix =
  T.posixToParts Time.utc