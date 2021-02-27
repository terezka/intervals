module TestTimes exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Values exposing (..)
import Time
import Time.Extra as T


suite : Test
suite =
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


toPosix : T.Parts -> Time.Posix
toPosix =
  T.partsToPosix Time.utc