module TimeTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Intervals as Intervals
import Time.Intervals as Intervals
import Time


suite : Test
suite =
  describe "Time.Intervals"
    [ test "Can make nice time 1" <| \_ ->
        let result =
              Intervals.values Time.utc 10 (Intervals.Range 1361796125 1614260525)
                |> List.map .timestamp
                |> List.map Time.posixToMillis

            expected =
              [1594800000,1566000000,1537200000,1508400000,1479600000,1450800000,1422000000,1393200000]
        in
        Expect.equal expected result
    ]