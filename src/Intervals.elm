module Intervals exposing (Amount, around, exactly, Range, ints, floats, custom, times, Time, Unit(..))


{-| Produce "nice" intervals for e.g. axis labels.

** What are "nice" numbers/integers/datetimes? **

When I say "nice", I just mean that I try to calculate intervals which begin
with 10, 5, 3, 2, 1 (adjusted to magnitude, of course!). For dates, I try to
hit whole days, weeks, months or hours, minutes, and seconds.

# Nice numbers
@docs ints, floats, Amount, around, exactly, Range

# Custom numbers
@docs custom

# Nice times
@docs times, Time, Unit


-}

import Time
import Round
import Intervals.Time as T


{-| -}
type Amount
    = Exactly Int
    | Around Int


{-| Will get you around the amount of numbers you pass it, although it will
prioritize getting "nice" numbers.
-}
around : Int -> Amount
around =
    Around



{-| Will get you _closer_ to the amount of numbers you pass it,
although not actually _exactly_, since you still want decently "nice" numbers.

P.S. If you have a better name for this function, please contact me.

-}
exactly : Int -> Amount
exactly =
    Exactly


{-| The upper and lower bound of your numbers/timestamps.

-}
type alias Range =
    { min : Float
    , max : Float
    }


{-| Produce a list of "nice" integers.

-}
ints : Amount -> Range -> List Int
ints amount range =
    List.map round <|
        case amount of
            Exactly number ->
                values False True number range

            Around number ->
                values False False number range


{-| Produce a list of "nice" floats.

-}
floats : Amount -> Range -> List Float
floats amount =
    case amount of
        Exactly number ->
            values True True number

        Around number ->
            values True False number



{-| Makes evenly spaced floats.

Arguments:
  1. A number which must be in your resulting numbers (commonly 0).
  2. The interval between your numbers.
  3. The range which your numbers must be between.

    Intervals.custom 45 10 (Range 25 100)
    -- ^ Makes [ 25, 35, 45, 55, 65, 75, 85, 95 ]

    Intervals.custom 30 20 (Range 25 100)
    -- ^ Makes [ 30, 50, 70, 90 ]

-}
custom : Float -> Float -> Range -> List Float
custom intersection interval range =
    let
        offset value =
            interval * toFloat (floor (value / interval))

        beginning =
            intersection - offset (intersection - range.min)
    in
    positions range beginning interval 0 []



-- TIMES


{-| A timestamp with extra info helpful for formatting. Explanation:

  - ** timestamp ** is the position where the tick goes on the axis.
  - ** isFirst ** is whether this is the first tick or not.
  - ** interval ** is the interval at which all the ticks are spaced.
  - ** change ** is a `Just` when the tick is changing to a larger unit
    than used in the interval. E.g. if the interval is 2 hours, then
    this will be a `Just Day` when the day changes. Useful if you
    want a different formatting for those ticks!

-}
type alias Time =
  { timestamp : Time.Posix
  , zone : Time.Zone
  , isFirst : Bool
  , unit : Unit
  , multiple : Int
  , change : Maybe Unit
  }


{-| You can format your tick label differently based on it's unit.
-}
type Unit
  = Millisecond
  | Second
  | Minute
  | Hour
  | Day
  | Month
  | Year


{-| Produce a list of "nice" dates.

Arguments:

1.  The timezone which the chart is being shown in.
2.  The maximum amount of times you'd like to have produced.
3.  The beginning timestamp (as a float for chart convenience)
4.  The ending timestamp (as a float for chart convenience)

-}
times : Time.Zone -> Int -> Range -> List Time
times zone amount range =
    let translateUnit time =
           { timestamp = time.timestamp
            , zone = time.zone
            , isFirst = time.isFirst
            , unit = toUnit time.unit
            , multiple = time.multiple
            , change = Maybe.map toUnit time.change
            }

        toUnit unit =
            case unit of
                T.Millisecond -> Millisecond
                T.Second -> Second
                T.Minute -> Minute
                T.Hour -> Hour
                T.Day -> Day
                T.Month -> Month
                T.Year -> Year

        fromMs ts =
            Time.millisToPosix (round ts)
    in
    T.values zone amount (fromMs range.min) (fromMs range.max)
        |> List.map translateUnit



-- INTERNAL / NUMBERS


values : Bool -> Bool -> Int -> Range -> List Float
values allowDecimals exact amountRough range =
    let
        amountRoughSafe =
            if amountRough == 0 then
                1

            else
                amountRough

        intervalRough =
            (range.max - range.min) / toFloat amountRough

        interval =
            getInterval intervalRough allowDecimals exact

        intervalSafe =
            if interval == 0 then
                1

            else
                interval

        beginning =
            getBeginning range.min intervalSafe
    in
    positions range beginning intervalSafe 0 []


getBeginning : Float -> Float -> Float
getBeginning min interval =
    let
        multiple =
            min / interval

        -- TODO figure out precision
    in
    if multiple == toFloat (round multiple) then
        min

    else
        ceilingTo interval min


positions : Range -> Float -> Float -> Float -> List Float -> List Float
positions range beginning interval m acc =
    let
        nextPosition =
            correctFloat (getPrecision interval) (beginning + (m * interval))
    in
    if nextPosition > range.max then
        acc

    else
        positions range beginning interval (m + 1) (acc ++ [ nextPosition ])


getInterval : Float -> Bool -> Bool -> Float
getInterval intervalRaw allowDecimals hasTickAmount =
    let
        magnitude =
            toMagnitude intervalRaw

        normalized =
            intervalRaw / magnitude

        multiples =
            getMultiples magnitude allowDecimals hasTickAmount

        findMultiple multiples_ =
            case multiples_ of
                m1 :: m2 :: rest ->
                    if normalized <= (m1 + m2) / 2 then
                        m1

                    else
                        findMultiple (m2 :: rest)

                m1 :: rest ->
                    if normalized <= m1 then
                        m1

                    else
                        findMultiple rest

                [] ->
                    1

        findMultipleExact multiples_ =
            case multiples_ of
                m1 :: rest ->
                    if m1 * magnitude >= intervalRaw then
                        m1

                    else
                        findMultipleExact rest

                [] ->
                    1

        multiple =
            if hasTickAmount then
                findMultipleExact multiples

            else
                findMultiple multiples

        precision =
            getPrecision magnitude + getPrecision multiple
    in
    correctFloat precision (multiple * magnitude)


getMultiples : Float -> Bool -> Bool -> List Float
getMultiples magnitude allowDecimals hasTickAmount =
    let
        defaults =
            if hasTickAmount then
                [ 1, 1.2, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10 ]

            else
                [ 1, 2, 2.5, 5, 10 ]
    in
    if allowDecimals then
        defaults

    else if magnitude == 1 then
        List.filter (\n -> toFloat (round n) == n) defaults

    else if magnitude <= 0.1 then
        [ 1 / magnitude ]

    else
        defaults


{-| -}
correctFloat : Int -> Float -> Float
correctFloat prec =
    Round.round prec >> String.toFloat >> Maybe.withDefault 0


{-| -}
getPrecision : Float -> Int
getPrecision number =
    case String.split "e" (String.fromFloat number) of
        [ before, after ] ->
            String.toInt after |> Maybe.withDefault 0 |> abs

        _ ->
            case String.split "." (String.fromFloat number) of
                [ before, after ] ->
                    String.length after

                _ ->
                    0


ceilingTo : Float -> Float -> Float
ceilingTo prec number =
    prec * toFloat (ceiling (number / prec))


toMagnitude : Float -> Float
toMagnitude num =
    toFloat <| 10 ^ floor (logBase e num / logBase e 10)

