module Intervals exposing (Amount, around, exactly, Range, ints, floats, custom, times)


{-| Produce "nice" numbers.

** What are "nice" numbers/integers/datetimes? **
When I say "nice", I just mean that I try to calculate intervals which begin
with 10, 5, 3, 2, 1 (adjusted to magnitude, of course!). For dates, I try to
hit whole days, weeks, months or hours, minutes, and seconds.

# Nice numbers
@docs int, float, Amount, around, exactly

# Custom numbers
@docs custom

# Nice times
@docs times


-}

import Time
import Time.Extra
import Round


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


{-| -}
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
    , interval : Interval
    , change : Maybe Unit
    }


{-| The interval at which ticks are spaced. If ticks a spaced with two hours,
this will be `{ unit = Hour, multiple = 2 }`.

-}
type alias Interval =
    { unit : Unit
    , multiple : Int
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
2.  The rough amount of times you'd like to have produced.
3.  The upper and lower bound of dates in the list.

-}
times : Time.Zone -> Int -> Range -> List Time
times =
  timeValues



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



-- INTERNAL / TIMES


timeValues : Time.Zone -> Int -> Range -> List Time
timeValues zone amountRough range =
    let
        intervalRough =
            (range.max - range.min) / toFloat amountRough

        unit =
            findBestUnit intervalRough all

        multiple =
            findBestMultiple intervalRough unit

        interval =
            toMs unit * toFloat multiple

        beginning =
            beginAt zone (floatToPosix range.min) unit multiple

        toPositions acc i =
            let
                next_ =
                    next zone beginning unit (i * multiple)
            in
            if posixsToFloat next_ > range.max then
                acc

            else
                toPositions (acc ++ [ next_ ]) (i + 1)

        toTimes values_ unitChange acc =
            case values_ of
                value :: next_ :: rest ->
                    let
                        isFirst =
                            List.isEmpty acc

                        newAcc =
                            toTime unitChange value isFirst :: acc

                        newUnitChange =
                            getUnitChange unit zone value next_
                    in
                    toTimes (next_ :: rest) newUnitChange newAcc

                [ value ] ->
                    toTime unitChange value (List.isEmpty acc) :: acc

                [] ->
                    acc

        toTime unitChange value isFirst =
            { change = unitChange
            , interval = Interval unit multiple
            , timestamp = value
            , isFirst = isFirst
            , zone = zone
            }
    in
    toTimes (toPositions [] 0) Nothing []



-- INTERNAL


{-| Find the best fitted unit for a given interval and unit options.
-}
findBestUnit : Float -> List Unit -> Unit
findBestUnit interval units_ =
    let
        findBest_ units__ u0 =
            case units__ of
                u1 :: u2 :: rest ->
                    if interval <= middleOfNext u1 u2 then
                        u1

                    else
                        findBest_ (u2 :: rest) u1

                u :: _ ->
                    u

                [] ->
                    Year

        middleOfNext u1 u2 =
            (toMs u1 * highestMultiple (timeMultiples u1) + toMs u2) / 2
    in
    findBest_ units_ Year


{-| Finds the best fit multiple given the interval and it's best fit unit.
-}
findBestMultiple : Float -> Unit -> Int
findBestMultiple interval unit =
    let
        findBest_ multiples_ =
            case multiples_ of
                m1 :: m2 :: rest ->
                    if interval <= middleOfNext m1 m2 then
                        m1

                    else
                        findBest_ (m2 :: rest)

                m :: _ ->
                    m

                [] ->
                    1

        middleOfNext m1 m2 =
            (toFloat m1 * toMs unit + toFloat m2 * toMs unit) / 2
    in
    findBest_ (timeMultiples unit)


{-| Find the best position for the first tick.
-}
beginAt : Time.Zone -> Time.Posix -> Unit -> Int -> Time.Posix
beginAt zone min unit multiple =
    min
        |> Time.Extra.add (toExtraUnit unit) multiple zone
        |> Time.Extra.ceiling (toExtraUnit unit) zone


next : Time.Zone -> Time.Posix -> Unit -> Int -> Time.Posix
next zone timestamp unit multiple =
    Time.Extra.add (toExtraUnit unit) multiple zone timestamp


getUnitChange : Unit -> Time.Zone -> Time.Posix -> Time.Posix -> Maybe Unit
getUnitChange interval zone value next_ =
    let
        equalBy unit =
            Time.Extra.diff (toExtraUnit unit)
                zone
                (Time.Extra.floor (toExtraUnit unit) zone value)
                (Time.Extra.floor (toExtraUnit unit) zone next_)
                == 0

        unitChange_ units =
            case units of
                unit :: rest ->
                    if toMs unit <= toMs interval then
                        unitChange_ rest

                    else if not (equalBy unit) then
                        Just unit

                    else
                        Nothing

                [] ->
                    Nothing
    in
    unitChange_ all



-- HELPERS


all : List Unit
all =
    [ Millisecond, Second, Minute, Hour, Day, Month, Year ]


allReversed : List Unit
allReversed =
    List.reverse all


toMs : Unit -> Float
toMs unit =
    case unit of
        Millisecond ->
            1

        Second ->
            1000

        Minute ->
            60000

        Hour ->
            3600000

        Day ->
            24 * 3600000

        Month ->
            28 * 24 * 3600000

        Year ->
            364 * 24 * 3600000


timeMultiples : Unit -> List Int
timeMultiples unit =
    case unit of
        Millisecond ->
            [ 1, 2, 5, 10, 20, 25, 50, 100, 200, 500 ]

        Second ->
            [ 1, 2, 5, 10, 15, 30 ]

        Minute ->
            [ 1, 2, 5, 10, 15, 30 ]

        Hour ->
            [ 1, 2, 3, 4, 6, 8, 12 ]

        Day ->
            [ 1, 2 ]

        Month ->
            [ 1, 2, 3, 4, 6 ]

        Year ->
            [ 1, 2, 5, 10, 20, 25, 50, 100, 200, 500, 1000, 10000 ]


toExtraUnit : Unit -> Time.Extra.Interval
toExtraUnit unit =
    case unit of
        Millisecond ->
            Time.Extra.Millisecond

        Second ->
            Time.Extra.Second

        Minute ->
            Time.Extra.Minute

        Hour ->
            Time.Extra.Hour

        Day ->
            Time.Extra.Day

        Month ->
            Time.Extra.Month

        Year ->
            Time.Extra.Year


highestMultiple : List Int -> Float
highestMultiple =
    List.reverse >> List.head >> Maybe.withDefault 0 >> toFloat


timeMagnitude : Float -> Unit -> Float
timeMagnitude interval unit =
    case unit of
        Year ->
            max 1 (toMagnitude interval)

        _ ->
            1


floatToPosix : Float -> Time.Posix
floatToPosix ms =
    Time.millisToPosix (Basics.round ms)


posixsToFloat : Time.Posix -> Float
posixsToFloat posix =
    Basics.toFloat (Time.posixToMillis posix)
