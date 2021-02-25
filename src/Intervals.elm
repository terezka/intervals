module Intervals exposing (Amount, around, exactly, Range, ints, floats, custom)

import Round


{-| -}
type Amount
    = Exactly Int
    | Around Int


{-| -}
around : Int -> Amount
around =
    Around


{-| -}
exactly : Int -> Amount
exactly =
    Exactly


{-| -}
type alias Range =
    { min : Float
    , max : Float
    }


{-| -}
ints : Amount -> Range -> List Int
ints amount range =
    List.map round <|
        case amount of
            Exactly number ->
                values False True number range

            Around number ->
                values False False number range


{-| -}
floats : Amount -> Range -> List Float
floats amount =
    case amount of
        Exactly number ->
            values True True number

        Around number ->
            values True False number


{-| -}
custom : Float -> Float -> Range -> List Float
custom intersection interval range =
    let
        offset value =
            interval * toFloat (floor (value / interval))

        beginning =
            intersection - offset (intersection - range.min)
    in
    positions range beginning interval 0 []



-- INTERNAL


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
        next =
            correctFloat (getPrecision interval) (beginning + (m * interval))
    in
    if next > range.max then
        acc

    else
        positions range beginning interval (m + 1) (acc ++ [ next ])


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
