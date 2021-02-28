module Values exposing (..)


import Time
import Time.Extra as T
import TimeExtra as T
import Round


type Unit
    = Millisecond
    | Second
    | Minute
    | Hour
    | Day
    | Month
    | Year


values : Time.Zone -> Int -> Time.Posix -> Time.Posix -> ( Unit, Int )
values zone amount min max =
  let mults = getMultiples zone min max
      toNice unit actual =
        let niceNums = niceMultiples unit
            maybeNiceNum = List.filter (\n -> n <= (actual // amount)) niceNums
        in
        case List.head maybeNiceNum of
          Just niceNum ->
            ( unit, niceNum )

          Nothing ->
            case smallerUnit unit of
              Just smaller -> toNice smaller (unitFromDiff smaller mults)
              Nothing -> ( Millisecond, unitFromDiff Millisecond mults )
  in
  toNice Year mults.year


getMultiples : Time.Zone -> Time.Posix -> Time.Posix -> Diff
getMultiples zone a b =
  let diff unit property =
        let ceiled = T.ceiling unit zone a in
        if toMs ceiled > toMs b then -1
        else property (getDiff zone ceiled b)

      timeDiff unit ms =
        let ceiled = T.ceiling unit zone a in
        if toMs ceiled > toMs b then -1
        else (toMs b - toMs ceiled) // ms
  in
  { year = diff T.Year .year + 1
  , month = diff T.Month (\d -> d.month + d.year * 12) + 1
  , day = timeDiff T.Day oneDay + 1
  , hour = timeDiff T.Hour oneHour + 1
  , minute = timeDiff T.Minute oneMinute + 1
  , second = timeDiff T.Second oneSecond + 1
  , millisecond = (toMs b - toMs a) + 1
  }


niceMultiples : Unit -> List Int
niceMultiples unit =
  case unit of
    Millisecond -> [ 500, 200, 100, 50, 25, 20, 20, 10, 5, 2, 1 ]
    Second      -> [ 30, 15, 10, 5, 2, 1 ]
    Minute      -> [ 30, 15, 10, 5, 2, 1 ]
    Hour        -> [ 12, 8, 6, 4, 3, 2, 1 ]
    Day         -> [ 2, 1 ]
    Month       -> [ 6, 4, 3, 2, 1 ]
    Year        -> [ 10000000, 1000000, 10000, 1000, 500, 200, 100, 50, 25, 20, 10, 5, 2, 1 ]


niceMultiplesBack : Unit -> List Int
niceMultiplesBack unit =
  case unit of
    Millisecond -> [ 1, 2, 5, 10, 20, 25, 50, 100, 200, 500 ]
    Second      -> [ 1, 2, 5, 10, 15, 30 ]
    Minute      -> [ 1, 2, 5, 10, 15, 30 ]
    Hour        -> [ 1, 2, 3, 4, 6, 8, 12 ]
    Day         -> [ 1, 2 ]
    Month       -> [ 1, 2, 3, 4, 6 ]
    Year        -> [ 1, 2, 5, 10, 20, 25, 50, 100, 200, 500, 1000, 10000, 1000000, 10000000 ]


-- HELPERS


type alias Diff =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    }


getDiff : Time.Zone -> Time.Posix -> Time.Posix -> Diff
getDiff zone a b =
  let ( aP, bP ) =
        if toMs a < toMs b then
          ( T.posixToParts zone a
          , T.posixToParts zone b
          )
        else
          ( T.posixToParts zone b
          , T.posixToParts zone a
          )

      dMsX = bP.millisecond - aP.millisecond
      dMs = if dMsX < 0 then 1000 + dMsX else dMsX

      dSecondX = bP.second - aP.second + (if dMsX < 0 then -1 else 0)
      dSecond = if dSecondX < 0 then 60 + dSecondX else dSecondX

      dMinuteX = bP.minute - aP.minute + (if dSecondX < 0 then -1 else 0)
      dMinute = if dMinuteX < 0 then 60 + dMinuteX else dMinuteX

      dHourX = bP.hour - aP.hour + (if dMinuteX < 0 then -1 else 0)
      dHour = if dHourX < 0 then 24 + dHourX else dHourX

      dDayX = bP.day - aP.day + (if dHourX < 0 then -1 else 0)
      dDay = if dDayX < 0 then T.daysInMonth bP.year bP.month + dDayX else dDayX

      dMonthX = monthAsInt bP.month - monthAsInt aP.month + (if dDayX < 0 then -1 else 0)
      dMonth = if dMonthX < 0 then 12 + dMonthX else dMonthX

      dYearX = bP.year - aP.year + (if dMonthX < 0 then -1 else 0)
      dYear = if dYearX < 0 then monthAsInt bP.month + dYearX else dYearX
  in
  { year = dYear
  , month = dMonth
  , day = dDay
  , hour = dHour
  , minute = dMinute
  , second = dSecond
  , millisecond = dMs
  }


unitFromDiff : Unit -> Diff -> Int
unitFromDiff unit diff =
  case unit of
    Millisecond -> diff.millisecond
    Second -> diff.second
    Minute -> diff.minute
    Hour -> diff.hour
    Day -> diff.day
    Month -> diff.month
    Year -> diff.year


largerUnit : Unit -> Unit
largerUnit unit =
  case unit of
    Millisecond -> Second
    Second -> Minute
    Minute -> Hour
    Hour -> Day
    Day -> Month
    Month -> Year
    Year -> Year


smallerUnit : Unit -> Maybe Unit
smallerUnit unit =
  case unit of
    Millisecond -> Nothing
    Second -> Just Millisecond
    Minute -> Just Second
    Hour -> Just Minute
    Day -> Just Hour
    Month -> Just Day
    Year -> Just Month


toExtraUnit : Unit -> T.Interval
toExtraUnit unit =
  case unit of
    Millisecond -> T.Millisecond
    Second -> T.Second
    Minute -> T.Minute
    Hour -> T.Hour
    Day -> T.Day
    Month -> T.Month
    Year -> T.Year


monthAsInt : Time.Month -> Int
monthAsInt month =
  case month of
    Time.Jan -> 1
    Time.Feb -> 2
    Time.Mar -> 3
    Time.Apr -> 4
    Time.May -> 5
    Time.Jun -> 6
    Time.Jul -> 7
    Time.Aug -> 8
    Time.Sep -> 9
    Time.Oct -> 10
    Time.Nov -> 11
    Time.Dec -> 12


toMs : Time.Posix -> Int
toMs =
  Time.posixToMillis


fromMs : Int -> Time.Posix
fromMs =
  Time.millisToPosix


oneSecond : Int
oneSecond =
  1000


oneMinute : Int
oneMinute =
  oneSecond * 60


oneHour : Int
oneHour =
  oneMinute * 60


oneDay : Int
oneDay =
  oneHour * 24
