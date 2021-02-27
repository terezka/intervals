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


values : Time.Zone -> Int -> Time.Posix -> Time.Posix -> List Time.Posix
values zone amount min max =
  let minP = T.posixToParts zone min
      maxP = T.posixToParts zone max

      --ideal = (toMs max - toMs min) // amount
      --idealMult = getMultiple zone min (fromMs (toMs min + ideal))
      --bestOrLarger = findBestOrLarger idealMult
  in
  []



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
  let aP = T.posixToParts zone a
      bP = T.posixToParts zone b

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




findBestOrLarger : ( Unit, Int ) -> ( Unit, Int )
findBestOrLarger ( unit, ideal ) =
  let niceMults = niceMultiples unit
      bestMultiple = List.head (List.filter (\n -> n < ideal) niceMults)
  in
  case bestMultiple of
    Just niceMult -> ( unit, niceMult )
    Nothing -> ( largerUnit unit, 1 )


niceMultiples : Unit -> List Int
niceMultiples unit =
  case unit of
    Millisecond -> [ 1, 2, 5, 10, 20, 25, 50, 100, 200, 500 ]
    Second      -> [ 1, 2, 5, 10, 15, 30 ]
    Minute      -> [ 1, 2, 5, 10, 15, 30 ]
    Hour        -> [ 1, 2, 3, 4, 6, 8, 12 ]
    Day         -> [ 1, 2 ]
    Month       -> [ 1, 2, 3, 4, 6 ]
    Year        -> [ 1, 2, 5, 10, 20, 25, 50, 100, 200, 500, 1000, 10000, 1000000, 10000000 ]


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


progression : List ( Unit, Int )
progression =
  List.concatMap (\(u, ls) -> List.map (Tuple.pair u) ls)
    [ ( Year, [ 10000000, 1000000, 10000, 1000, 500, 100, 200, 50, 25, 20, 10, 5, 2, 1 ] )
    , ( Month, [ 6, 4, 3, 2, 1 ] )
    , ( Day, [ 2, 1 ] )
    , ( Hour, [ 12, 8, 6, 4, 3, 2, 1 ] )
    , ( Minute, [ 30, 15, 10, 5, 2, 1 ] )
    , ( Second, [ 30, 15, 10, 5, 2, 1 ] )
    , ( Millisecond, [ 500, 200, 100, 50, 25, 20, 10, 5, 2, 1 ] )
    ]


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




----


  --   diff unit = Debug.log "diff" <| T.diff unit zone (ceiled unit a) b
  --    ceiled unit = T.ceiling (Debug.log "unit" unit) zone
  --in
  --let years = Debug.log "years" <| diff T.Year in
  -- if years > 0 then ( Year, years ) else
  --let months = diff T.Month in
  -- if months > 0 then ( Month, months ) else
  --let days = diff T.Day in
  -- if days > 0 then ( Day, days ) else
  --let hours = diff T.Hour in
  -- if hours > 0 then ( Hour, hours ) else
  --let minutes = diff T.Minute in
  -- if minutes > 0 then ( Minute, minutes ) else
  --let second = diff T.Second in
  -- if second > 0 then ( Second, second ) else
  --( Millisecond, diff T.Millisecond )

