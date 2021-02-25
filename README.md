# intervals

A library for producing nice intervals for charts. Useful in combination with `terezka/charts`.

```bash
$ elm install terezka/intervals
```

```elm

import Intervals


integerTicks : List Int
integerTicks =
  Intervals.ints (Intervals.around 10) (Intervals.Range 0 100)
  -- [ 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ]


floatTicks : List Int
floatTicks =
  Intervals.floats (Intervals.around 5) (Intervals.Range -5 10)
  -- [ -5, -2.5, 0, 2.5, 5, 7.5, 10 ]


```