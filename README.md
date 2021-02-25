# intervals

A library for producing nice intervals for charts.

```bash
$ elm install terezka/intervals
```

```elm

import Intervals


ticks : List Int
ticks =
  Intervals.ints (Intervals.around 10) (Intervals.Range 0 100)
  -- [ 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ]


```