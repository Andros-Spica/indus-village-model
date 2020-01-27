;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Household Storage model
;;  Copyright (C) 2020 Andreas Angourakis (andros.spica@gmail.com)
;;  available at https://www.github.com/Andros-Spica/indus-village-model
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;
;;;;; BREEDS ;;;;
;;;;;;;;;;;;;;;;;

breed [ households household ]

;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals
[
  ;;; modified parameters

  ;;;; initialisation
  initialNumHouseholds

  ;;;; parameters for simulating inputs
  ;;;; Their values should eventually come from other submodels.
  desiredDietMin desiredDietMax             ; Minimum and maximum desired diet of households (stock units). Desired diet is randomised in each household at initialisation.
  minMaxSurplusToSell maxMaxSurplusToSell   ;

  productionMin productionMax               ; Minimum and maximum production per household/time-step (stock units). Production is randomised in each household every time-step.

  ;;;; parameters of the decay curve
  maxStockAge                               ; maximum number of time steps that a stock takes to fully decay (time-steps).
  decayShape                                ; the exponential used to describe the shape of the decay curve.

  ;;; variables

  market_stocks                             ; Stocks available for households to acquire distinguised by stock age (list of floats; stock units).

  ;;;; auxiliar

  ;;;; counters and final measures
  sumOfStocks                               ; Sum of all households stocks (stock units).
  sumOfStocksPerAge                         ; Sum of all households stocks separated by stock age (list of floats; stock units).

  ;;; INFLOWS
  sumOfCurrentProduction                    ; Sum of all households recently produced stocks (stock units).

  ;;; OUTFLOWS
  sumOfLosses                               ; Sum of all households stocks losses due to decay (stock units).

  ;;; Market INFLOWS & OUTFLOWS
  market_purchases                          ; Total stock units acquired by the market from households with surplus
  market_sells                              ; Total stock units sold by the market to households with unfullfiled diets
  market_sumOfLosses                        ; Sum of market stocks losses due to decay (stock units).

]

;;; agents variables

households-own
[
  hh_dietDesired                   ; The amount of stock units desired by the household in each time-step (stock units).
  hh_dietUnfulfilled              ; The amount of stock units desired by the household, but currently unsatisfied (stock units).

  hh_maxSurplusToSell              ; Maximum proportion of surplus that the household is willing to sell to the market (% of surplus).

  hh_stocks                        ; Household stocks distinguised by stock age (list of floats; stock units).
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  set-parameters

  setup-households

  setup-market

  reset-counters

  update-counters

  reset-ticks

end

to set-parameters

  ; set random seed
  random-seed SEED

  ; check parameters values
  parameters-check1

  ;;; setup parameters depending on the type of experiment

  if (type-of-experiment = "user-defined")
  [
    ;;; load parameters from user interface
    set initialNumHouseholds initial-num-households

    set desiredDietMax desired-diet-max
    set desiredDietMin desired-diet-min
    set minMaxSurplusToSell min-max-surplus-to-sell
    set maxMaxSurplusToSell max-max-surplus-to-sell

    set productionMax production-max
    set productionMin production-min

    set maxStockAge max-stock-age
    set decayShape decay-shape
  ]
  if (type-of-experiment = "random")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    set initialNumHouseholds 1 + random initial-num-households ; at least one household

    set desiredDietMin random desired-diet-min
    set desiredDietMax desiredDietMin + random desired-diet-max
    set minMaxSurplusToSell random-float min-max-surplus-to-sell
    set maxMaxSurplusToSell minMaxSurplusToSell + random-float max-max-surplus-to-sell

    set productionMin random production-min
    set productionMax productionMin + random production-max

    set maxStockAge 1 + random maxStockAge
    set decayShape 1 + random decay-shape
  ]

  ; check parameters values
  parameters-check2

end

to parameters-check1

  ;;; check if values were reset to 0
  ;;; and set default values
  if (initial-num-households = 0)               [ set initial-num-households               25 ]

  if (desired-diet-max = 0)                     [ set desired-diet-max                     80 ]
  if (desired-diet-min = 0)                     [ set desired-diet-min                     30 ]

  ;if (max-max-surplus-to-sell = 0)              [ set max-max-surplus-to-sell              80 ]
  ;if (min-max-surplus-to-sell = 0)              [ set min-max-surplus-to-sell              20 ]

  if (production-max = 0)                       [ set production-max                       80 ]
  if (production-min = 0)                       [ set production-min                       30 ]

  if (max-stock-age = 0)                        [ set max-stock-age                         3 ]
  if (decay-shape = 0)                          [ set decay-shape                           3 ]

end

to parameters-to-default

  ;;; set parameters to a default value
  set max-iterations                     2000

  set initial-num-households               25

  set desired-diet-max                     80
  set desired-diet-min                     30

  set max-max-surplus-to-sell              80
  set min-max-surplus-to-sell              20

  set production-max                       80
  set production-min                       30

  set max-stock-age                         3
  set decay-shape                           3

end

to parameters-check2

  ;;; initial parameter check (e.g., avoiding division per zero error)
  check-par-is-positive "initialNumHouseholds" initialNumHouseholds

  check-par-is-positive "maxStockAge" maxStockAge

end

to check-par-is-positive [ parName parValue ]

  if (parValue <= 0)
  [
    print (word "ERROR: " parName " must be greater than zero")
    stop
  ]

end

to check-par-is-range [ parName parListMinMax ]

  if ( item 0 parListMinMax > item 1 parListMinMax)
  [
    print (word "ERROR: " parName " minimum (first item) must be less than or equal to maximum (second item)")
    stop
  ]

end

to setup-market

  ;;; Initialise initial market stocks
  ;;; It creates retrospectively one stock per year of production,
  ;;; according to the baseline decay of the specific foodstuff and number of households

  set market_stocks []

  ;;; iterate per stock age (from CURRENT to OLDEST),
  ;;; adding the estimated production of that year and discounting decay.
  ;;; current-stock converge into 0 once number of iterations reach maxStockAge

  let index 0

  repeat maxStockAge
  [
    ;;; get new value according to decay proportional to years-old
    ;;; and add it as the stock of this year
    set market_stocks lput (sum [hh_get-init-stock index] of households) market_stocks

    set index index + 1
  ]

end

to setup-households

  ;;; create households (position are not relevant)
  repeat initialNumHouseholds
  [
    create-households 1
    [
      setxy random-xcor random-ycor
      hh_initialise
    ]
  ]

end

;;; HOUSEHOLDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to hh_initialise

  set hh_dietDesired hh_get-dietDesired

  set hh_maxSurplusToSell hh_get-maxSurplusToSell

  hh_initialise-stocks

end

to hh_initialise-stocks

  ;;; Initialise initial stocks
  ;;; It creates one stock per year of production,
  ;;; according to the baseline decay of the specific foodstuff

  set hh_stocks []

  ;;; iterate per stock age (from CURRENT to OLDEST),
  ;;; adding the estimated production of that year and discounting decay.
  ;;; current-stock converge into 0 once number of iterations reach maxStockAge

  let index 0

  repeat maxStockAge
  [
    ;;; get new value according to decay proportional to years-old
    ;;; and add it as the stock of this year
    set hh_stocks lput (hh_get-init-stock index) hh_stocks

    set index index + 1
  ]

end

to-report hh_get-dietDesired

  report get-random-in-range desiredDietMin desiredDietMax

end

to-report hh_get-maxSurplusToSell

  report get-random-in-range minMaxSurplusToSell maxMaxSurplusToSell

end

to-report hh_get-init-stock [ stockAge ]

  ;;; Get an initial quantity of production per year as stochastic variation,
  ;;; discount the decay proportional to stockAge,
  ;;; and reduce it to a fraction (minimise excitability of model at initialisation).

  report hh_get-stocks-production * (get-stock-preservation-rate stockAge) * 0.1

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  reset-counters

  market-stocks-decay

  update-households

  update-counters

  if (ticks = max-iterations)
  [
    ;if (length behaviorspace-experiment-name > 0 and count households > 0) [ export-households ]
    stop
  ]
  ;print "-------tick----"
  tick

end

;;; GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to market-stocks-decay

  ;;; iterate per stock age (from OLDEST to CURRENT),
  ;;; discounting decay and moving the stock to an older index
  ;;; The oldest (index = maxStorageAge) is lost

  let index (maxStockAge - 1)

  repeat (maxStockAge - 1) ; "- 1" garantees that index 0 (the current produce) is ignored, i.e. it is still to be filled by this time step produce
  [
    ;;; get new value according to decay proportional to years-old
    let newValue (item (index - 1) market_stocks) * (get-stock-preservation-rate index)

    ;;; add value to the record of market losses
    set market_sumOfLosses market_sumOfLosses + ((item (index - 1) market_stocks) - newValue)

    ;;; and add it as the stock of this year
    set market_stocks replace-item index market_stocks newValue

    set index index - 1
  ]

  ;;; reset most recent stock to 0 (it may be later filled by households selling their surplus)
  set market_stocks replace-item 0 market_stocks 0

end

to update-households

  reset-diet-satisfaction

  stocks-update

end

to reset-diet-satisfaction

  ask households
  [
    ;;; set the diet unfullfilled as the desired diet
    set hh_dietUnfulfilled hh_dietDesired
  ]

end

to stocks-update

  ;;; DECAY & PRODUCE
  ;;; both are done together because they are intrinsic processes
  ;;; (not influenced by other households)
  ask households
  [
    ;;; older stocks suffers lossess
    hh_stocks-decay

    ;;; recently produced stock is added to hh_stocks
    hh_stocks-produce
  ]

  ;;; CONSUME, first attempt (before exchange)
  ask households
  [
    hh_stocks-consume
  ]

  ;;; SELL
  ;;; households with surplus contribute a fraction of their surplus
  ;;; to a common exchange pool ('market')
  ask households with [hh_dietUnfulfilled = 0]
  [
    hh_stocks-sell
  ]

  ;;; BUY
  ;;; households with unfullfilled diets may complement their stocks progressivelly.
  ;;; by importing/acquiring/buying additional foodstuff at the common 'market'.
  ;;; The iteration advances one unit of stock at a time
  ;;; and stops when the market supply or demand is exhausted.

  ;;; calculate the number of possible transactions as the minimum between supply (market-stocks) and demand (sum [hh_dietUnfulfilled] of households)
  let numberOftransactions min (list (sum market_stocks) (sum [hh_dietUnfulfilled] of households) ) ; (sum [min (list hh_dietUnfulfilled hh_exchangePower)] of households) )

  repeat numberOftransactions
  [
    ask one-of households with [hh_dietUnfulfilled > 0]
    [
      ;;; search & acquire
      hh_stocks-buy

      ;;; and then consume it
      hh_stocks-consume
    ]
  ]

end

;;; HOUSEHOLDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report hh_get-diet-available

  report sum hh_stocks

end

to hh_stocks-produce

  ; place the last production as the younger stock
  set hh_stocks replace-item 0 hh_stocks hh_get-stocks-production

end

to-report hh_get-stocks-production

  ;;; Get production of foodstuff

  report get-random-in-range productionMin productionMax

  ;;; Once integrated with crop model and other food production inputs,
  ;;; this procedure will return values as a funtion of land conditions and labour invested

end

to hh_stocks-decay

  ;;; iterate per stock age (from OLDEST to CURRENT),
  ;;; discounting decay and moving the stock to an older index
  ;;; The oldest (index = maxStorageAge) is lost

  let index (maxStockAge - 1)

  repeat (maxStockAge - 1) ; "- 1" garantees that index 0 (the current produce) is ignored, i.e. suffers no decay
  [
    ;;; get new value according to decay proportional to years-old
    let newValue (item (index - 1) hh_stocks) * (get-stock-preservation-rate index)

    ;;; add value to the population-wide record of losses
    set sumOfLosses sumOfLosses + ((item (index - 1) hh_stocks) - newValue)

    ;;; and add it as the stock of this year
    set hh_stocks replace-item index hh_stocks newValue

    set index index - 1
  ]

end

to hh_stocks-consume

  ;;; iterate per stock age (from OLDEST to CURRENT),
  ;;; consuming stock and trying to fulfilled the desired diet

  let index (maxStockAge - 1)

  repeat maxStockAge
  [
    ifelse (item index hh_stocks >= hh_dietUnfulfilled)
    [
      ; this stock alone provides enough food
      ; subtract from the used stock
      let newStockValue (item index hh_stocks) - hh_dietUnfulfilled

      set hh_stocks replace-item index hh_stocks newStockValue

      ; the desired diet is fullfilled
      set hh_dietUnfulfilled 0
    ]
    [
      ; this stock alone is not sufficient for covering the diet desired
      set hh_dietUnfulfilled hh_dietUnfulfilled - (item index hh_stocks)

      ; use out this stock
      set hh_stocks replace-item index hh_stocks 0
    ]

    set index index - 1
  ]

end

to hh_stocks-sell

  ;;; The household will sell a fraction of surplus to the market.
  ;;; Households will only sell surplus from the most recent stock.
  ;;; The corresponding "demand" is assumed to exist in the market

  ;;; The fraction to sell depends on the household disposition (hh_maxSurplusToSell)
  let transactionStocks (item 0 hh_stocks) * (hh_maxSurplusToSell / 100)

  ;;; subtract transactionStocks from the most recent of household stocks
  set hh_stocks replace-item 0 hh_stocks ((item 0 hh_stocks) - transactionStocks)

  ;;; add transactionStocks to the most recent of market stocks
  set market_stocks replace-item 0 market_stocks ((item 0 market_stocks) + transactionStocks)

  ;;; add transactionStocks to the household exchange power
  ;set hh_exchangePower hh_exchangePower + transactionStocks

  ;;; add transactionStocks to the record of market purchases
  set market_purchases market_purchases + transactionStocks

end

to hh_stocks-buy

  ;;; The household will acquire one unit of stock from the market

  let stocksToBuy 1

  ;;; search the most recent produce available (assuming freshness is preferred)
  ;;; and buy until the unit is fullfilled or there is no more stocks in the market
  let index 0

  repeat maxStockAge
  [
    if (stocksToBuy > 0 and (item index market_stocks) > 0)
    [
      ;;; Stock units to be acquired are 1 or less (in case the market is almost depleted)
      let transactionStocks min (list stocksToBuy (item index market_stocks))

      ;;; add transactionStocks to the most recent of household stocks
      set hh_stocks replace-item index hh_stocks ((item index hh_stocks) + transactionStocks)

      ;;; subtract transactionStocks from the most recent of market stocks
      set market_stocks replace-item index market_stocks ((item index market_stocks) - transactionStocks)

      ;;; subtract transactionStocks from the household exchange power
      ;set hh_exchangePower hh_exchangePower - transactionStocks

      ;;; add transactionStocks to the record of market sells
      set market_sells market_sells + transactionStocks

      ;;; discount bought stock from stocksToBuy (in most cases a single purchase is enough)
      set stocksToBuy stocksToBuy - transactionStocks

      set index index + 1
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERIC FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-random-in-range [ minValue maxValue ]

  report (minValue + random-float (maxValue - minValue))

end

to-report get-stock-preservation-rate [ stockAge ]

  ;;; This is a generic function representing a crescent decay proportional to yearsOld
  ;;; This function should be ideall calibrated to each type of foodstuff
  report (1 - (stockAge / maxStockAge) ^ decayShape)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTERS AND MEASURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reset-counters

  set sumOfStocksPerAge n-values maxStockAge [i -> 0]

  set sumOfStocks 0

  set sumOfCurrentProduction 0

  set sumOfLosses 0

  set market_purchases 0
  set market_sells 0
  set market_sumOfLosses 0

end

to update-counters

  set sumOfStocksPerAge n-values maxStockAge [i -> sum [item i hh_stocks] of households]

  set sumOfStocks sum sumOfStocksPerAge

  set sumOfCurrentProduction sum [item 0 hh_stocks] of households

  ;;; sumOfLosses is updated every iteration of hh_stocks-decay.
  ;;; market_purchases, market_sells, and market_sumOfLosses are updated
  ;;; every iteration of hh_stocks-buy, hh_stocks-sell, and market-stocks-decay, respectively.

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to plot-table [ values ]

  let j 0
  foreach values
  [
    i ->
    plotxy j i
    set j j + 1
  ]
  plot-pen-up

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPORT DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to export-households

  let FilePath "output//"

  file-open (word FilePath behaviorspace-experiment-name behaviorspace-run-number "_households.csv")

  file-print "household,hh_dietDesired,hh_stocks"

  foreach sort households
  [
    hh ->
    ask hh
    [
      file-type who file-type ", "
      file-type (word "'" hh_dietDesired "'") file-type ", "
      file-type (word "'" hh_stocks "'") file-type ", "
    ]
  ]

  file-close

end
@#$#@#$#@
GRAPHICS-WINDOW
248
10
560
323
-1
-1
16.0
1
10
1
1
1
0
1
1
1
-9
9
-9
9
0
0
1
ticks
30.0

BUTTON
18
20
81
53
NIL
setup
NIL
1
T
OBSERVER
NIL
1
NIL
NIL
1

BUTTON
96
21
151
54
NIL
go
NIL
1
T
OBSERVER
NIL
2
NIL
NIL
1

INPUTBOX
41
78
115
138
SEED
0.0
1
0
Number

CHOOSER
49
150
187
195
type-of-experiment
type-of-experiment
"user-defined" "random"
0

INPUTBOX
27
365
149
425
initial-num-households
25.0
1
0
Number

MONITOR
39
425
140
462
NIL
initialNumHouseholds
0
1
9

BUTTON
162
21
217
54
NIL
go
T
1
T
OBSERVER
NIL
3
NIL
NIL
1

INPUTBOX
116
78
196
138
max-iterations
2000.0
1
0
Number

BUTTON
50
218
185
251
NIL
export-households
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
41
288
193
321
parameters to default
parameters-to-default
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
239
501
326
561
max-stock-age
3.0
1
0
Number

MONITOR
243
560
322
597
NIL
maxStockAge
0
1
9

SLIDER
164
375
365
408
desired-diet-min
desired-diet-min
0
desired-diet-max
30.0
1
1
NIL
HORIZONTAL

SLIDER
365
375
557
408
desired-diet-max
desired-diet-max
desired-diet-min + 1
200
80.0
1
1
NIL
HORIZONTAL

SLIDER
365
408
557
441
production-max
production-max
production-min
200
80.0
1
1
NIL
HORIZONTAL

SLIDER
163
408
365
441
production-min
production-min
0
production-max
30.0
1
1
NIL
HORIZONTAL

PLOT
576
422
992
559
Sum of stocks per age
stock age
stock units
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -0.1 (maxStockAge + 1)" "clear-plot\nset-plot-x-range -0.1 (maxStockAge + 1)"
PENS
"default" 1.0 1 -16777216 true "" "plot-table sumOfStocksPerAge"

MONITOR
998
12
1073
49
NIL
sumOfStocks
2
1
9

PLOT
577
10
992
142
Households stocks INFLOWS & OUTFLOWS
time steps
stock units
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"production" 1.0 0 -13840069 true "" "plot sumOfCurrentProduction"
"losses" 1.0 0 -2674135 true "" "plot sumOfLosses"
"total" 1.0 0 -16777216 true "" "plot sumOfStocks"

SLIDER
342
513
514
546
decay-shape
decay-shape
0
5
3.0
0.01
1
NIL
HORIZONTAL

MONITOR
349
556
421
593
NIL
decayShape
2
1
9

TEXTBOX
294
477
500
511
Decay curve parameters
14
0.0
1

TEXTBOX
261
347
471
381
PRODUCTION & CONSUMPTION
14
0.0
1

PLOT
577
142
992
272
Market stocks INFLOWS & OUTFLOWS
time steps
stock units
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"total" 1.0 0 -16777216 true "" "plot sum market_stocks"
"losses" 1.0 0 -2674135 true "" "plot market_sumOfLosses"
"purchases" 1.0 0 -13840069 true "" "plot market_purchases"
"sells" 1.0 0 -8630108 true "" "plot market_sells"

SLIDER
164
441
365
474
min-max-surplus-to-sell
min-max-surplus-to-sell
0
max-max-surplus-to-sell
20.0
0.01
1
%
HORIZONTAL

SLIDER
364
441
557
474
max-max-surplus-to-sell
max-max-surplus-to-sell
min-max-surplus-to-sell
100
80.0
0.01
1
%
HORIZONTAL

MONITOR
995
148
1085
185
NIL
sum market_stocks
2
1
9

MONITOR
995
191
1097
228
NIL
market_sumOfLosses
2
1
9

MONITOR
995
228
1080
265
NIL
market_purchases
2
1
9

MONITOR
1082
228
1148
265
NIL
market_sells
2
1
9

MONITOR
998
49
1073
86
NIL
sumOfLosses
2
1
9

MONITOR
997
86
1115
123
NIL
sumOfCurrentProduction
2
1
9

PLOT
577
272
992
422
Household diet
time steps
stock units
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"desired" 1.0 0 -16777216 true "" "plot sum [hh_dietDesired] of households"
"unfulfilled" 1.0 0 -5298144 true "" "plot sum [hh_dietUnfulfilled] of households"

@#$#@#$#@
## Development notes

TASK:
Add parametric tabu restrictions to the formation of couples. Now, it is possible to have any given single individual marrying any other single, even siblings and, less likely, mother-son/father-daughter... 
DONE

IMPROVEMENTS:
- apply-nuptiality: now this procedure follows a more elegant and clear process

ISSUES:
- Households now track their lineage and match making is controlled by the similarity of lineage. However, this mechanisms reflects a purely lineal track of descent, meaning that relatively close blood relatives are not tracked if they do not share the lineage; e.g., a man under a matrilocal-matrilineal rule will loose track of all 'former' blood relative once they move away from their original household. Unfortunately, this solution still do not control for extreme rare cases such as marriages between siblings and other close relatives, if considering widowing. The only definitive solution is too increase complexity of the information at individual level.
- Initialisation: the result given by the procedure 'get-kinship-degree' assumes all initial households descend from a single common parent household (returns 2). So, if the acceptable kinship level is more than 2, the formation of couples during the first generation will be restricted to those using external individuals ('create-couple-external').

## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="exp-trajectories" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>SEED</metric>
    <metric>maturityAge</metric>
    <metric>initialNumHouseholds</metric>
    <metric>householdInitialAgeDistribution</metric>
    <metric>maxCoupleCountDistribution</metric>
    <metric>cdmlt-level</metric>
    <metric>coale-demeny-region</metric>
    <metric>c1-fert</metric>
    <metric>mu-fert</metric>
    <metric>sigma1-fert</metric>
    <metric>sigma2-fert</metric>
    <metric>residence-rule</metric>
    <metric>acceptable-kinship-degree-for-couples</metric>
    <metric>c1-women</metric>
    <metric>c1-men</metric>
    <metric>mu-women</metric>
    <metric>mu-men</metric>
    <metric>sigma1-women</metric>
    <metric>sigma1-men</metric>
    <metric>sigma2-women</metric>
    <metric>sigma2-men</metric>
    <metric>totalHouseholds</metric>
    <metric>totalIndividuals</metric>
    <metric>totalPopulationGrowth</metric>
    <metric>totalWomen</metric>
    <metric>totalMen</metric>
    <metric>femaleRatio</metric>
    <metric>womenAgeStructure</metric>
    <metric>menAgeStructure</metric>
    <metric>womenFirstAgeGroup</metric>
    <metric>menFirstAgeGroup</metric>
    <metric>womenBirths</metric>
    <metric>menBirths</metric>
    <metric>womenDeaths</metric>
    <metric>menDeaths</metric>
    <metric>womenIn</metric>
    <metric>menIn</metric>
    <metric>womenOut</metric>
    <metric>menOut</metric>
    <metric>totalOrphans</metric>
    <steppedValueSet variable="SEED" first="0" step="1" last="99"/>
  </experiment>
  <experiment name="exp-endstates" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>SEED</metric>
    <metric>maturityAge</metric>
    <metric>initialNumHouseholds</metric>
    <metric>householdInitialAgeDistribution</metric>
    <metric>maxCoupleCountDistribution</metric>
    <metric>cdmlt-level</metric>
    <metric>coale-demeny-region</metric>
    <metric>c1-fert</metric>
    <metric>mu-fert</metric>
    <metric>sigma1-fert</metric>
    <metric>sigma2-fert</metric>
    <metric>residence-rule</metric>
    <metric>acceptable-kinship-degree-for-couples</metric>
    <metric>c1-women</metric>
    <metric>c1-men</metric>
    <metric>mu-women</metric>
    <metric>mu-men</metric>
    <metric>sigma1-women</metric>
    <metric>sigma1-men</metric>
    <metric>sigma2-women</metric>
    <metric>sigma2-men</metric>
    <metric>totalHouseholds</metric>
    <metric>totalIndividuals</metric>
    <metric>totalPopulationGrowth</metric>
    <metric>totalWomen</metric>
    <metric>totalMen</metric>
    <metric>femaleRatio</metric>
    <metric>womenAgeStructure</metric>
    <metric>menAgeStructure</metric>
    <metric>womenFirstAgeGroup</metric>
    <metric>menFirstAgeGroup</metric>
    <metric>womenBirths</metric>
    <metric>menBirths</metric>
    <metric>womenDeaths</metric>
    <metric>menDeaths</metric>
    <metric>womenIn</metric>
    <metric>menIn</metric>
    <metric>womenOut</metric>
    <metric>menOut</metric>
    <metric>totalOrphans</metric>
    <steppedValueSet variable="SEED" first="0" step="1" last="4999"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
