;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Household Nutrition model
;;  Copyright (C) 2021 Andreas Angourakis (andros.spica@gmail.com)
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

extensions [csv]

;;;;;;;;;;;;;;;;;
;;;;; BREEDS ;;;;
;;;;;;;;;;;;;;;;;

breed [ households household ]

;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals
[
  ;;; constants
  nutritionEffectSteepness

  ;;; modified parameters

  ;;;; initialisation
  initialNumHouseholds

  ;;;; parameter tables (loaded from csv files)

  ;;;;;; Nutrients Requirement Table ==================================================
  ; table holding the average amount of each nutrient type required per age group and sex (units are specific to nutrient types).
  ; Structure: age groups (rows) x sex (first column), nutrient types (second to n-th columns)
  nutrientRequirementsTable_ageAndSexGroup       ; sex-age groups. Two columns of the original table combined as a list of strings with format: "<age (integer)> | <sex (string)>"
                                                  ;;; sex (includes values "pregnancy" and "lactation")
                                                  ;;; age group (years old; trimester for pregnancy; semester for lactation)

  nutrientRequirementsTable_referenceBodyWeight  ; reference body weight (Kg)
  ;;;;; MAJOR NUTRIENTS
  nutrientRequirementsTable_water                ; water RI (liter)
  nutrientRequirementsTable_energy               ; energy AR (MJ) for PAL 1.4 to 2.0 (four columns)
  nutrientRequirementsTable_CHO                  ; minimum and maximum total carbohydrates RI (E%)
  nutrientRequirementsTable_fibre                ; dietary fibre RI (g)
  nutrientRequirementsTable_fat                  ; minimum and maximum total fat RI (E%)
  nutrientRequirementsTable_protein              ; protein (g/Kg of body weight)
  ;;;;; MINOR NUTRIENTS
  ;;;;;; MINERALS
  ;;; Calcium PRI (mg), Iron PRI (mg), Zinc PRI (mg), Copper AI (mg), Magnesium AI (mg),
  ;;; Fluoride AI (mg), Iodine AI (micrag), Manganese AI (mg), Molybdenum AI (micrag),
  ;;; Phosphorus AI (mg), Potassium AI (mg), Selenium AI (micrag)
  nutrientRequirementsTable_mineralNames
  nutrientRequirementsTable_minerals
  ;;;; VITAMINS
  ;;; Vitamin A or total provitamin A carotenoids (PRI, micrag), Vitamin B1 or Thiamine (PRI, mg),
  ;;; Vitamin B2 or Riboflavin (PRI, mg), Vitamin B3 or Niacin/Niacin Equivalent (PRI, mg),
  ;;; Vitamin B5 or Pantothenic acid (AI, mg), Vitamin B6 (PRI, mg),
  ;;; Vitamin B7 or Biotin (AI, micrag), Vitamin B9 or total Folates (PRI, micrag),
  ;;; Vitamin B12 or Cobalamin (AI, micrag),
  ;;; Vitamin C or ascorbic and L-dehydroascorbic acids (PRI, micrag),
  ;;; Vitamin D including D2 or Ergocalciferol (AI, micrag),
  ;;; Vitamin E or alfa-Tocopherol and equivalent (AI, mg),
  ;;; Vitamin K or Phylloquinones (AI, micrag),
  ;;; Choline (AI, mg)
  nutrientRequirementsTable_vitaminNames
  nutrientRequirementsTable_vitamins
  ;;;;;; ================================================================================
  ;;;;;; Nutrients per foodstuff Table ==================================================
  ; table holding the average minimum and maximum amount of each nutrient type per 100g of foodstuff type.
  ; Structure: foodstuff types (rows) x nutrient types (columns)
  nutrientContentsPerFoodstuffTable_foodstuffTypes
  nutrientContentsPerFoodstuffTable_foodstuffGroup
  nutrientContentsPerFoodstuffTable_ediblePortion

  ;;;;; MAJOR NUTRIENTS
  nutrientContentsPerFoodstuffTable_water                ; water (g)
  nutrientContentsPerFoodstuffTable_energy               ; energy (kJ)
  nutrientContentsPerFoodstuffTable_CHO                  ; total carbohydrates (g)
  nutrientContentsPerFoodstuffTable_fibre                ; dietary fibre (g)
  nutrientContentsPerFoodstuffTable_fat                  ; total fat (g)
  nutrientContentsPerFoodstuffTable_protein              ; protein (g)
  ;;;;; MINOR NUTRIENTS
  ;;;;;; MINERALS
  ;;; Calcium (mg), Iron (mg), Zinc (mg), Copper AI (mg), Magnesium (mg),
  ;;; Fluoride (mg), Iodine (micrag), Manganese (mg), Molybdenum (micrag),
  ;;; Phosphorus (mg), Potassium (mg), Selenium (micrag)
  nutrientContentsPerFoodstuffTable_mineralNames
  nutrientContentsPerFoodstuffTable_minerals
  ;;;; VITAMINS
  ;;; Vitamin A or total provitamin A carotenoids (micrag), Vitamin B1 or Thiamine (mg),
  ;;; Vitamin B2 or Riboflavin (mg), Vitamin B3 or Niacin/Niacin Equivalent (mg),
  ;;; Vitamin B5 or Pantothenic acid (mg), Vitamin B6 (mg),
  ;;; Vitamin B7 or Biotin (micrag), Vitamin B9 or total Folates (micrag),
  ;;; Vitamin B12 or Cobalamin (micrag),
  ;;; Vitamin C or ascorbic and L-dehydroascorbic acids (micrag),
  ;;; Vitamin D including D2 or Ergocalciferol (micrag),
  ;;; Vitamin E or alfa-Tocopherol and equivalent (mg),
  ;;; Vitamin K or Phylloquinones (micrag),
  ;;; Choline (mg)
  nutrientContentsPerFoodstuffTable_vitaminNames
  nutrientContentsPerFoodstuffTable_vitamins
  ;;;;;; ==============================================================================

  ;;;; parameters for simulating inputs
  ;;;; Their values should eventually come from other submodels.
  consumedDietMin consumedDietMax           ; Minimum and maximum consumed diet of households (list of floats per foodstuff type, stock units). Consumed diet is randomised in each household very time step.

  desiredDietMin desiredDietMax             ; Minimum and maximum desired diet of households (list of floats per foodstuff type, stock units). Desired diet is randomised in each household at initialisation.

  ;;; TO-DO: nutrientsRequirementDeviation and nutrientsPerFoodstuffDeviation are candidates for being external tables
  nutrientsRequirementDeviation             ; the standard deviation applied to values from Nutrients Requirement Table to be set for each household member upon initialisation (list of floats per nutrient type).
                                            ; It represents all factors of individual variance that are not explained by sex and age group (including pregnancy and lactation).

  nutrientsPerFoodstuffDeviation            ; the standard deviation applied to values from Nutrient per Foodstuff Table to be set globally upon initialisation (list of floats per nutrient type).

  ;;; variables

  ;;;; auxiliar

  ;;;; counters and final measures
  sumOfConsumedDiet                         ; Sum of all households consumed diet (list of floats per foodstuff type, stock units).
  sumOfNutrientRequired                     ; Sum of all households required nutrients separated by nutrient type (list of floats; nutrient units).

  totalHouseholds                ; count households (integer)
  totalIndividuals               ; sum of members of households (integer)
  totalWomen                     ; sum of female members of households (integer)
  totalMen                       ; sum of male members of households (integer)
  femaleRatio                    ; total number of women over total number of individuals (float)
  womenAgeStructure              ; merge list of all households female members ages (list of integers)
  menAgeStructure                ; merge list of all households male members ages (list of integers)
  womenFirstAgeGroup             ; count of women with age from 0 to 4 years old (base of typical population pyramid) (integer)
  menFirstAgeGroup               ; count of men with age from 0 to 4 years old (base of typical population pyramid) (integer)
]

;;; agents variables

households-own
[
  hh_dietConsumed                 ; The amounts of each foodstuff type consumed by the household in each time-step (stock units).
  hh_nutrientsRequired            ; The amount of each nutrient type desired by the household, but currently unsatisfied (stock units).

  hh_membersAge                  ; ages of every household member (list of integer)
  hh_membersSex                  ; sex of every household member (list of true/false, i.e. is female?)
  hh_pregnancyTrimester          ; pregnancy trimester of every household member (list of integer, values are 1, 2, and 3)
  hh_lactancySemester            ; lactancy semester of every household member (list of integer, values are 1 and 2)
  hh_menbersPAL                  ; physical activity level of every household member (list of strings, values are "1.4", "1.6", "1.8", and "2.0")
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  set-constants

  load-nutrition-tables

  ;set-parameters

  ;setup-households

  reset-counters

  update-counters

  reset-ticks

end

to set-constants

  ; "constants" are variables that will not be explored as parameters
  ; and may be used during a simulation.

  set nutritionEffectSteepness 3

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

    set consumedDietMax consumed-diet-max
    set consumedDietMin consumed-diet-min
  ]
  if (type-of-experiment = "random")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    set initialNumHouseholds 1 + random initial-num-households ; at least one household

    set desiredDietMin random desired-diet-min
    set desiredDietMax desiredDietMin + random desired-diet-max

    set consumedDietMin random consumed-diet-min
    set consumedDietMax consumedDietMin + random consumed-diet-max
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

  if (consumed-diet-max = 0)                    [ set consumed-diet-max                    80 ]
  if (consumed-diet-min = 0)                    [ set consumed-diet-min                    30 ]

end

to parameters-to-default

  ;;; set parameters to a default value
  set max-iterations                     2000

  set initial-num-households               25

  set desired-diet-max                     80
  set desired-diet-min                     30

  set consumed-diet-max                    80
  set consumed-diet-min                    30

end

to parameters-check2

  ;;; initial parameter check (e.g., avoiding division per zero error)
  check-par-is-positive "initialNumHouseholds" initialNumHouseholds

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

  ;set hh_dietDesired hh_get-dietDesired

  ;hh_initialise-stocks

end

to-report hh_get-dietDesired

  report get-random-in-range desiredDietMin desiredDietMax

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  reset-counters

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

to update-households

  reset-diet-satisfaction

  ;stocks-update

end

to reset-diet-satisfaction

  ask households
  [
    ;;; set the diet unfullfilled as the desired diet
    ;set hh_dietUnfulfilled hh_dietDesired
  ]

end

;;; HOUSEHOLDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report hh_get-diet-available

  ;report sum hh_stocks

end

to-report hh_get-nutrition

  let nutritionScore 0

  let nutrientAmountsRequiredByHousehold get-nutrient-requirements-of-group hh_membersAge hh_membersSex hh_pregnancyTrimester hh_lactancySemester hh_menbersPAL

  let nutrientAmountsInHouseholdDiet get-nutrient-contents-in-diet hh_dietConsumed

  ;;; compare each nutrient using logistic curve centred at y=0 and x=required amount.
  ;;; For example: (2(1/(1+exp(rate(required-consumed)))) - 1)
  ;;; NOTE: rate could be defined on a nutrient basis, but it would need more info on nutrition



  report nutritionScore

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERIC FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-random-in-range [ minValue maxValue ]

  report (minValue + random-float (maxValue - minValue))

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTERS AND MEASURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reset-counters



end

to update-counters



end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to plot-table [ values ]

  let j 0
  foreach values
  [
    i ->
    if (i != "") ;;; not null
    [ plotxy j i ]
    set j j + 1
  ]
  plot-pen-up

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NUTRITION TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to load-nutrition-tables

  load-nutrient-requirements-table

  load-nutrient-contents-per-foodstuff-table

end

;;; ---------NUTRIENT REQUIREMENTS--------------------------------------------------------------------------------------------------------------------------

to load-nutrient-requirements-table

  ;;; SOURCE:
  ;;; Values are informed or derived from:

  ;;; EFSA's Dietary Reference Values for nutrients.
  ;;; https://efsa.onlinelibrary.wiley.com/doi/toc/10.1002/(ISSN)1831-4732.021217
  ;;; specifically from tables in Summary report (2017):
  ;;; https://efsa.onlinelibrary.wiley.com/doi/pdf/10.2903/sp.efsa.2017.e15121

  ;;; abbreviations
  ;;; AR: average requirement
  ;;; PRI: population reference intake
  ;;; RI: reference intake

  ;;; this procedure loads the values of the nutrients requirement table
  ;;; the table contains:
  ;;;   1. two lines of headers with comments (metadata, to be ignored)
  ;;;   2. two lines with statements mapping rows and the different variables (columns)
  ;;;   3. the header of the table with the names of variables
  ;;;   4. remaining rows containing row name and values

  let nutrientsRequirementTable csv:from-file "nutrientRequirementsTable.csv"

  ;;;==================================================================================================================
  ;;; mapping coordinates (row or columns) in lines 3 and 4 (= index 2 and 3) -----------------------------------------
  ;;; NOTE: always correct raw mapping coordinates (start at 1) into list indexes (start at 0)

  ;;; line 3 (= index 2), row indexes
  let ageGroupsRowRange (list ((item 1 (item 2 nutrientsRequirementTable)) - 1) ((item 3 (item 2 nutrientsRequirementTable)) - 1))

  ;;; line 4 (= index 3), column indexes

  ;;; sex (includes values "pregnancy" and "lactation")
  let sexGroupColumn (item 1 (item 3 nutrientsRequirementTable)) - 1

  ;;; age group (years old; trimester for pregnancy; semester for lactation)
  let ageGroupColumn (item 3 (item 3 nutrientsRequirementTable)) - 1

  ;;; reference body weight (Kg)
  let bodyWeightColumn (item 5 (item 3 nutrientsRequirementTable)) - 1

  ;;; MAJOR NUTRIENTS
  ;;; water RI (liter)
  let waterColumn (item 7 (item 3 nutrientsRequirementTable)) - 1

  ;;; energy AR (MJ) for PAL 1.4 to 2.0 (four columns)
  let energyColumns (list ((item 9 (item 3 nutrientsRequirementTable)) - 1) ((item 11 (item 3 nutrientsRequirementTable)) - 1) )

  ;;; minimum and maximum total carbohydrates RI (E%)
  let carbohydratesColumns (list ((item 13 (item 3 nutrientsRequirementTable)) - 1) ((item 15 (item 3 nutrientsRequirementTable)) - 1) )

  ;;; dietary fibre RI (g)
  let fibreColumn (item 17 (item 3 nutrientsRequirementTable)) - 1

  ;;; minimum and maximum total fat RI (E%)
  let fatColumns (list ((item 19 (item 3 nutrientsRequirementTable)) - 1) ((item 21 (item 3 nutrientsRequirementTable)) - 1) )

  ;;; protein (g/Kg of body weight)
  ;let proteinColumn (item 23 (item 3 nutrientsRequirementTable)) - 1 ;;; protein AR
  let proteinColumn (item 25 (item 3 nutrientsRequirementTable)) - 1 ;;; protein PRI

  ;;; MINOR NUTRIENTS
  ;;; MINERALS
  let mineralColumns (list ((item 27 (item 3 nutrientsRequirementTable)) - 1) ((item 29 (item 3 nutrientsRequirementTable)) - 1) )

  ;;; VITAMINS
  let vitaminsColumns (list ((item 31 (item 3 nutrientsRequirementTable)) - 1) ((item 33 (item 3 nutrientsRequirementTable)) - 1) )

  ;;;==================================================================================================================
  ;;; extract data---------------------------------------------------------------------------------------

  ;;; get list of mineral and vitamins names in the order they are in the table as columns
  set nutrientRequirementsTable_mineralNames sublist (item 4 nutrientsRequirementTable) (item 0 mineralColumns) (item 1 mineralColumns + 1)
  set nutrientRequirementsTable_vitaminNames sublist (item 4 nutrientsRequirementTable) (item 0 vitaminsColumns) (item 1 vitaminsColumns + 1)

  ;;; read variables (list of lists, matrix: sex-age groups x nutrient variables)
  let nutrientsRequirementData sublist nutrientsRequirementTable (item 0 ageGroupsRowRange) (item 1 ageGroupsRowRange + 1)

  ;;; extract and combine/codify age and sex columns
  set nutrientRequirementsTable_ageAndSexGroup (
    map [row -> (word (item sexGroupColumn row) " | " (item ageGroupColumn row) ) ] nutrientsRequirementData
    )

  ;;; major nutrients as separate list and list of lists variables

  ;;; reference body weight (Kg)
  set nutrientRequirementsTable_referenceBodyWeight map [row -> item bodyWeightColumn row ] nutrientsRequirementData

  ;;; water RI (liter)
  set nutrientRequirementsTable_water map [row -> item waterColumn row ] nutrientsRequirementData

  ;;; energy AR (MJ) for PAL 1.4 to 2.0 (four columns)
  set nutrientRequirementsTable_energy extract-subtable nutrientsRequirementData (item 0 energyColumns) (item 1 energyColumns)

  ;;; minimum and maximum total carbohydrates RI (E%) - CHO = Carbon, Hydrogen, Oxigen (two columns)
  set nutrientRequirementsTable_CHO extract-subtable nutrientsRequirementData (item 0 carbohydratesColumns) (item 1 carbohydratesColumns)

  ;;; dietary fibre RI (g)
  set nutrientRequirementsTable_fibre map [row -> item fibreColumn row ] nutrientsRequirementData

  ;;; minimum and maximum total fat RI (E%) (two columns)
  set nutrientRequirementsTable_fat extract-subtable nutrientsRequirementData (item 0 fatColumns) (item 1 fatColumns)

  ;;; protein PRI (g/Kg of body weight)
  set nutrientRequirementsTable_protein map [row -> item proteinColumn row ] nutrientsRequirementData

  ;;; minor nutrients as two list of lists variables

  ;;; MINERALS
  ;;; Calcium PRI (mg), Iron PRI (mg), Zinc PRI (mg), Copper AI (mg), Magnesium AI (mg),
  ;;; Fluoride AI (mg), Iodine AI (micrag), Manganese AI (mg), Molybdenum AI (micrag),
  ;;; Phosphorus AI (mg), Potassium AI (mg), Selenium AI (micrag)
  set nutrientRequirementsTable_minerals extract-subtable nutrientsRequirementData (item 0 mineralColumns) (item 1 mineralColumns)

  ;;; VITAMINS
  ;;; Vitamin A or total provitamin A carotenoids (PRI, micrag), Vitamin B1 or Thiamine (PRI, mg),
  ;;; Vitamin B2 or Riboflavin (PRI, mg), Vitamin B3 or Niacin/Niacin Equivalent (PRI, mg),
  ;;; Vitamin B5 or Pantothenic acid (AI, mg), Vitamin B6 (PRI, mg),
  ;;; Vitamin B7 or Biotin (AI, micrag), Vitamin B9 or total Folates (PRI, micrag),
  ;;; Vitamin B12 or Cobalamin (AI, micrag),
  ;;; Vitamin C or ascorbic and L-dehydroascorbic acids (PRI, micrag),
  ;;; Vitamin D including D2 or Ergocalciferol (AI, micrag),
  ;;; Vitamin E or alfa-Tocopherol and equivalent (AI, mg),
  ;;; Vitamin K or Phylloquinones (AI, micrag),
  ;;; Choline (AI, mg)
  set nutrientRequirementsTable_vitamins extract-subtable nutrientsRequirementData (item 0 vitaminsColumns) (item 1 vitaminsColumns)

end

to-report get-nutrient-requirements-of-group [ peopleAge peopleSex peoplePregnancyTrimester peopleLactancySemester peoplePAL ]

  ;;; Returns a list of amounts per nutrient required by a group of people, given lists of people ages, sex, pregnancy trimester, lactancy semester, and PAL (physical activity level).

  ;;; Initialise list of nutrients
  let numberOfNutrients 6 + (length nutrientRequirementsTable_mineralNames) + (length nutrientRequirementsTable_vitaminNames)
  let listOfNutrientAmountsRequiredByGroup n-values numberOfNutrients [ i -> 0 ]

  foreach n-values (length peopleAge) [ i -> i ]
  [
    personIndex ->

    let personAge (item personIndex peopleAge)
    let personSex (item personIndex peopleSex)
    let personPregnancyTrimester (item personIndex peoplePregnancyTrimester)
    let personLactancySemester (item personIndex peopleLactancySemester)
    let personPAL (item personIndex peoplePAL)

    ;;; get nutrition content values per 100g of this foodstuff
    ;;; Nutrient amounts required are sampled out of an uniform probability distribution between the minimum and maximum values determined in nutrientContentsPerFoodstuffTable.
    let minNutrientAmountsRequiredByPerson get-nutrient-requirements-of-person personAge personSex personPregnancyTrimester personLactancySemester personPAL "min"
    let maxNutrientAmountsRequiredByPerson get-nutrient-requirements-of-person personAge personSex personPregnancyTrimester personLactancySemester personPAL "max"
    let listOfNutrientAmountsRequiredByPerson (map [ [ minI maxI ] -> minI + random-float maxI ] minNutrientAmountsRequiredByPerson maxNutrientAmountsRequiredByPerson)

    ;;; add values to the list of nutrient content amounts required by the group
    set listOfNutrientAmountsRequiredByGroup (map [ [ i j ] -> i + j ] listOfNutrientAmountsRequiredByGroup listOfNutrientAmountsRequiredByPerson)
  ]

  report listOfNutrientAmountsRequiredByGroup

end

to-report get-nutrient-requirements-of-person [ personAge personSex personPregnancyTrimester personLactancySemester personPAL minOrMax ]

  let sexAgeGroupIndex get-index-of-sex-age-group personAge personSex personPregnancyTrimester personLactancySemester
  let suffix (word " " minOrMax)
  let minOrMaxIndex position minOrMax [ "min" "max" ]

  let listOfNutrientAmountRequired (list
    (item sexAgeGroupIndex (item minOrMaxIndex nutrientRequirementsTable_water))
    (item sexAgeGroupIndex (item minOrMaxIndex nutrientRequirementsTable_energy))
    (item sexAgeGroupIndex (item minOrMaxIndex nutrientRequirementsTable_CHO))
    (item sexAgeGroupIndex (item minOrMaxIndex nutrientRequirementsTable_fibre))
    (item sexAgeGroupIndex (item minOrMaxIndex nutrientRequirementsTable_fat))
    (item sexAgeGroupIndex (item minOrMaxIndex nutrientRequirementsTable_protein))
    )

  foreach nutrientRequirementsTable_mineralNames
  [
    mineralName ->
    set listOfNutrientAmountRequired lput (get-required-mineral-value sexAgeGroupIndex (word mineralName suffix)) listOfNutrientAmountRequired
  ]

  foreach nutrientRequirementsTable_vitaminNames
  [
    vitaminName ->
    set listOfNutrientAmountRequired lput (get-required-vitamin-value sexAgeGroupIndex (word vitaminName suffix)) listOfNutrientAmountRequired
  ]

  report listOfNutrientAmountRequired

end

to-report get-nutrient-requirement-of-person [ nutrientName personAge personSex pregnancyTrimester lactancySemester personPAL ]

  let value 0
  let sexAgeGroupIndex get-index-of-sex-age-group personAge personSex pregnancyTrimester lactancySemester

  if (nutrientName = "water")
  [
    set value item sexAgeGroupIndex nutrientRequirementsTable_water
  ]
  if (nutrientName = "energy")
  [
    let palIndex position personPAL [ "1.4" "1.6" "1.8" "2.0" ]

    set value item sexAgeGroupIndex (item palIndex nutrientRequirementsTable_energy)
  ]
  if (nutrientName = "carbohydrates")
  [
    let minValue item sexAgeGroupIndex (item 0 nutrientRequirementsTable_CHO)
    let maxValue item sexAgeGroupIndex (item 1 nutrientRequirementsTable_CHO)
    set value minValue + random-float (maxValue - minValue)
  ]
  if (nutrientName = "fibre")
  [
    set value item sexAgeGroupIndex nutrientRequirementsTable_fibre
  ]
  if (nutrientName = "fat")
  [
    let minValue item sexAgeGroupIndex (item 0 nutrientRequirementsTable_fat)
    let maxValue item sexAgeGroupIndex (item 1 nutrientRequirementsTable_fat)
    set value minValue + random-float (maxValue - minValue)
  ]
  if (nutrientName = "protein")
  [
    set value item sexAgeGroupIndex nutrientRequirementsTable_protein
  ]
  if (member? nutrientName nutrientRequirementsTable_mineralNames)
  [ set value item sexAgeGroupIndex (item (position nutrientName nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) ]
  if (member? nutrientName nutrientRequirementsTable_vitaminNames)
  [ set value item sexAgeGroupIndex (item (position nutrientName nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_minerals) ]

  report value

end

to-report get-nutrient-requirement-per-sex-group [ nutrientName sexGroup ]

  let indexRange get-index-range-of-sex-group sexGroup
  let series 0

  if (nutrientName = "water (L)")
  [
    set series sublist nutrientRequirementsTable_water (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "energy PAL=1.4")
  [
    set series sublist (item 0 nutrientRequirementsTable_energy) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "energy PAL=1.6")
  [
    set series sublist (item 1 nutrientRequirementsTable_energy) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "energy PAL=1.8")
  [
    set series sublist (item 2 nutrientRequirementsTable_energy) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "energy PAL=2.0")
  [
    set series sublist (item 3 nutrientRequirementsTable_energy) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "carbohydrates (E%) min.")
  [
    set series sublist (item 0 nutrientRequirementsTable_CHO) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "carbohydrates (E%) max.")
  [
    set series sublist (item 1 nutrientRequirementsTable_CHO) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "fibre (g)")
  [
    set series sublist nutrientRequirementsTable_fibre (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "fat (E%) min.")
  [
    set series sublist (item 0 nutrientRequirementsTable_fat) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "fat (E%) max.")
  [
    set series sublist (item 1 nutrientRequirementsTable_fat) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "protein (g/Kg of body weight)")
  [
    set series sublist nutrientRequirementsTable_protein (item 0 indexRange) (item 1 indexRange)
  ]
  ;;; MINERALS
  if (nutrientName = "Calcium PRI (mg)")
  [
    set series sublist (item (position "Calcium" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Iron PRI (mg)")
  [
    set series sublist (item (position "Iron" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Zinc PRI (mg)")
  [
    set series sublist (item (position "Zinc" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Copper AI (mg)")
  [
    set series sublist (item (position "Copper" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Magnesium AI (mg)")
  [
    set series sublist (item (position "Magnesium" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Fluoride AI (mg)")
  [
    set series sublist (item (position "Fluoride" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Iodine AI (micrag)")
  [
    set series sublist (item (position "Iodine" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Manganese AI (mg)")
  [
    set series sublist (item (position "Manganese" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Molybdenum AI (micrag)")
  [
    set series sublist (item (position "Molybdenum" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Phosphorus AI (mg)")
  [
    set series sublist (item (position "Phosphorus" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Potassium AI (mg)")
  [
    set series sublist (item (position "Potassium" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Selenium AI (micrag)")
  [
    set series sublist (item (position "Selenium" nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals) (item 0 indexRange) (item 1 indexRange)
  ]
  ;;; VITAMINS
  if (nutrientName = "Vitamin A PRI (micrag)")
  [
    set series sublist (item (position "Vitamin A" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin B1 PRI (mg/MJ)")
  [
    set series sublist (item (position "Vitamin B1" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin B2 PRI (mg)")
  [
    set series sublist (item (position "Vitamin B2" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin B3 PRI (micrag/MJ)")
  [
    set series sublist (item (position "Vitamin B3" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin B5 AI (mg)")
  [
    set series sublist (item (position "Vitamin B5" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin B6 PRI (mg)")
  [
    set series sublist (item (position "Vitamin B6" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin B7 AI (micrag)")
  [
    set series sublist (item (position "Vitamin B7" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin B9 PRI (micrag Dietary Folate Equivalents)")
  [
    set series sublist (item (position "Vitamin B9" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin B12 AI (micrag)")
  [
    set series sublist (item (position "Cobalamin" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin C PRI (micrag)")
  [
    set series sublist (item (position "Vitamin C" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin D AI (micrag)")
  [
    set series sublist (item (position "Vitamin D" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin E AI (mg)")
  [
    set series sublist (item (position "Vitamin E" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Vitamin K AI (micrag)")
  [
    set series sublist (item (position "Vitamin K" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]
  if (nutrientName = "Choline AI (mg)")
  [
    set series sublist (item (position "Choline" nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins) (item 0 indexRange) (item 1 indexRange)
  ]

  report series

end

to-report get-required-mineral-value [ sexAgeGroupIndex mineralVariableName ]

  ;;; Returns the value at the given (row) index for the given mineral variable from the nutrientRequirementsTable

  report (item sexAgeGroupIndex (item (position mineralVariableName nutrientRequirementsTable_mineralNames) nutrientRequirementsTable_minerals))

end

to-report get-required-vitamin-value [ sexAgeGroupIndex vitaminVariableName ]

  ;;; Returns the value at the given (row) index for the given vitamin variable from the nutrientRequirementsTable

  report (item sexAgeGroupIndex (item (position vitaminVariableName nutrientRequirementsTable_vitaminNames) nutrientRequirementsTable_vitamins))

end

to-report get-index-of-sex-age-group [ personAge personSex personPregnancyTrimester personLactancySemester ]

  ;;; Returns the numeric row index of the sex-age group of the given person in nutrientsRequirementTable.
  ;;; WARNINGS:
  ;;; 1. sexGroup must be written the same in "nutrientsRequirementTable.csv"

  let index 0
  let sexAgeGroup (word personSex " | " personAge )
  if (personPregnancyTrimester != 0)
  [ set sexAgeGroup (word personSex " | " personPregnancyTrimester ) ]
  if (personLactancySemester != 0)
  [ set sexAgeGroup (word personSex " | " personLactancySemester ) ]

  foreach n-values (length nutrientRequirementsTable_ageAndSexGroup) [ j -> j ]
  [
    i ->
    if (sexAgeGroup = (item i nutrientRequirementsTable_ageAndSexGroup))
    [
      set index i
    ]
  ]

  report index

end

to-report get-index-range-of-sex-group [ sexGroup ]

  ;;; Returns the range of numeric row indexes of the given sex group ("female", "male", "pregnancy", "lactation") in nutrientsRequirementTable.
  ;;; WARNINGS:
  ;;; 1. sexGroup must be written the same in "nutrientsRequirementTable.csv"
  ;;; 2. rows in the same sex group must be contiguous in "nutrientsRequirementTable.csv"

  let indexes (list)

  foreach n-values (length nutrientRequirementsTable_ageAndSexGroup) [ j -> j ]
  [
    i ->
    if (member? sexGroup (item i nutrientRequirementsTable_ageAndSexGroup))
    [
      set indexes lput i indexes
    ]
  ]

  report (list (first indexes) (last indexes))

end

;;; ---------NUTRIENT CONTENTS--------------------------------------------------------------------------------------------------------------------------

to load-nutrient-contents-per-foodstuff-table

  ;;; SOURCE:
  ;;; Values are informed or derived from:

  ;;; Pakistan: Food Composition Table for Pakistan (revised 2001).
  ;;; Department of Agricultural Chemistry and Human Nutrition, NWFP,
  ;;; Agricultural University Peshawar: UNICEF Islamabad;
  ;;; Ministry of Planning and Development, Government of Pakistan Islamabad - 2001.
  ;;; https://www.fao.org/fileadmin/templates/food_composition/documents/regional/Book_Food_Composition_Table_for_Pakistan_.pdf

  ;;; India: Indian Food Composition Tables (2017).
  ;;; T. Longvah; R. Ananthan; K. Bhaskarachary; K. Venkaiah.
  ;;; The PDF file can be downloaded from http://www.ifct2017.com/

  ;;; Bangladesh: Food Composition Table for Bangladesh -
  ;;; Nazma Shaheen, Abu Torab MA Rahim, Md. Mohiduzzaman, Cadi Parvin Banu,
  ;;; Md. Latiful Bari, Avonti Basak Tukun,  MA Mannan, Lalita Bhattacharjee, Barbara Stadlmayr.
  ;;; Institute of Nutrition and Food Science; Centre for Advanced Research in Sciences;
  ;;; University of Dhaka, Dhaka-1000, Bangladesh. 2013 (English).

  ;;; These and more food tables per country can be found at: https://www.fao.org/infoods/infoods/tables-and-databases/en/

  ;;; this procedure loads the values of the nutrients requirement table
  ;;; the table contains:
  ;;;   1. two lines of headers with comments (metadata, to be ignored)
  ;;;   2. two lines with statements mapping rows and the different variables (columns)
  ;;;   3. the header of the table with the names of variables
  ;;;   4. remaining rows containing row name and values

  let nutrientContentsPerFoodstuffTable csv:from-file "nutrientContentsPerFoodstuffTable.csv"

  ;;;==================================================================================================================
  ;;; mapping coordinates (row or columns) in lines 3 and 4 (= index 2 and 3) -----------------------------------------
  ;;; NOTE: always correct raw mapping coordinates (start at 1) into list indexes (start at 0)

  ;;; line 3 (= index 2), row indexes
  let foodstuffRowRange (list ((item 1 (item 2 nutrientContentsPerFoodstuffTable)) - 1) ((item 3 (item 2 nutrientContentsPerFoodstuffTable)) - 1))

  ;;; line 4 (= index 3), column indexes

  ;;; foodstuff names
  let foodstuffColumn (item 1 (item 3 nutrientContentsPerFoodstuffTable)) - 1

  ;;; skip source scientific name

  ;;; foodstuff groups
  let foodstuffGroupColumn (item 5 (item 3 nutrientContentsPerFoodstuffTable)) - 1

  ;;; edible portion
  let ediblePortionColumn (item 7 (item 3 nutrientContentsPerFoodstuffTable)) - 1

  ;;; MAJOR NUTRIENTS
  ;;; water (g)
  let waterColumns (list ((item 9 (item 3 nutrientContentsPerFoodstuffTable)) - 1) ((item 11 (item 3 nutrientContentsPerFoodstuffTable)) - 1) )

  ;;; energy (kJ)
  let energyColumns (list ((item 13 (item 3 nutrientContentsPerFoodstuffTable)) - 1) ((item 15 (item 3 nutrientContentsPerFoodstuffTable)) - 1) )

  ;;; minimum and maximum total carbohydrates RI (E%)
  let carbohydratesColumns (list ((item 17 (item 3 nutrientContentsPerFoodstuffTable)) - 1) ((item 19 (item 3 nutrientContentsPerFoodstuffTable)) - 1) )

  ;;; dietary fibre (g)
  let fibreColumns (list ((item 21 (item 3 nutrientContentsPerFoodstuffTable)) - 1) ((item 23 (item 3 nutrientContentsPerFoodstuffTable)) - 1) )

  ;;; total fat (g)
  let fatColumns (list ((item 25 (item 3 nutrientContentsPerFoodstuffTable)) - 1) ((item 27 (item 3 nutrientContentsPerFoodstuffTable)) - 1) )

  ;;; protein (g)
  let proteinColumns (list ((item 29 (item 3 nutrientContentsPerFoodstuffTable)) - 1) ((item 31 (item 3 nutrientContentsPerFoodstuffTable)) - 1) )

  ;;; MINOR NUTRIENTS
  ;;; MINERALS
  let mineralColumns (list ((item 33 (item 3 nutrientContentsPerFoodstuffTable)) - 1) ((item 35 (item 3 nutrientContentsPerFoodstuffTable)) - 1) )

  ;;; VITAMINS
  let vitaminsColumns (list ((item 37 (item 3 nutrientContentsPerFoodstuffTable)) - 1) ((item 39 (item 3 nutrientContentsPerFoodstuffTable)) - 1) )

  ;;;==================================================================================================================
  ;;; extract data---------------------------------------------------------------------------------------

  ;;; get list of mineral and vitamins names (min and max) in the order they are in the table as columns
  set nutrientContentsPerFoodstuffTable_mineralNames sublist (item 4 nutrientContentsPerFoodstuffTable) (item 0 mineralColumns) (item 1 mineralColumns + 1)
  set nutrientContentsPerFoodstuffTable_vitaminNames sublist (item 4 nutrientContentsPerFoodstuffTable) (item 0 vitaminsColumns) (item 1 vitaminsColumns + 1)

  ;;; read variables (list of lists, matrix: foodstuff types x source scientific names, foodstuff groups, edible portion, nutrient variables)
  let nutrientsPerFoodstuffData sublist nutrientContentsPerFoodstuffTable (item 0 foodstuffRowRange) (item 1 foodstuffRowRange + 1)

  ;;; get list of foodstuff types names and foodstuff group names in the order they are in the table as rows
  set nutrientContentsPerFoodstuffTable_foodstuffTypes map [ row -> item foodstuffColumn row ] nutrientsPerFoodstuffData

  set nutrientContentsPerFoodstuffTable_foodstuffGroup map [ row -> item foodstuffGroupColumn row ] nutrientsPerFoodstuffData

  ;;; edible portion
  set nutrientContentsPerFoodstuffTable_ediblePortion map [ row -> item ediblePortionColumn row ] nutrientsPerFoodstuffData

  ;;; major nutrients as separate list of lists variables with minimum and maximum values per foodstuff

  ;;; water (g)
  set nutrientContentsPerFoodstuffTable_water extract-subtable nutrientsPerFoodstuffData (item 0 waterColumns) (item 1 waterColumns)

  ;;; energy (kJ)
  set nutrientContentsPerFoodstuffTable_energy extract-subtable nutrientsPerFoodstuffData (item 0 energyColumns) (item 1 energyColumns)

  ;;; total carbohydrates (g) - CHO = Carbon, Hydrogen, Oxigen (two columns)
  set nutrientContentsPerFoodstuffTable_CHO extract-subtable nutrientsPerFoodstuffData (item 0 carbohydratesColumns) (item 1 carbohydratesColumns)

  ;;; dietary fibre (g)
  set nutrientContentsPerFoodstuffTable_fibre extract-subtable nutrientsPerFoodstuffData (item 0 fibreColumns) (item 1 fibreColumns)

  ;;; total fat (g)
  set nutrientContentsPerFoodstuffTable_fat extract-subtable nutrientsPerFoodstuffData (item 0 fatColumns) (item 1 fatColumns)

  ;;; protein (g)
  set nutrientContentsPerFoodstuffTable_protein extract-subtable nutrientsPerFoodstuffData (item 0 proteinColumns) (item 1 proteinColumns)

  ;;; minor nutrients as two list of lists variables

  ;;; MINERALS
  ;;; Calcium (mg), Iron (mg), Zinc (mg), Copper AI (mg), Magnesium (mg),
  ;;; Fluoride (mg), Iodine (micrag), Manganese (mg), Molybdenum (micrag),
  ;;; Phosphorus (mg), Potassium (mg), Selenium (micrag)
  set nutrientContentsPerFoodstuffTable_minerals extract-subtable nutrientsPerFoodstuffData (item 0 mineralColumns) (item 1 mineralColumns)

  ;;; VITAMINS
  ;;; Vitamin A or total provitamin A carotenoids (micrag), Vitamin B1 or Thiamine (mg),
  ;;; Vitamin B2 or Riboflavin (mg), Vitamin B3 or Niacin/Niacin Equivalent (mg),
  ;;; Vitamin B5 or Pantothenic acid (mg), Vitamin B6 (mg),
  ;;; Vitamin B7 or Biotin (micrag), Vitamin B9 or total Folates (micrag),
  ;;; Vitamin B12 or Cobalamin (micrag),
  ;;; Vitamin C or ascorbic and L-dehydroascorbic acids (micrag),
  ;;; Vitamin D including D2 or Ergocalciferol (micrag),
  ;;; Vitamin E or alfa-Tocopherol and equivalent (mg),
  ;;; Vitamin K or Phylloquinones (micrag),
  ;;; Choline (mg)
  set nutrientContentsPerFoodstuffTable_vitamins extract-subtable nutrientsPerFoodstuffData (item 0 vitaminsColumns) (item 1 vitaminsColumns)

end

to-report get-nutrient-contents-in-diet [ listOfFoodstuffAmountInDiet ]

  ;;; Returns a list of amounts per nutrient given a list of amounts per foodstuff in grams.

  ;;; Initialise list of nutrients
  let numberOfNutrients 6 + (length nutrientContentsPerFoodstuffTable_mineralNames) + (length nutrientContentsPerFoodstuffTable_vitaminNames)
  let listOfNutrientAmountsInDiet n-values numberOfNutrients [ i -> 0 ]

  foreach nutrientContentsPerFoodstuffTable_foodstuffTypes
  [
    foodstuffTypeName ->

    let foodstuffTypeIndex (position foodstuffTypeName nutrientContentsPerFoodstuffTable_foodstuffTypes)

    ;;; get nutrition content values per 100g of this foodstuff
    ;;; Nutrient amounts are sampled out of an uniform probability distribution between the minimum and maximum values determined in nutrientContentsPerFoodstuffTable.
    let minNutrientContentsPer100g get-nutrient-contents-of-foodstuff-type foodstuffTypeName "min"
    let maxNutrientContentsPer100g get-nutrient-contents-of-foodstuff-type foodstuffTypeName "min"
    let nutrientContentsPer100g (map [ [ minI maxI ] -> minI + random-float maxI ] minNutrientContentsPer100g maxNutrientContentsPer100g)

    ;;; calculate total nutrition content values for edible portion and given foodstuff amount
    let edibleAmount (item foodstuffTypeIndex nutrientContentsPerFoodstuffTable_ediblePortion) * (item foodstuffTypeIndex listOfFoodstuffAmountInDiet)
    let totalNutrientContents map [ i -> i * (edibleAmount / 100) ] nutrientContentsPer100g

    ;;; add values to the overall list of nutrient contents in diet
    set listOfNutrientAmountsInDiet (map [ [ i j ] -> i + j ] listOfNutrientAmountsInDiet totalNutrientContents)
  ]

  report listOfNutrientAmountsInDiet

end

to-report get-nutrient-contents-of-foodstuff-type [ foodstuffTypeName minOrMax ]

  ;;; Returns a list of all minimum or maximum nutrient content values corresponding to the given foodstuff type (row) in nutrientContentsPerFoodstuffTable.

  let index (position foodstuffTypeName nutrientContentsPerFoodstuffTable_foodstuffTypes)
  let suffix (word " " minOrMax)
  let minOrMaxIndex position minOrMax [ "min" "max" ]

  let series (list
    (item index (item minOrMaxIndex nutrientContentsPerFoodstuffTable_water))
    (item index (item minOrMaxIndex nutrientContentsPerFoodstuffTable_energy))
    (item index (item minOrMaxIndex nutrientContentsPerFoodstuffTable_CHO))
    (item index (item minOrMaxIndex nutrientContentsPerFoodstuffTable_fibre))
    (item index (item minOrMaxIndex nutrientContentsPerFoodstuffTable_fat))
    (item index (item minOrMaxIndex nutrientContentsPerFoodstuffTable_protein))
    )

  foreach nutrientContentsPerFoodstuffTable_mineralNames
  [
    mineralName ->
    set series lput (get-content-mineral-value index (word mineralName suffix)) series
  ]

  foreach nutrientContentsPerFoodstuffTable_vitaminNames
  [
    vitaminName ->
    set series lput (get-content-vitamin-value index (word vitaminName suffix)) series
  ]

  report series

end

to-report get-content-mineral-value [ foodstuffIndex mineralVariableName ]

  ;;; Returns the value at the given (row) index for the given mineral variable from the nutrientContentsPerFoodstuffTable

  report (item foodstuffIndex (item (position mineralVariableName nutrientContentsPerFoodstuffTable_mineralNames) nutrientContentsPerFoodstuffTable_minerals))

end

to-report get-content-vitamin-value [ foodstuffIndex vitaminVariableName ]

  ;;; Returns the value at the given (row) index for the given vitamin variable from the nutrientContentsPerFoodstuffTable

  report (item foodstuffIndex (item (position vitaminVariableName nutrientContentsPerFoodstuffTable_vitaminNames) nutrientContentsPerFoodstuffTable_vitamins))

end

to-report get-nutrient-content-of-foodstuff-types [ nutrientVariableName minOrMaxIndex ]

  ;;; Returns all (min or max) content of the given nutrient for all foodstuff types in nutrientContentsPerFoodstuffTable.
  ;;; NOTE: Useful for querying the table and plotting

  let suffix item minOrMaxIndex (list "min" "max")

  if (nutrientVariableName = "water (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_water) ]
  if (nutrientVariableName = "energy (kJ)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_energy) ]
  if (nutrientVariableName = "carbohydrates (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_CHO) ]
  if (nutrientVariableName = "fibre (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_fibre) ]
  if (nutrientVariableName = "fat (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_fat) ]
  if (nutrientVariableName = "protein (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_protein) ]

  let variableName (substring nutrientVariableName 0 (position "(" nutrientVariableName))
  set variableName (word variableName suffix)

  if (member? variableName nutrientContentsPerFoodstuffTable_mineralNames)
  [ report (item (position variableName nutrientContentsPerFoodstuffTable_mineralNames) nutrientContentsPerFoodstuffTable_minerals) ]

  if (member? variableName nutrientContentsPerFoodstuffTable_vitaminNames)
  [ report (item (position variableName nutrientContentsPerFoodstuffTable_vitaminNames) nutrientContentsPerFoodstuffTable_vitamins) ]

  report 0

end

to-report extract-subtable [ table startColumnIndex endColumnIndex ]

  let subtable (list)
  let columnsCount ((endColumnIndex + 1) - startColumnIndex)
  foreach n-values columnsCount [ j -> j ]
  [
    i ->
    let columnIndex startColumnIndex + i
    set subtable lput (map [row -> item columnIndex row ] table) subtable
  ]
  report subtable

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
      ;file-type (word "'" hh_dietDesired "'") file-type ", "
      ;file-type (word "'" hh_stocks "'") file-type ", "
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
16
310
138
370
initial-num-households
25.0
1
0
Number

MONITOR
143
319
244
356
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
0.0
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
42
260
194
293
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

TEXTBOX
434
323
667
357
desired and consumed FOODSTUFFs 
14
0.0
1

PLOT
571
10
872
236
Nutrient per sex-age
age group
nutrients required
0.0
10.0
0.0
10.0
false
true
"" "clear-plot\nif (nutrientRequirementsTable_ageAndSexGroup = 0) [ load-nutrient-requirements-table ]\nset-plot-x-range 0 100\nlet nutrientPerGroupFemale get-nutrient-requirement-per-sex-group required-nutrient-to-plot \"female\"\nlet nutrientPerGroupMale get-nutrient-requirement-per-sex-group required-nutrient-to-plot \"male\"\nset-plot-y-range -0.001 (precision (max (list (max nutrientPerGroupFemale) (max nutrientPerGroupMale)) + 0.001) 3)"
PENS
"females" 1.0 0 -5298144 true "\n" "plot-table get-nutrient-requirement-per-sex-group required-nutrient-to-plot \"female\""
"males" 1.0 0 -14070903 true "" "plot-table get-nutrient-requirement-per-sex-group required-nutrient-to-plot \"male\""

INPUTBOX
247
405
1230
465
desired-diet-min
\"[  ]\"
1
0
String

INPUTBOX
247
527
1231
587
consumed-diet-min
0
1
0
String

INPUTBOX
246
464
1230
524
desired-diet-max
0
1
0
String

INPUTBOX
247
586
1231
646
consumed-diet-max
0
1
0
String

MONITOR
248
347
1229
392
NIL
nutrientContentsPerFoodstuffTable_foodstuffTypes
0
1
11

CHOOSER
572
238
873
283
required-nutrient-to-plot
required-nutrient-to-plot
"water (L)" "energy PAL=1.4" "energy PAL=1.6" "energy PAL=1.8" "energy PAL=2.0" "carbohydrates (E%) min." "carbohydrates (E%) max." "fibre (g)" "fat (E%) min." "fat (E%) max." "protein (g/Kg of body weight)" "Calcium PRI (mg)" "Iron PRI (mg)" "Zinc PRI (mg)" "Copper AI (mg)" "Magnesium AI (mg)" "Fluoride AI (mg)" "Iodine AI (micrag)" "Manganese AI (mg)" "Molybdenum AI (micrag)" "Phosphorus AI (mg)" "Potassium AI (mg)" "Selenium AI (micrag)" "Vitamin A PRI (micrag)" "Vitamin B1 PRI (mg/MJ)" "Vitamin B2 PRI (mg)" "Vitamin B3 PRI (micrag/MJ)" "Vitamin B5 AI (mg)" "Vitamin B6 PRI (mg)" "Vitamin B7 AI (micrag)" "Vitamin B9 PRI (micrag)" "Vitamin B12 AI (micrag)" "Vitamin C PRI (micrag)" "Vitamin D AI (micrag)" "Vitamin E AI (mg)" "Vitamin K AI (micrag)" "Choline AI (mg)"
9

BUTTON
573
285
675
318
NIL
update-plots
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
877
10
1177
236
Nutrient per foodstuff types
foodstuff type index
nutrient amount per 100g
0.0
10.0
0.0
10.0
true
true
"" "clear-plot\nif (nutrientContentsPerFoodstuffTable_foodstuffTypes = 0) [ load-nutrient-contents-per-foodstuff-table ]\nset-plot-x-range -1 ((length nutrientContentsPerFoodstuffTable_foodstuffTypes) + 1)\nlet nutrientPerFoodstuffMax (max (get-nutrient-content-of-foodstuff-types nutrient-content-to-plot 1))\nset-plot-y-range -0.001 (precision (nutrientPerFoodstuffMax + 0.001) 3)"
PENS
"min" 1.0 1 -16777216 true "" "plot-table get-nutrient-content-of-foodstuff-types nutrient-content-to-plot 0"
"max" 1.0 1 -7500403 true "" "plot-table get-nutrient-content-of-foodstuff-types nutrient-content-to-plot 1"

CHOOSER
878
239
1178
284
nutrient-content-to-plot
nutrient-content-to-plot
"water (g)" "energy (kJ)" "carbohydrates (g)" "fibre (g)" "fat (g)" "protein (g)" "Calcium (mg)" "Iron (mg)" "Zinc (mg)" "Copper (mg)" "Magnesium (mg)" "Fluoride (mg)" "Iodine (micrag)" "Manganese (mg)" "Molybdenum (micrag)" "Phosphorus (mg)" "Potassium (mg)" "Selenium (micrag)" "Vitamin A (micrag)" "Vitamin B1 (mg)" "Vitamin B2 (mg)" "Vitamin B3 (mg)" "Vitamin B5 (mg)" "Vitamin B6 (mg)" "Vitamin B7 (micrag)" "Vitamin B9 (micrag)" "Vitamin B12 AI (micrag)" "Vitamin C (micrag)" "Vitamin D (micrag)" "Vitamin E (mg)" "Vitamin K (micrag)" "Choline (mg)"
4

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
