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
  yearLengthInDays
  maturityAgeInYears              ; defaults to 15 years old; it affects the minimum age acceptable for individuals to keep a household without older individuals

  ;;; modified parameters

  ;;;; parameter tables (loaded from csv files)

  ;;;;;; Nutrients Requirement Table ==================================================
  ; table holding the average amount of each nutrient type required per age group and sex (units are specific to nutrient types).
  ; Structure: age groups (rows) x sex (first column), nutrient types (second to n-th columns)
  nutrientRequirementsTable_ageAndSexGroup       ; sex-age groups. Two columns of the original table combined as a list of strings with format: "<age (integer)> | <sex (string)>"
                                                  ;;; sex (includes values "pregnancy" and "lactation")
                                                  ;;; age group (years old; trimester for pregnancy; semester for lactation)

  nutrientRequirementsTable_referenceBodyWeight  ; reference body weight (kg)
  ;;;;; MAJOR NUTRIENTS
  nutrientRequirementsTable_water                ; water RI (liter)
  nutrientRequirementsTable_energy               ; energy AR (MJ) for PAL 1.4 to 2.0 (four columns)
  nutrientRequirementsTable_CHO                  ; minimum and maximum total carbohydrates RI (E%)
  nutrientRequirementsTable_fibre                ; dietary fibre RI (g)
  nutrientRequirementsTable_fat                  ; minimum and maximum total fat RI (E%)
  nutrientRequirementsTable_protein              ; protein (g/kg of body weight)
  ;;;;; MINOR NUTRIENTS
  ;;;;;; MINERALS
  ;;; Calcium PRI (mg), Iron PRI (mg), Zinc PRI (mg), Copper AI (mg), Magnesium AI (mg),
  ;;; Fluoride AI (mg), Iodine AI (micrag), Manganese AI (mg), Molybdenum AI (micrag),
  ;;; Phosphorus AI (mg), Potassium AI (mg), Selenium AI (micrag)
  nutrientRequirementsTable_mineralColumnNames
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
  nutrientRequirementsTable_vitaminColumnNames
  nutrientRequirementsTable_vitaminNames
  nutrientRequirementsTable_vitamins

  nutrientRequirementsTable_numberOfNutrients    ; total number of nutrients accounted in nutrientRequirementsTable

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
  nutrientContentsPerFoodstuffTable_mineralColumnNames
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
  nutrientContentsPerFoodstuffTable_vitaminColumnNames
  nutrientContentsPerFoodstuffTable_vitaminNames
  nutrientContentsPerFoodstuffTable_vitamins

  nutrientContentsPerFoodstuffTable_numberOfNutrients    ; total number of nutrients accounted in nutrientRequirementsTable

  ;;;;;; ==============================================================================

  ;;;; parameters for simulating inputs
  ;;;; Their values should eventually come from other submodels.

  ;;;; Household Demography parameters
  ;;; demography tables
  fertilityTable
  nuptialityTable-women nuptialityTable-men

  ;;; individual parameters
  initialNumHouseholds
  householdInitialAgeDistribution ; (list minimum maximum)

  ;;;; input variables from other submodels
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
  hh_dietDesired                  ; The amounts of each foodstuff type desired by the household in each time-step (kg).
  hh_dietConsumed                 ; The amounts of each foodstuff type consumed by the household in each time-step (kg).
  hh_nutrientsInDiet              ; The amount of each nutrient type contained in the current diet consumed by the household (g, mg, micrag, depending on nutrient).
  hh_nutrientsRequired            ; The amount of each nutrient type required by the household (g, mg, micrag, depending on nutrient).
  hh_nutritionScore               ; score calculated from the contrast between hh_nutrientsInDiet and hh_nutrientsRequired.
                                  ; A single score exists for a household, meaning we assume food to be distributed internally in proportion to the needs of individual members.

  hh_age                         ; number of days during which the household existed (integer)

  hh_membersAge                  ; ages of every household member in days (list of integer)
  hh_membersSex                  ; sex of every household member (list of true/false, i.e. is female?)
  hh_pregnancyTrimester          ; pregnancy trimester of every household member (list of integer, values are 0 (null), 1, 2, and 3)
  hh_lactancySemester            ; lactancy semester of every household member (list of integer, values are 0 (null), 1 and 2)
  hh_menbersPAL                  ; physical activity level of every household member (list of strings, values are "1.4", "1.6", "1.8", and "2.0")
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  ; --- loading/testing parameters -----------

  set-constants

  set-parameters

  build-demography-tables

  load-nutrition-tables

  ; --- core procedures ----------------------

  setup-households

  ; --- counters -----------------------------

  reset-counters

  update-counters

  ; --- display -----------------------------

  print-foodstuff-types-list

  update-display

  ; --- tick ---------------------------------

  reset-ticks

end

to set-constants

  ; "constants" are variables that will not be explored as parameters
  ; and may be used during a simulation.

  set yearLengthInDays 365

  set maturityAgeInYears 15

  set nutritionEffectSteepness 3

end

to set-parameters

  ; set random seed
  random-seed SEED

  ; check parameters values
  parameters-check1

  ;;; setup parameters depending on the type of experiment
  set householdInitialAgeDistribution read-from-string ( word "[" household-initial-age-distribution "]")

  if (type-of-experiment = "user-defined")
  [
    ;;; load parameters from user interface
    set initialNumHouseholds initial-num-households
  ]
  if (type-of-experiment = "random")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    set initialNumHouseholds 1 + random initial-num-households ; at least one household
    set householdInitialAgeDistribution (list
      (1 + random (item 0 householdInitialAgeDistribution))   ; minimum
      (
        (item 0 householdInitialAgeDistribution)
        + 1
        + random ((item 1 householdInitialAgeDistribution) - (item 0 householdInitialAgeDistribution))
      )   ; maximum
      )
  ]

  ; check parameters values
  parameters-check2

end

to parameters-check1

  ;;; check if values were reset to 0
  ;;; and set default values
  if (initial-num-households = 0)               [ set initial-num-households               25 ]

  ;;; string type inputs (vector of values)
  if (household-initial-age-distribution = 0 or
    length household-initial-age-distribution = 1)   [ set household-initial-age-distribution   "0 30" ]

end

to parameters-to-default

  ;;; set parameters to a default value
  set max-iterations                     2000

  set initial-num-households               25
  set household-initial-age-distribution   "0 30"

end

to parameters-check2

  ;;; initial parameter check (e.g., avoiding division per zero error)
  check-par-is-positive "initialNumHouseholds" initialNumHouseholds

  ;;; check if given min values are less than max values
  check-par-is-range "householdInitialAgeDistribution" householdInitialAgeDistribution

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

  ;;; create households
  repeat initialNumHouseholds
  [
    create-households 1
    [
      hh_initialise
    ]
  ]

end

;;; HOUSEHOLDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to hh_initialise

  ;;; Initialization of household members (age and sex)

  set hh_age get-initial-household-age

  hh_initialise-members

  ;;; nutrition model specific procedures
  hh_initialise-diet

  hh_update-nutrition

  ;;; move to empty or less crowded patch, just in sake of visualisation
  move-to min-one-of patches [count households-here]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; START of simplified Household Demography model procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-initial-household-age

  let ageInYears item 0 householdInitialAgeDistribution + random (item 1 householdInitialAgeDistribution - item 0 householdInitialAgeDistribution + 1)

  report (ageInYears * yearLengthInDays) + (random yearLengthInDays)

end

to hh_initialise-members

  hh_reset-members

  ;;; assures that at least two member are above (fertility age + household age) and have different sex
  foreach (list 0 1)
  [
    firstMembersIndex ->
    set hh_membersSex lput (firstMembersIndex = 0) hh_membersSex
    let marriageAge (get-initial-marriage-age (item firstMembersIndex hh_membersSex))
    set hh_membersAge lput (hh_age + marriageAge) hh_membersAge
  ]

  ;;; calculate offspring and offspring age
  let offspringProb 0
  let i hh_age
  repeat hh_age
  [
    ;;; get the probability of the couple having descendence for a past day i
    ;;; i.e. woman fertility at the corresponding age
    set offspringProb get-fertility-of-day ((item 0 hh_membersAge) - i)

    ; test the probability and add the offspring with the corresponding age
    if (random-float 1 < offspringProb)
    [
      hh_add-offspring i ; a child with i days old
    ]
    set i i - 1
  ]

  ;;; initialise pregnancy, lactancy and PAL at base default values
  set hh_pregnancyTrimester map [memberIndex -> 0] hh_membersSex
  set hh_lactancySemester map [memberIndex -> 0] hh_membersSex
  set hh_menbersPAL map [memberIndex -> "1.4"] hh_membersSex

end

to-report get-initial-marriage-age [ isFemale ]

  ; get a marriage age in days as a stochastic function of nuptiality rates in earlier years
  let notMarried true
  let ageInYears 0

  while [notMarried AND ageInYears < 100]
  [
    if ((get-nuptiality isFemale ageInYears) > random-float 1)
    [
      set notMarried false
    ]
    set ageInYears ageInYears + 1
  ]

  report (ageInYears * yearLengthInDays) + (random yearLengthInDays)

end

to hh_reset-members

  set hh_membersAge (list)
  set hh_membersSex (list)
  set hh_pregnancyTrimester (list)
  set hh_lactancySemester (list)
  set hh_menbersPAL (list)

end

to-report get-fertility-of-day [ ageInDays ]

  let ageInYears get-age-in-years-old ageInDays

  report (get-fertility ageInYears) / yearLengthInDays

end

to-report get-fertility [ ageInYears ]

  report (item ageInYears fertilityTable)

end

to-report get-nuptiality [ isFemale ageInYears ]

  let probMarry 0

  ifelse (isFemale)
  [
    set probMarry item ageInYears nuptialityTable-women
  ]
  [
    set probMarry item ageInYears nuptialityTable-men
  ]

  report probMarry

end

to hh_add-offspring [ initialAge ]

  ; add a newborn to the household
  ; the specification of initialAge is needed to use this in setup
  set hh_membersAge lput initialAge hh_membersAge
  set hh_membersSex lput (random 2 = 0) hh_membersSex

end

to-report get-age-in-years-old [ ageInDays ]

  report floor (ageInDays / yearLengthInDays)

end

to-report hh_count-members

  report length hh_membersSex

end

to-report hh_membersIndexes

  ; report a list of members indexes from 0 to n

  report (n-values hh_count-members [ i -> i ])

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEMOGRAPHY TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to build-demography-tables

  ;;; load demographic data tables into lists

  ;=======FERTILITY========================================================

  build-fertility-tables

  ;=======NUPTIALITY========================================================

  build-nuptiality-tables

end


to build-fertility-tables

  ;;; to keep it simple, we fix the fertility parameters to the default values in the Household Demography model
  ;;; The point is to get a stochastic, yet realistic, age distribution.
  let c1-fert 0.85
  let mu-fert 25
  let sigma1-fert 5
  let sigma2-fert 10

  set fertilityTable load-peristeri-kostaki-model-table c1-fert mu-fert sigma1-fert sigma2-fert

end

to build-nuptiality-tables

  ;;; Again, to keep it simple, we fix the nuptiality parameters to the default values in the Household Demography model
  ;;; The point is to get a stochastic, yet realistic, age distribution.
  let c1-women 0.85
  let mu-women 25
  let sigma1-women 5
  let sigma2-women 10
  let c1-men 0.9
  let mu-men 15
  let sigma1-men 5
  let sigma2-men 5

  set nuptialityTable-women load-peristeri-kostaki-model-table c1-women mu-women sigma1-women sigma2-women

  set nuptialityTable-men load-peristeri-kostaki-model-table c1-men mu-men sigma1-men sigma2-men

end

to-report load-peristeri-kostaki-model-table [ c1 mu sigma1 sigma2 ]

  ;;; The following correspond to the first parametric model in:

  ;;; Peristeva and Kostaki, 2009, p. 147
  ;;; "Modeling fertility in modern populations"
  ;;; Demographic Research 16: 141-194
  ;;; Available from: https://dx.doi.org/10.4054/DemRes.2007.16.6

  ;;; Peristeva and Kostaki (2015), p. 133
  ;;; "A parametric model for estimating nuptiality patterns in modern populations"
  ;;; Canadian studies in population 42(2):130-148. DOI: 10.25336/P6TK56
  ;;; Available from: https://www.researchgate.net/publication/285457704_A_parametric_model_for_estimating_nuptiality_patterns_in_modern_populations [accessed Nov 27 2018].
  ;;; use "demoTables/compareNuptialityModel.R" to test shapes

  let marriageProbs (list)

  foreach n-values 151 [ i -> i ]
  [
    i ->
    let sigma sigma1
    if (i > mu) [ set sigma sigma2 ]

    set marriageProbs lput (
      c1 * exp (-1 * (((i - mu) / sigma) ^ 2))
    ) marriageProbs
  ]

  report marriageProbs

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END of simplified Household Demography model procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to hh_initialise-diet

  hh_update-nutrientsRequired

  set hh_dietDesired (get-initial-dietDesired hh_nutrientsRequired)

  set hh_dietConsumed hh_dietDesired

end

to-report get-initial-dietDesired [ nutrientsRequired ]

  ;;; derive initial desired diet from the nutritional requirements,
  ;;; by randomly adding 1g of foodstuff one type at a time, until the nutrition score is neutral
  ;;; NOTE: this is not very computationally efficient, and the diet return will be often too random (with lots of the abundant nutrients and lack of minor rarer ones).
  ;;; Probably best to search for an empirical reference to set one or a few diets.

  ;;; initialise list with a 0 for each foodstuff type
  let desiredDiet map [foodstuffTypeIndex -> 0] nutrientContentsPerFoodstuffTable_foodstuffTypes
  let nutritionScore -1

;  let maxIterations 5000 ; stop iteration if reaching 5 kg of food
;  while [ (maxIterations > 0) and (nutritionScore < 0) ]
;  [
;    ;;; get random foodstuffType index
;    let foodstuffTypeIndex random (length nutrientContentsPerFoodstuffTable_foodstuffTypes)
;
;    ;;; add 100g of the selected foodstuffType
;    let currentAmount item foodstuffTypeIndex desiredDiet
;    set desiredDiet replace-item foodstuffTypeIndex desiredDiet (currentAmount + 1)
;
;    ;;; calculate the current nutrition score
;    set nutritionScore (get-nutrition-score (get-nutrient-contents-in-diet desiredDiet) nutrientsRequired)
;
;    ;print desiredDiet
;    ;print nutritionScore
;
;    set maxIterations maxIterations - 1
;    if (maxIterations = 0) [ print "Warning: failed to calculate sufficient initial diet (more than 5kg required)." ]
;  ]

  ;;; OR

  ;;; set values manually to make it faster during testing
  set desiredDiet (list 114 123 122 122 115 114 131 123 108 113 134 124)

  report desiredDiet

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  ; --- core procedures -------------------------

  reset-counters

  update-households

  ; --- counters and display --------------------

  update-counters

  update-display

  ; -- time -------------------------------------

  ;print "-------tick----"
  tick

  ; --- stop conditions -------------------------

  if (ticks = max-iterations)
  [
    ;if (length behaviorspace-experiment-name > 0 and count households > 0) [ export-households ]
    stop
  ]

end

;;; GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-households

  ask households
  [

    hh_update-diet-consumed

    hh_update-nutrition

    ;stocks-update

  ]

end

;;; HOUSEHOLDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to hh_update-diet-consumed

  set hh_dietConsumed hh_dietDesired ;;; !!! TO DO

end

to-report hh_get-diet-available

  ;report sum hh_stocks ;;; !!! TO DO

end

to hh_update-nutrition

  hh_update-nutrientsRequired

  hh_update-nutrientsInDiet

  set hh_nutritionScore (get-nutrition-score hh_nutrientsInDiet hh_nutrientsRequired)

end

to hh_update-nutrientsRequired

  let membersAgeInYears map [ ageInDays -> get-age-in-years-old ageInDays ] hh_membersAge

  set hh_nutrientsRequired get-nutrient-requirements-of-group membersAgeInYears hh_membersSex hh_pregnancyTrimester hh_lactancySemester hh_menbersPAL

end

to hh_update-nutrientsInDiet

  set hh_nutrientsInDiet get-nutrient-contents-in-diet hh_dietConsumed

end

to-report get-nutrition-score [ nutrientsInDiet nutrientsRequired ]

  ;;; get a list of nutrition scores per nutrient and then aggregate them into a single score

  let scoresPerNutrient (get-nutrition-scores-per-nutrient nutrientsInDiet nutrientsRequired)

  ;;; Option A
  ;;; return the mean of scoresPerNutrient
  let score mean scoresPerNutrient

  ;;; Option BA
  ;;; return a
  ;;; sum all values, knowing that negative and positive values will cancel each other out.
  ;;; The upside of the averaging effect is that neutrality (0) becomes possible, if option A in get-nutrition-scores-per-nutrient is used.
  ;let score sum scoresPerNutrient
  ;;; Rescale score to [-1, 1] interval, assuming they use the scale [-lenghtOfNutrientList, lenghtOfNutrientList]
  ;let lenghtOfNutrientList length nutrientsInDiet
  ;set score (score / lenghtOfNutrientList)

  ;;; Option B
  ;;; return the minimum score, following the rationale of the least consumed required nutrient becomes the limiting factor
  ;;; NOTE: if using option A in get-nutrition-scores-per-nutrient, the nutrition score will always be either -1 or 1 (and often it will be -1)
  ;let score min scoresPerNutrient

  report score

end

to-report get-nutrition-scores-per-nutrient [ nutrientsInDiet nutrientsRequired ]

  ;;; Option A
  ;;; check if each nutrient requirement is fullfilled and assign scores of either -1 (not fullfilled) or 1 (fullfilled)
  ;;; the advantage of this option is that it do not require any extra information, nor assume anything besides the concept of "requirement"
  ;;; the downside is that there is no neutral point.
  ;report (map [ [ nutrientInDiet nutrientRequired ] -> ifelse-value (nutrientInDiet >= nutrientRequired) [ 1 ] [ -1 ] ] nutrientsInDiet nutrientsRequired)

  ;;; Option B
  ;;; compare each nutrient using logistic curve centred at y=0 and x=required amount.
  ;;; For example: (2(1/(1+exp(rate(required-consumed)/required))) - 1)
  ;;; visualise function in Wolfram|Alpha:
  ;;; https://www.wolframalpha.com/input?i2d=true&i=plot+%5C%2840%292%5C%2840%29Divide%5B1%2C%5C%2840%291%2Bexp%5C%2840%2910Divide%5B%5C%2840%29100-x%5C%2841%29%2C100%5D%5C%2841%29%5C%2841%29%5D%5C%2841%29+-+1%5C%2841%29+x%3D0+to+200
  ;;; Rate should be defined on a nutrient basis, or at least on the basis of their scale (mg, micrag).
  ;;; For this option to be viable, we need either more detailed information on nutrition (i.e., the response to under and over ingestion),
  ;;; or, as an intermediate option, to keep track of minimum (the neutral point) and maximum nutrientsRequired and then extrapolate the value of rate (linearly, instead of logistically?)

  let rate 10
  report (map [ [ nutrientInDiet nutrientRequired ] -> (2 * (1 / (1 + (exp (rate * (nutrientRequired - nutrientInDiet) / nutrientRequired)))) - 1) ] nutrientsInDiet nutrientsRequired)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTERS AND MEASURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reset-counters

  set totalWomen 0
  set totalMen 0
  set femaleRatio -1
  set womenAgeStructure (list)
  set menAgeStructure (list)
  set womenFirstAgeGroup 0
  set menFirstAgeGroup 0

end

to update-counters

  set totalHouseholds count households
  let oldTotalIndividual totalIndividuals
  set totalIndividuals 0

  ask households
  [
    foreach hh_membersIndexes
    [
      i ->
      let ageInYears get-age-in-years-old (item i hh_membersAge)
      ifelse (item i hh_membersSex)
      [
        set totalWomen totalWomen + 1
        set womenAgeStructure lput ageInYears womenAgeStructure
        if (ageInYears < 5)
        [ set womenFirstAgeGroup womenFirstAgeGroup + 1 ]
      ]
      [
        set totalMen totalMen + 1
        set menAgeStructure lput ageInYears menAgeStructure
        if (ageInYears < 5)
        [ set menFirstAgeGroup menFirstAgeGroup + 1 ]
      ]
      set totalIndividuals totalIndividuals + 1
    ]
  ]

  carefully [ set femaleRatio totalWomen / totalIndividuals ] [ set femaleRatio "" ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-display

  ask households [ hh_resize-with-nutritionScore ]

end

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

to plot-table-transposed [ values ]

  let j 0
  foreach values
  [
    i ->
    if (i != "") ;;; not null
    [ plotxy i j ]
    set j j + 1
  ]
  plot-pen-up

end

to-report get-itemwise-mean-in-list-of-lists [ listOfLists aggregationFunction ]

  if (length listOfLists = 0)
  [ report 0 ]

  if (length listOfLists = 1)
  [ report item 0 listOfLists ]

  let acceptedAggregationFunctions [ "mean" "median" "max" "min" "standard-deviation"]
  if (not member? aggregationFunction acceptedAggregationFunctions)
  [ error (word "aggregation function name not accepted. Please enter any of the following: " acceptedAggregationFunctions) ]

  let numberOfItems length (item 0 listOfLists)
  let outputList (list)

  foreach (n-values numberOfItems [ i -> i ])
  [
    index ->

    let listOfItemsWithIndex (list)

    foreach listOfLists
    [
      anInputList ->

      set listOfItemsWithIndex lput (item index anInputList) listOfItemsWithIndex
    ]

    let value 0

    if (aggregationFunction = "mean") [ set value mean listOfItemsWithIndex ]
    if (aggregationFunction = "median") [ set value median listOfItemsWithIndex ]
    if (aggregationFunction = "max") [ set value max listOfItemsWithIndex ]
    if (aggregationFunction = "min") [ set value min listOfItemsWithIndex ]
    if (aggregationFunction = "standard-deviation") [ set value standard-deviation listOfItemsWithIndex ]

    set outputList lput value outputList
  ]

  report outputList

end

to print-foodstuff-types-list

  foreach n-values (length nutrientContentsPerFoodstuffTable_foodstuffTypes) [j -> j]
  [
    foodstuffTypeIndex ->
    output-print item foodstuffTypeIndex nutrientContentsPerFoodstuffTable_foodstuffTypes
  ]

end

to hh_resize-with-nutritionScore

  set size 1 + hh_nutritionScore * 0.5

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

  ;;; reference body weight (kg)
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

  ;;; protein (g/kg of body weight)
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
  set nutrientRequirementsTable_mineralColumnNames sublist (item 4 nutrientsRequirementTable) (item 0 mineralColumns) (item 1 mineralColumns + 1)
  set nutrientRequirementsTable_vitaminColumnNames sublist (item 4 nutrientsRequirementTable) (item 0 vitaminsColumns) (item 1 vitaminsColumns + 1)
  ;;; get lists of mineral and vitamins names, without the min/max suffixes
  ;;; NOTE: for now, there is no max and min for these in nutrientRequirementsTable.
  set nutrientRequirementsTable_mineralNames remove-duplicates map [columnName -> remove " max" (remove " min" columnName)] nutrientRequirementsTable_mineralColumnNames
  set nutrientRequirementsTable_vitaminNames remove-duplicates map [columnName -> remove " max" (remove " min" columnName)] nutrientRequirementsTable_vitaminColumnNames

  ;;; read variables (list of lists, matrix: sex-age groups x nutrient variables)
  let nutrientsRequirementData sublist nutrientsRequirementTable (item 0 ageGroupsRowRange) (item 1 ageGroupsRowRange + 1)

  ;;; extract and combine/codify age and sex columns
  set nutrientRequirementsTable_ageAndSexGroup (
    map [row -> (word (item sexGroupColumn row) " | " (item ageGroupColumn row) ) ] nutrientsRequirementData
    )

  ;;; major nutrients as separate list and list of lists variables

  ;;; reference body weight (kg)
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

  ;;; protein PRI (g/kg of body weight)
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

  ;;; calculate the total number of nutrients accounted in this table (saved for further use)
  set nutrientRequirementsTable_numberOfNutrients 6 + (length nutrientRequirementsTable_mineralNames) + (length nutrientRequirementsTable_vitaminNames)

end

to-report get-nutrient-requirements-of-group [ peopleAge peopleSex peoplePregnancyTrimester peopleLactancySemester peoplePAL ]

  ;;; Returns a list of amounts per nutrient required by a group of people, given lists of people ages, sex, pregnancy trimester, lactancy semester, and PAL (physical activity level).

  ;;; Initialise list of nutrients
  let listOfNutrientAmountsRequiredByGroup n-values nutrientRequirementsTable_numberOfNutrients [ i -> 0 ]

  foreach n-values (length peopleAge) [ i -> i ]
  [
    personIndex ->

    let personAge (item personIndex peopleAge)
    let personSex ifelse-value (item personIndex peopleSex) [ "female" ] [ "male" ] ;;; this converts the true/false values into the "female"/"male" of the nutrientRequirementsTable
    let personPregnancyTrimester (item personIndex peoplePregnancyTrimester)
    let personLactancySemester (item personIndex peopleLactancySemester)
    let personPAL (item personIndex peoplePAL)

    ;;; get nutrition requirements per pèrson according to their sex, age, pregnancy or lactancy status, and physical activity level
    ;;; Nutrient with minimum and maximum values in nutrientRequirementsTable are sampled randomly from an uniform probability distribution between these.
    let listOfNutrientAmountsRequiredByPerson (get-nutrient-requirements-of-person personAge personSex personPregnancyTrimester personLactancySemester personPAL)

    ;;; add values to the list of nutrient content amounts required by the group
    set listOfNutrientAmountsRequiredByGroup (map [ [ i j ] -> i + j ] listOfNutrientAmountsRequiredByGroup listOfNutrientAmountsRequiredByPerson)
  ]

  report listOfNutrientAmountsRequiredByGroup

end

to-report get-nutrient-requirements-of-person [ personAge personSex personPregnancyTrimester personLactancySemester personPAL ]

  let sexAgeGroupIndex get-index-of-sex-age-group personAge personSex personPregnancyTrimester personLactancySemester

  let listOfNutrientAmountRequired (list
    (get-water-requirement-of-person sexAgeGroupIndex)             ;;; water
    (get-energy-requirement-of-person sexAgeGroupIndex personPAL)  ;;; energy
    (get-CHO-requirement-of-person sexAgeGroupIndex personPAL)     ;;; carbohydrates (CHO)
    (get-fibre-requirement-of-person sexAgeGroupIndex)             ;;; fibre
    (get-fat-requirement-of-person sexAgeGroupIndex personPAL)     ;;; fat
    (get-protein-requirement-of-person sexAgeGroupIndex)           ;;; protein
    )

  foreach nutrientRequirementsTable_mineralNames
  [
    mineralName ->
    set listOfNutrientAmountRequired lput (get-mineral-requirement-of-person sexAgeGroupIndex mineralName) listOfNutrientAmountRequired
  ]

  foreach nutrientRequirementsTable_vitaminNames
  [
    vitaminName ->
    set listOfNutrientAmountRequired lput (get-vitamin-requirement-of-person sexAgeGroupIndex vitaminName) listOfNutrientAmountRequired
  ]

  report listOfNutrientAmountRequired

end

to-report get-nutrient-requirement-of-person [ nutrientName personAge personSex pregnancyTrimester lactancySemester personPAL ]

  let sexAgeGroupIndex get-index-of-sex-age-group personAge personSex pregnancyTrimester lactancySemester

  if (nutrientName = "water")
  [
    report (get-water-requirement-of-person sexAgeGroupIndex)
  ]
  if (nutrientName = "energy")
  [
    report (get-energy-requirement-of-person sexAgeGroupIndex personPAL)
  ]
  if (nutrientName = "carbohydrates")
  [
    report (get-CHO-requirement-of-person sexAgeGroupIndex personPAL)
  ]
  if (nutrientName = "fibre")
  [
    report (get-fibre-requirement-of-person sexAgeGroupIndex)
  ]
  if (nutrientName = "fat")
  [
    report (get-fat-requirement-of-person sexAgeGroupIndex personPAL)
  ]
  if (nutrientName = "protein")
  [
    report (get-protein-requirement-of-person sexAgeGroupIndex)
  ]
  if (member? nutrientName nutrientRequirementsTable_mineralNames)
  [
    report (get-mineral-requirement-of-person sexAgeGroupIndex nutrientName)
  ]
  if (member? nutrientName nutrientRequirementsTable_vitaminNames)
  [
    report (get-vitamin-requirement-of-person sexAgeGroupIndex nutrientName)
  ]

  error (word "ERROR (get-nutrient-requirement-of-person): " nutrientName " not recognised.")

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
  if (nutrientName = "protein (g/kg of body weight)")
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

to-report get-water-requirement-of-person [ sexAgeGroupIndex ]

  report (item sexAgeGroupIndex nutrientRequirementsTable_water)

end

to-report get-energy-requirement-of-person [ sexAgeGroupIndex personPAL ]

  let palIndex position personPAL [ "1.4" "1.6" "1.8" "2.0" ]

  report (item sexAgeGroupIndex (item palIndex nutrientRequirementsTable_energy)) * 1000 ;;; convert from MJ to kJ

end

to-report get-CHO-requirement-of-person [ sexAgeGroupIndex personPAL ]

  let totalE% (get-energy-requirement-of-person sexAgeGroupIndex personPAL)

  let minValue (item sexAgeGroupIndex (item 0 nutrientRequirementsTable_CHO))
  let maxValue (item sexAgeGroupIndex (item 1 nutrientRequirementsTable_CHO))
  let CHOE% get-random-float-in-range minValue maxValue

  report (get-g-from-E%-CHO CHOE% totalE%)

end

to-report get-fat-requirement-of-person [ sexAgeGroupIndex personPAL ]

  let totalE% (get-energy-requirement-of-person sexAgeGroupIndex personPAL)

  let minValue (item sexAgeGroupIndex (item 0 nutrientRequirementsTable_fat))
  let maxValue (item sexAgeGroupIndex (item 1 nutrientRequirementsTable_fat))
  let fatE% get-random-float-in-range minValue maxValue

  report (get-g-from-E%-fat fatE% totalE%)

end

to-report get-g-from-E%-CHO [ E% totalE ]

  ;;; define Atwater general factor system's value for carbohydrates
  ;;; NOTE: this value includes fibre.
  ;;; See: CHAPTER 3: CALCULATION OF THE ENERGY CONTENT OF FOODS - ENERGY CONVERSION FACTORS. (2003).
  ;;; In Food energy—Methods of analysis and conversion factors. Report of a Technical Workshop, Rome, 3-6 December 2002. FAO.
  ;;; https://www.fao.org/3/y5022e/y5022e04.htm

  let AtwaterGeneralFactor_CHO 17 ;;; 17 kJ/g or 4 kcal/g

  ;;; NOTE: assuming totalE is in kJ
  report totalE / AtwaterGeneralFactor_CHO

end

to-report get-g-from-E%-fat [ E% totalE ]

  ;;; define Atwater general factor system's value for fat
  ;;; NOTE: this value includes fibre.
  ;;; See: CHAPTER 3: CALCULATION OF THE ENERGY CONTENT OF FOODS - ENERGY CONVERSION FACTORS. (2003).
  ;;; In Food energy—Methods of analysis and conversion factors. Report of a Technical Workshop, Rome, 3-6 December 2002. FAO.
  ;;; https://www.fao.org/3/y5022e/y5022e04.htm

  let AtwaterGeneralFactor_fat 37 ;;; 37 kJ/g or 9 kcal/g

  ;;; NOTE: assuming totalE is in kJ
  report totalE / AtwaterGeneralFactor_fat

end

to-report get-fibre-requirement-of-person [ sexAgeGroupIndex ]

  report (item sexAgeGroupIndex nutrientRequirementsTable_fibre)

end

to-report get-protein-requirement-of-person [ sexAgeGroupIndex ]

  let referenceBodyWeight (item sexAgeGroupIndex nutrientRequirementsTable_referenceBodyWeight)

  let proteinPerBodykg (item sexAgeGroupIndex nutrientRequirementsTable_protein)

  report referenceBodyWeight * proteinPerBodykg

end

to-report get-mineral-requirement-of-person [ sexAgeGroupIndex mineralVariableName ]

  ;;; Returns the value at the given (row) index for the given mineral variable from the nutrientRequirementsTable

  let mineralVariableIndex (position mineralVariableName nutrientRequirementsTable_mineralColumnNames)

  report (item sexAgeGroupIndex (item mineralVariableIndex nutrientRequirementsTable_minerals))

end

to-report get-vitamin-requirement-of-person [ sexAgeGroupIndex vitaminVariableName ]

  ;;; Returns the value at the given (row) index for the given vitamin variable from the nutrientRequirementsTable

  let vitaminVariableIndex (position vitaminVariableName nutrientRequirementsTable_vitaminColumnNames)

  report (item sexAgeGroupIndex (item vitaminVariableIndex nutrientRequirementsTable_vitamins))

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

  ;;; get lists of mineral and vitamins column names (min and max) in the order they are in the table as columns
  set nutrientContentsPerFoodstuffTable_mineralColumnNames sublist (item 4 nutrientContentsPerFoodstuffTable) (item 0 mineralColumns) (item 1 mineralColumns + 1)
  set nutrientContentsPerFoodstuffTable_vitaminColumnNames sublist (item 4 nutrientContentsPerFoodstuffTable) (item 0 vitaminsColumns) (item 1 vitaminsColumns + 1)
  ;;; get lists of mineral and vitamins names, without the min/max suffixes
  set nutrientContentsPerFoodstuffTable_mineralNames remove-duplicates map [columnName -> remove " max" (remove " min" columnName)] nutrientContentsPerFoodstuffTable_mineralColumnNames
  set nutrientContentsPerFoodstuffTable_vitaminNames remove-duplicates map [columnName -> remove " max" (remove " min" columnName)] nutrientContentsPerFoodstuffTable_vitaminColumnNames

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

  ;;; calculate the total number of nutrients accounted in this table (saved for further use)
  set nutrientContentsPerFoodstuffTable_numberOfNutrients 6 + (length nutrientContentsPerFoodstuffTable_mineralNames) + (length nutrientContentsPerFoodstuffTable_vitaminNames)

end

to-report get-nutrient-contents-in-diet [ listOfFoodstuffAmountInDiet ]

  ;;; Returns a list of amounts per nutrient given a list of amounts per foodstuff in grams.

  ;;; Initialise list of nutrients
  let listOfNutrientAmountsInDiet n-values nutrientContentsPerFoodstuffTable_numberOfNutrients [ i -> 0 ]

  foreach nutrientContentsPerFoodstuffTable_foodstuffTypes
  [
    foodstuffType ->

    let foodstuffTypeIndex (position foodstuffType nutrientContentsPerFoodstuffTable_foodstuffTypes)

    ;;; get nutrition content values per 100g of this foodstuff
    ;;; Nutrient amounts are sampled out of an uniform probability distribution between the minimum and maximum values determined in nutrientContentsPerFoodstuffTable.
    let nutrientContentsPer100g (get-nutrient-contents-of-foodstuff-type foodstuffType)

    ;;; calculate edible amount
    let edibleAmount (get-edible-amount-in-foodstuff-type foodstuffTypeIndex) * (item foodstuffTypeIndex listOfFoodstuffAmountInDiet)

    ;;; calculate total nutrition content values for edible portion and given foodstuff amount
    let totalNutrientContents map [ i -> i * (edibleAmount / 100) ] nutrientContentsPer100g

    ;;; add values to the overall list of nutrient contents in diet
    set listOfNutrientAmountsInDiet (map [ [ i j ] -> i + j ] listOfNutrientAmountsInDiet totalNutrientContents)
  ]

  report listOfNutrientAmountsInDiet

end

to-report get-nutrient-contents-of-foodstuff-type [ foodstuffType ]

  ;;; Returns a list of nutrient content values corresponding to the given foodstuff type (row) in nutrientContentsPerFoodstuffTable.
  ;;; Values are sampled randomly from an uniform probability distribution between minimum and maximum values in nutrientRequirementsTable.

  let foodstuffTypeIndex (position foodstuffType nutrientContentsPerFoodstuffTable_foodstuffTypes)

  let listOfNutrientAmountsInFoodstuffType (list
    (get-water-content-of-foodstuffType foodstuffTypeIndex)
    (get-energy-content-of-foodstuffType foodstuffTypeIndex)
    (get-CHO-content-of-foodstuffType foodstuffTypeIndex)
    (get-fibre-content-of-foodstuffType foodstuffTypeIndex)
    (get-fat-content-of-foodstuffType foodstuffTypeIndex)
    (get-protein-content-of-foodstuffType foodstuffTypeIndex)
    )

  foreach nutrientContentsPerFoodstuffTable_mineralNames
  [
    mineralName ->
    set listOfNutrientAmountsInFoodstuffType lput (get-mineral-content-of-foodstuffType foodstuffTypeIndex mineralName) listOfNutrientAmountsInFoodstuffType
  ]

  foreach nutrientContentsPerFoodstuffTable_vitaminNames
  [
    vitaminName ->
    set listOfNutrientAmountsInFoodstuffType lput (get-vitamin-content-of-foodstuffType foodstuffTypeIndex vitaminName) listOfNutrientAmountsInFoodstuffType
  ]

  report listOfNutrientAmountsInFoodstuffType

end

to-report get-edible-amount-in-foodstuff-type [ foodstuffTypeIndex ]

  report (item foodstuffTypeIndex nutrientContentsPerFoodstuffTable_ediblePortion)

end

to-report get-water-content-of-foodstuffType [ foodstuffTypeIndex ]

  let minValue (item foodstuffTypeIndex (item 0 nutrientContentsPerFoodstuffTable_water))
  let maxValue (item foodstuffTypeIndex (item 1 nutrientContentsPerFoodstuffTable_water))

  report get-random-float-in-range minValue maxValue

end

to-report get-energy-content-of-foodstuffType [ foodstuffTypeIndex ]

  let minValue (item foodstuffTypeIndex (item 0 nutrientContentsPerFoodstuffTable_energy))
  let maxValue (item foodstuffTypeIndex (item 1 nutrientContentsPerFoodstuffTable_energy))

  report get-random-float-in-range minValue maxValue

end

to-report get-CHO-content-of-foodstuffType [ foodstuffTypeIndex ]

  let minValue (item foodstuffTypeIndex (item 0 nutrientContentsPerFoodstuffTable_CHO))
  let maxValue (item foodstuffTypeIndex (item 1 nutrientContentsPerFoodstuffTable_CHO))

  report get-random-float-in-range minValue maxValue

end

to-report get-fat-content-of-foodstuffType [ foodstuffTypeIndex ]

  let minValue (item foodstuffTypeIndex (item 0 nutrientContentsPerFoodstuffTable_fat))
  let maxValue (item foodstuffTypeIndex (item 1 nutrientContentsPerFoodstuffTable_fat))

  report get-random-float-in-range minValue maxValue

end

to-report get-fibre-content-of-foodstuffType [ foodstuffTypeIndex ]

  let minValue (item foodstuffTypeIndex (item 0 nutrientContentsPerFoodstuffTable_fibre))
  let maxValue (item foodstuffTypeIndex (item 1 nutrientContentsPerFoodstuffTable_fibre))

  report get-random-float-in-range minValue maxValue

end

to-report get-protein-content-of-foodstuffType [ foodstuffTypeIndex ]

  let minValue (item foodstuffTypeIndex (item 0 nutrientContentsPerFoodstuffTable_protein))
  let maxValue (item foodstuffTypeIndex (item 1 nutrientContentsPerFoodstuffTable_protein))

  report get-random-float-in-range minValue maxValue

end

to-report get-mineral-content-of-foodstuffType [ foodstuffIndex mineralName ]

  ;;; Returns a random value between minimum and maximum values at the given (row) index for the given mineral variable from the nutrientContentsPerFoodstuffTable

  let mineralVariableIndexMin (position (word mineralName " min") nutrientContentsPerFoodstuffTable_mineralColumnNames)
  let mineralVariableIndexMax (position (word mineralName " max") nutrientContentsPerFoodstuffTable_mineralColumnNames)

  let minValue (item foodstuffIndex (item mineralVariableIndexMin nutrientContentsPerFoodstuffTable_minerals))
  let maxValue (item foodstuffIndex (item mineralVariableIndexMax nutrientContentsPerFoodstuffTable_minerals))

  report get-random-float-in-range minValue maxValue

end

to-report get-vitamin-content-of-foodstuffType [ foodstuffIndex vitaminName ]

  ;;; Returns a random value between minimum and maximum values at the given (row) index for the given vitamin variable from the nutrientContentsPerFoodstuffTable

  let vitaminVariableIndexMin (position (word vitaminName " min") nutrientContentsPerFoodstuffTable_vitaminColumnNames)
  let vitaminVariableIndexMax (position (word vitaminName " max") nutrientContentsPerFoodstuffTable_vitaminColumnNames)

  let minValue (item foodstuffIndex (item vitaminVariableIndexMin nutrientContentsPerFoodstuffTable_vitamins))
  let maxValue (item foodstuffIndex (item vitaminVariableIndexMax nutrientContentsPerFoodstuffTable_vitamins))

  report get-random-float-in-range minValue maxValue

end

to-report get-nutrient-content-of-foodstuff-types [ nutrientName minOrMax ]

  ;;; Returns all (min or max) content of the given nutrient for all foodstuff types in nutrientContentsPerFoodstuffTable.
  ;;; NOTE: Useful for querying the table and plotting

  let suffix (word " " minOrMax)
  let minOrMaxIndex position minOrMax [ "min" "max" ]

  if (nutrientName = "water (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_water) ]
  if (nutrientName = "energy (kJ)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_energy) ]
  if (nutrientName = "carbohydrates (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_CHO) ]
  if (nutrientName = "fibre (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_fibre) ]
  if (nutrientName = "fat (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_fat) ]
  if (nutrientName = "protein (g)")
  [ report (item minOrMaxIndex nutrientContentsPerFoodstuffTable_protein) ]

  let variableName (substring nutrientName 0 (position "(" nutrientName))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERIC FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-random-float-in-range [ minValue maxValue ]

  report (minValue + random-float (maxValue - minValue))

end
@#$#@#$#@
GRAPHICS-WINDOW
226
10
538
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
308
138
368
initial-num-households
10.0
1
0
Number

MONITOR
26
372
127
409
NIL
initialNumHouseholds
0
1
9

PLOT
21
437
425
557
Age structure (population)
age
NIL
0.0
10.0
0.0
10.0
true
true
"set-histogram-num-bars 20\nset-plot-x-range -1 max (sentence womenAgeStructure menAgeStructure)" "set-plot-y-range 0 10\n;set-histogram-num-bars 20\nset-plot-x-range -1 max (sentence womenAgeStructure menAgeStructure)"
PENS
"women" 1.0 1 -2674135 true "" "histogram womenAgeStructure"
"men" 1.0 1 -13345367 true "" "histogram menAgeStructure"

MONITOR
427
436
500
481
NIL
femaleRatio
4
1
11

MONITOR
415
480
515
525
% 0-4 (women)
100 * womenFirstAgeGroup / totalWomen
2
1
11

MONITOR
415
524
515
569
% 0-4 (men)
100 * menFirstAgeGroup / totalMen
2
1
11

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

PLOT
636
317
937
543
Nutrient per sex-age
age group
nutrients required
0.0
10.0
0.0
10.0
false
true
"" "clear-plot\nif (nutrientRequirementsTable_ageAndSexGroup = 0) [ load-nutrient-requirements-table ]\nset-plot-x-range -1 100\nlet nutrientPerGroupFemale get-nutrient-requirement-per-sex-group required-nutrient-to-plot \"female\"\nlet nutrientPerGroupMale get-nutrient-requirement-per-sex-group required-nutrient-to-plot \"male\"\nset-plot-y-range -0.001 (precision (max (list (max nutrientPerGroupFemale) (max nutrientPerGroupMale)) + 0.001) 3)"
PENS
"females" 1.0 0 -5298144 true "\n" "plot-table get-nutrient-requirement-per-sex-group required-nutrient-to-plot \"female\""
"males" 1.0 0 -14070903 true "" "plot-table get-nutrient-requirement-per-sex-group required-nutrient-to-plot \"male\""

CHOOSER
637
545
938
590
required-nutrient-to-plot
required-nutrient-to-plot
"water (L)" "energy PAL=1.4" "energy PAL=1.6" "energy PAL=1.8" "energy PAL=2.0" "carbohydrates (E%) min." "carbohydrates (E%) max." "fibre (g)" "fat (E%) min." "fat (E%) max." "protein (g/kg of body weight)" "Calcium PRI (mg)" "Iron PRI (mg)" "Zinc PRI (mg)" "Copper AI (mg)" "Magnesium AI (mg)" "Fluoride AI (mg)" "Iodine AI (micrag)" "Manganese AI (mg)" "Molybdenum AI (micrag)" "Phosphorus AI (mg)" "Potassium AI (mg)" "Selenium AI (micrag)" "Vitamin A PRI (micrag)" "Vitamin B1 PRI (mg/MJ)" "Vitamin B2 PRI (mg)" "Vitamin B3 PRI (micrag/MJ)" "Vitamin B5 AI (mg)" "Vitamin B6 PRI (mg)" "Vitamin B7 AI (micrag)" "Vitamin B9 PRI (micrag)" "Vitamin B12 AI (micrag)" "Vitamin C PRI (micrag)" "Vitamin D AI (micrag)" "Vitamin E AI (mg)" "Vitamin K AI (micrag)" "Choline AI (mg)"
7

BUTTON
526
356
628
389
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
939
317
1239
543
Nutrient per foodstuff types
foodstuff type index
nutrient amount per 100g
0.0
10.0
0.0
10.0
true
true
"" "clear-plot\nif (nutrientContentsPerFoodstuffTable_foodstuffTypes = 0) [ load-nutrient-contents-per-foodstuff-table ]\nset-plot-x-range -1 ((length nutrientContentsPerFoodstuffTable_foodstuffTypes) + 1)\nlet nutrientPerFoodstuffMax (max (get-nutrient-content-of-foodstuff-types nutrient-content-to-plot \"max\"))\nset-plot-y-range -0.001 (precision (nutrientPerFoodstuffMax + 0.001) 3)"
PENS
"min" 1.0 1 -16777216 true "" "plot-table get-nutrient-content-of-foodstuff-types nutrient-content-to-plot \"min\""
"max" 1.0 1 -7500403 true "" "plot-table get-nutrient-content-of-foodstuff-types nutrient-content-to-plot \"max\""

CHOOSER
940
546
1240
591
nutrient-content-to-plot
nutrient-content-to-plot
"water (g)" "energy (kJ)" "carbohydrates (g)" "fibre (g)" "fat (g)" "protein (g)" "Calcium (mg)" "Iron (mg)" "Zinc (mg)" "Copper (mg)" "Magnesium (mg)" "Fluoride (mg)" "Iodine (micrag)" "Manganese (mg)" "Molybdenum (micrag)" "Phosphorus (mg)" "Potassium (mg)" "Selenium (micrag)" "Vitamin A (micrag)" "Vitamin B1 (mg)" "Vitamin B2 (mg)" "Vitamin B3 (mg)" "Vitamin B5 (mg)" "Vitamin B6 (mg)" "Vitamin B7 (micrag)" "Vitamin B9 (micrag)" "Vitamin B12 AI (micrag)" "Vitamin C (micrag)" "Vitamin D (micrag)" "Vitamin E (mg)" "Vitamin K (micrag)" "Choline (mg)"
4

INPUTBOX
146
344
365
404
household-initial-age-distribution
0 30
1
0
String

TEXTBOX
154
332
310
355
<minimum><SPACE><maximum>
9
0.0
1

MONITOR
370
355
512
392
NIL
houseHoldInitialAgeDistribution
17
1
9

PLOT
1175
34
1398
312
Diet satisfaction
mass unit
foodstuff type index
0.0
10.0
0.0
10.0
true
true
"" "clear-plot"
PENS
"desired mean" 1.0 0 -16777216 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_dietDesired] of households) \"mean\"" "plot-table-transposed get-itemwise-mean-in-list-of-lists ([hh_dietDesired] of households) \"mean\""
"desired min" 1.0 0 -14070903 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_dietDesired] of households) \"min\"" "plot-table-transposed get-itemwise-mean-in-list-of-lists ([hh_dietDesired] of households) \"min\""
"desired max" 1.0 0 -5298144 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_dietDesired] of households) \"max\"" "plot-table-transposed get-itemwise-mean-in-list-of-lists ([hh_dietDesired] of households) \"max\""
"consumed mean" 1.0 0 -7500403 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_dietConsumed] of households) \"mean\"" "plot-table-transposed get-itemwise-mean-in-list-of-lists ([hh_dietConsumed] of households) \"mean\""
"consumed min" 1.0 0 -10649926 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_dietConsumed] of households) \"min\"" "plot-table-transposed get-itemwise-mean-in-list-of-lists ([hh_dietConsumed] of households) \"min\""
"consumed max" 1.0 0 -2139308 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_dietConsumed] of households) \"max\"" "plot-table-transposed get-itemwise-mean-in-list-of-lists ([hh_dietConsumed] of households) \"max\""

PLOT
543
133
872
292
Nutrition satisfaction
nutrient index
NIL
0.0
10.0
0.0
10.0
true
true
"" "clear-plot"
PENS
"required mean" 1.0 0 -16777216 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsRequired] of households) \"mean\"" "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsRequired] of households) \"mean\""
"required min" 1.0 0 -14070903 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsRequired] of households) \"min\"" "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsRequired] of households) \"min\""
"required max" 1.0 0 -5298144 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsRequired] of households) \"max\"" "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsRequired] of households) \"max\""
"in-diet mean" 1.0 0 -7500403 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsInDiet] of households) \"mean\"" "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsInDiet] of households) \"mean\""
"in-diet min" 1.0 0 -10649926 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsInDiet] of households) \"min\"" "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsInDiet] of households) \"min\""
"in-diet max" 1.0 0 -2139308 true "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsInDiet] of households) \"max\"" "plot-table get-itemwise-mean-in-list-of-lists ([hh_nutrientsInDiet] of households) \"max\""

OUTPUT
887
34
1175
312
11

TEXTBOX
984
10
1109
28
Foodstuff types
14
0.0
1

PLOT
542
10
871
130
Nutrition score
ticks
NIL
0.0
10.0
0.0
2.0
true
true
"set-plot-y-range -2 2" ""
PENS
"mean" 1.0 0 -16777216 true "" "plot mean [hh_nutritionScore] of households"
"min" 1.0 0 -13345367 true "" "plot min [hh_nutritionScore] of households"
"max" 1.0 0 -2674135 true "" "plot max [hh_nutritionScore] of households"

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
NetLogo 6.2.2
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
