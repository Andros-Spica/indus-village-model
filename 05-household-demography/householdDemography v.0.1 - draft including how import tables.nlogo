;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  <MODEL NAME>
;;  Copyright (C) <YEAR> <AUTHORS (EMAIL)>
;;  Based on the 'householdDemography' template by Andreas Angourakis (andros.spica@gmail.com), 2018
;;  available at https://www.github.com/Andros-Spica/abm-templates
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

extensions [ Rnd ]

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
  ;;;; Demography tables
  fertilityMinMax                 ; (list min max) the minimum and maximum age considered in the fertility table (list of integer)
  fertilityCohortSize             ; the size in years of age groups (cohort) in the fertility table (integer)
  nuptialityMinMax                 ; (list min max) the minimum and maximum age considered in the nuptiality table (list of integer)
  nuptialityCohortSize             ; the size in years of age groups (cohort) in the nuptiality table (integer)
  mortalityMinMax                 ; (list min max) the minimum and maximum age considered in the mortality table (list of integer)
  mortalityCohortSize             ; the size in years of age groups (cohort) in the mortality table (integer)
  womenFertilityTable             ; (list cohort1 cohort2 ... )
  womenNuptialityTable            ; (list cohort1 cohort2 ... )
  womenMortalityTable             ; (list cohort1 cohort2 ... )
  menNuptialityTable              ; (list cohort1 cohort2 ... )
  menMortalityTable               ; (list cohort1 cohort2 ... )

  ;;; modified parameters
  initialNumHouseholds
  householdInitialAge             ; (list minimum maximum)
  maxCoupleCountDistribution      ; (list minimum maximum)

  ;;; variables
  ;;;; auxiliar
  womenToMarry                   ; list of string-coded individuals ("<household who> <member index>")
  menToMarry

  ;;;; counters and final measures
  totalHouseholds                ; count households (integer)
  totalIndividuals               ; sum of members of households (integer)
  naturalPopulationGrowth        ; % of last totalIndividuals
  totalWomen                     ; sum of female members of households (integer)
  totalMen                       ; sum of male members of households (integer)
  femaleRatio                    ; total number of women over total number of individuals
  womenAgeStructure              ; merge list of all households female members ages (list of integers)
  menAgeStructure                ; merge list of all households male members ages (list of integers)
  womenFirstAgeGroup             ; count of women with age from 0 to 4 years old (base of typical population pyramid)
  menFirstAgeGroup               ; count of men with age from 0 to 4 years old (base of typical population pyramid)
  womenBirths                    ; number of births (integer)
  menBirths
  womenDeaths                    ; number of deaths (integer)
  menDeaths
]

;;; agents variables

households-own
[
  hh_householdAge ; number of years during which the household existed (integer)
  hh_maxCoupleCount ; max. number of couples accepted within a household (integer)
  hh_membersAge ; ages of every household member (list of integer)
  hh_membersSex ; sex of every household member (list of true/false, i.e. is female?)
  hh_membersMarriage ; couple index of every member (list of integers; 0-Inf: couple index, -1: member is single)

  ;;; auxiliar
  hh_memberDataToDelete
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  set-parameters

  set-tables

  setup-households

  update-counters

  refresh-view

  reset-ticks

end

to set-parameters

  ; set random seed
  random-seed SEED

  ;;; setup parameters depending on the type of experiment
  set householdInitialAge read-from-string ( word "[" household-initial-age "]")
  set maxCoupleCountDistribution read-from-string ( word "[" max-couple-count-distribution "]")

  if (experiment-type = "user-defined")
  [
    ;;; load parameters from user interface
    set initialNumHouseholds initial-num-households
  ]
  if (experiment-type = "random")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    set initialNumHouseholds 1 + random initial-num-households ; at least one household
    set householdInitialAge (list
      (1 + random (item 0 householdInitialAge))   ; minimum
      (
        (item 0 householdInitialAge)
        + 1
        + random ((item 1 householdInitialAge) - (item 0 householdInitialAge))
      )   ; maximum
      )
    set maxCoupleCountDistribution (list
      (1 + random (item 0 maxCoupleCountDistribution))   ; minimum
      (
        (item 0 householdInitialAge)
        + 1
        + random ((item 1 maxCoupleCountDistribution) - (item 0 maxCoupleCountDistribution))
      )   ; maximum
      )
  ]

  ; check parameters values
  parameters-check

end

to parameters-check

  ;;; initial parameter check (e.g., avoiding division per zero error)
  check-par-is-positive "initialNumHouseholds" initialNumHouseholds

  ;;; check if given min values are less than max values
  check-par-is-range "householdInitialAge" householdInitialAge
  check-par-is-range "maxCoupleCountDistribution" maxCoupleCountDistribution

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

to set-tables

  load-demography-tables

  ; check tables values
  tables-check

end

to tables-check

  ;;; check that tables have data and it adds to a positive sum
  check-table-loaded "womenFertilityTable" womenFertilityTable

  check-table-loaded "womenNuptialityTable" womenNuptialityTable
  check-table-loaded "menNuptialityTable" menNuptialityTable

  check-table-loaded "womenMortalityTable" womenMortalityTable
  check-table-loaded "menMortalityTable" menMortalityTable

end

to check-table-loaded [ tableName tableValues ]

  if (tableValues = 0) ; which means that the data was not loaded at all
  [
    print (word "ERROR: " tableName " was not loaded. Please verify the content of the data file.")
    stop
  ]

  if (sum tableValues <= 0) ; which means that the values were not read correctly
  [
    print (word "ERROR: " tableName " total is less or equal to 0. Please verify the original data file.")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  reset-counters

  update-households

  update-counters

  if (totalHouseholds = 0) [ stop ]

  tick

end

;;; GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-households

  apply-mortality

  apply-nuptiality

  apply-fertility

end

to apply-mortality

  ask households
  [
    hh_aging
  ]

end

to apply-nuptiality

  ; initialize population lists of marrying candidates for this year
  set womenToMarry (list)
  set menToMarry (list)

  ; fill lists
  ask households
  [
    hh_set-members-to-marry

    set hh_memberDataToDelete (list)
  ]

  ; match every women to marry with a men to marry until there is no combinations possible
  let matchIndex (n-values length womenToMarry [ i -> i ])
  foreach matchIndex
  [
    i ->

    if (i < length menToMarry - 1) ; if there is possible match
    [
      create-couple i
    ]
  ]

  ; delete recently married individuals from their parent household
  ask households
  [
    hh_delete-members-in-cue
  ]

end

to apply-fertility

  ask households
  [
    hh_reproduction
  ]

end

to create-couple [ index ]

  ; load woman and man data according to index of the womenToMarry or menToMarry lists
  let womanData item index womenToMarry ; list holding the household and member index
  let manData item index menToMarry

  let womanHousehold item 0 womanData
  let womanIndex item 1 womanData
  let manHousehold item 0 manData
  let manIndex item 1 manData

  ;print (word "index " womanIndex " in " [hh_membersAge] of womanHousehold ", female, " womanHousehold)
  ;print (word "index " manIndex " in " [hh_membersAge] of manHousehold ", male, " manHousehold)

  if (residence-rule = "patrilocal-patrilineal")
  [
    ask manHousehold
    [
      hh_try-to-add-couple manIndex womanData
    ]
  ]
  if (residence-rule = "matrilocal-matrilineal")
  [
    ask womanHousehold
    [
      hh_try-to-add-couple womanIndex manData
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HOUSEHOLDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialisation ==============================================================================
to hh_initialise

  ;;; Initialization of households

  set hh_householdAge hh_get-initial-age
  set hh_maxCoupleCount hh_get-initial-max-couple-count

  hh_initialise-members

  ;;; move to empty or less crowded patch, just in sake of visualisation
  move-to min-one-of patches [count households-here]

end

to-report hh_get-initial-age

  report item 0 householdInitialAge + random (item 1 householdInitialAge)

end

to-report hh_get-initial-max-couple-count

  report item 0 maxCoupleCountDistribution + random (item 1 maxCoupleCountDistribution)

end

to hh_initialise-members

  hh_reset-members

  ; assures that at least two member are above (fertility age + household age) and have different sex
  foreach (list 0 1)
  [
    ?1 ->
    set hh_membersSex lput (?1 = 0) hh_membersSex
    let marriageAge (hh_get-initial-marriage-age (item ?1 hh_membersSex))
    set hh_membersAge lput (hh_householdAge + marriageAge) hh_membersAge
    set hh_membersMarriage lput 0 hh_membersMarriage ; this couple will have the 0 index
  ]

  ; calculate offspring and offspring age
  let offspringProb 0
  let i hh_householdAge
  repeat hh_householdAge
  [
    ; get the probability of the couple having descendence for a past year i
    ; i.e. woman fertility at the corresponding age
    set offspringProb get-fertility ((item 0 hh_membersAge) - i)

    ; test the probability and add the offspring with the corresponding age
    if (random-float 1 < offspringProb)
    [
      hh_add-offspring i ; a child with i years old
    ]
    set i i - 1
  ]

end

to-report hh_get-initial-marriage-age [ isFemale ]

  ; get a marriage age as a stochastic function of nuptiality rates in earlier years
  let notMarried true
  let i item 0 nuptialityMinMax

  while [notMarried AND i < item 1 nuptialityMinMax]
  [
    if ((get-nuptiality isFemale i) > random-float 1)
    [
      set notMarried false
    ]
    set i i + 1
  ]

  report i

end

; Main procedures ==============================================================================
to hh_aging

  ; household aging
  set hh_householdAge hh_householdAge + 1

  ; members aging
  hh_update-members-age

  ; apply mortality rate
  hh_update-members-survival

end

to hh_update-members-age

  ; household members aging
  set hh_membersAge map [ i -> i + 1 ] hh_membersAge

end

to hh_update-members-survival

  ; applies age-specific mortality rates and delete dying members

  let membersIndex (n-values length hh_membersAge [ i -> i ])

  ; define a list with true/false values flagging which members is dying during the current year
  let dying? map
  [
    i ->
    (random-float 1 < get-mortality (item i hh_membersSex) (item i hh_membersAge))
  ] membersIndex

  ; iterates the members from last to first, eliminating those that should be dying
  let index (length membersIndex) - 1
  repeat length membersIndex
  [
    if (item index dying?)
    [
      ; add to mortality
      ifelse (item index hh_membersSex)
      [ set womenDeaths womenDeaths + 1 ]
      [ set menDeaths menDeaths + 1 ]

      ;print (word "member from " self " dies with age " (item index hh_membersAge))
      hh_delete-member index
    ]
    set index index - 1
  ]

  ; update the index of members
  set membersIndex (n-values length hh_membersAge [ i -> i ])

  ; update hh_membersMarriage to consider any widow/widower as single
  let hh_membersMarriageCopy hh_membersMarriage
  foreach membersIndex
  [
    i ->
    if (item i hh_membersMarriage != -1                                           ; if the member is married
      and length filter [j -> j = item i hh_membersMarriage] hh_membersMarriage = 1) ; and her/his partner died this year (so she/he is the only member with the marriage index)
    [
      set hh_membersMarriage replace-item i hh_membersMarriage -1
      ;print (word "a member of " self " has lost her/his partner.")
    ]
  ]

end

to hh_set-members-to-marry

  ; applies age-specific nuptiality rates

  let membersIndex (n-values length hh_membersAge [ i -> i ])

  ; iterate for each member, finding which one is marrying during the current year, according to age cohort and sex
  foreach membersIndex
  [
    i ->
    if (item i hh_membersMarriage = -1) ; member is not married
    [
      let sex item i hh_membersSex

      if (random-float 1 < get-nuptiality sex (item i hh_membersAge))  ; passes nuptiality test according to age
      [
        ifelse (sex)
        [
          set womenToMarry lput (list self i) womenToMarry
        ]
        [
          set menToMarry lput (list self i) menToMarry
        ]
      ]
    ]
  ]

end

to hh_try-to-add-couple [ index spouseData ]

  ; check if new couple fits in man's household
  let tooManyCouples (hh_count-couples + 1) > hh_maxCoupleCount

  ifelse (tooManyCouples)
  [
    ; the new couple must found a new household, descending from ego's household
    ;print "household fission"
    hh_household-fission index spouseData
  ]
  [
    ; the new couple can stay in the ego parent's household
    hh_add-couple index spouseData
  ]

end

to hh_household-fission [ index spouseData ]

  ; creates a new household descending from ego household,
  ; adding ego and spouse and
  ; deleting them from the respective parent households
  let egoHousehold self

  ; create new household w/ ego and spouse
  hatch 1
  [
    set hh_householdAge 0
    ; inherit hh_maxCoupleCount value from ego's parent

    hh_reset-members

    ; copy ego
    hh_add-member-from (list egoHousehold index)
    ; copy spouse
    hh_add-member-from spouseData

    ; account for the new couple
    set hh_membersMarriage lput 0 hh_membersMarriage ; 0 because this is the first couple of the new household
    set hh_membersMarriage lput 0 hh_membersMarriage

    ;;; move to empty or less crowded patch, just in sake of visualisation
    move-to min-one-of patches [count households-here]
  ]

end

to hh_add-couple [ index spouseData ]

  ; add spouse to ego's household

  ; copy spouse
  hh_add-member-from spouseData

  ; account for the new couple
  let newCoupleIndex 1 + max hh_membersMarriage ; create a new couple index
  set hh_membersMarriage replace-item index hh_membersMarriage newCoupleIndex ; update ego marriage status
  set hh_membersMarriage lput newCoupleIndex hh_membersMarriage

end

;to update-members-marriage
;
;  ; applies age-specific nuptiality rates,
;  ; delete every men marrying (assumes men are more mobile)
;  ; or, if women,
;  ; add a new (random) husband for every women marrying (if tolerable number of couples)
;  ; or create a new household with the woman and her husband.
;
;  let membersIndex (n-values length hh_membersAge [ i -> i ])
;
;  ; define a list with true/false values flagging which members is marrying during the current year, according to her/his age cohort
;  let marrying? map
;  [
;    i ->
;    (random-float 1 < get-nuptiality (item i hh_membersSex) (item i hh_membersAge))
;  ] membersIndex
;
;  ; iterates the members from last to first
;  let i (length membersIndex) - 1
;  repeat length membersIndex
;  [
;    if (item i marrying? and (item i hh_membersMarriage = -1)) ; the second term makes sure that the member is single
;    [
;      ;print (word "member from " self " is marrying with age " (item i hh_membersAge))
;      ifelse (not item i hh_membersSex)
;      [
;        ; a marrying man exits the household
;        hh_delete-member i
;        ;print "  He is going out of the household."
;        set husbandsOut husbandsOut + 1
;      ]
;      [
;        ifelse (hh_maxCoupleCount < hh_count-couples + 1)
;        [
;          ; a marrying woman that cannot form a new couple inside her household
;          ; creates a new household with the woman and her new husband
;          hh_household-fission i (item i hh_membersAge)
;          ; the marrying woman exits the household
;          hh_delete-member i
;          ;print (word "  She is founding the new " (max-one-of households [who]))
;        ]
;        [
;          ; a marrying woman that can form a new couple inside her household
;          ; add her new husband to this household
;          let newCoupleIndex 1 + max hh_membersMarriage ; create a new couple index
;          set hh_membersMarriage replace-item i hh_membersMarriage newCoupleIndex ; the woman is now married
;          add-husband newCoupleIndex
;          ;print "  Her husband joins the household."
;          set husbandsIn husbandsIn + 1
;        ]
;      ]
;    ]
;    set i i - 1
;  ]
;
;end

;to hh_household-fission [ founderIndex founderAge ]
;
;  ; Creates a new household with the founder member
;  ; and set variables in new household.
;  let parentHousehold self
;
;  hatch 1
;  [
;    set hh_householdAge 0
;    ; inherit hh_maxCoupleCount value from parent
;
;    set hh_membersAge (list)
;    set hh_membersSex (list)
;    set hh_membersMarriage (list)
;
;    ; copy woman founding the new household
;    set hh_membersAge lput founderAge hh_membersAge
;    set hh_membersSex lput true hh_membersSex ; it is a woman
;    set hh_membersMarriage lput 0 hh_membersMarriage ; this is the first couple of the new household
;
;    add-husband 0 ; add the husband as part of the first couple
;
;    ;;; move to empty or less crowded patch, just in sake of visualisation
;    move-to min-one-of patches [count households-here]
;  ]
;
;end

;to add-husband [ coupleIndex ]
;
;  ; add a male (with valid age of marriage) to the household
;  let ageOfMarriage hh_get-initial-marriage-age false
;  set hh_membersAge lput ageOfMarriage hh_membersAge
;  set hh_membersSex lput false hh_membersSex
;  set hh_membersMarriage lput coupleIndex hh_membersMarriage
;
;end

to hh_reproduction

  ; iterates the members, up to the number of couples,
  ; testing women for the corresponding fertility rate
  ; and generates a new born individual if passing test.

  let membersIndex (n-values length hh_membersAge [ i -> i ])
  let couplesToTest hh_count-couples

  foreach membersIndex
  [
    i ->
    ; there is still a couple to consider and the member is female
    if (couplesToTest > 0 and item i hh_membersSex)
    [
      if (random-float 1 < get-fertility (item i hh_membersAge))
      [
        hh_add-offspring 0 ; add a newborn
        ;print (word "a new member is born in " self)
      ]
      set couplesToTest couplesToTest - 1
    ]
  ]

end

to hh_add-member-from [ memberData ]

  let aHousehold item 0 memberData
  let index item 1 memberData

  set hh_membersAge lput item index ([hh_membersAge] of aHousehold) hh_membersAge
  set hh_membersSex lput item index ([hh_membersSex] of aHousehold) hh_membersSex

  ; cue deletion of member from original parent household
  ask aHousehold
  [
    ;print (word "add-member spouse " memberData)
    set hh_memberDataToDelete lput index hh_memberDataToDelete
  ]

end
to hh_add-offspring [ initialAge ]

  ; add a newborn to the household
  set hh_membersAge lput initialAge hh_membersAge
  set hh_membersSex lput (random 2 = 0) hh_membersSex
  set hh_membersMarriage lput -1 hh_membersMarriage ; any offspring will be single

  ifelse (last hh_membersSex)
  [ set womenBirths womenBirths + 1 ]
  [ set menBirths menBirths + 1 ]

end

to hh_delete-members-in-cue

  ; delete members in cue following decresing order (so indexes still to go remain valid)
  foreach sort-by > hh_memberDataToDelete
  [
    i ->
    ; delete member from this household
    hh_delete-member i
  ]

end

to hh_delete-member [ index ]

  set hh_membersAge remove-item index hh_membersAge
  set hh_membersSex remove-item index hh_membersSex
  set hh_membersMarriage remove-item index hh_membersMarriage
  ; if the member was married, it will imply that there will be an odd number of married members,
  ; thus discounting a couple in hh_count-couples

  if (length hh_membersAge = 0) [ die ] ; delete empty household

end

to-report hh_count-couples

  let marriedMembers filter [ i -> (i >= 0) ] hh_membersMarriage

  report floor ((length marriedMembers) / 2)

end

to hh_reset-members

  set hh_membersAge (list)
  set hh_membersSex (list)
  set hh_membersMarriage (list)

  set hh_memberDataToDelete (list)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTERS AND MEASURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reset-counters

  set womenBirths 0
  set menBirths 0
  set womenDeaths 0
  set menDeaths 0

end

to update-counters

  set totalHouseholds count households
  let oldTotalIndividual totalIndividuals
  set totalIndividuals 0
  set totalWomen 0
  set totalMen 0
  set femaleRatio -1
  set womenAgeStructure (list)
  set menAgeStructure (list)
  set womenFirstAgeGroup 0
  set menFirstAgeGroup 0

  ask households
  [
    foreach (n-values length hh_membersAge [j -> j])
    [
      i ->
      ifelse (item i hh_membersSex)
      [
        set totalWomen totalWomen + 1
        set womenAgeStructure lput item i hh_membersAge womenAgeStructure
        if (item i hh_membersAge < 5)
        [ set womenFirstAgeGroup womenFirstAgeGroup + 1 ]
      ]
      [
        set totalMen totalMen + 1
        set menAgeStructure lput item i hh_membersAge menAgeStructure
        if (item i hh_membersAge < 5)
        [ set menFirstAgeGroup menFirstAgeGroup + 1 ]
      ]
      set totalIndividuals totalIndividuals + 1
    ]
  ]

  carefully [ set naturalPopulationGrowth 100 * (totalIndividuals - oldTotalIndividual) / oldTotalIndividual ] [ ]

  carefully [ set femaleRatio totalWomen / totalIndividuals ] [ set femaleRatio "" ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to refresh-view



end

to plot-table [ values ]

  let j 0
  foreach values
  [
    i ->
    plotxy j i
    set j j + 1
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEMOGRAPHY TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NOTE: may be greatly simplified if tables are pre-processed
; to express probabilities for one-year-old cohorts

to load-demography-tables

  ;;; load demographic data tables into lists
  ;;; NOTE: in any case, values are probabilities of events for individuals either
  ;;; with x years old or from n years old to x years old.

  ;=======FERTILITY========================================================

  if (input-fertility-table = "1970-1972 India")
  [
    set womenFertilityTable load-table-empirical "womenFertilityTable_1year"
    ;set womenFertilityTable convert-to-single-age womenFertilityTable "fertility"
  ]

  if (input-fertility-table = "Coale-Trussell model")
  [
    set womenFertilityTable load-table-ctmodel
    set womenFertilityTable convert-to-single-age womenFertilityTable "fertility"
  ]

  if (input-fertility-table = "Beta distribution model")
  [
    set womenFertilityTable load-table-betaDist
  ]

  ;=======NUPTIALITY========================================================

  if (input-nuptiality-table = "1901-1911 India")
  [
    set womenNuptialityTable load-table-empirical "womenNuptialityTable_1year"
    ;set womenNuptialityTable convert-to-single-age womenNuptialityTable "nuptiality"

    set menNuptialityTable load-table-empirical "menNuptialityTable_1year"
    ;set menNuptialityTable convert-to-single-age menNuptialityTable "nuptiality"
  ]

  if (input-nuptiality-table = "Coale double exp model")
  [
    set womenNuptialityTable load-table-cdemodel

    set menNuptialityTable load-table-cdemodel
  ]

  ;=======MORTALITY========================================================

  if (input-life-table = "1901 India")
  [

    set womenMortalityTable load-table-empirical "womenMortalityTable_1year"
    ;set womenMortalityTable convert-to-single-age womenMortalityTable "mortality"

    set menMortalityTable load-table-empirical "menMortalityTable_1year"
    ;set menMortalityTable convert-to-single-age menMortalityTable "mortality"
  ]

  if (input-life-table = "Coale-Demeny Model")
  [
    set womenMortalityTable load-table-cdmodel "F"
    set womenMortalityTable convert-to-single-age womenMortalityTable "mortality"

    set menMortalityTable load-table-cdmodel "M"
    set menMortalityTable convert-to-single-age menMortalityTable "mortality"
  ]

end

to-report convert-to-single-age [ ageGroupTable tableType ]

  ;;; converts ago cohort rates/probabilities (between age x and x+n) to instantaneous rates (age x)
  let singleAgeTable (list)

  if (tableType = "fertility")
  [
    let ages n-values 100 [ i -> i ]
    foreach ages
    [
      i ->
      let value 0
      if (i >= item 0 fertilityMinMax and i < item 1 fertilityMinMax)
      [
        let cohort get-cohort
            i
            fertilityCohortSize
            (floor (item 0 fertilityMinMax) / fertilityCohortSize) ; discounts the cohorts that may exist before minimum age in table
        set value (item cohort ageGroupTable) ;/ fertilityCohortSize
      ]

      set singleAgeTable lput value singleAgeTable
    ]
  ]
  if (tableType = "nuptiality")
  [
    let ages n-values 100 [ i -> i ]
    foreach ages
    [
      i ->
      let value 0
      if (i >= item 0 nuptialityMinMax and i < item 1 nuptialityMinMax)
      [
        let cohort 0
        let thisCohortSize nuptialityCohortSize

        ; handle cohort 0-5 (actually interval of six years old?)
        ifelse (i >= 0 and i <= 5)
        [
          set thisCohortSize 6
        ]
        [
          set cohort get-cohort
            (i - 1) ; include threshold age inside the earlier cohort (e.g., 15 count as 14)
            nuptialityCohortSize
            (floor (item 0 nuptialityMinMax) / nuptialityCohortSize) ; discounts the cohorts that may exist before minimum age in table
          set thisCohortSize nuptialityCohortSize
        ]
        set value (item cohort ageGroupTable) ;/ thisCohortSize
      ]
      set singleAgeTable lput value singleAgeTable
    ]
  ]
  if (tableType = "mortality")
  [
    let ages n-values 100 [ i -> i ]
    foreach ages
    [
      i ->
      let value 0
      let cohort i
      let thisCohortSize mortalityCohortSize

      ifelse (i = 0 or i = 1)
      [
        ; mortality tables have rates specific to ages 0 and 1, so age functions as cohort
        ; and the rates are passed as it is
        set thisCohortSize 1
      ]
      [
        ifelse (i >= item 1 mortalityMinMax) ; if a member ages beyond the maximum in the table
        [
          set cohort (length ageGroupTable) - 1 ; make it count as the last cohort
        ]
        [
          set cohort get-cohort
            (i - 1) ; include threshold age inside the earlier cohort (e.g., 15 count as 14)
            mortalityCohortSize
            -2 ; accounts for the two first 1-year cohorts (adds 2 instead of discounting)
        ]
        ; solve the exception of cohort 2-5 years old
        if (i <= 5)
        [
          set thisCohortSize 4
        ]
      ]

      ; except for cohorts 0, 1, and 2-5 years old, the rate is distributed for each year within mortalityCohortSize years
      set value (item cohort ageGroupTable) ;/ thisCohortSize
      set singleAgeTable lput value singleAgeTable
    ]
  ]

  report singleAgeTable

end

to-report get-fertility [ age ]

  ; return 0 if outside the range of ages considered in the table
  if (age >= 100) [ report 0 ]

  report item age womenFertilityTable

end

to-report get-nuptiality [ isFemale age ]

  ; return 0 if outside the range of ages considered in the table
  if (age >= 100) [ report 0 ]

  ifelse (isFemale)
  [
    report item age womenNuptialityTable
  ]
  [
    report item age menNuptialityTable
  ]

end

to-report get-mortality [ isFemale age ]

  ; return 1 if outside the range of ages considered in the table
  if (age >= 100) [ report 1 ]

  ifelse (isFemale)
  [
    report item age womenMortalityTable
  ]
  [
    report item age menMortalityTable
  ]

end

to-report get-cohort [ age cohortSize cohortAdjustment ]

  report (floor ( (age) / cohortSize)) - cohortAdjustment

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORT/GENERATE TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report load-table-empirical [ tableName ]

  ;;; this function assumes there is a text file (.../demoTables/<tableName>.txt)
  ;;; containing:
  ;;; 1. a reference text encloused in "" (first line)
  ;;; 2. a row containing min age<SPACE>max age<SPACE>cohort size in years (second line)
  ;;; 3. two columns separated by <SPACE> containing:
  ;;;   a. row identifier (e.g. cohort age range)
  ;;;   b. data (e.g. fertility rate)
  let demoTable (list)

  let FilePath "demoTables//"
  let filename (word FilePath "table_" tableName ".txt")
  let temp 0
  file-open filename

  ;set temp file-read ; this line skips the reference text
  ; load the three table parameters (NOTE: there is a redundancy, assuming female/male tables of a type have the same values)
  ;load-table-empirical-pars tableName

  while [not file-at-end?]
  [
    set temp file-read ; this line skips the row identifier

    set demoTable lput file-read demoTable ; load row datum
  ]
  file-close

  load-table-empirical-pars tableName demoTable

  report demoTable

end

to load-table-empirical-pars [ tableName table ]

  if (member? "Fertility" tableName)
  [
    set fertilityMinMax get-table-min-max table;(list file-read file-read)
    ;set fertilityCohortSize file-read
  ]
  if (member? "Nuptiality" tableName)
  [
    set nuptialityMinMax  get-table-min-max table;(list file-read file-read)
    ;set nuptialityCohortSize file-read
  ]
  if (member? "Mortality" tableName)
  [
    set mortalityMinMax  get-table-min-max table;(list file-read file-read)
    ;set mortalityCohortSize file-read
  ]

end

to-report get-table-min-max [ table ]

  let tempAgeList (list 0)

  foreach (n-values 148 [ i -> i ])
  [
    i ->
    if (item (i + 1) table = 0) [ set tempAgeList lput (i + 1) tempAgeList ]
  ]
  set tempAgeList lput 150 tempAgeList

  let minMax (list (first tempAgeList) (last tempAgeList))

  report minMax

end

;to-report load-betaDist-table
;
;  ;;; this function calculate the age-specific fertility rate 'ASFR' (probabilities for a woman to give birth between at age i)
;  ;;; using a beta distribution scaled to the average number of offspring per woman during the entire fertile period
;  ;;; Beta distribution is either obtained with the external R script or by using the 'rngs' community extension.
;  ;;; NOTE: 2.1 of offspring alive during fertile period is the minimum replacement rate (i.e. fertility must compensate infant mortality)
;  ;;; Approach taken from Peter Ellis
;  ;;; http://freerangestats.info/blog/2018/06/26/fertility-rate
;  ;;; https://github.com/ellisp/blog-source/tree/master/_working/0122-demographics
;
;  let birthProbs (list)
;
;  let FilePath "demoTables//"
;  let filename (word FilePath "betaDist.txt")
;  let temp 0
;  file-open filename
;  set temp file-read ; skips column name
;
;  while [not file-at-end?]
;  [
;    set temp file-read ; this line skips the row identifier
;    set birthProbs lput file-read birthProbs ; load row datum
;  ]
;  file-close
;
;  set birthProbs map [ i -> i * average-births-per-woman / (length filter [j -> j > 0] birthProbs) ] birthProbs
;
;  report birthProbs
;
;end


to-report load-table-ctmodel ;;; INCOMPLETE

  ;;; this function creates the probability of generating offspring for every one-year age cohort
  ;;; using the Coale-Trussel model from:
  ;;; Ansley J. Coale and T. James Trussell. "Model fertility schedules: variations in
  ;;; the age structure of childbearing in human populations", Population Index, vol. 40. No. 2 (April 1974). pp. 185-258.
  ;;; see page 23-4 (Chapter 1) in
  ;;; http://www.un.org/en/development/desa/population/publications/manual/estimate/demographic-estimation.shtml
  let demoTable (list)

  ; standard pattern of natural fertility and of deviations from natural fertility
  ; (Table 3, p. 24)
  ; h(i)
  let naturalFertility (list
    0.411 ; 15-19
    0.460 ; 20-24
    0.431 ; 25-29
    0.395 ; 30-34
    0.322 ; 35-39
    0.167 ; 40-44
    0.024 ; 45-49
    )
  ; h(i)
  let devNaturalFertility (list
    0.000  ; 15-19
    0.000  ; 20-24
    -0.279 ; 25-29
    -0.667 ; 30-34
    -1.042 ; 35-39
    -1.414 ; 40-44
    -1.671 ; 45-49
    )

  let cohorts n-values 7 [ i -> i ]
  foreach cohorts
  [
    i ->
    set demoTable lput (
      ct-level-natural-fertility * (item i naturalFertility) * exp (ct-level-fertility-control * (item i devNaturalFertility))
    ) demoTable
  ]

  set fertilityMinMax (list 15 45)
  set fertilityCohortSize 5

  report demoTable

end

to-report load-table-cdemodel

  ;;; this function creates the probability of (first) marriage for every one-year age cohort
  ;;; using a version of the double exponential model (closed form) proposed by
  ;;; A. J. Coale, "Age patterns of marriage",
  ;;; see page 22 (Chapter 1) in
  ;;; http://www.un.org/en/development/desa/population/publications/manual/estimate/demographic-estimation.shtml
  ;;; Also in
  ;;; Coale, A.J., and D.R. McNeil. 1972.
  ;;; The distribution by age of the frequency of first marriage in a female cohort. Journal of  the American Statistical Association 67(340):743–49.
  ;;; REF obtained in: A parametric model for estimating nuptiality patterns in modern populations.
  ;;; Available from: https://www.researchgate.net/publication/285457704_A_parametric_model_for_estimating_nuptiality_patterns_in_modern_populations [accessed Nov 27 2018].

  ;;; The model is aimed to fit women nuptiality, but is here used for both female and male individuals
  ;;; copy&run the following command in R to visualise the function output for ages between 0 and 100:
  ;;; plot(1:100, 0.1946 * exp(-0.174 * (1:100 - 6.06) - exp(-0.2881 * (1:100 - 6.06))))
  ; given estimated values for parameters are:
  ; alpha = 0.1946 --- regulates the overall level of nuptiality
  ; beta = -0.174 --- regulates the overall level of nuptiality
  ; gamma = -0.2881 --- absolute greater values bring less nuptiality to earlier ages
  ; delta = 6.06 --- absolute greater values bring higher nuptiality to earlier ages

  let demoTable (list)

  let cohorts n-values 100 [ i -> i ]
  foreach cohorts
  [
    i ->
;    let term1 i - cde-delta
;    let term2 exp(cde-gamma * term1)
;
;    set demoTable lput (
;      cde-alpha * exp(cde-beta * term1 - term2)
;    ) demoTable

    let term1 cde-E / cde-sigma
    let term2 ((i - cde-mu) / cde-sigma) + 0.805
    set demoTable lput (
      term1 * 1.2813 * exp (-1.145 * term2 - exp (-1.896 * term2) )
    ) demoTable
  ]

  set nuptialityMinMax (list 0 100)
  set nuptialityCohortSize 1

  report demoTable

end

to-report load-table-cdmodel [ sex ]

  ;;; this function assumes there is a text file (.../demoTables/cdmlt<coale-demeny-region><sex>.txt)
  ;;; containing a matrix with prob. of death for life-expentancy-level (rows) by age cohort (columns).
  ;;; values of the first row and columns should be skipped
  let demoTable (list)

  let FilePath "demoTables//"
  let filename (word FilePath "cdmlt" (first coale-demeny-region) sex ".txt")
  let temp 0
  file-open filename
  ; first line and set pars
  set temp read-from-string (word "[" file-read-line "]") ; the first line is the age cohort identifiers
  set mortalityMinMax (list (read-from-string item 0 temp) (read-from-string item (length temp - 1) temp))
  set mortalityCohortSize 5

  ; read lines and get the one corresponding to coale-demeny-life-expectancy-level
  let i 1
  while [not file-at-end?]
  [
    set temp read-from-string (word "[" file-read-line "]")
    if (read-from-string item 0 temp = coale-demeny-life-expectancy-level)
    [
      set demoTable remove-item 0 temp
    ]
    set i i + 1
  ]

  file-close

  report demoTable

end
@#$#@#$#@
GRAPHICS-WINDOW
713
11
1025
324
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
97
10
152
43
NIL
setup
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
98
48
153
81
NIL
go
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
14
38
88
98
SEED
0.0
1
0
Number

CHOOSER
14
125
152
170
experiment-type
experiment-type
"user-defined" "random"
0

INPUTBOX
13
178
135
238
initial-num-households
0.0
1
0
Number

MONITOR
14
237
115
274
NIL
initialNumHouseholds
0
1
9

INPUTBOX
11
287
122
347
household-initial-age
0
1
0
String

INPUTBOX
11
352
132
412
max-couple-count-distribution
0
1
0
String

PLOT
7
466
572
616
Households
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"households" 1.0 0 -16777216 true "plot totalHouseholds" "plot totalHouseholds"
"couples" 1.0 0 -7500403 true "" "plot sum [hh_count-couples] of households"

PLOT
135
316
605
436
Age structure
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-histogram-num-bars 20\nset-plot-x-range -1 101;max (sentence womenAgeStructure menAgeStructure)" ";set-histogram-num-bars 20\n;set-plot-x-range -1 max (sentence womenAgeStructure menAgeStructure)"
PENS
"women" 1.0 1 -2674135 true "" "histogram womenAgeStructure"
"men" 1.0 1 -13345367 true "" "histogram menAgeStructure"

MONITOR
526
370
599
415
NIL
femaleRatio
4
1
11

PLOT
166
10
607
160
Population
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"total" 1.0 0 -16777216 true "" "plot totalIndividuals"
"women" 1.0 0 -2674135 true "" "plot totalWomen"
"men" 1.0 0 -13345367 true "" "plot totalMen"

PLOT
8
628
568
754
maxCoupleCount and actual count
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-y-range -1 (1 + item 1 maxCoupleCountDistribution)" ""
PENS
"mean" 1.0 0 -8053223 true "" "plot mean [hh_maxCoupleCount] of households"
"max" 1.0 0 -8630108 true "" "plot max [hh_maxCoupleCount] of households"
"min" 1.0 0 -5825686 true "" "plot min [hh_maxCoupleCount] of households"
"count mean" 1.0 0 -15582384 true "" "plot mean [hh_count-couples] of households"
"count max" 1.0 0 -14454117 true "" "plot max [hh_count-couples] of households"
"count min" 1.0 0 -12345184 true "" "plot min [hh_count-couples] of households"

MONITOR
611
149
712
194
women death rate
womenDeaths / totalWomen
4
1
11

MONITOR
611
196
709
241
men death rate
menDeaths / totalMen
4
1
11

PLOT
155
167
606
317
Births and deaths
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"women deaths" 1.0 0 -11053225 true "" "plot womenDeaths"
"men deaths" 1.0 0 -4539718 true "" "plot menDeaths"
"births" 1.0 0 -14439633 true "" "plot womenBirths + menBirths"
"deaths" 1.0 0 -16777216 true "" "plot womenDeaths + menDeaths"
"women births" 1.0 0 -13210332 true "" "plot womenBirths"
"men births" 1.0 0 -11881837 true "" "plot menBirths"

MONITOR
611
246
704
291
women birth rate
womenBirths / totalWomen
4
1
11

MONITOR
610
292
702
337
men birth rate
menBirths / totalMen
4
1
11

BUTTON
97
87
152
120
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
579
571
784
616
input-life-table
input-life-table
"1901 India" "Coale-Demeny Model"
1

SLIDER
578
636
786
669
coale-demeny-life-expectancy-level
coale-demeny-life-expectancy-level
1
25
0.0
1
1
NIL
HORIZONTAL

CHOOSER
580
668
721
713
coale-demeny-region
coale-demeny-region
"west" "east" "south" "north"
2

CHOOSER
793
572
973
617
input-nuptiality-table
input-nuptiality-table
"1901-1911 India" "Coale double exp model"
1

CHOOSER
995
570
1169
615
input-fertility-table
input-fertility-table
"1970-1972 India" "Coale-Trussell model" "Beta distribution model"
2

SLIDER
797
632
986
665
cde-alpha
cde-alpha
0.01
0.3
0.0
0.0001
1
dft: 0.1946
HORIZONTAL

SLIDER
796
669
976
702
cde-beta
cde-beta
-1
0
0.0
0.0001
1
dft: -0.174
HORIZONTAL

SLIDER
797
706
998
739
cde-gamma
cde-gamma
-0.4
0
0.0
0.0001
1
dft: -0.2881
HORIZONTAL

SLIDER
996
628
1185
661
ct-level-natural-fertility
ct-level-natural-fertility
0
1.5
0.0
0.001
1
NIL
HORIZONTAL

SLIDER
998
666
1191
699
ct-level-fertility-control
ct-level-fertility-control
-1
1
0.0
0.001
1
NIL
HORIZONTAL

PLOT
578
446
784
566
mortality (prob. dying)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "clear-plot\nset-plot-y-range -0.001 (precision (max (list max womenMortalityTable max menMortalityTable) + 0.001) 0.01)"
PENS
"default" 1.0 0 -5298144 true "" "plot-table womenMortalityTable"
"pen-1" 1.0 0 -14070903 true "" "plot-table menMortalityTable"

PLOT
792
446
992
566
nuptiality (prob. marrying)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "clear-plot \nset-plot-y-range -0.001 (precision (max (list max womenNuptialityTable max menNuptialityTable) + 0.001) 0.01)"
PENS
"default" 1.0 0 -5298144 true "" "plot-table womenNuptialityTable"
"pen-1" 1.0 0 -14070903 true "" "plot-table menNuptialityTable"

PLOT
994
445
1215
565
fertility (prob. of giving birth)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "clear-plot \nset-plot-y-range -0.001 (precision (max womenFertilityTable + 0.001) 0.01)"
PENS
"default" 1.0 0 -5298144 true "" "plot-table womenFertilityTable"

SLIDER
999
712
1214
745
betaDist-av-num-offspring
betaDist-av-num-offspring
0
30
0.0
0.001
1
NIL
HORIZONTAL

PLOT
787
324
987
444
couple count
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range -0.001 (item 1 maxCoupleCountDistribution + 0.01)" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [hh_count-couples] of households"

CHOOSER
8
415
134
460
residence-rule
residence-rule
"patrilocal-patrilineal" "matrilocal-matrilineal"
0

SLIDER
796
744
968
777
cde-delta
cde-delta
0
10
0.0
.01
1
dft: 6.06
HORIZONTAL

MONITOR
610
354
699
391
% 0-4 (women)
100 * womenFirstAgeGroup / totalWomen
2
1
9

MONITOR
610
390
699
427
% 0-4 (men)
100 * menFirstAgeGroup / totalMen
2
1
9

MONITOR
562
89
680
134
nat. pop. growth (%)
naturalPopulationGrowth
2
1
11

SLIDER
1251
555
1423
588
cde-E
cde-E
0
1
0.0
0.001
1
NIL
HORIZONTAL

SLIDER
1254
600
1426
633
cde-sigma
cde-sigma
0
5
0.0
0.001
1
NIL
HORIZONTAL

SLIDER
1255
646
1427
679
cde-mu
cde-mu
0
40
0.0
0.001
1
NIL
HORIZONTAL

@#$#@#$#@
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
