## Overview

This model simulates the demographic evolution of a population over time. It doesn't just track individuals but organizes them into households, which function as the basic social and economic units. The simulation models key life events: births, marriages, and deaths. A central feature of this model is its sophisticated mortality system, where the chance of an individual's death is not only based on their age and sex but is also influenced by population density and age-specific requirements.

## Purpose

The model aims to explore demographic patterns and the effects of various parameters on population dynamics, providing insights into how households interact and evolve over time. It can be used for educational purposes, research, or policy analysis related to population studies.

## Core Concepts

- Households: The population is composed of households, not just free-floating individuals. Each household has a lineage, a maximum size (in terms of married couples), and a collection of members with specific ages and sexes.

- Life Cycle: Individuals age year by year. As they age, their probabilities of marrying, having children, and dying change according to predefined demographic models.

- Marriage (Nuptiality): Single individuals can marry based on age-specific probabilities. It also simulates different post-marriage residence patterns, such as the couple living with the husband's family (patrilocal) or the wife's family (matrilocal).

- Births (Fertility): Married women have an annual probability of giving birth, which is primarily dependent on their age.

- Deaths (Mortality): Each person has a standard chance of dying based on their age and sex, derived from historical demographic data (Coale-Demeny life tables).

- Household Dynamics: New households are formed when a newly married couple cannot fit into an existing household, causing a "fission" event. Conversely, if all the adults in a household die, the children become orphans and are adopted by other surviving households.

- Orphan Management: If a household loses all adult members, children are classified as orphans and can be adopted by other households.

- Parameterization: Users can define various parameters, such as the initial number of households, age distributions, and carrying capacity, allowing for different simulation scenarios.

## Simulation Dynamics

The simulation progresses through a series of defined procedures executed in a specific order during each tick, representing a year in the model. The following sequence outlines the key procedures:

1. Aging of Individuals: The age-households procedure is called first, where each household undergoes aging through the `hh_aging` function. This updates the age of all household members.

2. Mortality Application: Following aging, the apply-mortality procedure is executed. This procedure invokes `hh_update-members-survival`, which applies age-specific mortality rates to each member, removing those who die from their households.

3. Nuptiality (Marriage): The apply-nuptiality procedure is then called, which includes `build-lists-to-marry` to identify eligible singles. The `try-to-form-couples` function attempts to create couples based on the nuptiality model, utilizing the `hh_set-members-to-marry` function to populate lists of potential partners.

4. Fertility (Births): After addressing marriages, the apply-fertility procedure is executed, which calls `hh_reproduce`. This function tests married women for childbirth probabilities and adds newborns to households if conditions are met.

5. Orphan Management: The manage-orphanhood procedure is invoked to check for households without adults. If a household is left with only children, the `hh_disolve-if-no-adults` function adds these children to the orphan list, which is later processed to distribute orphans among surviving households.

Throughout these procedures, the model manages household dynamics, including the formation of new households during marriage through the `hh_try-to-add-couple` function, which may trigger a "fission" event if capacity limits are reached.

At the end of each tick, the `update-counters` procedure aggregates demographic metrics, such as total households and population growth rates, which are then visualized through plots and monitors.

By following this structured sequence of procedures, the model effectively simulates the complex interactions and dynamics of households over time, allowing for the exploration of various demographic scenarios and their implications.

## Parametric models

The model includes two parametric models, Coale-Demeny life tables for mortality and Peristeva and Kostaki Model for fertility and nuptiality, which allow defining the age- and sex-specific probablity of births, marriages and deaths, and so representing various demographic dynamics defined by intelligible parameters. All these models are implemented through procedures and variables that manage the demographic tables and their parameters.

The `apply-mortality` procedure utilizes the `get-mortality` function to apply age-specific mortality rates derived from the Coale-Demeny life tables. Similarly, the `apply-fertility` procedure calls `hh_reproduce`, which uses the `get-fertility` function to determine the probability of childbirth based on the fertility table. Nuptiality is managed through the `apply-nuptiality` procedure, which is somewhat more complex than `apply-mortality` and `apply-fertility`, yet also uses the `get-nuptiality` function to access age-specific marriage probabilities from the nuptiality table.

By parameterizing these models, users can explore various demographic scenarios and their implications on population dynamics.

### Peristeva and Kostaki Model

The Peristeva and Kostaki model is utilized to estimate fertility and nuptiality patterns in modern populations. This model is implemented through the following procedures and variables:

- Procedures:  
  - `build-demography-tables`: This procedure initializes the demographic tables, including the fertility and nuptiality tables.  
  - `build-fertility-tables`: This procedure loads the fertility data into the fertilityTable variable using the `load-peristeri-kostaki-model-table` function.  
  - `build-nuptiality-tables`: This procedure loads the nuptiality data for women and men into the `nuptialityTable-women` and `nuptialityTable-men` variables, respectively.

- Variables:  
  - `fertilityTable`: A list that holds the probabilities of childbirth based on age.  
  - `nuptialityTable-women` and `nuptialityTable-men`: Lists that hold the probabilities of marriage for women and men, respectively.

- Parameters:  
- `c1-fert`, `mu-fert`, `sigma1-fert`, `sigma2-fert`: Parameters that define the shape of the fertility distribution.  
- `c1-women`, `mu-women`, `sigma1-women`, `sigma2-women`: Parameters that define the shape of the nuptiality distribution for women.  
- `c1-men`, `mu-men`, `sigma1-men`, `sigma2-men`: Parameters that define the shape of the nuptiality distribution for men.

Peristeva and Kostaki (2009). "Modeling fertility in modern populations". *Demographic Research* 16: 141-194, p. 147. DOI: https://dx.doi.org/10.4054/DemRes.2007.16.6

>Peristeva and Kostaki (2015). "A parametric model for estimating nuptiality patterns in modern populations". *Canadian studies in population* 42(2):130-148, p. 133. DOI: 10.25336/P6TK56
>Available at: https://www.researchgate.net/publication/285457704_A_parametric_model_for_estimating_nuptiality_patterns_in_modern_populations [accessed Nov 27 2018].

The model is expressed as a parametric equation that can be easily implemented in programming languages such as NetLogo.

Use "demoTables/compareNuptialityModel.R" to test age-sex probability shapes.

### Coale-Demeny life tables

The Coale-Demeny life tables are a set of demographic models that provide age-specific mortality rates based on historical data. They are widely used in demographic studies to simulate mortality patterns in populations. The model allows users to select from different Coale-Demeny regions, each representing distinct mortality patterns, which can be adjusted through parameters such as the region and the year of the life table.

This model is implemented through the following procedures and variables:

- Procedures:  
  - `build-demography-tables`: This procedure also initializes the mortality tables by calling `build-mortality-tables`.  
  - `build-mortality-tables`: This procedure loads the mortality data into the `mortalityTable-women` and `mortalityTable-men` variables using the `load-coale-demeny-table` function.

- Variables:  
  - `mortalityTable-women`: A list that holds the probabilities of death for women based on age.  
  - `mortalityTable-men`: A list that holds the probabilities of death for men based on age.

- Parameters:  
  - `cdmlt-level`: This parameter defines the life expectancy level used to select the appropriate mortality table.  
  - `coale-demeny-region`: This parameter allows users to select from different Coale-Demeny regions, each representing distinct mortality patterns.

The Coale-Demeny Life Tables loaded into NetLogo were generated using the R script 'importCoaleDemenyLifeTables.R' included in the 'demoTables' folder. This script uses the `cdmlt` functions in `demoR` package.

>James Holland Jone (2007). demogR: A Package for the Construction and Analysis of Age-structured Demographic Models in R. Journal of Statistical Software, 22(10), 1-28. URL http://dx.doi.org/10.18637/jss.v022.i10.

>Coale, A., P. Demeny, and B. Vaughn. 1983. Regional model life tables and stable populations. 2nd ed. New York: Academic Press.
