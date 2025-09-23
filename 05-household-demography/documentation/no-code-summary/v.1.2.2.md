## Overview

This model simulates the demographic evolution of a population over time. It doesn't just track individuals but organizes them into households, which function as the basic social and economic units. The simulation models key life events: births, marriages, and deaths. A central feature of this model is its sophisticated mortality system, where the chance of an individual's death is not only based on their age and sex but is also influenced by population density and age-specific requirements and labor productivity. In essence, the model creates a dynamic feedback loop where the population's own structure and productivity directly influence its survival rates, providing a more realistic mechanism for population regulation than a simple, fixed population cap.

## Purpose

The model aims to explore demographic patterns and the effects of various parameters on population dynamics, providing insights into how households interact and evolve over time. It can be used for educational purposes, research, or policy analysis related to population studies.

## Core Concepts

- Households: The population is composed of households, not just free-floating individuals. Each household has a lineage, a maximum size (in terms of married couples), and a collection of members with specific ages and sexes.

- Life Cycle: Individuals age year by year. As they age, their probabilities of marrying, having children, and dying change according to predefined demographic models.

- Marriage (Nuptiality): Single individuals can marry based on age-specific probabilities. The model includes rules that prevent marriage between close relatives by tracking household lineage. It also simulates different post-marriage residence patterns, such as the couple living with the husband's family (patrilocal) or the wife's family (matrilocal).

- Births (Fertility): Married women have an annual probability of giving birth, which is primarily dependent on their age.

- Household Dynamics: New households are formed when a newly married couple cannot fit into an existing household, causing a "fission" event. Conversely, if all the adults in a household die, the children become orphans and are adopted by other surviving households.

- Orphan Management: If a household loses all adult members, children are classified as orphans and can be adopted by other households.

- Parameterization: Users can define various parameters, such as the initial number of households, age distributions, and carrying capacity, allowing for different simulation scenarios.

For more details on the core concepts, see the [v.1.0 no code summary](v.1.0.md).

## Density-Dependent Mortality

An individual's probability of dying is determined by two main factors:

- Baseline Mortality: Each person has a standard chance of dying based on their age and sex, derived from historical demographic data (Coale-Demeny life tables).

- Density Effect: This is an additional risk of death that applies to everyone when the population strains its environmental resources. It works as follows:

- Carrying Capacity: The model defines a base carrying capacity—the theoretical maximum number of people the environment can sustain.

- Increased Mortality: As the total population size approaches this effective carrying capacity, a "density stress" factor increases. This factor adds a direct, additional probability of death for every single individual in the simulation, simulating the effects of resource scarcity and environmental strain.

### Carrying capacity demand weighted by age group requirements

- Age Group Requirements: Each age group has different resource needs, which are factored into the carrying capacity. For example, children and the elderly require more resources per capita than prime-age adults.  
- Demand Calculation: The model calculates the total resource demand based on the age distribution of the population. This demand is then compared to the carrying capacity to determine if the population is exceeding its sustainable limits.

### Carrying capacity weighted by labor contribution

- Weighted Carrying Capacity: The base carrying capacity is adjusted by the population's labor output. If the total available labor is insufficient to meet the needs of the population, the effective carrying capacity is reduced. This simulates the idea that a less productive population cannot support as many people.  
- Labor Contribution: Not everyone contributes equally to supporting the population. The model calculates the total available labor based on the age of all individuals (children and the elderly contribute less labor than prime-age adults).
