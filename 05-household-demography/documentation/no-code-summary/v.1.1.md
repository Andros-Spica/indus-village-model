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

For more details on the core concepts, see the [v.1.0 no code summary](v.1.0.md).

## Kinship tabu in marriage

The model incorporates a kinship tabu system that restricts marriage between individuals who are closely related. The kinship tabu in marriage is a fundamental aspect of the model that shapes the social structure and demographic evolution of the population. By restricting marriages based on kinship, the model not only adheres to realistic social norms but also enhances the overall integrity and diversity of the simulated population.

Here’s a detailed breakdown of how this system operates:

- Definition of Kinship Degree: The model defines a parameter called acceptableKinshipDegreeForCouples, which determines the maximum degree of kinship allowed between individuals who wish to form a couple. For instance, a degree of 1 indicates that individuals from the same household cannot marry, while a degree of 0.5 might allow for first cousins to marry.

- Lineage Tracking: Each household maintains a lineage history, represented as a list of integers. This lineage is crucial for determining the kinship degree between potential couples. When a couple is formed, the model checks their lineage to ensure that they do not share a close blood relationship.

- Couple Formation Process: During the marriage process, the model first attempts to pair individuals within the same population. If a potential couple violates the kinship tabu (i.e., their kinship degree exceeds the acceptable limit), the model will not allow the marriage to occur. If no suitable partner is found within the population due to kinship restrictions, the model allows for the creation of couples with individuals from external populations, thereby introducing genetic diversity and preventing inbreeding.

- Residence Rules: The model also simulates different residence patterns post-marriage, such as patrilocal (where the couple resides with the husband's family) or matrilocal (where they reside with the wife's family). These rules can influence the dynamics of kinship and household formation.

By enforcing kinship tabus, the model reflects real-world social norms and practices regarding marriage. This restriction helps to prevent inbreeding and promotes genetic diversity within the population, which is essential for the long-term viability of the demographic structure.

## Potential for Future Extensions:

The current implementation primarily tracks lineage in a linear fashion, which may overlook complex familial relationships. Future enhancements could involve a more intricate tracking system that accounts for various forms of kinship, thereby allowing for a more nuanced simulation of marriage practices.
