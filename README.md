# Indus Village model

Collection of files associated with the Indus Village model, including separate modules implementations, demonstrations, tests, and pseudo-code diagrams.

Overall design of the Indus Village model:

![diagrams/00-OverallDesign.png](diagrams/00-OverallDesign.png)

## Weather model

See the study on precipitation patterns done for the development of the parametric precipitation model: https://github.com/Andros-Spica/parModelPrecipitation

![screenshots/01-weather interface.png](screenshots/01-weather%20interface.png)

## Soil Water Balance model

![02-Soil-Water-Balance-model/plots/SoilWaterBalanceModel_Rakhigarhi_1990-2000-plotSet.png](02-Soil-Water-Balance-model/plots/SoilWaterBalanceModel_Rakhigarhi_1990-2000-plotSet.png)

## Land model

![screenshots/03-terrainBuilder_waterFlow interface.png](screenshots/03-terrainBuilder_waterFlow%20interface.png)

## Crop model

![diagrams/04-SIMPLE crop model by Zhao-et-al-2019.png](diagrams/04-SIMPLE%20crop%20model%20by%20Zhao-et-al-2019.png)

![screenshots/04-SIMPLE-crop-model interface.png](screenshots/04-SIMPLE-crop-model%20interface.png)

## Household Demography model

![diagrams/05-householdDemographyModel.png](diagrams/05-householdDemographyModel.png)

![screenshots/05-householdDemography interface.png](screenshots/05-householdDemography%20interface.png)

## Food Storage model

![diagrams/06-FoodStorageModel.png](diagrams/06-FoodStorageModel.png)

![screenshots/06-storage interface.png](screenshots/06-storage%20interface.png)

### With local exchange mechanism

In this version, the Food Storage model is complemented by a exchange mechanism that describes a common "market" that "buys" the surplus stock units released by households, stores them, and "sells" them back to households needing extra units to fulfill their desired diet. This "market" is called as such in sake of implementation simplicity; it is actually more representative of a centralised redistribution institution, because there is no accountability of value of stock units "bought" or "sold" by households and, thus, no stock unit price.

![diagrams/06-FoodStorageModel_withExchange.png](diagrams/06-FoodStorageModel_withExchange.png)

![screenshots/06-storage interface_withExchange.png](screenshots/06-storage%20interface_withExchange.png)
