This repository holds a collection of files associated with the Indus Village model, including separate modules implementations, demonstrations, tests, and pseudo-code diagrams.

## What is the Indus Village model?

UNDER CONSTRUCTION

## Overall design

The overall design of the Indus Village model is as follows:

[[https://github.com/Andros-Spica/indus-village-model/blob/master/diagrams/00-OverallDesign.png | width=400px | alt=Overall design]]

[Link to file](https://https://github.com/Andros-Spica/indus-village-model/blob/master/diagrams/00-OverallDesign.png)

## Road map of development and integration of submodels

[[https://github.com/Andros-Spica/indus-village-model/blob/master/diagrams/RoadMapSoFar_2021-05.png | alt=Overall design]]

(consolidated by Jan 2022) | [Link to file](https://https://github.com/Andros-Spica/indus-village-model/blob/master/diagrams/RoadMapSoFar_2021-05.png)

## Submodels and integrated versions

> **Task management legend**:  
> ⬜️ = to do  
> ✅ = completed (🔛 if still open for development)  
> ✏️ = under development  

1. [**Weather model**](https://github.com/Andros-Spica/indus-village-model/wiki/1.-Weather-model)
    1. _**Base version**_  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)  
    2. _**With spatial diversity**_  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)  

2. [**Soil Water model**](https://github.com/Andros-Spica/indus-village-model/wiki/2.-Soil-Water-model)   
(already integrated with Weather model)  
    1. _**Base version**_  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)  
    2. _**With spatial diversity**_  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)  

3. [**Land model**](https://github.com/Andros-Spica/indus-village-model/wiki/3.-Land-model)  
    1. _**Base version**_ (only elevation)  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)  
    2. _**With flow directions**_  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)  
    3. _**With flow accumulations**_  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)  
    4. _**With soil properties**_  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)  
    5. _**With initial ecological communities**_  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)   

---

I1. [**Integrated Land Water model**: Weather, Soil Water Balance, and Land models](https://github.com/Andros-Spica/indus-village-model/wiki/I1.-Integrated-Land-Water-model)  
        (design: ✅🔛 | implementation: ✅ | documentation: ✅🔛)  

---

4. [**Crop model**](https://github.com/Andros-Spica/indus-village-model/wiki/4.-Crop-model)   
(already integrated with Weather and Soil Water Balance models)  
    1. _**Base version**_  
        (design: ✅🔛 | implementation: ✅ | documentation: ✅🔛)  
    2. _**With spatial diversity**_  
        (design: ✅🔛 | implementation: ✅ | documentation: ✅🔛)  

---

I2. [**Integrated Land Crop model**: Weather, Soil Water Balance, Land, and Crop models](https://github.com/Andros-Spica/indus-village-model/wiki/I1.-Integrated-Land-Crop-model)  
        (design: ✅🔛 | implementation: ✅ | documentation: ✅🔛)

---

5. [**Household Demography model**](https://github.com/Andros-Spica/indus-village-model/wiki/5.-Household-Demography-model)
    1. _**Base version**_  
        (design: ✅🔛 | implementation: ✅🔛 | documentation: ✅🔛)  
    2. _**With kinship taboo for new couples**_  
        (design: ✅🔛 | implementation: ✅🔛 | documentation: ✏️)  
    3. _**With mortality regulated by nutrition score**_  
        (design: ✅🔛 | implementation: ✅🔛 | documentation: ✏️)
    4. _**With daily time step**_  
        (design: ✅🔛 | implementation: ✅🔛 | documentation: ✏️)    

6. [**Food Storage model**](https://github.com/Andros-Spica/indus-village-model/wiki/6.-Food-Storage-model)
    1. _**Base version**_  
        (design: ✅ | implementation: ✅ | documentation: ✅🔛)  
    2. _**With local exchange**_  
        (design: ✅🔛 | implementation: ✅🔛 | documentation: ✅🔛)  

7. [**Nutrition model**](https://github.com/Andros-Spica/indus-village-model/wiki/7.-Nutrition-model)
    1. _**Base version**_  
        (design: ✅🔛 | implementation: ✅🔛 | documentation: ✏️) 

8. [**Exchange model**](https://github.com/Andros-Spica/indus-village-model/wiki/9.-Exchange-model)  
    1. _**Base version**_  
        (design: ✅🔛 | implementation: ⬜️ | documentation: ⬜️)  

---

I3. [**Integrated Household Food Economy model**: Food Storage, Exchange, Nutrition, and Household Demography models](https://github.com/Andros-Spica/indus-village-model/wiki/I3.-Integrated-Household-Economy-model)  
        (design: ✅🔛 | implementation: ⬜️ | documentation: ⬜️)  

---

(NOTE: possibly, enlist here the algorithm to determine food production strategy as a separate model instead of having it as an aspect of I4)

---

I4. [**Integrated Landed Household Food Economy model**: integrated Land Crop and Household Food Economy models](https://github.com/Andros-Spica/indus-village-model/wiki/I4.-Integrated-Land-Household-Economy-model)  
        (design: ✅🔛 | implementation: ⬜️ | documentation: ⬜️)  

---

1. [**Household Position model**](https://github.com/Andros-Spica/indus-village-model/wiki/8.-Household-position-model)  
    1. _**Base version**_  
        (design: ✅🔛 | implementation: ⬜️ | documentation: ⬜️)  

---

I5. [**Indus Village model**: integrated Household Food Economy and Household Position models](https://github.com/Andros-Spica/indus-village-model/wiki/I5.-Indus-Village-model)  
        (design: ✅🔛 | implementation: ⬜️ | documentation: ⬜️)  

---

## Related publications

> Angourakis, A., Bates, J., Baudouin, J.-P., Giesche, A., Ustunkaya, M. C., Wright, N., Singh, R. N., & Petrie, C. A. (2020). How to ‘downsize’ a complex society: An agent-based modelling approach to assess the resilience of Indus Civilisation settlements to past climate change. Environmental Research Letters, 15(11), 115004. https://doi.org/10.1088/1748-9326/abacf9
