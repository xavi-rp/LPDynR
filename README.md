# LPDynR

As part of the UN Sustainable Development Goal 15 (Life on Land), the [indicator 15.3.1](https://knowledge.unccd.int/ldn/ldn-monitoring/sdg-indicator-1531) is adopted to measure the Land Degradation Neutrality (stable —or increasing— state regarding the amount and quality of land resources required to support ecosystem functions and services and enhance food security during a certain period of time). It is a binary indicator (i.e. degraded/not degraded), expressed as the proportion (%) of land that is degraded over total land area, and is based on three sub-indicators: (1) Trends in Land Cover, (2) Land Productivity and (3) Carbon Stocks. 

The Land Productivity sub-indicator (LP) refers to the total above-ground Net Primary Production and reflects changes in health and productive capacity of the land. Its declining trends can be usually understood as land degradation. LP is calculated using the Land Productivity Dynamics (LPD) approach, first developed by Ivits and Cherlet (2013). The LPD approach uses phenological and productivity variables derived from time series of remote sensed imagery, particularly the normalized difference vegetation index (NDVI), to estimate ecosystem dynamics and change. 

LPD is the methodological basis of the *LPDynR* package. It is based on a combined assessment of two sources of information, as seen in Figure 1. On the one hand, the first layer is the Long Term Change Map and, in general terms, it shows the tendency of change of land productivity (positive or negative) and the effect that this tendency might have had on a particular original point after a certain period of time. On the other hand, the second layer is the Current Status Map, which provides information on the current efficiency levels of vegetation on the productivity or, in other words, the current level of land productivity in relation to its potential. Further explanations for both branches can be found in this [preprint](https://eartharxiv.org/repository/view/2294/). The final result of the indicator is a categorical map with 5 classes of land productivity dynamics, ranging from declining to increasing productivity.

&nbsp;

![](doc/graph02.png?raw=true)
Figure1: Flowchart of the process to calculate the Land Productivity Dynamics indicator and followed by *LPDynR*


&nbsp;

### To install the latest stable version from CRAN (https://cran.r-project.org/web/packages/LPDynR/index.html):

```
install.packages("LPDynR")
```

&nbsp;


### To install the latest 'under development' version:

```
library(devtools)
install_github("xavi-rp/LPDynR")
```

&nbsp;


### Examples:

After installing the package, you can check the vignettes for examples on how to calculate the LPD indicator using *LPDynR*.

```
library(LPDynR)
vignette(package = "LPDynR")  # to see available vignettes
```
&nbsp;


In the vignette *LPD_simple_example* we show a simple example on how to run the functions included in the *LPDynR* package to calculate the indicator from scratch. It uses the data set included in the package, but also you can use your own data.

```
vignette(topic = "LPD_simple_example", package = "LPDynR")
```
&nbsp;


The LPD indicator shows the dynamics of the land productivity giving higher importance to the point at which we are now and where we come from (i.e. the very last year of the time series and the first ones). To better understand the dynamics along the time series as well as the final result, the user might want to calculate several "partial indicators" using only part of the time series in addition to the one for the whole data set. In the vignette *LPD_PartialTimeSeries_example* we show an example on how to calculate these "partial indicators". 


```
vignette(topic = "LPD_PartialTimeSeries_example", package = "LPDynR")
```
&nbsp;



&nbsp;

### References

- Ivits, E. and M. Cherlet. 2013. 'Land-Productivity Dynamics Towards Integrated Assessment of Land Degradation at Global Scales'. Technical Report EUR 26052. Joint Research Centre of the European Commission.

- Rotllan-Puig, X., Ivits, E. and M. Cherlet. 2021. 'LPDynR: a new tool to calculate the Land Productivity Dynamics indicator'. Preprint in EarthArXiv. https://eartharxiv.org/repository/view/2294/
