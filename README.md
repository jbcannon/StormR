
# StormR

<!-- badges: start -->
<!-- badges: end -->

## Overview

The goal of StormR is to compute regimes of wind and other indices of interests generated by 
Tropical Cyclones over the whole world.

## Models

Two cyclonic models are available within this package in order to compute radial wind speed:

Willoughby et al. 2006
```math
\left\{
\begin{aligned}
v_r &= msw\left(\frac{r}{rmw}\right)^{nn} \quad if \quad r <rmw\\
v_r &= msw((1-AA))e^{-\frac{|r-rmw|}{XX1}} + AA e^{-\frac{|r-rmw|}{XX2}}) \quad if \quad r \geq rmw\\
\end{aligned}
\right.
```




Holand et al. 1980


## Installation

You can install the development version of StormR like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Usage

This is a basic example which shows you how to solve some common problems

``` r
library(StormR)

######################
#Focus on a single TC#
######################

#Load TC
sts = getStorm(time_period = 2015, name = "PAM", loi = "Vanuatu")

#Plot TC over the location of interest
plotStorms(sts, labels = T, legends = T)

#Compute Maximum Sustained Wind raster according to Willoughby et al. 2006 analytic model
#adding version 2 formula of asymmetry 
msw = stormBehaviour(sts, asymmetry = "V2", verbose = T)

#Plot the above raster alongside with the track of the storm
plotBehaviour(sts, msw, labels = T)


######################
#Focus on several TCs#
######################

#Load TC
sts = getStorm(time_period = c(2003, 2021), name = c("ERICA", "NIRAN"), loi = "New Caledonia")

#Plot TCs over the location of interest
plotStorms(sts, labels = T, legends = T)

#Plot NIRAN alone over the location of interest
plotStorms(sts, names = "NIRAN", labels = T)

#Compute Power Dissipation index raster according to Willoughby et al. 2006 analytic model
pdi = stormBehaviour(sts, product = "PDI" , verbose = T)

#Plot the PDI for ERICA alongside with the ERICA's track 
plotBehaviour(sts, pdi[["ERICA_PDI"]], labels = T)


#Compute time series of wind speed on coordinates contained in df according Willoughby et al. 2006 #analytic model, adding version 2 formula of asymmetry 
df = data.frame(lon = c(166.5, 166.7), lat = c(-22.1, - 22.3))
wind.ts = stormBehaviour(sts, result = df, verbose = T)

##############
#Get all TCs #
##############

#Get all TCs over Tropical Depression around the world between 1980 and 2021
sts = getStorm(basin = "SA", verbose = T)

#Plot TCs over category 5
plotStorms(sts, category = 5)


```

## Getting help

If you have any question or suggestion or if you want to report a bug, please do it via the GitHub issues.
Thanks for that, this would greatly help us to improve this package.


