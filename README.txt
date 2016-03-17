This repository contains data and R code for analyses of benthic freshwater algae communities in the South Fork of the Eel River at the Anglo Reserve in Mendocino County, California. 

The data was collected by Mary E. Power. 
This repository was established by Jonah Piovia-Scott.

File Identities

#### Formatting Scripts ####
add and accumulate columnes, they do not reduce or summarize rows

    AlgalTransects.Format.R
master formatting script
          checked: 18-Oct-2014


    AlgalTransects_LightModel_inputscript.R
This script takes the txt output file from Collin's light model and transforms it for manipulations in R. The script also tidys up the data and renames fields etc.
          checked: 18-Oct-2014



    AlgalTransects_Summaries.R
Create useful subsets and summaries of algal transect data
          checked: 18-Oct-2014
          after line 84 the code doesn't work, but it seems that most of the data summarizing has been done by then

#### Hydrology Data ####

    



#### Light Data #####



    AlgalTransects_LightModel_LightSummerAvg.R 
calculates the average summer light irradiance along Mary's 4 transects from April 15 - June 21
          checked: 18-Oct-2014

    AlgalTransects_LightModel_CummulativeWattHours.R
his script calculates  the cumulative watt hours of sunlight that have hit a given point from April 15 to the summer solstice (yearday = 172) for each point in all the transects.
          checked: 18-Oct-2014





  AlgalTransects_CladLightFlood.R - DIDNT FINISH
Examine relationship between light, flooding, and maximum Cladophora height in a given year at each sampling point
          checked: 18-Oct-2014

###
