# Examine relationship between light, flooding, and maximum Cladophora height in a given year at each sampling point

# Script created by Jonah P-S October 2013

# to do 
  # change to reading in algae data from file
  # add light info here

# Read data #######

# Maximum clad height at each point in each year
 # file: AlgalTransects_PointCladMaxHeight.csv (created by script 'AlgalTransects_Summaries')

CladMaxPointWet = read.csv(file.choose(),header=T)

# Light data
  # LightSummerAvg.csv (created by script 'AlgalTransects_LightSummerAvg')

wattavgf = read.csv(file.choose())

head(CladMaxPointWet)  

# look at distribution of max heights in flood and non-flood years
  
  MaxHeight_p2 = ggplot(data=CladMaxPointWet, aes(x=CladInt)) + geom_density() + facet_grid(Flood~Transect, scales='free') 
  
# look at growth
  
  with(CladMaxPointWet, table(Flood,CladGrowth, Transect))
  
  with(CladMaxPointWet, prop.table(table(Flood,CladGrowth, Transect), margin=c(1,3)))
  
# Add average growing season light input for each transect point to CladMaxPointWet data frame
  
 	CladMaxPointWet<-merge(CladMaxPointWet,wattavgf)
  	head(CladMaxPointWet,30)
  	tail(CladMaxPointWet)
  	dim(CladMaxPointWet)  	
  	
 # Initial plots to visualize the data 	
 	library(ggplot2)
 
 
	p<-ggplot(data=CladMaxPointWet, aes(x=watt_avg,y=CladInt,group=Transect))
  
  	p + geom_point() + facet_grid(Flood~Transect, scales='free_y')
  
	p2<-ggplot(data=CladMaxPointWet, aes(x=xstrm,y=CladInt,group=Transect))
 
 	p2 + geom_point() + geom_line(aes(x=xstrm,y=watt_avg/5), color="blue") + facet_grid(Flood~Transect, scales='free_y')

