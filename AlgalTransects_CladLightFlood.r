# Examine relationship between light, flooding, and Clad growth on an annual basis

# Code created by Jonah P-S October 2013

# first run "Algaltransects_format_2013-07-19.R" script

# look at max Clad height for each survey point in each year

  # get max clad height in each year for each point

  CladMaxPoint = aggregate(CladInt ~ Transect + xstrm + year + Flood, data=AlgalTransects2, FUN=max)
  
  # find and eliminate points that did not have water during growing season (which starts Apr 1 and ends July 31 - yday=91 thru 212) 
    # use only points that still had water on the last survey of the growing season in each year
      # want transect and survey points from the last survey of each transect in the growing season
  
  # find last survey during grow season
  LastGrowSurvey = aggregate(yearday ~ Transect + year, data=subset(AlgalTransects2, yearday<=212 & yearday>=91), FUN=max)
  
  # get transect points present at last survey in each year
  WetPoints = AlgalTransects2[which(paste(AlgalTransects2$Transect,AlgalTransects2$year,AlgalTransects2$yearday,sep='-') %in% paste(LastGrowSurvey$Transect,LastGrowSurvey$year,LastGrowSurvey$yearday,sep='-')),c('Transect','year','xstrm','yearday','depth')]
    
  # remove non-integer points
  WetPoints = WetPoints[which(WetPoints$xstrm%%1 == 0),]
  
  # look at width of channel
  
  aggregate(xstrm ~ Transect,data=WetPoints,FUN=min)
  aggregate(xstrm ~ Transect,data=WetPoints,FUN=max)
  
# Refine Clad Max data to include only wet points
  
  CladMaxPointWet = CladMaxPoint[which(paste(CladMaxPoint$Transect,CladMaxPoint$year,CladMaxPoint$xstrm)%in%paste(WetPoints$Transect,WetPoints$year,WetPoints$xstrm)),]
  
# look at distribution of max heights
  library(ggplot2)

  hist(log(CladMaxPointWet$CladInt[which(CladMaxPointWet$CladInt>0)], base=10), breaks=20)
  
  MaxHeight_p1 = ggplot(data=subset(CladMaxPointWet, CladInt>0), aes(x=CladInt)) + geom_density() + facet_grid(.~Transect) + scale_x_continuous(limits=c(0,50))
  
  # use 10 cm as a cutoff: >10 = Clad growth, <10 no growth

# establish column for Clad growth
CladMaxPointWet$CladGrowth= CladMaxPointWet$CladInt>=10
  
head(CladMaxPointWet)  

# look at distribution of max heights in flood and non-flood years
  
  MaxHeight_p2 = ggplot(data=CladMaxPointWet, aes(x=CladInt)) + geom_density() + facet_grid(Flood~Transect, scales='free') 
  
# look at growth
  
  with(CladMaxPointWet, table(Flood,CladGrowth, Transect))
  
  with(CladMaxPointWet, prop.table(table(Flood,CladGrowth, Transect), margin=c(1,3)))
  
 #Merge the CladMaxPointWet and wattavgf (from "AlgalTransects_SummerAvg.R script") datasets together
 
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

