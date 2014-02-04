# Create useful subsets and summaries of algal transect data

# Read data #######

# Algal transects data
 # file: AlgalTransectsFormatted.txt (created by script 'AlgalTransects_format')

AlgalTransects2 = read.table(file.choose(),sep='\t',header=T,quote='')

# Find max Clad height for each survey point in each year #####

  # get max clad height in each year for each point

  CladMaxPoint = aggregate(CladInt ~ Transect + xstrm + year + Flood + PrevYearFlood, data=AlgalTransects2, FUN=max)
  
  # find and eliminate points that did not have water during growing season (which starts Apr 1 and ends July 31 - yday=91 thru 212) 
    # use only points that still had water on the last survey of the growing season in each year
      # want transect and survey points from the last survey of each transect in the growing season
  
  # find last survey during grow season
  LastGrowSurvey = aggregate(yearday ~ Transect + year, data=subset(AlgalTransects2, yearday<=212 & yearday>=91), FUN=max)
  
  # get transect points present at last survey from growing season in each year
  WetPoints = AlgalTransects2[which(paste(AlgalTransects2$Transect,AlgalTransects2$year,AlgalTransects2$yearday,sep='-') %in% paste(LastGrowSurvey$Transect,LastGrowSurvey$year,LastGrowSurvey$yearday,sep='-')),c('Transect','year','xstrm','yearday','depth')]
    
  # remove non-integer points
  WetPoints = WetPoints[which(WetPoints$xstrm%%1 == 0),]
  
  # look at width of channel
  
  aggregate(xstrm ~ Transect,data=WetPoints,FUN=min)
  aggregate(xstrm ~ Transect,data=WetPoints,FUN=max)
  
# Refine Clad Max data to include only wet points
  
  CladMaxPointWet = CladMaxPoint[which(paste(CladMaxPoint$Transect,CladMaxPoint$year,CladMaxPoint$xstrm)%in%paste(WetPoints$Transect,WetPoints$year,WetPoints$xstrm)),]

# add growing season averages of flow, depth, and light

  FlowAvg = aggregate(flow ~ year + Transect + xstrm, data=subset(AlgalTransects2, yearday<=212 & yearday>=91), FUN=mean)

  DepthAvg = aggregate(depth ~ year + Transect + xstrm, data=subset(AlgalTransects2, yearday<=212 & yearday>=91), FUN=mean)

  CladMaxPointWet = merge(CladMaxPointWet, FlowAvg, all.x=T)
  CladMaxPointWet = merge(CladMaxPointWet, DepthAvg, all.x=T)

# add substrate data (this still needs work)

  with(WetPoints, table(xstrm,substr,Transect))

  SubAvg = aggregate(substr ~ year + Transect + xstrm, data=subset(AlgalTransects2, yearday<=212 & yearday>=91), FUN=mode)


# add binary variable for clad growth

# look at distribution of max heights
  library(ggplot2)

  hist(log(CladMaxPointWet$CladInt[which(CladMaxPointWet$CladInt>0)], base=10), breaks=20)
  
  MaxHeight_p1 = ggplot(data=subset(CladMaxPointWet, CladInt>0), aes(x=CladInt)) + geom_density() + facet_grid(.~Transect) + scale_x_continuous(limits=c(0,50))
  
  # use 10 cm as a cutoff: >10 = Clad growth, <10 no growth

# establish column for Clad growth
CladMaxPointWet$CladGrowth= CladMaxPointWet$CladInt>=10
  
head(CladMaxPointWet)  

# write file to .csv
  # file name: 'AlgalTransects_PointCladMaxHeight.csv'
write.csv(CladMaxPointWet, file=file.choose(), row.names=F)
