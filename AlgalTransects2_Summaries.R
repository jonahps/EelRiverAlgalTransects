# Create useful subsets and summaries of algal transect data

## Read in data
  # Algal transects data
  # file: AlgalTransectsFormatted.txt (created by script 'AlgalTransects_Format')

  AlgalTransects2.sum <- read.table(file.choose(),sep='\t',header=T,quote='')

#### Find max Clad height for each survey point in each year #####
## get max clad height in each year for each point
  CladMaxPoint <- aggregate(CladInt ~ Transect + xstrm + year + Flood + PrevYearFlood , data=AlgalTransects2.sum, FUN= max)

  ## find and eliminate points that did not have water during growing season (which starts Apr 15 and ends July 31  (yearday= 105 thru 212))
    # use only points that still had water on the last survey of the growing season in each year
      # want transect and survey points from the last survey of each transect in the growing season

## find last survey during grow season
  LastGrowSurvey <- aggregate(yearday ~ Transect + year, data=subset(AlgalTransects2.sum, yearday<=212 & yearday>=105), FUN=max)

## get transect points present at last survey from growing season in each year
  WetPoints <- AlgalTransects2.sum[which(paste(AlgalTransects2.sum$Transect,
                                               AlgalTransects2.sum$year,
                                               AlgalTransects2.sum$yearday,
                                               sep='-')
                                         %in%
                                           paste(LastGrowSurvey$Transect,
                                                 LastGrowSurvey$year,
                                                 LastGrowSurvey$yearday,
                                                 sep='-')),
                                   c('Transect', 'year', 'xstrm', 'yearday', 'depth')]

## remove non-integer points
  WetPoints <- WetPoints[which(WetPoints$xstrm%%1 == 0),]

  table(WetPoints$xstrm%%1 == 0)
  table(WetPoints$xstrm)

## look at width of channel
  aggregate(xstrm ~ Transect, data=WetPoints, FUN=min)
  aggregate(xstrm ~ Transect, data=WetPoints, FUN=max)

## Refine Clad Max data to include only wet points
  CladMaxPointWet <- CladMaxPoint[which(paste(CladMaxPoint$Transect,
                                              CladMaxPoint$year,
                                              CladMaxPoint$xstrm)
                                        %in%
                                          paste(WetPoints$Transect,
                                                WetPoints$year,
                                                WetPoints$xstrm)), ]

## add growing season averages of flow, depth, and light
  FlowAvg <- aggregate(flow ~ year + Transect + xstrm,
                       data= subset(AlgalTransects2.sum, yearday<=212 & yearday>=105), FUN=mean)
  DepthAvg <- aggregate(depth ~ year + Transect + xstrm,
                        data= subset(AlgalTransects2.sum, yearday<=212 & yearday>=105), FUN=mean)

  CladMaxPointWet <- merge(CladMaxPointWet, FlowAvg, all.x=T)
  CladMaxPointWet <- merge(CladMaxPointWet, DepthAvg, all.x=T)

#### Add substrate data (this still needs work) ####
  # create stability index
    # bedrock=3
    # boulders=2
    # cobbles=1
    # gravel,mud,pebbles,roots,sand,silt,wood=0

  AlgalTransects2.sum$stab <- as.numeric(with(AlgalTransects2.sum,
                                              ifelse(substr=='NAN', NA,
                                                     ifelse(substr=='bedrock', 3,
                                                            ifelse(substr=='boulders', 2,
                                                                   ifelse(substr=='cobbles', 1, 0
                                                                   ))))))

  library(plyr)
  SubAvg <- aggregate(stab ~ year + Transect + xstrm,
                     data= subset(AlgalTransects2.sum, yearday<=212 & yearday>=105), FUN= mean)

  SubAvg2 <- ddply(AlgalTransects2.sum[which(AlgalTransects2.sum$yearday<=212 & AlgalTransects2.sum$yearday>=105), ], c("year", "Transect", "xstrm"), summarise,
                   N = length(stab),
                   stab.mean = mean(stab, na.rm= T),
                   stab.sd = sd(stab, na.rm= T)
                   )
  subset(SubAvg2, Transect == 2)
  CladMaxPointWet = merge(CladMaxPointWet, SubAvg, all.x=T)
?ddply
#### Add binary variable for clad growth ####

## Look at distribution of max heights
  library(ggplot2)
  hist(log(CladMaxPointWet$CladInt[which(CladMaxPointWet$CladInt>0)], base=10), breaks=20)

  MaxHeight_p1 <- ggplot(data=subset(CladMaxPointWet, CladInt>0), aes(x=CladInt)) + geom_density() + facet_grid(.~Transect) + scale_x_continuous(limits=c(0,50))
  MaxHeight_p1

## use 10 cm as a cutoff: >10 = Clad growth, <10 no growth
  CladMaxPointWet$CladGrowth <-  CladMaxPointWet$CladInt >= 10

  head(CladMaxPointWet)

## write file to .csv
  #setwd("/Users/keithgregson/Google Drive/Algal Timeseries/Processed data files/Algae")
  #write.csv(CladMaxPointWet, file="AlgalTransects_PointCladMaxHeight.csv", row.names=F)

# Find most common substrate at each point during the growth season ######

###### Jonah's previous script (I don't understand what tranxstr is) ####
library(ggplot2)

names(AlgalTransects2.sum)

levels(AlgalTransects2.sum$substr)

PtsSampled = aggregate(substr~tranxstr,data=AlgalTransects2.sum,FUN=length)

PtsSampled[order(PtsSampled$substr,decreasing=T),]

hist(log(PtsSampled$substr,10))

 # Find how much substrate data at exists at xstrm point in each year

subLength <- aggregate(substr ~ Transect + xstrm + year, data=AlgalTransects2.sum, FUN=length)

str(subLength)
unique(subLength$substr)
hist(subLength$substr)

subMode <- aggregate(as.numeric(substr) ~ Transect + xstrm + year, data=AlgalTransects2.sum, FUN=mode)

str(subMode)
unique(subMode$substr)
?mod
str(AlgalTransects2.sum  )

?aggregate

# use only points that have at least 10 observations for evaluation

PtsSampled10 = droplevels(PtsSampled[which(PtsSampled$substr>=10),'tranxstr'])

atSubstrEval= droplevels(AlgalTransects2.sum[which(AlgalTransects2.sum$tranxstr %in% PtsSampled10),])

# plots

# all data
ggplot(atSubstrEval,aes(x=xstrm,fill=substr)) + geom_bar() + facet_grid(.~transect)

# growth season only
ggplot(subset(atSubstrEval,yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_grid(.~transect)

# using stability from CladMaxPointsWet
names(CladMaxPointWet)[1]='transect'

ggplot(AlgalTransects2.sum,aes(x=xstrm,fill=substr)) + geom_bar() + facet_grid(.~transect) + geom_smooth(data=CladMaxPointWet,aes(x=xstrm,y=stab*100),se=F)

  # not working

# transects by year

ggplot(subset(AlgalTransects2.sum,transect==2.5&yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_wrap('year') + xlab('Transect 2.5')

ggplot(subset(AlgalTransects2.sum,transect==2&yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_wrap('year') + xlab('Transect 2')

ggplot(subset(AlgalTransects2.sum,transect==3&yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_wrap('year') + xlab('Transect 3')

ggplot(subset(AlgalTransects2.sum,transect==4&yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_wrap('year') + xlab('Transect 4')
