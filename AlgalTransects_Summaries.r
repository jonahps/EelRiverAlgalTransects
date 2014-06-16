# Create useful subsets and summaries of algal transect data

# Read data #######

# Algal transects data
 # file: AlgalTransectsFormatted.txt (created by script 'AlgalTransects_format')

AlgalTransects2 = read.table(file.choose(),sep='\t',header=T,quote='')

# Find max Clad height for each survey point in each year #####

  # get max clad height in each year for each point

  CladMaxPoint = aggregate(CladInt ~ Transect + xstrm + year , data=AlgalTransects2, FUN=max)
  
# + Flood + PrevYearFlood

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

# create stability index
  # bedrock=3
  # boulders=2
  # cobbles=1
  # gravel,mud,pebbles,roots,sand,silt,wood=0

AlgalTransects2$stab = as.numeric(with(AlgalTransects2, ifelse(substr=='NAN',NA,ifelse(substr=='bedrock',3,ifelse(substr=='boulders',2,ifelse(substr=='cobbles',1,0))))))

SubAvg = aggregate(stab ~ year + Transect + xstrm, data=subset(AlgalTransects2, yearday<=212 & yearday>=91), FUN=mean)

CladMaxPointWet = merge(CladMaxPointWet,SubAvg,all.x=T)

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

# Find most common substrate at each point during the growth season ######

library(ggplot2)

names(AlgalTransects2)

levels(AlgalTransects2$substr)

PtsSampled = aggregate(substr~tranxstr,data=AlgalTransects2,FUN=length)

PtsSampled[order(PtsSampled$substr,decreasing=T),]

hist(log(PtsSampled$substr,10))

# use only points that have at least 10 observations for evaluation

PtsSampled10 = droplevels(PtsSampled[which(PtsSampled$substr>=10),'tranxstr'])

atSubstrEval= droplevels(AlgalTransects2[which(AlgalTransects2$tranxstr %in% PtsSampled10),])

# plots

# all data
ggplot(atSubstrEval,aes(x=xstrm,fill=substr)) + geom_bar() + facet_grid(.~transect)

# growth season only
ggplot(subset(atSubstrEval,yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_grid(.~transect)

# using stability from CladMaxPointsWet
names(CladMaxPointWet)[1]='transect'

ggplot(AlgalTransects2,aes(x=xstrm,fill=substr)) + geom_bar() + facet_grid(.~transect) + geom_smooth(data=CladMaxPointWet,aes(x=xstrm,y=stab*100),se=F) 

  # not working

# transects by year

ggplot(subset(AlgalTransects2,transect==2.5&yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_wrap('year') + xlab('Transect 2.5')

ggplot(subset(AlgalTransects2,transect==2&yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_wrap('year') + xlab('Transect 2')

ggplot(subset(AlgalTransects2,transect==3&yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_wrap('year') + xlab('Transect 3')

ggplot(subset(AlgalTransects2,transect==4&yearday>=105&yearday<=172),aes(x=xstrm,fill=substr)) + geom_bar() + facet_wrap('year') + xlab('Transect 4')
