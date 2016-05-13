# Examine relationship between light, flooding, and maximum Cladophora height in a given year at each sampling point

# Script created by Jonah P-S October 2013

# to do
  # change to reading in algae data from file
  # add light info here

#### Read data #######

## Maximum clad height at each point in each year
  # file: AlgalTransects_PointCladMaxHeight.csv (created by script 'AlgalTransects_Summaries')
  CladMaxPointWet.clf <- read.csv(file.choose(),header=T)
  #CladMaxPointWet.clf$xstrmInt <- round(CladMaxPointWet.clf$xstrm, digits = 0)

## Light data
  # average watt hours/day at each xstrm
  # LightModel_MarysTransects_SummerAvg.csv (created by script 'AlgalTransects_LightSummerAvg')
  wattavgf <- read.csv(file.choose())
  names(wattavgf)[2] <- "xstrm"

  # Cumulative Watt Hours on the solstice
    # LightModel_MarysTransects_CumulativeWattHoursSolstice.csv" (created by script "AlgalTransects_LightModel_CumulativeWattHours.R")
  cwatthours <- read.csv(file.choose())
  names(cwatthours)[2] <- "xstrm"

## Cumulative discharge for growing season
  # cumulative discharge from Apr 15 - June 21
  # File = "GrowSeason_TotalDischarge.txt"
  tot.discharge <- read.table(file.choose(), sep='\t', header=T)
  names(tot.discharge)[2] <- "discharge_cms"

## Spate data

  # Mary's spate data
  # file= "SpringSpatesMEP_8713.txt"
  #spatesMEP = read.table(file.choose(), header=T, sep='\t', quote='')

  # Jonah/Keith definition >5 cms after May 15
    # File "SpringSpatesJK_8713.txt"
    # spates <- read.table(file.choose(), header=T, sep= '\t', quote= '')
    # spate years = 1990, 1993, 1996, 2005, 2010
    CladMaxPointWet.clf$spate <- ifelse(CladMaxPointWet.clf$year == 1990, 1,
                                   ifelse(CladMaxPointWet.clf$year == 1993, 1,
                                          ifelse(CladMaxPointWet.clf$year == 1996, 1,
                                                 ifelse(CladMaxPointWet.clf$year == 2005, 1,
                                                        ifelse(CladMaxPointWet.clf$year == 2010, 1, 0)))))

#### Prepare Data  #######

## Merge light data into clad max data set
  cml <- merge(CladMaxPointWet.clf, cwatthours, all.x=T)
  cml <- merge(cml, wattavgf, all.x=T)

## merge discharge data into clad max data set
  cml <- merge(cml, tot.discharge, all.x=T)

## convert PrevYearFlood to factor and fix NAs
  cml$PrevYearFlood <- as.factor(cml$PrevYearFlood)
  cml$PrevYearFlood <- replace(cml$PrevYearFlood, which(cml$PrevYearFlood=='NA'),NA)

## order by year
  cml <- cml[order(cml$year, cml$transect), ]

## Write cml as a .csv "CladLightFloodOutput"
    #write.csv(cml, file="CladLightFloodOutput.csv", row.names = FALSE)

#### Plot prep ######

## Relationship between predictors
  library(ggplot2)

  pairs(~ flow + depth + cwatts_solstice + watt_avg, data=cml)

  plot_theme_CLF <- theme(panel.grid = element_blank(),
                     panel.background = element_blank(),
                     axis.text = element_text(colour="black"),
                     axis.line = element_line(colour="black"),
                     legend.key=element_blank())

#### Flood plots #####

## Distribution of max heights in flood and non-flood years

  MaxHeight.plot.f1 <- ggplot(data=subset(cml, PrevYearFlood!='NA'), aes(x=CladInt, color=Transect)) + geom_density() + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme_CLF; MaxHeight.plot.f1

## look at each transect
  # get color palette
  TransectColors = hcl(h=seq(15, 375, length=5),l=65,c=100)[1:4]

## transect 2
ggplot(data= subset(cml, PrevYearFlood!= 'NA' & Transect==2), aes(x= CladInt+1)) + geom_density(fill= TransectColors[1]) + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme_CLF + xlab('') + ylab('2') + theme(axis.title.y = element_text(face="bold", size=30))

# transect 2.5
ggplot(data=subset(cml, PrevYearFlood!='NA' & Transect==2.5), aes(x=CladInt+1)) + geom_density(fill=TransectColors[2]) + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme_CLF + xlab('') + ylab('2.5') + theme(axis.title.y = element_text(face="bold", size=30))

# transect 3
ggplot(data=subset(cml, PrevYearFlood!='NA' & Transect==3), aes(x=CladInt+1)) + geom_density(fill=TransectColors[3]) + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme_CLF + xlab('') + ylab('3') + theme(axis.title.y = element_text(face="bold", size=30))

# transect 4
ggplot(data=subset(cml, PrevYearFlood!='NA' & Transect==4), aes(x=CladInt+1)) + geom_density(fill=TransectColors[4]) + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme_CLF + xlab('') + ylab('4') + theme(axis.title.y = element_text(face="bold", size=30))

## look at growth
  with(cml, table(Flood,CladGrowth, Transect))
  with(CladMaxPointWet.clf, prop.table(table(Flood,CladGrowth, Transect), margin=c(1,3)))

#### Spate plots ####

# Mar 50 cms (1= spate in March >50 cms, 0= no spate)
ggplot(data=cml, aes(x=CladInt+1, color=Flood)) + geom_density() + facet_grid(mar50~.) + scale_x_log10() + plot_theme_CLF

# May 5 cms (1= spate in May >5 cms, 0= no spate)
ggplot(data=cml, aes(x=CladInt+1, color=Flood)) + geom_density() + facet_grid(may5~.) + scale_x_log10() + plot_theme_CLF + ggtitle(">5 cms after May 1") + theme(plot.title = element_text(lineheight=.8, face="bold"))

# Jun 5 cms
ggplot(data=cml, aes(x=CladInt+1, color=Flood)) + geom_density() + facet_grid(jun5~.) + scale_x_log10() + plot_theme_CLF + ggtitle(">5 cms after June 1") + theme(plot.title = element_text(lineheight=.8, face="bold"))

#### Light plots ####
	p <- ggplot(data=cml, aes(x= watt_avg, y= CladInt, group= Transect))
  p + geom_point() + facet_grid(Flood~Transect, scales='free_y') + plot_theme_CLF

	p2 <- ggplot(data=cml, aes(x= xstrm, y= CladInt+1, group= Transect))
 	p2 + geom_point(alpha=.2, size=2) + geom_line(aes(x=xstrm,y=watt_avg/5), color="orangered") + facet_grid(.~Transect, scales='free_y') + geom_ribbon(aes(ymin=(watt_avg - watt_stdev)/5, ymax=(watt_avg + watt_stdev)/5),fill='orangered',alpha=0.2) + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3) + plot_theme_CLF

  p3 <- ggplot(data=cml, aes(x=cwatts_solstice/10000,y=CladInt,group=Transect))
  p3 + geom_point() + facet_grid(Flood~Transect, scales='free_y') + theme_bw()

ggplot(data=cml, aes(x=cwatts_solstice, y=watt_avg)) + geom_point() + facet_grid(.~Transect) + theme_bw()

# depth plots ##########
 	p2 + geom_point(alpha=.2, size=2) + geom_smooth(data=subset(cml, depth>0),aes(x=xstrm,y=depth*8), color='orchid',fill='orchid',alpha=.3) + facet_grid(.~Transect, scales='free_y') + plot_theme1 + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3)

# flow plots ##########

   p2 + geom_point(alpha=.2, size=2) + geom_smooth(aes(x=xstrm,y=flow*5+1), color='blue',fill='blue',alpha=.2) + facet_grid(.~Transect, scales='free_y') + plot_theme_CLF + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3)

# substrate plots

   p2 + geom_point(alpha=.2, size=2) + geom_smooth(aes(x=xstrm,y=(stab*300)+1), color='brown',fill='brown',alpha=.2) + facet_grid(.~Transect, scales='free_y') + plot_theme_CLF + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3)

# all spatial predictors

p2 + geom_point(alpha=.2, size=2) + facet_grid(.~Transect, scales='free_y') + plot_theme_CLF + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3) +  geom_line(aes(x=xstrm,y=watt_avg/5), color="orangered") +  geom_smooth(data=subset(cml, depth>0),aes(x=xstrm,y=depth*8), color='orchid',fill='orchid',alpha=.3,se=F) + geom_smooth(aes(x=xstrm,y=flow*5+1), color='blue',fill='blue',alpha=.2, se=F) + geom_smooth(aes(x=xstrm,y=flow*5+1), color='blue',fill='blue',alpha=.2, se=F) + geom_smooth(aes(x=xstrm,y=(stab*300)+1), color='brown',fill='brown',alpha=.2,se=F)
