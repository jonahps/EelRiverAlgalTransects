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

# Spate data

# Mary's spate data
# file="SpringSpatesMEP_8713.txt"
    
    spatesMEP = read.table(file.choose(), header=T, sep='\t', quote='')

# prepare data  #######

#Merge light data into clad max data set

cml = merge(CladMaxPointWet, wattavgf, all.x=T)

# merge spate data into clad max data set

cml = merge(cml, spatesMEP, all.x=T)

# convert PrevYearFlood to factor and fix NAs

cml$PrevYearFlood = as.factor(cml$PrevYearFlood)
cml$PrevYearFlood = replace(cml$PrevYearFlood, which(cml$PrevYearFlood=='NA'),NA)

# Plot prep ######

library(ggplot2)

plot_theme1<-theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_text(colour="black"), axis.line = element_line(colour="black"), legend.key=element_blank())

# Flood plots #####

# look at distribution of max heights in flood and non-flood years
  
MaxHeight.plot.f1 = 
  ggplot(data=subset(cml, PrevYearFlood!='NA'), aes(x=CladInt+1, color=Transect)) + geom_density() + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme1

# look at each transect

# get color palette

TransectColors = hcl(h=seq(15, 375, length=5),l=65,c=100)[1:4]

# transect 2
ggplot(data=subset(cml, PrevYearFlood!='NA' & Transect==2), aes(x=CladInt+1)) + geom_density(fill=TransectColors[1]) + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme1 + xlab('') + ylab('2') + theme(axis.title.y = element_text(face="bold", size=30))

# transect 2.5
ggplot(data=subset(cml, PrevYearFlood!='NA' & Transect==2.5), aes(x=CladInt+1)) + geom_density(fill=TransectColors[2]) + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme1 + xlab('') + ylab('2.5') + theme(axis.title.y = element_text(face="bold", size=30))

# transect 3
ggplot(data=subset(cml, PrevYearFlood!='NA' & Transect==3), aes(x=CladInt+1)) + geom_density(fill=TransectColors[3]) + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme1 + xlab('') + ylab('3') + theme(axis.title.y = element_text(face="bold", size=30))

# transect 4
ggplot(data=subset(cml, PrevYearFlood!='NA' & Transect==4), aes(x=CladInt+1)) + geom_density(fill=TransectColors[4]) + facet_grid(Flood~PrevYearFlood) + scale_x_log10() + plot_theme1 + xlab('') + ylab('4') + theme(axis.title.y = element_text(face="bold", size=30))

# look at growth
  
  with(cml, table(Flood,CladGrowth, Transect))
  
  with(CladMaxPointWet, prop.table(table(Flood,CladGrowth, Transect), margin=c(1,3)))
 
# Spate plots ############

# Mar 50 cms
ggplot(data=cml, aes(x=CladInt+1, color=Flood)) + geom_density() + facet_grid(mar50~.) + scale_x_log10() + plot_theme1

# May 5 cms
ggplot(data=cml, aes(x=CladInt+1, color=Flood)) + geom_density() + facet_grid(may5~.) + scale_x_log10() + plot_theme1 + ggtitle(">5 cms after May 1") + theme(plot.title = element_text(lineheight=.8, face="bold"))

# Jun 5 cms
ggplot(data=cml, aes(x=CladInt+1, color=Flood)) + geom_density() + facet_grid(jun5~.) + scale_x_log10() + plot_theme1 + ggtitle(">5 cms after June 1") + theme(plot.title = element_text(lineheight=.8, face="bold"))

# Light plots ##########
	p<-ggplot(data=cml, aes(x=watt_avg,y=CladInt,group=Transect))
  
  	p + geom_point() + facet_grid(Flood~Transect, scales='free_y')
  
	p2<-ggplot(data=cml, aes(x=xstrm,y=CladInt+1,group=Transect))

 	p2 + geom_point(alpha=.2, size=2) + geom_line(aes(x=xstrm,y=watt_avg/5), color="orangered") + facet_grid(.~Transect, scales='free_y') + plot_theme1 + geom_ribbon(aes(ymin=(watt_avg - watt_stdev)/5, ymax=(watt_avg + watt_stdev)/5),fill='orangered',alpha=0.2) + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3) 

# depth plots ##########

 	p2 + geom_point(alpha=.2, size=2) + geom_smooth(data=subset(cml, depth>0),aes(x=xstrm,y=depth*8), color='orchid',fill='orchid',alpha=.3) + facet_grid(.~Transect, scales='free_y') + plot_theme1 + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3) 

# flow plots ##########

   p2 + geom_point(alpha=.2, size=2) + geom_smooth(aes(x=xstrm,y=flow*5+1), color='blue',fill='blue',alpha=.2) + facet_grid(.~Transect, scales='free_y') + plot_theme1 + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3)

# substrate plots

   p2 + geom_point(alpha=.2, size=2) + geom_smooth(aes(x=xstrm,y=(stab*300)+1), color='brown',fill='brown',alpha=.2) + facet_grid(.~Transect, scales='free_y') + plot_theme1 + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3)

# all spatial predictors

p2 + geom_point(alpha=.2, size=2) + facet_grid(.~Transect, scales='free_y') + plot_theme1 + scale_y_log10(limits=c(1,1000)) + geom_smooth(color='green2',fill='green2',se=T, alpha=.3) +  geom_line(aes(x=xstrm,y=watt_avg/5), color="orangered") +  geom_smooth(data=subset(cml, depth>0),aes(x=xstrm,y=depth*8), color='orchid',fill='orchid',alpha=.3,se=F) + geom_smooth(aes(x=xstrm,y=flow*5+1), color='blue',fill='blue',alpha=.2, se=F) + geom_smooth(aes(x=xstrm,y=flow*5+1), color='blue',fill='blue',alpha=.2, se=F) + geom_smooth(aes(x=xstrm,y=(stab*300)+1), color='brown',fill='brown',alpha=.2,se=F) 