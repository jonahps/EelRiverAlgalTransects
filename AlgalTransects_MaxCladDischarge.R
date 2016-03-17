## This Script will plot the max clad height against the mean discharge during the growing season
## Created by K. Bouma-Gregson Nov. 2013

## Keith and Jonah looked at discharge data from 1990-2012
## and determined that a growing season start date of May 15th
## excluded most winter rain events, while still including anomylous summer spates.
## however, after talking with Mary, we decided on a April 15th start date

## discharge in cubic meters per second

## Input Files
  # a2 = file="AlgalTransects_PointCladMaxHeight.csv";
  # a3 = file = "2013-08-27_AlgalTransects.txt")
    a2 <- read.csv(file.choose())
    #a3 <- read.table(file.choose(),sep='\t',header=T,quote='')

## Calculate the average maximum Cladophora height at each transect per year
  # a2$CladInt is the maximum height of algae at each survey point for each year
  aMax <- aggregate(data= a2, CladInt ~  year + Transect, FUN=mean)
  head(aMax)

  #aMean<-aggregate(data=a2, CladInt ~ Transect + Rdate, FUN=mean)
  #aMean2<-transform(aMean, year = as.numeric(format(Rdate, "%Y")))
  #aMax<-aggregate(data=aMean2, CladInt ~ year + Transect, FUN=max)
  #aMax2<-aggregate(data=a2, CladInt ~  year + Transect, FUN=max)
  #aMax3<-aggregate(data=a3, CladInt ~  year + Transect, FUN=max)

## Add discharge data (file="AngeloDischarge_DailyMean_1987-2013.txt")
  # cms = cubic meters per second
  dis <- read.table(file.choose(),header=T)


## Add flood data (file="AngeloBankfullFlood_1988-2013.txt")
  fl <- read.table(file.choose(), header=T, sep='\t', quote='')
  names(fl)[1:2] = c('year', "flood")

## Add julianday, month, and year columns
  dis$Date <- as.Date(dis$Date, "%Y-%m-%d")
  dis <- transform(dis, Discharge_cms= as.numeric(Discharge_cms),
                   julian= as.numeric(format(Date, '%j')),
                   month= format(Date, '%b'),
                   year= as.numeric(format(Date, '%Y')))
  dis$month <- factor(dis$month, levels(dis$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
  colnames(dis)[1:2] <- c("date","dis_cms")

## Extract data for growing season May 15 - September 1 (using Julian days)
    disSeason <- dis[which(as.numeric(dis$julian) > 135 & as.numeric(dis$julian) < 245),]
    disSeason <- droplevels(disSeason)

# Calculate average discharge for each year
    library(plyr)
    monthly <- ddply(disSeason, c("month", "year"), summarise,
                      N= length(dis_cms),
                      mean.dis_cms= round(mean(dis_cms), 3),
                      se.dis_cms= round(sd(dis_cms) / sqrt(N), 2)
                      )
    head(monthly)

    yearly <- ddply(disSeason, c("year"), summarise,
                      N= length(dis_cms),
                      mean.dis_cms= round(mean(dis_cms), 3),
                      se.dis_cms= round(sd(dis_cms) / sqrt(N), 2)
                     )
    yearly$month <- as.factor("Season_mean")
    yearly <- yearly[,c(5,1:4)]
    disMean <- rbind(yearly, monthly)
    str(disMean)

# Merge discharge and flood data
    disMean <- merge(disMean,fl)
    str(disMean)

# Merge algae and discharge data together
    adis <- merge(disMean, aMax)

    adis$Transect <- as.factor(adis$Transect)
    colnames(adis)[7] <- "transect"

    head(adis)
    str(adis)


# Write the adis dataframe to a text file

    #setwd("/Users/keithgregson/Google Drive/Algal Timeseries/Processed data files/Algae")
    #write.table(adis, file="MaxClad_and_Discharge.txt", sep='\t', row.names=F, quote=F)


## Plot discharge data
    library(ggplot2)

    p <- ggplot(data= adis, aes(x= year,y= mean.dis_cms, group= month))
    p + geom_line(aes(color= month)) + facet_grid(month ~ ., scales="free_y") + theme_bw(base_size = 20)


# Histogram of discharge and CladInt distribution
    hist(adis[which(adis$month == "Season_mean"),"mean.dis_cms"])
    hist(log(adis[which(adis$month == "Season_mean"),"mean.dis_cms"]))

    hist(adis[which(adis$month == "Season_mean"),"CladInt"])
    hist(log(adis[which(adis$month == "Season_mean"),"CladInt"]))

## Plot clad and discharge

    c <- ggplot(data= adis[which(adis$month=="Season_mean"),], aes(x= mean.dis_cms, y= CladInt, group= transect))

    c + geom_point(aes(color= flood)) + stat_smooth(se= FALSE) + facet_grid(transect ~ ., scales= "free_y") + theme_bw(base_size = 20)


    logc <- ggplot(data= adis[which(adis$month=="Season_mean"),], aes(x= log(mean.dis_cms), y= CladInt, group= transect))
    logc + geom_point(aes(color= flood)) + stat_smooth(method=lm, se=FALSE) + facet_grid(transect~., scales="free_y") + labs(x= "Discharge log(m^3/s)", y="Avg. Max. Clad height (cm)") + theme_bw(base_size = 20)
    logc + geom_text(aes(color=flood, label=year), size=3) + stat_smooth(method=lm, se=FALSE) + facet_grid(transect~., scales="free_y") + labs(x= "Discharge log(m^3/s)", y="Avg. Max. Clad height (cm)") + ggtitle("Max. Cladophora height and Avg. Discharge\n(May 15 - Aug. 31)") + theme_bw(base_size = 20)

    logcMay <- ggplot(data=adis[which(adis$month=="May"),], aes(x=log(dis_cms), y=CladInt, group=transect)) + theme_bw(base_size = 20)
    logcMay + geom_point(aes(color=flood)) + stat_smooth(method=lm, se=FALSE) + facet_grid(transect~., scales="free_y") + theme_bw(base_size = 20)

# average across transects

# compute averages

adis.avg = aggregate(cbind(CladInt, mean.dis_cms) ~ year + flood, data=adis[which(adis$month=="Season_mean"),], FUN=mean)

# make plot for discharge
ggplot(data=adis.avg, aes(x=log(mean.dis_cms), y=CladInt)) + geom_text(aes(color=flood, label=year), size=3) + labs(x= "Discharge log(m^3/s)", y="Avg. Max. Clad Height (cm)") + ggtitle("Max. Cladophora height and Avg. Discharge\n (May 15 - Aug. 31)") + theme_bw(base_size = 20)

