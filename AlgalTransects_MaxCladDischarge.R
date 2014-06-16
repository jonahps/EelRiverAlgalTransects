# This Script will plot the max clad height against the mean discharge during the growing season
# Created by K. Bouma-Gregson Nov. 2013

# Keith and Jonah looked at discharge data from 1990-2012 and determined that a growing season start date of May 15th excluded most winter rain events, while still including anomylous summer spates.

# discharge in cubic meters per second

# Input Files = (a2<-file="AlgalTransects_PointCladMaxHeight.csv"; a3<-file = "2013-08-27_AlgalTransects.txt") 

    a2<-read.csv(file.choose())
    #a3<-AlgalTransects2
    

# Calculate the average maximum Cladophora height at each transect per year  
# a2$CladInt is the maximum height of algae at each survey point for each year
    
    aMax<-aggregate(data=a2, CladInt ~  year + Transect, FUN=mean)
    
    head(aMax)
    #aMean<-aggregate(data=a2, CladInt ~ Transect + Rdate, FUN=mean)
    #aMean2<-transform(aMean, year = as.numeric(format(Rdate, "%Y")))
    #aMax<-aggregate(data=aMean2, CladInt ~ year + Transect, FUN=max)
    #aMax2<-aggregate(data=a2, CladInt ~  year + Transect, FUN=max)
    #aMax3<-aggregate(data=a3, CladInt ~  year + Transect, FUN=max)
    

# Add discharge data (file="AngeloDischarge_DailyMean_1987-2013.txt")
    
    dis<-read.table(file.choose(),header=T)
    
    
# Add flood data (file="AngeloBankfullFlood_1988-2013.txt")
    
    fl<-read.table(file.choose(), header=T, sep='\t', quote='')
    names(fl)[1:2] = c('year',"flood")
    
# Add julianday, month, and year columns    
    
    dis$Date<-as.Date(dis$Date,"%Y-%m-%d")
    
    dis<- transform(dis, Discharge_cms = as.numeric(Discharge_cms),  julian = as.numeric(format(Date, '%j')), month = format(Date, '%b'), year = as.numeric(format(Date, '%Y')))    
    
    dis$month<-factor(dis$month,levels(dis$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
    colnames(dis)[1:2]<-c("date","dis_cms")
    
    #head(dis)
    #str(dis)
    

# Extract data for growing season May 15 - September 1 
    
    disSeason<-dis[which(as.numeric(dis$julian) > 135 & as.numeric(dis$julian) < 245),]
    
    disSeason<-droplevels(disSeason)

    
# Load SummarySE function for calculating summary statistics
    
    summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                          conf.interval=.95, .drop=TRUE) {
      require(plyr)
      
      # New version of length which can handle NA's: if na.rm==T, don't count them
      length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
      }
      
      # This does the summary. For each group's data frame, return a vector with
      # N, mean, and sd
      datac <- ddply(data, groupvars, .drop=.drop,
                     .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm)
                       )
                     },
                     measurevar
      )
      
      # Rename the "mean" column    
      datac <- rename(datac, c("mean" = measurevar))
      
      datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
      
      # Confidence interval multiplier for standard error
      # Calculate t-statistic for confidence interval: 
      # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
      ciMult <- qt(conf.interval/2 + .5, datac$N-1)
      datac$ci <- datac$se * ciMult
      
      return(datac)
    }
  
# Calculate average discharge for each year
    
    monthly<-summarySE(data=disSeason, measurevar="dis_cms", groupvars=c("month","year"))
    
    yearly<-summarySE(data=disSeason, measurevar="dis_cms", groupvars="year")
    yearly$month<-as.factor("Season_mean")
    yearly<-yearly[,c(7,1:6)]
    
    disMean<-rbind(yearly, monthly)
    
    str(disMean)

# Merge discharge and flood data
    
    disMean<-merge(disMean,fl)
    str(disMean)    
    
# Merge algae and discharge data together    
    
    adis<-merge(disMean,aMax)
    
    adis$Transect<-as.factor(adis$Transect)
    colnames(adis)[9]<-"transect"
    
    head(adis)
    str(adis)
    
    
# Write the adis dataframe to a text file
    
    #setwd("/Users/keithgregson/Google Drive/Algal Timeseries/Processed data files/Algae")
    #write.table(adis, file="MaxClad_and_Discharge.txt", sep='\t', row.names=F, quote=F)

                
# Plot discharge data
    
    library(ggplot2)
    
    p<-ggplot(data=adis, aes(x=year,y=dis_cms,group=month))
    
    p + geom_line(aes(color=month)) + facet_grid(month~., scales="free_y")
    

# Histogram of discharge and CladInt distribution
    
    hist(adis[which(adis$month == "Season_mean"),"dis_cms"])
    hist(log(adis[which(adis$month == "Season_mean"),"dis_cms"]))

    hist(adis[which(adis$month == "Season_mean"),"CladInt"])
    hist(log(adis[which(adis$month == "Season_mean"),"CladInt"]))
    
    head(adis)
    
    
# Plot clad and discharge    

    c<-ggplot(data=adis[which(adis$month=="Season_mean"),], aes(x=dis_cms, y=CladInt, group=transect))
    
    c + geom_point(aes(color=flood)) + stat_smooth() + facet_grid(transect~., scales="free_y")
    
    
    logc<-ggplot(data=adis[which(adis$month=="Season_mean"),], aes(x=log(dis_cms), y=CladInt, group=transect))
    
    logc + geom_point(aes(color=flood)) + stat_smooth(method=lm, se=FALSE) + facet_grid(transect~., scales="free_y") + labs(x= "Discharge log(m^3/s)", y="Avg. Max. Clad height (cm)")   
    
    logc + geom_text(aes(color=flood, label=year), size=3) + stat_smooth(method=lm, se=FALSE) + facet_grid(transect~., scales="free_y") + labs(x= "Discharge log(m^3/s)", y="Avg. Max. Clad height (cm)") + ggtitle("Max. Cladophora height and Avg. Discharge\n(May 15 - Aug. 31)")
    
    logcMay<-ggplot(data=adis[which(adis$month=="May"),], aes(x=log(dis_cms), y=CladInt, group=transect))
    
    logcMay + geom_point(aes(color=flood)) + stat_smooth(method=lm, se=FALSE) + facet_grid(transect~., scales="free_y")
  
# average across transects

# compute averages
    
adis.avg = aggregate(cbind(CladInt,dis_cms) ~ year + flood,data=adis[which(adis$month=="Season_mean"),], FUN=mean)

# make plot for discharge
ggplot(data=adis.avg, aes(x=log(dis_cms), y=CladInt)) + geom_text(aes(color=flood, label=year), size=3) + labs(x= "Discharge log(m^3/s)", y="Avg. Max. Clad Height (cm)") + ggtitle("Max. Cladophora height and Avg. Discharge\n(May 15 - Aug. 31)") + plot_theme1
    
