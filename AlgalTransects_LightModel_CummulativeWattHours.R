## This script will add a new column in the Algal Transects data frame that includes the cummulative watt hours of sunlight that have hit a given point from April 15 to the date of the data in that row

# Script created by K. Bouma-Gregson 12-June-2014

# first make a dataframe with all the irradiance cummulative sums. Each row will be a unique transect point and each colum will be the sum irradiance from April 15 to each successive week until reaching ~June 21. Then feed these values into the algal data frame


#### Light data ####


# Input file = "lightmodel_MarysTransects_clean_long.csv"
    # this file was the output from the "AlgalTransects_LightModel_inputscript.R" script

Lightdf<-read.csv(file.choose())

Ldf<-Lightdf


# Clean up data frame
Ldf<-Ldf[,-1]
Ldf$Transect<-as.factor(Ldf$Transect)

head(Ldf)
str(Ldf)


# Extract the weeks of the growing season April 15 - June 21 (approximately weeks 1 - 27 (July 1 is week 27 in case there are some survey dates just after June 21))
Ldf<-Ldf[which(Ldf$week >=15 & Ldf$week <=27),]


# Drop unused levels in transect factor (artifacts of raw data)
Ldf$tx<-droplevels(Ldf$tx)


# Remove un-needed columns
Ldfr<-Ldf[,c(-3,-4,-8)]


## Make Ldfr wide format
# load "reshape" library
library(reshape2)

Ldfw<-reshape(data=Ldfr, timevar= ("julianday"), idvar = c("Transect","xstrm","tx"), new.row.names=seq(1,71,1), direction="wide")

head(Ldfw)


# Remove row #52 in Ldfw because it is a typo. There can't be a value for 32 and 33 meters from the irradiance model
Ldfw<-Ldfw[-52,]


# Make a new data frame with each column as each day of the year ~April 15 - ~July 1 (needs to be a bit longer to match the irradiance dataJulian days 103-193). Since the first Julian Day from the irradiance model is 5, but the first day of the year is JD=1. I subtracted 4 from the irradiance JD values to make the values trace back to Jan. 1.


#Vector of Julian days
jd<-seq(from=99, to=189, by=1)
length(jd)


# New data frame of watt/hours with 91 columns, one for each julian day
juldf<-as.data.frame(rep(Ldfw[,c(4:16)],each = 7))
colnames(juldf)<-as.character(jd)
head(juldf)


#Empty data frame to put the cummulative sum values in

julCS<-data.frame(matrix(nrow=70, ncol=91))
colnames(julCS)<-as.character(jd)


# Loop to calculate the cummulative sum of watt hours per day and put it in the julCS data frame

for(i in 1:length(rownames(julCS))){
          julCS[i,]<-cumsum(as.numeric(juldf[i,]))
      }


## Add Transect, xstrm, and tx columns to the cummulative sum data

julCSb<-cbind(Ldfw[,c(1:3)], julCS)

str(julCSb)
head(julCSb)


#plot(y=julCSb[25,4:94], x=seq(99,189,1), xlab=("Julian Day"), ylab=("Watt Hours"), main="Cumulative Watt Hours for one Xstrm Location")



#### Fill in the 1m xstream values  #####



## create a vector of the 1m xstrm values

nxstrm<-c(2,seq(from=4, to=32, by=2), seq(from=1, to=31, by=2), seq(from=1, to=31, by=2), seq(from=1, to=35, by=2))


## Create a vector of Transect values

ntransect<-c(rep(2,16), rep(2.5, 16), rep(3, 16), rep(4, 18))


## Create new data frame

ndf2<-as.data.frame(matrix(nrow=66, ncol=93,0))
ndf2[,1:2]<-c(ntransect, nxstrm)

colnames(ndf2)<-c("ntransect", "nxstrm",as.character(jd))


## Calculate the mean cumulative irradiance of the xstrm values flanking the new xstrm value

#Means for Transect 2

for(i in 2:16){
      ndf2[i,3:93]<-as.numeric((julCSb[i,4:94] + julCSb[i+1,4:94])/2)
      }


# Make the Transect 2 2m value an average of the 0m and 3m value

  ndf2[1,3:93]<-as.numeric((julCSb[1,4:94] + julCSb[2,4:94])/2)


# Make the Transect 2 1m value the same as the 2m value

  ndf2[67,]<-ndf2[1,]
  ndf2[67,2]<-1


#Means forfor Transect 2.5
for(i in 17:32){
  ndf2[i,3:93]<-as.numeric((julCSb[i+1,4:94] + julCSb[i+2,4:94])/2)
  }    


#Means for Transect 3
for(i in 33:48){
  ndf2[i,3:93]<-as.numeric((julCSb[i+2,4:94] + julCSb[i+3,4:94])/2)
}  


#Averages for Transect 4
for(i in 49:66){
  ndf2[i,3:93]<-as.numeric((julCSb[i+3,4:94] + julCSb[i+5,4:94])/2)
}  


### Prepare to rbind ndf2 and julCSb which will create a df with the cumulative watt hours/day for every meter of the transects ###

  # rename ndf2 column titles
      #colnames(julCSb)    
      colnames(ndf2)[1:2]<-c("Transect", "xstrm")


  # Reformat ndf2 columns
      ndf2$Transect<-as.factor(ndf2$Transect)
      ndf2$xstrm<-as.integer(ndf2$xstrm)


  # Add a tx column in ndf2
      ndf2$tx<-as.factor(rep(NA,67))
      ndf2<-ndf2[,c(1,2,94,3:93)]


  # Rbind ndf2 and julCSb
  
    cwh<-rbind(julCSb, ndf2)
    cwh$Transect<-as.numeric(as.character((cwh$Transect)))


## cwh data fram has the cummulative watt hours for each julian days of the year from 99-189 for every 1 meter of transect.



#### Add cummulative Light data into the main algal data frame #####



# Algal data in "AlgaeKeith.txt" which came from AlgalTransects_Format.R script run on 12-June-2014 by Keith
# column with Julian day = Adfk$yearday

Adfk<-read.table(file.choose(),sep='\t',header=T,quote='')
str(Adfk)


## Create new cummulative watt hour column in the algal dataframe

    Adfk$cwatts<-NA


## Create an xstrm column of integers in the algal dataframe

    Adfk$xstrmInt<-as.integer(round(Adfk$xstrm, digits=0))


# Loop to input the cummulative watt hours from cwh data fram into the main algal data frame to the correct transect, xstrm, and julian day

# Vector of transect numbers

tran<-c(2, 2.5, 3, 4)  


# i = julian days, j = transects, k = xstrm
    # i corresponds to the columns in cwh


    for(i in 4:94){
      for(j in 1:4){
        for(k in 0:35){
          ifelse(which(Adfk$yearday == as.numeric(colnames(cwh))[i] & Adfk$Transect == tran[j] & Adfk$xstrmInt == k)>0, Adfk[which(Adfk$yearday == as.numeric(colnames(cwh))[i] & Adfk$Transect == tran[j]  & Adfk$xstrmInt == k),"cwatts"]<-cwh[which(cwh$Transect == tran[j]  & cwh$xstrm == k),i])
        }
      }
    }

#### Now the cummulative watt hours are in the cwatts column of the main algal dataframe

### Having some issues assigning cumulative watt hours to the algal database####
s
tr(Adfk)
Adfk_check<-Adfk[which(is.na(Adfk$cwatts) == "FALSE"),]

str(Adfk_check)
table(Adfk_check$yearday)

table(Adfk$yearday)

#### Visualizations of the cwh data frame #####
library(ggplot2)

dim(cwh)

# Make cwh long
?reshape
cwhl<-reshape(data=cwh, timevar="julianday", times= names(cwh[4:94]), varying=list(names(cwh[4:94])), direction="long"); head(cwhl)

colnames(cwhl)[5]<-"watth"
cwhl<-cwhl[,c(1:5)]


## Plot of julian days 99-189
p1<- ggplot(data=cwhl[which(cwhl$xstrm < 30),], aes(x=julianday, y=watth, group=xstrm))

p1 +geom_line(aes(color=xstrm)) + geom_vline(xintercept=c(22, 53, 74)) + facet_grid(Transect ~ .) + labs(x="Julian Day", y="Watt Hours") + ggtitle("Cumulative Watt Hours (Days 99-189)")+ theme_bw()


# Plot of cumulative watt hours on day 185

p2<- ggplot(data=cwhl[which(cwhl$xstrm < 30 & cwhl$julianday == 185),], aes(x=xstrm, y=watth, group=xstrm))

p2 + geom_line(aes(group=Transect), linetype="dotted") + geom_point(size=2) + facet_grid(Transect ~ .)  + labs(x="Cross Stream Distance (m)", y="Cumulative Watt Hours") + ggtitle("Cumulative Watt Hours (Day 185)") + theme_bw()


