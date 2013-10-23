## This script takes the txt output file from Collin's light model and transforms it into the long format to be used for manipulations in R. The script also tidy's up the data and renames fields etc.

# Created by Keith Bouma-Gregson

# txt file = "LightModel_MaryTransects_originalTXT"]

# Column titles: Transect, xstrm = meters along cross section, x = X coordinate, y = Y coordinate, tx = a unique identifier for each point in the dataset, dayXXX = the julian day for the year (data in theis column is in watts-hours / day) 



setwd("/Users/keithgregson/Google Drive/Algal Timeseries/Light Model")

##	Input the light model transect data
ld= read.table('LightModel_MaryTransects_originalTXT.txt', header=T, sep='\t', na.strings="NA", fill=T, quote='')


##	check the data
	dim(ld)
	sapply(ld,class)
	head(ld)


##	Remove data not a part of a transect
	ld= ld[!is.na(ld$Transect),];dim(ld)

##	Change ""Transect"" into a factor
	ld$Transect<-as.factor(ld$Transect);levels(ld$Transect)

##	Remove  columns w/o Data
	ld <- ld[,-c(5)];dim(ld)


## Change column titles into numeric vector for Julian Day

	doy<-(colnames(ld))[6:57]
	doyS<-(strsplit(doy,"y"))

	s<-rep(0,52)
	z<-1
 
 	while(z<53){
 		s[z]<-doyS[[z]][2]
 		z<-z+1
 	}
	(ld)[6:57]<-s

##	Check up on format
	head(ld)
	sapply(ld, class)
	dim(ld)
	names(ld)[4:55]


## Transform into long format

	library(reshape2)

	lld<-reshape(data=ld,timevar="julianday",times=names(ld[6:57]),varying=list(names(ld[6:57])), direction="long");head(lld);tail(lld)

## Delete "id" column
	lld<-lld[,-8]
	head(lld)
	colnames(lld)[8]

## Adjust Names of columns

	names(lld)[1]<-"transect"
	names(lld)[2]<-"xstream"
	names(lld)[7]<-"watts"


##	Change "lld$JulianDay"" into a numeric vector and add column for week of the year
	lld$Julianday<-as.numeric(lld$julianday)
	lld$week<-as.numeric(as.factor(lld$julianday))
	
# Check data
	
	head(lld);tail(lld)















	
	

