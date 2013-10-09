# Ceates a subset of algal data from full surveys of all 4 transects 
# computes date and size of peak algal bloom in each year based on full surveys

  # uses formatted data set from formatting script


	# Data from full surveys (ie, all 4 main transects within a 10 day window)
		# 2, 2.5, 3, 4
		# list of full survey dates assembled in another R script as a list of lists, with each item being a list of all the dates for a particular full survey
		# this gives us a list of 79 full surveys through the 2011 season
	
	with(AlgalTransects2, table(as.character(Rdate), Transect))
	
{	# Define full survey dates
	FullSurveyDates = list(
	as.Date('1988-05-08'), 
	as.Date('1988-07-02'), 
	seq(from=as.Date('1988-07-13'), to=as.Date('1988-07-14'),by=1),
	seq(from=as.Date('1988-07-19'), to=as.Date('1988-07-20'),by=1),
	seq(from=as.Date('1988-08-22'), to=as.Date('1988-08-28'),by=1),
	as.Date('1989-05-08'),
	seq(from=as.Date('1989-06-13'), to=as.Date('1989-06-16'),by=1),
	seq(from=as.Date('1989-07-03'), to=as.Date('1989-07-05'),by=1),
	as.Date('1990-05-13'),
	seq(from=as.Date('1990-07-03'), to=as.Date('1990-07-05'),by=1),
	as.Date('1990-09-01'),
	as.Date('1991-05-04'),
	as.Date('1991-06-26'),
	seq(from=as.Date('1991-07-14'), to=as.Date('1991-07-15'),by=1),
	as.Date('1991-07-31'),
	as.Date('1991-08-19'),
	seq(from=as.Date('1991-09-01'), to=as.Date('1991-09-03'),by=1),
	as.Date('1992-06-24'),
	as.Date('1992-07-08'),
	as.Date('1995-05-25'),
	seq(from=as.Date('1995-06-25'), to=as.Date('1995-06-25'),by=1),
	as.Date('1995-07-26'),
	seq(from=as.Date('1995-08-15'), to=as.Date('1995-08-16'),by=1),
	as.Date('1996-06-06'),
	seq(from=as.Date('1996-07-04'), to=as.Date('1996-07-09'),by=1),
	as.Date('1996-07-20'),
	as.Date('1996-08-23'),
	seq(from=as.Date('1997-05-16'), to=as.Date('1997-05-17'),by=1),
	seq(from=as.Date('1997-05-31'), to=as.Date('1997-06-01'),by=1),
	seq(from=as.Date('1997-07-05'), to=as.Date('1997-07-09'),by=1),
	as.Date('1998-06-28'),
	as.Date('1998-07-11'),
	as.Date('1998-09-08'),
	seq(from=as.Date('1998-07-31'), to=as.Date('1998-08-07'),by=1),
	seq(from=as.Date('1999-06-02'), to=as.Date('1999-06-03'),by=1),
	seq(from=as.Date('1999-07-24'), to=as.Date('1999-07-25'),by=1),
	as.Date('1999-08-23'),
	seq(from=as.Date('2000-06-13'), to=as.Date('2000-06-15'),by=1),
	as.Date('2000-06-28'),
	seq(from=as.Date('2000-07-17'), to=as.Date('2000-07-20'),by=1),
	seq(from=as.Date('2000-08-21'), to=as.Date('2000-08-22'),by=1),
	seq(from=as.Date('2001-08-01'), to=as.Date('2001-08-11'),by=1),
	as.Date('2001-08-25'),
	as.Date('2002-06-02'),
	seq(from=as.Date('2002-06-12'), to=as.Date('2002-06-14'),by=1),
	seq(from=as.Date('2002-07-11'), to=as.Date('2002-07-14'),by=1),
	as.Date('2002-08-02'),
	as.Date('2002-09-21'),
	as.Date('2003-06-18'),
	seq(from=as.Date('2003-08-04'), to=as.Date('2003-08-05'),by=1),
	as.Date('2004-06-05'),
	seq(from=as.Date('2004-06-23'), to=as.Date('2004-06-25'),by=1),
	as.Date('2004-07-12'),
	seq(from=as.Date('2004-09-12'), to=as.Date('2004-09-13'),by=1),
	as.Date('2005-07-18'),
	seq(from=as.Date('2005-08-02'), to=as.Date('2005-08-04'),by=1),
	as.Date('2005-08-20'),
	seq(from=as.Date('2005-09-05'), to=as.Date('2005-09-06'),by=1),
	seq(from=as.Date('2006-05-27'), to=as.Date('2006-05-28'),by=1),
	seq(from=as.Date('2006-06-17'), to=as.Date('2006-06-20'),by=1),
	as.Date('2006-07-12'),
	seq(from=as.Date('2007-05-12'), to=as.Date('2007-05-13'),by=1),
	seq(from=as.Date('2007-07-30'), to=as.Date('2007-07-31'),by=1),
	seq(from=as.Date('2007-08-26'), to=as.Date('2007-08-31'),by=1),
	seq(from=as.Date('2008-05-20'), to=as.Date('2008-05-21'),by=1),
	seq(from=as.Date('2008-06-04'), to=as.Date('2008-06-05'),by=1),
	as.Date('2008-07-01'),
	as.Date('2008-10-26'),
	seq(from=as.Date('2009-06-22'), to=as.Date('2009-06-23'),by=1),
	seq(from=as.Date('2009-07-12'), to=as.Date('2009-07-15'),by=1),
	seq(from=as.Date('2009-07-28'), to=as.Date('2009-07-29'),by=1),
	as.Date('2009-10-25'),
	seq(from=as.Date('2010-07-01'), to=as.Date('2010-07-06'),by=1),
	seq(from=as.Date('2010-08-15'), to=as.Date('2010-08-16'),by=1),
	as.Date('2011-05-24'),
	as.Date('2011-07-02'),
	seq(from=as.Date('2011-07-16'), to=as.Date('2011-07-19'),by=1),
	seq(from=as.Date('2011-07-27'), to=as.Date('2011-07-29'),by=1),
	seq(from=as.Date('2011-08-20'), to=as.Date('2011-08-21'),by=1),
	as.Date('2012-06-19'),
	seq(from=as.Date('2012-06-25'), to=as.Date('2012-07-03'),by=1),
	seq(from=as.Date('2012-07-24'), to=as.Date('2012-07-27'),by=1)),
	as.Date('2013-05-16'),
	seq(from=as.Date('2013-06-05'), to=as.Date('2013-06-06'),by=1)),
  seq(from=as.Date('2013-07-01'), to=as.Date('2013-07-04'),by=1)),
  as.Date('2013-07-21')
}	
	# subset data for full surveys
	AlgalTransectsFS = data.frame()	
	for (i in 1:length(FullSurveyDates)){
		x = AlgalTransects2[which(AlgalTransects2$Rdate %in% FullSurveyDates[[i]]),]
		x$FullSurvey = FullSurveyDates[[i]][1]
		AlgalTransectsFS = rbind(AlgalTransectsFS,x)
	}

	
# Average height across 4 transects in full surveys
	# mean of mean heights for each transect
		# only use data from points where depth >0

	# subset data for full surveys where depth>0
	
	atFSwet = AlgalTransectsFS[which(AlgalTransectsFS$depth > 0),]

	# mean for each transect at each survey date
	atFSwet_stMean = aggregate(CladInt ~ Transect + FullSurvey + year, data=atFSwet, FUN=mean) 
	
	# mean of transect means for each survey date
	atFSwet_sMean = aggregate(CladInt ~ FullSurvey + year, atFSwet_stMean, FUN=mean)

	#write.table(atFSwet_sMean, sep = '\t', file='AlgalTransects_FullSurveyMeanCladHeight.txt', col.names = T, row.names=F, quote=F)
	
	# Size and date of peak algal bloom
		# size is mean of transect means
		# use only data from full surveys
		
		# get row ids for maximum height in each year
		idMaxFS = sapply(split(1:nrow(atFSwet_sMean), atFSwet_sMean$year), function(x) x[which.max(atFSwet_sMean$CladInt[x])])
		
		# select rows with maximum height
		atFSwet_sMax = atFSwet_sMean[idMaxFS,]
		
		#write.table(atFSwet_sMax, sep = '\t', file='AlgalTransects_AnnMaxCladHeight.txt', col.names = T, row.names=F, quote=F)
		
		# note that this does not capture variation between transects in peak bloom, as algal heights are averaged over all transects
	
	