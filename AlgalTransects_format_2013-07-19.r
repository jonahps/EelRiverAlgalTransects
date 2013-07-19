# Mary Power algal transect data
	# note: do not use comma-delimited, as there are commas in data entries
	
	# make Angelo subset at appropriate place
	
# Read data

AlgalTransects = read.table('2013-03-21_AlgalTransects.txt', header=T, na.strings='', sep='\t', fill=TRUE, quote='')
	# fill=TRUE solves problems with unequal row lengths
	# quote = '' solves the problems created by apostrophes in the data set
	 
# look at class of columns
sapply(AlgalTransects, class)		

# Oddities
	# record id 7351: xloc is "\"marker RHS \"\"N\"\"\"" should be 'marker RHS "N"'
	# record id 7591:  has '48' for xloc and 
	# record id 7557: has '20 cm dbh' for xloc

{# Format data
	# transform and create columns as needed

AlgalTransects$Rdate = as.Date(AlgalTransects$date, format= '%m/%d/%Y')
	
AlgalTransects2 = transform(AlgalTransects, Transect = as.factor(Transect),	month = format(Rdate, '%m'),yearday = as.numeric(format(Rdate, '%j')), year = as.numeric(format(Rdate, '%Y')))
		
AlgalTransects2$WaterYear = as.factor(ifelse(as.numeric(format(AlgalTransects2$Rdate,'%m'))<=9, as.character(as.numeric(AlgalTransects2$year)-1), as.character(AlgalTransects2$year)))

# Create algal variables
	# Make an integrated Cladophora variable
		# shows Cladophora height, with 0 for no Cladophora

	AlgalTransects2$CladInt = 
		ifelse(AlgalTransects2$algaedom == 'Cladophora glomerata attached', AlgalTransects2$htdom, 
		ifelse(AlgalTransects2$algaesub == 'Cladophora glomerata attached' , AlgalTransects2$htsub,
		ifelse(AlgalTransects2$algaesub2 == 'Cladophora glomerata attached', AlgalTransects2$htsub2,0)))
	
	AlgalTransects2$CladInt[is.na(AlgalTransects2$CladInt)] = 0

	# Create columns for presence/absence of algal functional groups Cladophora, Nostoc/Rivularia, and Zygmales
		# edible, filamentous greens: Cladophora
		# N-fixing cyanobacteria: Rivularia, Nostoc
		# 'inedible' mucous-secreting greens: Zygnema, Mougeotia, Spirogyra
	
	levels(AlgalTransects2$algaedom)
	levels(AlgalTransects2$algaesub)
	levels(AlgalTransects2$algaesub2)
	
	# prevalence of different algae types
	
		# dominant algae
		table(AlgalTransects2$algaedom)
		
		# all algae
		table(as.factor(c(as.character(AlgalTransects2$algaedom),as.character(AlgalTransects2$algaesub),as.character(AlgalTransects2$algaesub2))))
		
		# Algal categories with Keith
			# Cladophora
			# sedge?
				# 244 points, about half of which are out of the water
			
	hist(AlgalTransects2[which(AlgalTransects2$algaedom == 'sedge'),'depth'])
	table(AlgalTransects2[which(AlgalTransects2$algaedom == 'sedge'),'Transect'])
	
	# Cladophora: is Clad present? (yes=1, no=0) 
	AlgalTransects2$Clad = as.numeric(AlgalTransects2$algaedom == 'Cladophora glomerata attached' | AlgalTransects2$algaedom == 'Cladophora glomerata loose' | AlgalTransects2$algaesub == 'Cladophora glomerata attached' | AlgalTransects2$algaesub == 'Cladophora glomerata loose' | AlgalTransects2$algaesub2 == 'Cladophora glomerata attached' | AlgalTransects2$algaesub2 == 'Cladophora glomerata loose')
	
	AlgalTransects2$Clad[is.na(AlgalTransects2$Clad)] = 0
		
	# N-fixing cyanobacteria: is Nos or riv present or Anabaena present? (yes=1, no=0) (only 2 anabaena in whole data set)
	AlgalTransects2$NosRiv = as.numeric(AlgalTransects2$algaedom == 'Nostoc balls' | AlgalTransects2$algaedom == 'Nostoc ears' | AlgalTransects2$algaedom == 'Rivularia' | AlgalTransects2$algaesub == 'Nostoc balls' | AlgalTransects2$algaesub == 'Nostoc ears' | AlgalTransects2$algaesub == 'Rivularia' | AlgalTransects2$algaesub2 == 'Nostoc balls' | AlgalTransects2$algaesub2 == 'Nostoc ears' | AlgalTransects2$algaesub2 == 'Rivularia')
		
	AlgalTransects2$NosRiv[is.na(AlgalTransects2$NosRiv)] = 0
	
	# are zygnemataceae present? (moug, spg, zyg)
	AlgalTransects2$Zyg = as.numeric(AlgalTransects2$algaedom == 'Mougeotia' | AlgalTransects2$algaedom == 'Zygnema' | AlgalTransects2$algaesub == 'Mougeotia' | AlgalTransects2$algaesub == 'Zygnema' | AlgalTransects2$algaesub2 == 'Mougeotia' | AlgalTransects2$algaesub2 == 'Zygnema')
		
	AlgalTransects2$Zyg[is.na(AlgalTransects2$Zyg)] = 0
}
{
# Create variables combining time and location

	# create an identifier for each transect point in each year
	AlgalTransects2$YearPoint = paste(AlgalTransects3$year,AlgalTransects3$TranXstr)
		
	# create an identifier for each transect-date (ie each survey of a transect)
	AlgalTransects2$survey = paste(AlgalTransects3$transect,AlgalTransects3$date)		
}	
# Subset data
	
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
	seq(from=as.Date('2012-07-24'), to=as.Date('2012-07-27'),by=1))
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
	
# Transect 2.5 average height on each survey, and date and size of peak algal bloom

		# create data frame of maximum mean modal biomass per year at transect 2.5

		Clad2.5DateMean = na.omit(with(AlgalTransects2[which(AlgalTransects2$Transect=='2.5' & AlgalTransects2$depth > 0),], aggregate(CladInt, by=list(year=year, date=date), FUN=mean)))
		names(Clad2.5DateMean)[3] = 'CladHt_cm'
		Clad2.5DateMean$Rdate = as.Date(Clad2.5DateMean$date, format= '%m/%d/%Y')
		write.table(Clad2.5DateMean, sep = '\t', file='AlgalTransect2.5_MeanCladHeight.txt', col.names = T, row.names=F, quote=F)
		
		# Create a data frame with maximum mean modal biomass in each year (and date at which it was attained)
			# write to text file

		idMax2.5 = sapply(split(1:nrow(Clad2.5DateMean), Clad2.5DateMean$year), function(x) x[which.max(Clad2.5DateMean$CladHt_cm[x])])
		Clad2.5AnnMax = Clad2.5DateMean[idMax2.5,]
		write.table(Clad2.5AnnMax, sep = '\t', file='AlgalTransect2.5_AnnMaxCladHeight.txt', col.names = T, row.names=F, quote=F)

		# the following code just gets you max per year (no date info)
		# Clad2.5AnnMax2 = 	with(Clad2.5DateMean, aggregate(ht, by=list(year=year), FUN=max))
		

######################################	
	
# Aggregate data
	
	# Average Clad height in each unique survey of a transect
	
	AlgalTransectsCladSurvey = aggregate(cbind(CladInt,year,month,yearday,TotalQ) ~ WaterYear + Transect + BFF + BFFminus1 + year + month + yearday, data=AlgalTransects3, FUN=mean)
	
	# average Clad height for each full survey
	
	CladFullSurveys_mean = aggregate(cbind(CladInt,year,month,yearday,TotalQ) ~ WaterYear + BFF + BFFminus1 + year + FullSurvey, data=AlgalTransectsFS, FUN=mean)
	
		# at points where depth > 0
		
		CladFullSurveys_mean2 = aggregate(cbind(CladInt,year,month,yearday,TotalQ) ~ WaterYear + BFF + BFFminus1 + year + FullSurvey, data=subset(AlgalTransectsFS, depth >0), FUN=mean)
	
	# remove hydrology, as its not yet added to data set
	CladFullSurveys_mean = aggregate(cbind(CladInt,year,month,yearday) ~ WaterYear + year + FullSurvey, data=AlgalTransectsFS, FUN=mean)
	
# Temporal subsets
	
	AlgalTransects_2008thru2012 = subset(AlgalTransects3, year == 2008 | year == 2009 | year == 2010 | year == 2011)

	AlgalTransectsCladSurvey_2008thru2012 = subset(AlgalTransectsCladSurvey, year == 2008 | year == 2009 | year == 2010 | year == 2011)
	

#Preliminary Analysis and visualization of Clad frequency and growth
	
	# load libraries
	library(lme4)
	library(mgcv)
	library(gamm4)
	library(lattice)
	library(MASS)
	
	# try Clad frequency
		Clad_gamm1 = gamm4(Clad ~ s(yearday), data=subset(AlgalTransects3, BFF==1), family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
		Clad_gamm2 = gamm4(Clad ~ s(yearday), data=subset(AlgalTransects3, BFF==0), family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
		plot(Clad_gamm1$gam, xaxt='n', xlab='Month', ylab='Cladophora occurrence', xlim=c(80,285), ylim=c(-5,4))
		windows()
		plot(Clad_gamm2$gam, xaxt='n', xlab='Month', ylab='Cladophora occurrence', xlim=c(80,285), ylim=c(-5,4))
		Clad_gamm3 = gamm4(Clad ~ BFF + s(yearday, by=BFF), data=AlgalTransects3, family=binomial, random=~(1|year)+(1|transect) +(1|TranXstr))
		plot(Clad_gamm3$gam, pages=1, shade=T, xlim=c(80,280))
		plot(Clad_gamm3$gam, xaxt='n', xlab='Month', ylab='Cladophora occurrence', xlim=c(80,285), ylim=c(-5,4))
		axis(1, at=c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349), labels=c('J','F','M','A','M','J','J','A','S','O','N','D'), lwd=0)
			# make figure with these side-by-side
		
		with(Pred, xyplot(fit~yearday|as.factor(BFF)))
	
	# try continuous Clad integrated variable 
		
		# plot survey averages
		
		CladGrowthSA_gamm1 = gamm4(CladInt ~ s(yearday, by=BFF), data=AlgalTransectsCladSurvey, random=~(1|year)+(1|transect))
		CladGrowthSA_gamm2 = gamm4(CladInt ~ s(yearday), data=subset(AlgalTransectsCladSurvey, BFF==1), random=~(1|year)+(1|transect))
		CladGrowthSA_gamm3 = gamm4(CladInt ~ s(yearday), data=subset(AlgalTransectsCladSurvey, BFF==0), random=~(1|year)+(1|transect))
		
		CladGrowthSA_gamm4 = gamm4(CladInt ~ s(yearday), data=subset(AlgalTransectsCladSurvey, BFF==1 & BFFminus1==1), random=~(1|year)+(1|transect))
		CladGrowthSA_gamm5 = gamm4(CladInt ~ s(yearday), data=subset(AlgalTransectsCladSurvey, BFF==1 & BFFminus1==0), random=~(1|year)+(1|transect))
		
		CladGrowthSA_gamm6 = gamm4(CladInt ~ s(yearday, by=BFFminus1), data=subset(AlgalTransectsCladSurvey, BFF==0), random=~(1|year)+(1|transect))
		CladGrowthSA_gamm7 = gamm4(CladInt ~ s(yearday, by=BFFminus1), data=subset(AlgalTransectsCladSurvey, BFF==1), random=~(1|year)+(1|transect))
		plot(CladGrowthSA_gamm7$gam, pages=1, shade=T)
		
			# flood years after flood years seem to show the most consistent pattern of unimodal Clad growth
		
		plot(CladGrowthSA_gamm4$gam, xlab='Month', ylab='Cladophora height', xlim=c(80,285), page=1, shade=T)
		axis(1, at=c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349), labels=c('J','F','M','A','M','J','J','A','S','O','N','D'), lwd=0)		
		
		# plot all points
		
		hist(AlgalTransects3$CladInt)
		summary(AlgalTransects3$CladInt); var(AlgalTransects3$CladInt)
			# mostly 0s, this will create some interesting distributional problems
	
		# try using only points that had Clad at some time in any particular year
		hist(AlgalTransectsCladGrowth$CladInt, breaks=20)
		summary(AlgalTransectsClad$CladInt); var(AlgalTransectsClad$CladInt)
			# still a lot of 0s, but not quite as many
			# may be able to fit with gamma-distributed errors?
				# this gives errors, so stick with Gaussian
		
		CladGrowth_gamm1 = gamm4(CladInt ~ BFF + s(yearday, by=BFF), data=AlgalTransectsCladGrowth, random=~(1|year)+(1|transect)+(1|TranXstr))
		plot(CladGrowth_gamm1$gam, xaxt='n', xlab='Month', ylab='Cladophora height')
		axis(1, at=c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349), labels=c('J','F','M','A','M','J','J','A','S','O','N','D'), lwd=0)
		
		# what about using a zero-inflated model using package COZIGAM?
			# this may not be developed for mixed models
		
		# what if we just use points where Clad is present?
		hist(AlgalTransectsClad$CladInt)
			# looks better, maybe not quite normal, but not bad
				# log transformation doesn't help
			# the problem is that this might not do a good job of modeling growth?
		
		CladGrowth_gamm2 = gamm4(CladInt ~ BFF + s(yearday, by=BFF), data=AlgalTransectsClad, random=~(1|year)+(1|transect)+(1|TranXstr))
		plot(CladGrowth_gamm2$gam)
			# the plot for BFF=0 is strange -- descends in a straight line
			
		# what if we try a 
	
		with(AlgalTransectsClad, xyplot(CladInt ~ yearday|BFF))
	
		CladGrowth_gamm3 = gamm4(CladInt ~ s(yearday), data=subset(AlgalTransectsClad, BFF==0), random=~(1|year)+(1|transect)+(1|TranXstr))
		CladGrowth_gamm4 = gamm4(CladInt ~ s(yearday), data=subset(AlgalTransectsClad, BFF==1), random=~(1|year)+(1|transect)+(1|TranXstr))
		plot(CladGrowth_gamm3$gam)
		plot(CladGrowth_gamm4$gam)
		
 # Preliminary analysis and visualization of Nostoc and Rivularia
	NosRiv_gamm1 = gamm4(NosRiv ~ s(yearday, by=BFF), data=AlgalTransects3, family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
	#NosRiv_gamm2 = gamm4(NosRiv ~ s(yearday), data=subset(AlgalTransects3, BFF==1), family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
	plot(NosRiv_gamm1$gam, xaxt='n', ylab='Nostoc/Rivularia occurrence', shade=T, pages=1, xlim=c(80,27)
	plot(NosRiv_gamm2$gam, xaxt='n', xlab='Month', ylab='Nostoc/Rivularia occurrence', shade=T)
	
# Preliminary analysis and visualization of Zygmatales
	Zyg_gamm1 = gamm4(Zyg ~ s(yearday, by=BFF), data=AlgalTransects3, family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
	#Zyg_gamm2 = gamm4(Zyg ~ s(yearday), data=subset(AlgalTransects3, BFF==1), family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
	plot(Zyg_gamm1$gam, xaxt='n', xlab='Month', ylab='', shade=T, pages=1)
	
# predictions on scale of response variable using predict.gam
	Pred = as.data.frame(cbind(rep(seq(0,364,1),2), c(rep(0,365), rep(1,365))))
	names(Pred) = c('yearday', 'BFF')
	
	CladOccPred = predict.gam(Clad_gamm3$gam, newdata=Pred[,1:2], type='response', se.fit=T)
	Pred$CladFit = CladOccPred$fit
	Pred$CladSe = CladOccPred$se.fit
	
	NosRivOccPred = predict.gam(NosRiv_gamm1$gam, newdata=Pred[,1:2], type='response', se.fit=T)
	Pred$NosRivFit = NosRivOccPred$fit
	Pred$NosRivSe = NosRivOccPred$se.fit
	
	ZygOccPred = predict.gam(Zyg_gamm1$gam, newdata=Pred[,1:2], type='response', se.fit=T)
	Pred$ZygFit = ZygOccPred$fit
	Pred$ZygSe = ZygOccPred$se.fit

	# save predictions
	
	write.table(Pred, file='AlgalPhenologyGAMMpred.txt', sep='\t', col.names=T, row.names=F)
	
# Incorporate hydrology data

	AngeloDis = read.table('AngeloDis.txt', sep = '\t', header=T)
	  
	AngeloDis$BFFminus1 = c('NA', AngeloDis$BFF[1:length(AngeloDis$BFF)-1])
	
	AlgalTransects3=merge(AlgalTransects2, AngeloDis, by='WaterYear', all.x=T)
	
	AlgalTransects3=transform(AlgalTransects3, BFF = as.factor(BFF), BFFminus1 = as.factor(BFFminus1),TotalBin = as.factor(TotalBin))
	
# Cladophora data
	
	AlgalTransectsClad = AlgalTransects3[which(AlgalTransects3$Clad==1),]
	
# Points at which Clad has been found to be present in a particular year
	
	CladYearPoint = unique(AlgalTransectsClad$YearPoint)
	
	AlgalTransectsCladGrowth = AlgalTransects3[AlgalTransects3$YearPoint %in% CladYearPoint,]