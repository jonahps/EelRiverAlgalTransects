# Mary Power's Algal Transect Data: Formatting	
	
  # This script reads in the comprehensive algal transect database, fixes problematic entries, adds additional variables, and creates useful summaries and subsets

  # the data set is in tab-delimited text format (.txt)
    # note: do not store data in a comma-delimited format, as there are commas in data entries


# Read data
	# use browser to get file from local directory

AlgalTransects = read.table(file.choose(), header=T, na.strings='', sep='\t', fill=TRUE, quote='')
	# fill=TRUE solves problems with unequal row lengths
	# quote = '' solves the problems created by apostrophes in the data set
	 
# look at class of columns
sapply(AlgalTransects, class)		

# Problematic entries
	# record id 7351: xloc should be 'marker RHS "N"'
    # this problem is addressed in current code
	# record id 7591: had '48' for xloc, modified in FilePro database
	# record id 7557: has '20 cm dbh' for xloc

{
# Format data
	# transform and create columns as needed

# format dates  
AlgalTransects$Rdate = as.Date(AlgalTransects$date, format= '%m/%d/%Y')
	
AlgalTransects2 = transform(AlgalTransects, Transect = as.factor(Transect),	month = format(Rdate, '%m'),yearday = as.numeric(format(Rdate, '%j')), year = as.numeric(format(Rdate, '%Y')))
		
AlgalTransects2$WaterYear = as.factor(ifelse(as.numeric(format(AlgalTransects2$Rdate,'%m'))<=9, as.character(as.numeric(AlgalTransects2$year)-1), as.character(AlgalTransects2$year)))

# Add Hydrologic data

  # flooding
    # bankfull flood as defined in Power et al 2008
    # currently only available 1988-2012

  # Flood years for the season preceding summer: 1988-2012
  flood = read.table(file.choose(), header=T, sep='\t', quote='')
  # note that year refers to the algae year, or 'SummerYear', not the water year

  # change name of year column to match algal transect data base
  
  names(flood)[1] = 'year'

  # add column for previous year's flooding 
    # not the winter directly preceding the current growing season, but the one before it

  flood$PrevYearFlood = c('NA', as.character(flood$Flood[1:length(flood$Flood)-1]))

  # add column for 2-year flood combinations

  flood$TwoYears = c('NA', as.character(paste(flood$PrevYearFlood[-1],':',flood$Flood[-1])))

  # integrate flooding data into Algal data base

  AlgalTransects2 = merge(AlgalTransects2,flood, by='year')

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
	
	#prevalence of the different algae types
	table(as.factor(c(as.character(AlgalTransects2$algaedom), as.character(AlgalTransects2$algaesub),as.character(AlgalTransects2$algaesub2))))

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
		
	# create an identifier for each transect-date (ie each survey of a transect)
	AlgalTransects2$survey = paste(AlgalTransects2$transect,AlgalTransects2$date)		
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
	
	
