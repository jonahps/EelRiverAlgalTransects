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

# Create single variable for dominant macroalgae
  # Reformat algaedom column

# Change NA to "bare"

AlgalTransects2$algaeStates<-factor(AlgalTransects2$algaedom, levels = c(levels(AlgalTransects2$algaedom),"bare"))
  
b<-which(is.na(AlgalTransects2$algaeStates)==TRUE)
AlgalTransects2$algaeStates[b]<-"bare"

# recode algaeStates into specificied categories

# install.packages ("car") to use "recode" command
library(car)
  
levels(AlgalTransects2$algaeStates)

# recode to desired categories
  
AlgalTransects2$algaeStates <-recode(AlgalTransects2$algaeStates, "c('black crust','Diatom skin','green skin','litter')='bare';c('Cladophora glomerata attached','Cladophora glomerata loose')='Cladophora'; c('Mougeotia','Zygnema')='Zygnematales';c('Cyanobac filaments','general blue-green algae','Nostoc balls','Nostoc ears','Rivularia')='Cyanobacteria'")


table(AlgalTransects2$algaeStates,exclude=NULL)  

  
# Create columns for presence/absence of algal functional groups Cladophora, Nostoc/Rivularia, and Zygnematales
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

# Create variables combining time and location
		
	# create an identifier for each transect-date (ie each survey of a transect)
	AlgalTransects2$survey = paste(AlgalTransects2$transect,AlgalTransects2$date)		

