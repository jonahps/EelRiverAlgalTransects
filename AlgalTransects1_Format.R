## Mary Power's Algal Transect Data: Formatting

## This script reads in the comprehensive algal transect data,
## fixes problematic entries, adds additional variables,
## and creates useful summaries and subsets.
## This script does not reduce the number of rows in the dataset

## the data set is in tab-delimited text format (.txt)
## note: do not store data in a comma-delimited format,
## as there are commas in data entries


#### Read in Data format data ####
	# use browser to get file from local directory
## Original file: 2013-08-27_AlgalTransects.txt (no substrate data)
  # Latest file from Collin: "2014-02-03_AlgalTransects_substrate.tab" (includes 6800 substrate entries, but missing 293 entries from 2013-08-27_AlgalTransects.txt file) USE THIS FILE

  AlgalTransects <- read.table(file.choose(), header=T, na.strings='', sep='\t', fill=TRUE, quote='', stringsAsFactors= FALSE)
	# fill=TRUE solves problems with unequal row lengths
	# quote = '' solves the problems created by apostrophes in the data set

# Data frame with the 293 rows present in the Original file, but missing
  # from the latest file
  # Read CSV file = MissingRowsFrom_2014-02-03_File.csv
    # CSV created in "AlgalTransects_FilemakerFileComparison.R" script
  missing.rows <- read.csv(file.choose(), stringsAsFactors= FALSE)

  # Merge two data frames together
  AlgalTransects2 <- merge(AlgalTransects, missing.rows, all= TRUE)


#### Format Columns ####

# look at class of columns
  #sapply(AlgalTransects2, class)

# Format column classes
  AlgalTransects2 <- transform(AlgalTransects2,
                               depth = as.numeric(depth),
                               substr = as.factor(substr),
                               subdom = as.factor(subdom),
                               flow = as.numeric(flow),
                               algaedom = as.factor(algaedom),
                               htdom = as.numeric(htdom),
                               dendom = as.numeric(dendom),
                               conddom = as.numeric(conddom),
                               algaesub = as.factor(algaesub),
                               htsub = as.numeric(htsub),
                               densub = as.numeric(densub),
                               condsub = as.numeric(condsub),
                               algaesub2 = as.factor(algaesub2),
                               htsub2 = as.numeric(htsub2),
                               densub2 = as.numeric(densub2),
                               condsub2 = as.numeric(condsub2))

# Problematic entries
	# record id 7351: xloc should be 'marker RHS "N"'
	# record id 7557: has '20 cm dbh' for xloc
  # record id 16488: depth is 3236

  # fix record 16488 -- assign depth to be NA
  AlgalTransects2[which(AlgalTransects2$id==16488), 'depth'] = NA

# Format dates
  AlgalTransects2$Rdate <- as.Date(AlgalTransects2$date, format= '%Y-%m-%d')

  AlgalTransects2 <- transform(AlgalTransects2,
                               transect = as.factor(transect),
                               month = format(Rdate, '%m'),
                               yearday = as.numeric(format(Rdate, '%j')),
                               year = as.numeric(format(Rdate, '%Y')))

  AlgalTransects2$WaterYear <- as.factor(ifelse(as.numeric(format(AlgalTransects2$Rdate,'%m')) <= 9,
                                                as.character(as.numeric(AlgalTransects2$year) - 1),
                                                as.character(AlgalTransects2$year)))

# Create variables combining time and location
	# create an identifier for each transect-date (ie each survey of a transect)
	AlgalTransects2$survey <- paste(AlgalTransects2$transect, AlgalTransects2$date)


#### Floods ####
  # flooding
    # bankfull flood as defined in Power et al 2008
    # currently only available 1988-2013

  # Flood years for the season preceding summer: 1988-2013
  # Input file: AngeloBankfullFlood_1988-2013.txt
  flood <- read.table(file.choose(), header=T, sep='\t', quote='')
  #
  # note that year refers to the algae year, or 'SummerYear', not the water year

  # change name of year column to match algal transect data base
  names(flood)[1] <- 'year'

  # add column for previous year's flooding
    # not the winter directly preceding the current growing season, but the one before it
  flood$PrevYearFlood <- c('NA', as.character(flood$Flood[1:length(flood$Flood)-1]))

  # add column for 2-year flood combinations
  flood$TwoYears <- c('NA', as.character(paste(flood$PrevYearFlood[-1], ':', flood$Flood[-1])))

  # Merge flooding data into Algal data base
  AlgalTransects2 <- merge(AlgalTransects2, flood, by='year', all.x=T)


#### Spring spates ####

# File "SpringSpatesJK_8713.txt"
  spatesJK <- read.table(file.choose(), header=T, sep= '\t', quote= '')
  spatesJK2 <- cbind(spatesJK[5, ], spatesJK[6, 2:3], spatesJK[7, 2:3])
  row.names(spatesJK2) <- 1
  spatesJK2[2, ] <- c(spatesJK[1, ], spatesJK[2, 2:3], c(NA,NA))
  spatesJK2[3, ] <- c(spatesJK[3, ], rep(NA, 4))
  spatesJK2[4, ] <- c(spatesJK[4, ], rep(NA, 4))
  spatesJK2[5, ] <- c(spatesJK[8, ], rep(NA, 4))

  names(spatesJK2)[2:7] <- c('ssMaxDischarge1','ssRdate1','ssMaxDischarge2','ssRdate2','ssMaxDischarge3','ssRdate3')

# File "SpringSpatesMEP_8713.txt"
  spatesMEP <- read.table(file.choose(), header=T, sep='\t', quote='')

  # Merge spate data
  AlgalTransects2 <- merge(AlgalTransects2, spatesJK2, by= 'year', all.x= T)
  AlgalTransects2 <- merge(AlgalTransects2, spatesMEP, by= 'year', all.x= T)


#### Add hydrologic data for discharge variables ####
  # the following two .txt files were created in the script:
  # "GrowthSeasonDischarge_2014-06-06.R"
  # annual summary of growing season =
  # cumulative discharge from Apr 15 - June 21
  # File = "GrowSeason_TotalDischarge.txt"
  tot.discharge <- read.table(file.choose(), sep='\t', header=T)
  names(tot.discharge)[2] <- "discharge_cms"
  AlgalTransects2 <- merge(AlgalTransects2, tot.discharge)

    # growing season to date =
  # average daily mean discharge from Apr 15 - current date
  # File = "GrowSeason_MeanDischargeByDay.txt"
  mean.daily.discharge <- read.table(file.choose(), sep='\t', header=T)
  names(mean.daily.discharge)[2] <- "yearday"
  AlgalTransects2 <- merge(AlgalTransects2, mean.daily.discharge, all.x =  T)

#### Substrate stability ####
  # create stability index of substrate
  # roots= 4, bedrock=3, boulders=2, cobbles=1, gravel,mud,pebbles,sand,silt,wood=0

  AlgalTransects2$stab <- as.numeric(with(AlgalTransects2,
                                   ifelse(substr=='NAN', NA,
                                          ifelse(substr=='roots', 4,
                                                 ifelse(substr=='bedrock', 3,
                                                        ifelse(substr=='boulders', 2,
                                                               ifelse(substr=='cobbles', 1, 0
                                                               )))))))


#### Make an integrated Cladophora variable ####
		# shows Cladophora height, with 0 for no Cladophora
	AlgalTransects2$CladInt <-
    ifelse(AlgalTransects2$algaedom == 'Cladophora glomerata attached', AlgalTransects2$htdom,
           ifelse(AlgalTransects2$algaesub == 'Cladophora glomerata attached' , AlgalTransects2$htsub,
                  ifelse(AlgalTransects2$algaesub2 == 'Cladophora glomerata attached', AlgalTransects2$htsub2,0)))
	AlgalTransects2$CladInt[is.na(AlgalTransects2$CladInt)] <- 0


#### Create single variable for dominant macroalgae ####
  # Reformat algaedom column
  # Change NA to "bare"
  AlgalTransects2$algaeStates <- factor(AlgalTransects2$algaedom,
                                        levels = c(levels(AlgalTransects2$algaedom), "bare"))
  b <- which(is.na(AlgalTransects2$algaeStates)==TRUE)
  AlgalTransects2$algaeStates[b] <- "bare"

  # recode algaeStates into specificied categories
  # install.packages ("car") to use "recode" command
  library(car)
  #levels(AlgalTransects2$algaeStates)

  # recode to desired categories
  AlgalTransects2$algaeStates <- recode(AlgalTransects2$algaeStates,
                                       "c('black crust','Diatom skin','green skin','litter') = 'bare';
                                       c('Cladophora glomerata attached','Cladophora glomerata loose')= 'Cladophora';
                                       c('Mougeotia','Zygnema')= 'Zygnematales';
                                       c('Cyanobac filaments','general blue-green algae','Nostoc balls','Nostoc ears','Rivularia')= 'Cyanobacteria'")

  #table(AlgalTransects2$algaeStates, exclude=NULL)


#### Create algal presence/absence variables ####
  # Create columns for presence/absence of algal functional groups Cladophora, Nostoc/Rivularia, and Zygnematales
	# edible, filamentous greens: Cladophora
	# N-fixing cyanobacteria: Rivularia, Nostoc
	# 'inedible' mucous-secreting greens: Zygnema, Mougeotia, Spirogyra

	# prevalence of the different algae types
	  # table(as.factor(c(as.character(AlgalTransects2$algaedom), as.character(AlgalTransects2$algaesub),as.character(AlgalTransects2$algaesub2))))

	# Cladophora: is Clad present? (yes=1, no=0)
	AlgalTransects2$Clad <- as.numeric(AlgalTransects2$algaedom == 'Cladophora glomerata attached' |
	                                     AlgalTransects2$algaedom == 'Cladophora glomerata loose' |
	                                     AlgalTransects2$algaesub == 'Cladophora glomerata attached' |
	                                     AlgalTransects2$algaesub == 'Cladophora glomerata loose' |
	                                     AlgalTransects2$algaesub2 == 'Cladophora glomerata attached' |
	                                     AlgalTransects2$algaesub2 == 'Cladophora glomerata loose')
	AlgalTransects2$Clad[is.na(AlgalTransects2$Clad)] <- 0

	# N-fixing cyanobacteria: is Nos or riv present or Anabaena present? (yes=1, no=0) (only 2 anabaena in whole data set)
	AlgalTransects2$NosRiv <- as.numeric(AlgalTransects2$algaedom == 'Nostoc balls' |
	                                       AlgalTransects2$algaedom == 'Nostoc ears' |
	                                       AlgalTransects2$algaedom == 'Rivularia' |
	                                       AlgalTransects2$algaesub == 'Nostoc balls' |
	                                       AlgalTransects2$algaesub == 'Nostoc ears' |
	                                       AlgalTransects2$algaesub == 'Rivularia' |
	                                       AlgalTransects2$algaesub2 == 'Nostoc balls' |
	                                       AlgalTransects2$algaesub2 == 'Nostoc ears' |
	                                       AlgalTransects2$algaesub2 == 'Rivularia')
	AlgalTransects2$NosRiv[is.na(AlgalTransects2$NosRiv)] <- 0

	# are Zygnematales present? (moug, spg, zyg)
	AlgalTransects2$Zyg <- as.numeric(AlgalTransects2$algaedom == 'Mougeotia' |
	                                    AlgalTransects2$algaedom == 'Zygnema' |
	                                    AlgalTransects2$algaesub == 'Mougeotia' |
	                                    AlgalTransects2$algaesub == 'Zygnema' |
	                                    AlgalTransects2$algaesub2 == 'Mougeotia' |
	                                    AlgalTransects2$algaesub2 == 'Zygnema')
	AlgalTransects2$Zyg[is.na(AlgalTransects2$Zyg)] <- 0


#### Irradiance ####
	#Add cumulative watt hours for the summer solstice (yearday = 172)
  #Read in cumulative watt hours data (.csv generated by "LightModel_MarysTransects_CumulativeWattHours.R" script)

  # input file = "LightModel_MarysTransects_CumulativeWattHoursSolstice.csv"
    solstice_watts <- read.csv(file.choose())
    names(solstice_watts)[1] <- "transect"
	  solstice_watts[, 1] <- as.factor(solstice_watts[, 1])

  # Create an xstrm column of integers in the algal dataframe
    AlgalTransects2$xstrmInt <- as.integer(round(AlgalTransects2$xstrm, digits=0))

  # Merge cumulative watt hours into AlgalTransects2
    AlgalTransects2 <- merge(AlgalTransects2, solstice_watts, all.x = TRUE)


#### Write processed data file ####

# Remove xdid and tranxstr columns
  AlgalTransectsWrite <- subset(AlgalTransects2, select= -c(xdid, tranxstr))
  #str(AlgalTransectsWrite)

# use dataframe name 'AlgalTransectsWrite'
  # write.table(AlgalTransectsWrite, file="AlgalTransectsFormatted_substrate_discharge.txt", sep='\t', quote=F, row.names= F)

