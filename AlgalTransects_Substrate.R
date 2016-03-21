## Investigate substrate variation at each xstrm within each year

## Use a stability index:  bedrock=3, boulders=2, cobbles=1, gravel,mud,pebbles,roots,sand,silt,wood=0
## We assume that the substrate should not change over the course of a summer
## because there are no high flows that would disturbe the bed in summer
## We calculate mean, std. dev., mode, min, and max, from multiple measurements
## at each xstrm within the same year.

## These will be used to determine what annual substrate value will be assigned to
## a particular xstrm for all the inter-annual comparisons

## Created by KBG March 2016

## Dimensions of data frame at_sub= 8625x34
## Dimensions of df atwp_sub (after accounting only for wetted points)dim= 6184x35
## Number of rows with no substrate data = 55 (all NAs)
## Number of rows with stability std. deviation == 0 within a year, indicating multiple transects with same stability entry = 557
## Number of rows with stability std. dev. == NA within a year, indicating only 1 or 2 measurements = 577


#### Read in data ####
# Algal transects data with all substrate included
# file: algae_fm_with_substr.tab
  at_sub <- read.table(file.choose(), header=T, na.strings='', sep='\t', fill=TRUE, quote='')
  str(at_sub)

## Format dates
  at_sub$Rdate <- as.Date(at_sub$date, format= '%Y-%m-%d')
  at_sub <- transform(at_sub,
                               transect = as.factor(transect),
                               month = format(Rdate, '%m'),
                               yearday = as.numeric(format(Rdate, '%j')),
                               year = as.numeric(format(Rdate, '%Y')))


## find and eliminate points that did not have water during growing season (which starts Apr 15 and ends July 31  (yearday= 105 thru 212))
  # use only points that still had water on the last survey of the growing season in each year
  # want transect and survey points from the last survey of each transect in the growing season

## find last survey during grow season
  LastGrowSurvey_sub <- aggregate(yearday ~ transect + year,
                                  data=subset(at_sub, yearday<=212 & yearday>=105),
                                  FUN=max)

## get transect points present at last survey from growing season in each year
  WetPoints_sub <- at_sub[which(paste(at_sub$transect,
                                      at_sub$year,
                                      at_sub$yearday,
                                      sep='-')
                                %in%
                                  paste(LastGrowSurvey_sub$transect,
                                        LastGrowSurvey_sub$year,
                                        LastGrowSurvey_sub$yearday,
                                        sep='-')),
                          c('transect', 'year', 'xstrm', 'yearday', 'depth')]

## remove non-integer points
  #WetPoints_sub <- WetPoints_sub[which(WetPoints_sub$xstrm%%1 == 0),]
  #table(WetPoints$xstrm%%1 == 0)
  #table(WetPoints$xstrm)

## Refine at_sub data to include only wet points
  atwp_sub <- at_sub[which(paste(at_sub$transect,
                                 at_sub$year,
                                 at_sub$xstrm)
                           %in%
                             paste(WetPoints_sub$transect,
                                   WetPoints_sub$year,
                                   WetPoints_sub$xstrm)), ]

## Add substrate data
  # create stability index
    # bedrock=3,
    # boulders=2
    # cobbles=1
    # gravel,mud,pebbles,roots,sand,silt,wood=0

  atwp_sub$stab <- as.numeric(with(atwp_sub,
                                   ifelse(substr=='NAN', NA,
                                          ifelse(substr=='bedrock', 3,
                                                 ifelse(substr=='boulders', 2,
                                                        ifelse(substr=='cobbles', 1, 0
                                                        ))))))

  #CladMaxPointWet = merge(CladMaxPointWet, SubAvg, all.x=T)

#### Calculate summary statistics ####
  ## to investigate variation in substrate stability within a year
  library(plyr)
  library(NCmisc) #Mode function

  sub.stat <- ddply(subset(atwp_sub, yearday<=212 & yearday>=105),
                    c("year", "transect", "xstrm"), summarise,
                    N = length(stab),
                    stab.mode = Mode(stab),
                    stab.mean = mean(stab, na.rm= T),
                    stab.sd = sd(stab, na.rm= T),
                    stab.min = min(stab),
                    stab.max = max(stab),
                    stab.diff = stab.max - stab.min)

  sub.stat[, 6:7] <- round(sub.stat[, 6:7], 2)
  sub.stat$rnd.mean <- round(sub.stat$stab.mean, 0)
  head(sub.stat)

#### Investigate the data ####
  head(sub.stat[which(is.na(sub.stat$stab.sd) == T), ], 20)
  dim(sub.stat[which(is.na(sub.stat$stab.sd) == T), ])
  (subset(sub.stat, stab.mode == 3 & stab.diff == 3))

  subset(atwp_sub, (year == 1991 & transect == 2.5 & xstrm == 13) & (yearday<=212 & yearday>=105))

#### Plot the data ####
  library(ggplot2)

  stab.sd.1 <- ggplot(data= sub.stat, aes(x= stab.sd))
  stab.sd.1 + geom_histogram(aes(fill= stab.sd == 0)) + theme_bw(base_size= 20)

  stab.diff.1 <- ggplot(data= sub.stat[which(sub.stat$stab.mode == 3), ], aes(x= stab.diff))
  stab.diff.1 + geom_histogram(aes(fill= stab.diff == 0)) + facet_grid(.~transect) + theme_bw(base_size= 20)

  stab.mean.mode.1 <- ggplot(data= sub.stat, aes(x= stab.mode, y= rnd.mean))
  stab.mean.mode.1 + geom_point(aes(color= factor(N)), size= 4, position= "jitter") + theme_bw(base_size = 20)



