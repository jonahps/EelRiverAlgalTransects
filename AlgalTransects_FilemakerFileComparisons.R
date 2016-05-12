## The file "2013-8027_AlgalTransects.txt" is missing almost all the substrate data
## In February 2014, Collin Bode created a new file populated with substrate data
## this file is called "algae_fm_with_substr.tab"
## However this file has 293 fewer rows than the original text file

## This script was used to ensure that the files were equivalent,
## with the exception of the missing 293 rows.

## Script created March 2016 by KBG


#### Read data ####

## Original file
  # 2013-08-27_AlgalTransects.txt
  df.og <- read.table(file.choose(), header=T, na.strings='', sep='\t', fill=TRUE, quote='')
<<<<<<< HEAD
<<<<<<< HEAD
  df.og2 <- df.og[, order(names(df.og))]
  df.og3 <- df.og2[, c(9, 26, 28)]
  names(df.og2)[2] <- "transect"
  df.og2$date <- as.Date(df.og2$date, "%m/%d/%Y")
  str(df.og2)


=======
=======
>>>>>>> master
  names(df.og)[2] <- "transect"
  df.og2 <- df.og[, order(names(df.og))]
  df.og2$date <- as.Date(df.og2$date, "%m/%d/%Y")
  str(df.og2)

<<<<<<< HEAD
>>>>>>> master
=======
>>>>>>> master
## File with substrate data
  # file: algae_fm_with_substr.tab
  df.sub <- read.table(file.choose(), header=T, na.strings='', sep='\t', fill=TRUE, quote='')
  df.sub2 <- df.sub[, order(names(df.sub))]
<<<<<<< HEAD
<<<<<<< HEAD
  df.sub3 <- df.sub2[, c(9, 26, 30)]
  df.sub2$date <- as.Date(df.sub2$date, "%Y-%m-%d")
  str(df.sub2)


=======
  df.sub2$date <- as.Date(df.sub2$date, "%Y-%m-%d")
  str(df.sub2)

>>>>>>> master
=======
  df.sub2$date <- as.Date(df.sub2$date, "%Y-%m-%d")
  str(df.sub2)

>>>>>>> master
## Order both df by id
  df.og2 <- df.og2[order(df.og2$id), ]
  df.sub2 <- df.sub2[order(df.sub2$id), ]
  head(df.og2)

## Compare the rows in df.og2 but not in df.sub2
  diff.df <- df.og2[-which(df.og2$id %in% df.sub2$id), ]
<<<<<<< HEAD
<<<<<<< HEAD

  str(diff.df)
  head(diff.df)

=======
=======
>>>>>>> master
  diff.df <- diff.df[, -22]
  str(diff.df)
  head(diff.df)

## Write a CSV
  #write.csv(diff.df, file= "MissingRowsFrom_2014-02-03_File.csv", row.names= FALSE)

## Format dates
<<<<<<< HEAD
>>>>>>> master
=======
>>>>>>> master
  diff.df$Rdate <- as.Date(diff.df$date, format= '%Y-%m-%d')
  diff.df <- transform(diff.df,
                        transect = as.factor(transect),
                        month = format(Rdate, '%m'),
                        yearday = as.numeric(format(Rdate, '%j')),
                        year = as.numeric(format(Rdate, '%Y')))

  table(diff.df$year)
  table(diff.df$transect)
<<<<<<< HEAD
<<<<<<< HEAD
=======



>>>>>>> master
=======



>>>>>>> master
