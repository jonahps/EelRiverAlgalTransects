# Calculate spring spates based on daily mean discharge
  # Jonah and Keith: Spates are defined as discharge events over 5 cms after May 15 
  # Mary defines spates in her 2008 paper using various thresholds
    # discharge: 5 cms and 50 cms
    # date: 1 March, 1 May, 1 June

# read data and format date ######

angQ_8713dm = read.table('AngeloDischarge_DailyMean_1987-2013.txt', sep='\t', header=T)

angQ_8713dm$Rdate = as.Date(angQ_8713dm$Date)

angQ_8713dm$year = format(angQ_8713dm$Rdate, '%Y')

angQ_8713dm$yday = as.numeric(as.character(format(angQ_8713dm$Rdate, '%j')))

# Jonah/Keith spates
  # after May 15
  # threshold: 5 cms
  # date max, max cms

# for each year
  # find intervals over 5 cms
  # find max and date for each interval

# find rows with spate conditions
intervals = subset(angQ_8713dm, yday > 135 & yday < 212 & Discharge_cms > 5)

# find start and end dates for sequences of rows
  # creates vector of row numbers with start and end dates
sequences <- function(x,incr = 1)
{
        ix <- which(abs(diff(c(FALSE,diff(x) == incr))) ==1)
        if(length(ix)%%2)c(ix,length(x))
        else ix
}


startend = sequences(intervals$yday)

startend2 = cbind(startend[seq(1,length(startend),2)],startend[seq(2,length(startend),2)])

spates = data.frame(year=as.numeric(),MaxDischarge=numeric(),Rdate=as.Date(character()))

for (i in 1:nrow(startend2)){
  spates[i,'year'] = intervals[startend2[i,1],'year']
  tmp = intervals[startend2[i,1]:startend2[i,2],]
  spates[i,'MaxDischarge'] = max(tmp[,"Discharge_cms"])
  spates[i,'Rdate'] = tmp[which.max(tmp[,"Discharge_cms"]),'Rdate']
}

write.table(spates,'SpringSpatesJK_8713.txt',sep='\t',row.names=F,quote=F)

unique(spates$year)

# Thresholds from Power et al 2008

# create subsets
mar = subset(angQ_8713dm, yday>59 & yday<212)
may = subset(angQ_8713dm, yday>120 & yday<212)
jun = subset(angQ_8713dm, yday>151 & yday<212)

# create data frame
MEPspate = data.frame(year=c(1987:2013))

MEPspate$mar5 = as.numeric(aggregate(Discharge_cms~year,data=mar,max)[,2]>5)

MEPspate$mar50 = as.numeric(aggregate(Discharge_cms~year,data=mar,max)[,2]>50)

MEPspate$may5 = as.numeric(aggregate(Discharge_cms~year,data=may,max)[,2]>5)

MEPspate$may50 = as.numeric(aggregate(Discharge_cms~year,data=may,max)[,2]>50)

MEPspate$jun5 = as.numeric(aggregate(Discharge_cms~year,data=jun,max)[,2]>5)

MEPspate$jun50 = as.numeric(aggregate(Discharge_cms~year,data=jun,max)[,2]>50)
  
write.table(MEPspate,'SpringSpatesMEP_8713.txt',sep='\t',row.names=F,quote=F)