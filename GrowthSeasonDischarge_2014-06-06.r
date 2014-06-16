# Calculate total discharge during the Cladophora growth season, and mean of daily mean for each day in the growing season

# read data and format date ######

angQ_8713dm = read.table('AngeloDischarge_DailyMean_1987-2013.txt', sep='\t', header=T)

angQ_8713dm$Rdate = as.Date(angQ_8713dm$Date)

angQ_8713dm$year = format(angQ_8713dm$Rdate, '%Y')

angQ_8713dm$yday = as.numeric(as.character(format(angQ_8713dm$Rdate, '%j')))

ddata = angQ_8713dm

# set days for growing season #########

start.grow = 105 #April 15
end.grow = 172  #June 21

# calculate cumulative discharge during growth season ####

gsTotal = aggregate(Discharge_cms~year,data=subset(ddata, yday>=start.grow & yday<=end.grow),FUN=sum)

# calculate mean discharge for all days preceding each day in each growth season #########

mean.cdmd = data.frame(NULL)

for (i in as.numeric(unique(ddata$year))) {
  for (j in start.grow:end.grow){
    mean.dmd = mean(ddata[which(ddata$year==i & ddata$yday<=j),'Discharge_cms'])
    mean.cdmd=rbind(mean.cdmd,cbind(i,j,mean.dmd))
    }   
  }  

names(mean.cdmd) = c('year','yday','MeanPrecedingDischarge_cms')

# write text files ############

write.table(gsTotal, file='GrowSeason_TotalDischarge.txt', sep = '\t', col.names = T, row.names=F,quote=F)

write.table(mean.cdmd, file='GrowSeason_MeanDischargeByDay.txt', sep = '\t', col.names = T, row.names=F,quote=F)
