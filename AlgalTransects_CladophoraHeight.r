# How does the timing and height of Cladophora blooms vary?

	
# average height on each survey at each transect 
# and date and size of peak algal bloom

		# create data frame of maximum mean modal biomass per year at transect 2.5

		Clad2DateMean = na.omit(with(AlgalTransects2[which(AlgalTransects2$Transect=='2' & AlgalTransects2$depth > 0),], aggregate(CladInt, by=list(year=year, date=date), FUN=mean)))
		names(Clad2DateMean)[3] = 'CladHt_cm'
		Clad2DateMean$Rdate = as.Date(Clad2DateMean$date, format= '%m/%d/%Y')
		#write.table(Clad2DateMean, sep = '\t', file='AlgalTransect2_MeanCladHeight.txt', col.names = T, row.names=F, quote=F)


		Clad2.5DateMean = na.omit(with(AlgalTransects2[which(AlgalTransects2$Transect=='2.5' & AlgalTransects2$depth > 0),], aggregate(CladInt, by=list(year=year, date=date), FUN=mean)))
		names(Clad2.5DateMean)[3] = 'CladHt_cm'
		Clad2.5DateMean$Rdate = as.Date(Clad2.5DateMean$date, format= '%m/%d/%Y')
		#write.table(Clad2.5DateMean, sep = '\t', file='AlgalTransect2.5_MeanCladHeight.txt', col.names = T, row.names=F, quote=F)
		
Clad3DateMean = na.omit(with(AlgalTransects2[which(AlgalTransects2$Transect=='3' & AlgalTransects2$depth > 0),], aggregate(CladInt, by=list(year=year, date=date), FUN=mean)))
		names(Clad3DateMean)[3] = 'CladHt_cm'
		Clad3DateMean$Rdate = as.Date(Clad3DateMean$date, format= '%m/%d/%Y')
		#write.table(Clad3DateMean, sep = '\t', file='AlgalTransect3_MeanCladHeight.txt', col.names = T, row.names=F, quote=F)		
Clad4DateMean = na.omit(with(AlgalTransects2[which(AlgalTransects2$Transect=='4' & AlgalTransects2$depth > 0),], aggregate(CladInt, by=list(year=year, date=date), FUN=mean)))
		names(Clad4DateMean)[3] = 'CladHt_cm'
		Clad4DateMean$Rdate = as.Date(Clad4DateMean$date, format= '%m/%d/%Y')
		#write.table(Clad4DateMean, sep = '\t', file='AlgalTransect4_MeanCladHeight.txt', col.names = T, row.names=F, quote=F)			
		


# Create a data frame with maximum mean modal biomass in each year (and date at which it was attained)

		idMax2 = sapply(split(1:nrow(Clad2DateMean), Clad2DateMean$year), function(x) 	x[which.max(Clad2DateMean$CladHt_cm[x])])
		Clad2AnnMax = Clad2DateMean[idMax2,]
		#write.table(Clad2AnnMax, sep = '\t', file='AlgalTransect2_AnnMaxCladHeight.txt', col.names = T, row.names=F, quote=F)

		idMax2.5 = sapply(split(1:nrow(Clad2.5DateMean), Clad2.5DateMean$year), function(x) x[which.max(Clad2.5DateMean$CladHt_cm[x])])
		Clad2.5AnnMax = Clad2.5DateMean[idMax2.5,]
		#write.table(Clad2.5AnnMax, sep = '\t', file='AlgalTransect2.5_AnnMaxCladHeight.txt', col.names = T, row.names=F, quote=F)
		
idMax3 = sapply(split(1:nrow(Clad3DateMean), Clad3DateMean$year), function(x) x[which.max(Clad3DateMean$CladHt_cm[x])])
		Clad3AnnMax = Clad3DateMean[idMax3,]
		#write.table(Clad3AnnMax, sep = '\t', file='AlgalTransect3_AnnMaxCladHeight.txt', col.names = T, row.names=F, quote=F)		

idMax4 = sapply(split(1:nrow(Clad4DateMean), Clad4DateMean$year), function(x) x[which.max(Clad4DateMean$CladHt_cm[x])])
		Clad4AnnMax = Clad4DateMean[idMax4,]
		#write.table(Clad4AnnMax, sep = '\t', file='AlgalTransect4_AnnMaxCladHeight.txt', col.names = T, row.names=F, quote=F)
		
		
## Make plot of the idMax data					
	t1<-as.data.frame(c(idMax2,idMax2.5, idMax3, idMax4))
	t1$year<-rep(c(1987:1992,1995:2012),4)
	t1$transect<-rep(c(2,2.5,3,4),24)
	t1$transect<-sort(t1$transect)
	t1$transect<-as.factor(t1$transect)
	names(t1)[1]<-'height'


		idMAplot<- ggplot(data=t1, aes(x=year, y=height, color=transect))

		idMaxAll_plot<- idMAplot + geom_line(aes(color=transect)) + facet_grid(transect 		~ ., scales 		= "free_y") + plot_theme1 + ggtitle("idMax Plot")
		ggsave(idMaxAll_plot,file= "idMaxAll_plot.png")


	
		
		
# create data frame of maximum mean modal biomass per year at each transect

		CladDateMean = na.omit(aggregate(CladInt ~ year+date+Transect, FUN=mean, data=AlgalTransects2[which(AlgalTransects2$depth > 0),]))
		names(CladDateMean)[4] = 'CladHt_cm'
		CladDateMean$Rdate = as.Date(CladDateMean$date, format= '%m/%d/%Y')
		write.table(CladDateMean, sep = '\t', file='AlgalTransectsAll_MeanCladHeight.txt', col.names = T, row.names=F, quote=F)
		
class(CladDateMean); head(CladDateMean)	



##Make plot of the CladDateMean data
		aht<-ggplot(data=CladDateMean, aes(x=Rdate, y=CladHt_cm, color=Transect))
		
		CladDateMean_plot<-aht + geom_line(aes(color=Transect)) + facet_grid(Transect ~ ., scales = 		"free_y") + plot_theme1	+ ggtitle("CladDateMean Plot")
		ggsave(CladDateMean_plot,file= "CladDateMean_plot.png")
		

##Make plot of algae heights from Full Survey Data (atFWwet_stMean data line 192)
		amean<-ggplot(data=atFSwet_stMean, aes(x=year, y=CladInt, color=Transect))

		atFWwet_stMean_plot<-amean + geom_line(aes(color=Transect)) + facet_grid(Transect ~ ., scales = 		"free_y") + plot_theme1	+ ggtitle("atFWet_stMean Plot")
		ggsave(atFWwet_stMean_plot,file= "atFWwet_stMean_plot.png")




# Create a data frame with maximum mean modal biomass in each year (and date at which it was attained)
			# write to text file


##this "idMax" code where I try to index by Transect and Year does not appear to have worked. The idMax list gives a different result for transect 2.5 than the idMax2.5 code in line 225
		idMax = sapply(split(1:nrow(CladDateMean), list(CladDateMean$Transect, CladDateMean$year)), function(x) x[which.max(CladDateMean$CladHt_cm[x])])
	
# line 305 does not work and I don't understand why
		#CladAnnMax = CladDateMean[idMax,]
		#write.table(idMax, sep = '\t', file='AlgalTransectsT_AnnMaxCladHeight.txt', col.names = T, row.names=F, quote=F)				


	

