# Algal Transect Data: Preliminary Analysis and Visualization of algal phenology

  # This script use generalized additive mixed models (GAMMs) to visualize seasonal patterns in algal growth
  # it needs work to get it up and running again

  # it uses the data set created by the AlgalTransects_format script

#Preliminary Analysis and visualization of Clad frequency and growth
  
	# load libraries
	library(lme4)
	library(mgcv)
	library(gamm4)
	library(lattice)
	library(MASS)
	
	# try Clad frequency
		Clad_gamm1 = gamm4(Clad ~ s(yearday), data=subset(AlgalTransects2, Flood=='flood'), family=binomial, random=~(1|year)+(1|Transect:xstrm))
		Clad_gamm2 = gamm4(Clad ~ s(yearday), data=subset(AlgalTransects2, BFF==0), family=binomial, random=~(1|year)+(1|Transect)+(1|TranXstr))
		plot(Clad_gamm1$gam, xaxt='n', xlab='Month', ylab='Cladophora occurrence', xlim=c(80,285), ylim=c(-5,4))
		windows()
		plot(Clad_gamm2$gam, xaxt='n', xlab='Month', ylab='Cladophora occurrence', xlim=c(80,285), ylim=c(-5,4))
		Clad_gamm3 = gamm4(Clad ~ BFF + s(yearday, by=BFF), data=AlgalTransects2, family=binomial, random=~(1|year)+(1|transect) +(1|TranXstr))
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
		
		hist(AlgalTransects2$CladInt)
		summary(AlgalTransects2$CladInt); var(AlgalTransects2$CladInt)
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
	NosRiv_gamm1 = gamm4(NosRiv ~ s(yearday, by=BFF), data=AlgalTransects2, family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
	#NosRiv_gamm2 = gamm4(NosRiv ~ s(yearday), data=subset(AlgalTransects2, BFF==1), family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
	plot(NosRiv_gamm1$gam, xaxt='n', ylab='Nostoc/Rivularia occurrence', shade=T, pages=1, xlim=c(80,27)
	plot(NosRiv_gamm2$gam, xaxt='n', xlab='Month', ylab='Nostoc/Rivularia occurrence', shade=T)
	
# Preliminary analysis and visualization of Zygmatales
	Zyg_gamm1 = gamm4(Zyg ~ s(yearday, by=BFF), data=AlgalTransects2, family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
	#Zyg_gamm2 = gamm4(Zyg ~ s(yearday), data=subset(AlgalTransects2, BFF==1), family=binomial, random=~(1|year)+(1|transect)+(1|TranXstr))
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
	
	AlgalTransects2=merge(AlgalTransects2, AngeloDis, by='WaterYear', all.x=T)
	
	AlgalTransects2=transform(AlgalTransects2, BFF = as.factor(BFF), BFFminus1 = as.factor(BFFminus1),TotalBin = as.factor(TotalBin))
	
# Cladophora data
	
	AlgalTransectsClad = AlgalTransects2[which(AlgalTransects2$Clad==1),]
	
# Points at which Clad has been found to be present in a particular year
	
	CladYearPoint = unique(AlgalTransectsClad$YearPoint)
	
	AlgalTransectsCladGrowth = AlgalTransects2[AlgalTransects2$YearPoint %in% CladYearPoint,]