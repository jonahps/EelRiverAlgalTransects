# Visualize data to investigate how the algal species changes at individual transect points both inter and intra annually

# Code initialized by KBG July 22, 2013

# Input file =  "AlgalTransectsFormated.txt"
# created from "AlgalTransects_format.r" script.

  AlgalTransects2<-read.csv(file.choose())


# Select data from May to September

	astates<-AlgalTransects2[which(as.numeric(AlgalTransects2$month) >= 5 & as.numeric(AlgalTransects2$month) <= 9 & AlgalTransects2$depth >= 0 ),]
	
	
	head(astates)
	#dim(astates)

table(astates$depth,exclude=NULL)

# Round xstrm to nearest 1 meter

	astates$xstrmRnd<-(round((astates$xstrm),0))

	#sort(unique(astates$xstrmRnd))

# Change TwoYears column to a factor

	astates$TwoYears<-as.factor(astates$TwoYears)
	astates$TwoYears<-factor(astates$TwoYears,levels(astates$TwoYears)[c(1,2,4,5,3)])
	#table(astates$TwoYears)
	
# Reformat algaedom column 

	# Change NA to "bare"
		
		astates$algaeStates<-factor(astates$algaedom, levels = c(levels(astates$algaedom),"bare"))
		
		b<-which(is.na(astates$algaeStates)==TRUE)
		astates$algaeStates[b]<-"bare"

		
	# Remove rows with algal classes we are not interested in, and then drop the unused levels
	
		astates<-astates[which(astates$algaeStates != "black crust" & astates$algaeStates != "green skin" & astates$algaeStates != "general blue-green algae" & astates$algaeStates != "Cyanobac filaments" & astates$algaeStates != "litter"),]

	
	# Combine some levels together
	
		# install.packages ("car") to use "recode" command
		library(car)
	
		astates$algaeStates <-recode(astates$algaeStates, " 'diatom skin' = 'Diatom skin' ")
		astates$algaeStates <-recode(astates$algaeStates, " 'Cladophora glomerata loose' = 'Cladophora glomerata attached' ")


	# Drop unused levels

		astates$algaeStates <-droplevels(astates$algaeStates)

	
	table(astates$algaeStates,exclude=NULL)	

# Drop the cross section widths that rarely have data
# T2<15m wide; T2.5<20m; T3<29m; T4<30m

	## Why doesn't this code work?
	##astatesT<-astates[-which(as.numeric(astates$Transect) == 2 & astates$xstrmRnd >= 16),]



#p2 <- ggplot(data=astatesT[which(astatesT$Transect==2),], aes(x= xstrmRnd, factor= algaeStates))
	library(ggplot2)
#	p2 + geom_histogram(stat="bin", binwidth = 1) + facet_grid(~ algaeStates, scales = "free_y") + ggtitle("Transect 2")



# Create palate for plots (www.iwanthue.com)
	kbgPal1<-c("#ED2A5B", "#6E645A", "#3474AE", "#D6E29A", "#400B1E", "#F3C6D1", "#CD5A01", "#951475", "#4E8921", "#DF9DF1")


# Plot the frequency of each algal class at each point in transect 2, stream channel usually narrower than 15m
	
	p2 <- ggplot(data=astates[which(astates$Transect==2 & astates$xstrmRnd <= 15 ),], aes(x= xstrmRnd, factor= algaeStates))
	
	
	
	p2 + geom_histogram(stat="bin", binwidth = 1) + facet_grid(~ algaeStates, scales = "free_y") + ggtitle("Transect 2")

	p2 + geom_bar(stat="bin", binwidth = 1, aes(fill= algaeStates)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ facet_grid(.~month) + ggtitle("Transect 2")


# Plot the frequency of each algal class at each point in transect 2.5, stream channel usually narrower than 20m
	
	p2.5 <- ggplot(data=astates[which(astates$Transect==2.5 & astates$xstrmRnd <= 15),], aes(x= xstrmRnd, factor= algaeStates))
	
	p2.5 + geom_histogram(stat="bin", binwidth = 1) + facet_grid(~ algaeStates, scales = "free_y") + plot_theme1 + ggtitle("Transect 2.5")

	p2.5 + geom_bar(stat="bin", binwidth = 1, aes(fill= algaeStates)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ facet_grid(.~month) + ggtitle("Transect 2.5")

	p2.5 + geom_bar(stat="bin", binwidth = 1, aes(fill= algaeStates)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1)) + ggtitle("Transect 2.5")


# Plot the frequency of each algal class at each point in transect 3, stream channel usually narrower than 29m
	
	p3 <- ggplot(data=astates[which(astates$Transect==3 & astates$xstrmRnd <= 29),], aes(x= xstrmRnd, factor= algaeStates))
	
	p3 + geom_histogram(stat="bin", binwidth = 1) + facet_grid(~ algaeStates, scales = "free_y") + plot_theme1 + ggtitle("Transect 3") 

	p3 + geom_bar(stat="bin", binwidth = 1, aes(fill= algaeStates)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ facet_grid(.~month) + ggtitle("Transect 3")

	p3 + geom_bar(stat="bin", binwidth = 1, aes(fill= algaeStates)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ ggtitle("Transect 3")
	
	
# Plot the frequency of each algal class at each point in transect 4, stream channel usually narrower than 30m
	
	p4 <- ggplot(data=astates[which(astates$Transect==4 & astates$xstrmRnd <= 30),], aes(x= xstrmRnd, factor= algaeStates))
	
	p4 + geom_histogram(stat="bin", binwidth = 1) + facet_grid(~ algaeStates, scales = "free_y") + plot_theme1 + ggtitle("Transect 4") 

	p4 + geom_bar(stat="bin", binwidth = 1, aes(fill= algaeStates)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ facet_grid(.~month) + ggtitle("Transect 4")

	p4 + geom_bar(stat="bin", binwidth = 1, aes(fill= algaeStates)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ ggtitle("Transect 4")	
	

## Calculate a diversity index for astates$AlgaeStates at each xstrmRnd point

	# Create a dataefram with a column for transect, xstrmRnd, algalState, Flood, and frequency of algal state
		
		stateFreq<-aggregate(astates$algaeStates,list(astates$xstrmRnd, astates$algaeStates, astates$Transect,astates$Flood,astates$TwoYears),length)

		colnames(stateFreq)<-c("xstrmRnd","algaeState", "Transect","Flood","TwoYears","Freq")


	#reshape stateFreq into wide format with each column a specific algal class, each row the xstrmRnd and the freq as the data

		library(reshape2)
	
		stateFreqw<-reshape(stateFreq, v.names = "Freq", timevar="algaeState" , idvar=c("xstrmRnd","Transect","Flood","TwoYears"), direction="wide")

		stateFreqw[is.na(stateFreqw)] = 0


#Calculate the Simpson's diversity Index for each transect
# see ?diversity for documentation
# simpson diversity index D = sum of all i species(proportion of species_i)^2	and is reported as 1-D

	library(vegan)

	simpd<-aggregate(astates$algaeStates,list(astates$xstrmRnd, astates$algaeStates, astates$Transect),length)		
	colnames(simpd)<-c("xstrmRnd","algaeState", "Transect","Freq")
	head(simpd)
	
	#reshape simpd into wide format with each column a specific algal class, each row the xstrmRnd and the freq as the data

		library(reshape2)
	
		simpdw<-reshape(simpd, v.names = "Freq", timevar="algaeState" , idvar=c("xstrmRnd","Transect"), direction="wide")
		simpdw[is.na(simpdw)] = 0	
		
#Create a dataframe for simpson index that includes the flood year categories		

	simpdF <-aggregate(astates$algaeStates,list(astates$xstrmRnd, astates$algaeStates, astates$Transect,astates$Flood),length)		
	colnames(simpdF)<-c("xstrmRnd","algaeState", "Transect","Flood","Freq")


	simpdFw<-reshape(simpdF, v.names = "Freq", timevar="algaeState" , idvar=c("xstrmRnd","Transect","Flood"), direction="wide")
		simpdFw[is.na(simpdFw)] = 0

head(simpdFw)

#Calculate the simpson diversity index		
	s<-diversity(simpdw[,3:12],index="simpson")

	simp<-cbind(simpdw[,1:2],s)
	head(simp)
	dim(simp)
	
#Calculate the simpson diversity index for Flood years	
	sf<-diversity(simpdFw[,4:13],index="simpson")

	simpF<-cbind(simpdFw[,1:3],sf)
	head(simpF)
	dim(simpF)
	
# Plot the Simpson index for each transect
	
	library(ggplot2)
	
	pSimp <- ggplot(data= simp, aes(x=xstrmRnd, y=s, factor=Transect))
	
	pSimp + geom_bar(stat="identity") + facet_grid(.~Transect)
	
# Plot the Simpson Flood index for each transect

	pSimpF <- ggplot(data= simpF, aes(x=xstrmRnd, y=sf, factor=Transect))
	
	pSimpF + geom_bar(stat="identity") + facet_grid(Transect~Flood)

# Conduct a t-test to see if the average index values are significantly different between flood and non flood years
# Only in T3 are the simpsons values significantly different

	
	simpFw2<-reshape(simpF[which(simpF$Transect==2),], v.names = "sf", timevar="Flood" , idvar=c("xstrmRnd","Transect"), direction="wide")
		simpFw2[is.na(simpFw2)] = 0
	t2<-t.test(simpFw2[,3],simpFw[,4], paired=TRUE);t2


	simpFw2.5<-reshape(simpF[which(simpF$Transect==2.5),-5], v.names = "sf", timevar="Flood" , idvar=c("xstrmRnd","Transect"), direction="wide")
		simpFw2.5[is.na(simpFw2.5)] = 0

	t2.5<-t.test(simpFw2.5[,3],simpFw2.5[,4], paired=TRUE);t2.5

	simpFw3<-reshape(simpF[which(simpF$Transect==3),-5], v.names = "sf", timevar="Flood" , idvar=c("xstrmRnd","Transect"), direction="wide")
		simpFw3[is.na(simpFw3)] = 0

	t3<-t.test(simpFw3[,3],simpFw3[,4], paired=TRUE);t3
	
	simpFw4<-reshape(simpF[which(simpF$Transect==4),-5], v.names = "sf", timevar="Flood" , idvar=c("xstrmRnd","Transect"), direction="wide")
		simpFw4[is.na(simpFw4)] = 0

	t4<-t.test(simpFw4[,3],simpFw4[,4], paired=TRUE);t4
	





	pSimp2 <- ggplot(data=simp[which(simp$Transect==2 & simp$xstrmRnd <= 15),], aes(x=xstrmRnd, y=s, factor=Flood))
	pSimp2  + geom_bar(stat="identity") + facet_grid(.~Flood)

	pSimp2.5 <- ggplot(data= simp[which(simp$Transect==2.5 & simp$xstrmRnd <= 15),], aes(x=xstrmRnd, y=s, factor=Flood))
	pSimp2.5 + geom_bar(stat="identity") + facet_grid(.~Flood)
	
	pSimp3 <- ggplot(data= simp[which(simp$Transect==3 & simp$xstrmRnd <= 29),], aes(x=xstrmRnd, y=s, factor=Flood))
	pSimp3 + geom_bar(stat="identity") + facet_grid(.~Flood)

	pSimp4 <- ggplot(data=simp[which(simp$Transect==4 & simp$xstrmRnd <= 30),], aes(x=xstrmRnd, y=s, factor=Flood))
	pSimp4 + geom_bar(stat="identity") + facet_grid(.~Flood)
	
	
	
	
# Calculate the Brillouin Index
	# used when species are not sampled randomly (http://www.pisces-conservation.com/sdrhelp/index.html?brillouind.htm)
	
	Br<-NULL
	for(i in 1:nrow(stateFreqw)){N<-sum(stateFreqw[i,5:14])
		Br[i]<-(lfactorial(N) - sum(lfactorial(stateFreqw[i,5:14])))/N
		}
	
	Brdata<-cbind(stateFreqw[,1:4],Br)

# Plot the Brillouin Index for each transect and flood category
	
	library(ggplot2)
	pBr <- ggplot(data= Brdata, aes(x=xstrmRnd, y=Br, factor=Transect))
	
	pBr + geom_bar(stat="identity") + facet_grid(.~Transect)

	pBr2 <- ggplot(data=Brdata[which(Brdata$Transect==2 & Brdata $xstrmRnd <= 15),], aes(x=xstrmRnd, y=Br, factor=Flood))
	pBr2 + geom_bar(stat="identity") + facet_grid(.~Flood)

	pBr2.5 <- ggplot(data=Brdata[which(Brdata$Transect==2.5 & Brdata $xstrmRnd <= 15),], aes(x=xstrmRnd, y=Br, factor=Flood))
	pBr2.5 + geom_bar(stat="identity") + facet_grid(.~Flood)
	
	pBr3 <- ggplot(data=Brdata[which(Brdata$Transect==3 & Brdata$xstrmRnd <= 29),], aes(x=xstrmRnd, y=Br, factor=Flood))
	pBr3 + geom_bar(stat="identity") + facet_grid(.~Flood)

	pBr4 <- ggplot(data=Brdata[which(Brdata$Transect==4 & Brdata$xstrmRnd <= 30),], aes(x=xstrmRnd, y=Br, factor=Flood))
	pBr4 + geom_bar(stat="identity") + facet_grid(.~Flood)

# Plot the Brillouin Index for each transect and TwoYears category


	pBr2ty <- ggplot(data=Brdata[which(Brdata$Transect==2 & Brdata $xstrmRnd <= 15),], aes(x=xstrmRnd, y=Br, factor=TwoYears))
	pBr2ty + geom_bar(stat="identity") + facet_grid(.~TwoYears)

	pBr2.5ty <- ggplot(data=Brdata[which(Brdata$Transect==2.5 & Brdata $xstrmRnd <= 15),], aes(x=xstrmRnd, y=Br, factor=TwoYears))
	pBr2.5ty + geom_bar(stat="identity") + facet_grid(.~TwoYears)

	pBr3ty <- ggplot(data=Brdata[which(Brdata$Transect==3 & Brdata$xstrmRnd <= 29),], aes(x=xstrmRnd, y=Br, factor=TwoYears))
	pBr3ty + geom_bar(stat="identity") + facet_grid(.~TwoYears)

###### Substrate experimentation

# how does the substrate class vary at each point

		substrFreq<-aggregate(astates$substr,list(astates$xstrm, astates$xstrmRnd, astates$substr, astates$Transect),length)
			colnames(substrFreq)<-c("xstrm","xstrmRnd","Substrate", "Transect","Freq")
		
		substrFreq$level<-as.numeric(substrFreq$Substrate)	
			
			
			pSub <- ggplot(data= substrFreq[which(substrFreq$Transect == 2),], aes(x=xstrmRnd, y=Freq, factor=Transect))
	
	pSub + geom_bar(stat="identity", aes(fill = Substrate)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))


			pSub <- ggplot(data= substrFreq[which(substrFreq$Transect == 4),], aes(x=xstrm, y=level, factor=Transect))

	pSub + geom_point(aes(color = Substrate)) + scale_color_manual(values = c(kbgPal1)) + plot_theme1

table(substrFreq$substr, exclude=NULL)


