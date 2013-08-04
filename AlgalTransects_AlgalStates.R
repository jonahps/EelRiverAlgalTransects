# Visualize data to investigate how the algal species changes at individual transect points both inter and intra annually

# Code initialized by KBG July 22, 2013

# Call dataframe "AlgalTransects2" from "AlgalTransects_format_2013-07-19.r" script.


# Select data from May to September

	astates<-AlgalTransects2[which(as.numeric(AlgalTransects2$month) >= 5 & as.numeric(AlgalTransects2$month) <= 9),]
	
	head(astates)
	#dim(astates)


# Round xstrm to nearest 1 meter

	astates$xstrmRnd<-(round((astates$xstrm),0))

	#sort(unique(astates$xstrmRnd))


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


# Create palate for plots (www.iwanthue.com)
	kbgPal1<-c("#ED2A5B", "#6E645A", "#3474AE", "#D6E29A", "#400B1E", "#F3C6D1", "#CD5A01", "#951475", "#4E8921", "#DF9DF1")


# Plot the frequency of each algal class at each point in transect 2, stream channel usually narrower than 15m
	
	p2 <- ggplot(data=astates[which(astates$Transect==2 & astates$xstrmRnd <= 15),], aes(x= xstrmRnd, factor= algaeStates))
	
	p2 + geom_histogram(stat="bin", binwidth = 1) + facet_grid(~ algaeStates, scales = "free_y") + plot_theme1 + ggtitle("Transect 2")

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
	

## Calculate the diversity index for astates$AlgaeStates at each xstrmRnd point

	# Create a dataefram with a column for transect, xstrmRnd, algalState, and frequency of algal state
		
		stateFreq<-aggregate(astates$algaeStates,list(astates$xstrmRnd, astates$algaeStates, astates$Transect),length)

		colnames(stateFreq)<-c("xstrmRnd","algaeState", "Transect","Freq")


	#melt stateFreq into wide format with each column a specific algal class, each row the xstrmRnd and the freq as the data

		library(reshape2)
	
		stateFreqw<-reshape(stateFreq, v.names = "Freq", timevar="algaeState" , idvar=c("xstrmRnd","Transect"), direction="wide")

		stateFreqw[is.na(stateFreqw)] = 0


# Calculate the Shannon Diversity Index

		library(vegan)

		stateFreqw2<-stateFreqw2[which(stateFreqw2$Transect==2),]
		
		divSh<-diversity(stateFreqw,"shannon")
	
		dt2<-as.data.frame(cbind(c(0,seq(0:14)),divT2Sh))
	colnames(dt2)<-c("xstrmRnd","ShannonDiv")
	
# Plot the Shannon Diversity Index against xstrmRnd

	pdiv2t<-ggplot(data=dt2, aes(x=xstrmRnd, y=ShannonDiv))
	
	pdiv2t + geom_bar(stat="identity")	


# Calculate the Brillouin Index
	Br<-NULL
	for(i in 1:nrow(stateFreqw){
		N<-sum(stateFreqw[i,3:12])
		Br[i]<-(lfactorial(N) - sum(lfactorial(stateFreqw[i,3:12])))/N	
	}

?while
	

Hb <- function(ns) {    N <- sum(ns)    (lfactorial(N) - sum(lfactorial(ns)))/N }


#### BELOW IS MY TEST SCRIPT FOR TRANSECT 2#############################



# Determine the proportion of algae at each point
	
	# Create a contingency table for the count of dominant algal classes at 	each xstrm
		
		freq <- table(t2dr$algaedom, t2dr$xstrm)
		freq<-as.data.frame(freq)
		colnames(freq)[c(1:3)]<-c("algaedom","xstrm","count")
	
	# Sum up the total number of algal counts at each xstrm
		
		prop<-aggregate(freq$count ~ freq$xstrm, FUN = sum)
		colnames(prop)<-c("pxstrm","sumcount")
	
		
	# Divide the count of each algal class at each xstrm by the sum of all 		algal classes counted		
		
		freq$sum<-rep(prop$sumcount,each=8)
		freq$p<- freq$count/freq$sum
		tail(freq,20)

# Plot the proportion of each algal class at each xstrm point in transect 2
	
	p2 <- ggplot(data=freq, aes(x=xstrm, y=p, factor=algaedom))
	
	# Histogram
		p2 + geom_histogram(stat="identity", binwidth = 0.5, 						aes(fill=algaedom)) + 	facet_wrap(~ algaedom, nrow=2, ncol=4, 	scales 		= "free_y") + plot_theme1			
		
	# Stacked bar graph showing same data
		p2 + geom_bar(stat="identity",aes(fill=algaedom)) + plot_theme1	
	
	# Multiple y-axes, plot freq$count and freq$p for each xstrm and faceted by algaedom


