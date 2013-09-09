# Visualize data to investigate how the algal species changes at individual transect points both inter and intra annually

# Code initialized by KBG July 22, 2013

# Call dataframe "AlgalTransects2" from "AlgalTransects_format_2013-07-19.r" script.


# Select data from May to September

	astates<-AlgalTransects2[which(as.numeric(AlgalTransects2$month) >= 5 & as.numeric(AlgalTransects2$month) <= 9 & AlgalTransects2$depth >= 0 ),]
	
	
	head(astates)
	#dim(astates)

table(astates$depth,exclude=NULL)

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
	

## Calculate a diversity index for astates$AlgaeStates at each xstrmRnd point

	# Create a dataefram with a column for transect, xstrmRnd, algalState, and frequency of algal state
		
		stateFreq<-aggregate(astates$algaeStates,list(astates$xstrmRnd, astates$algaeStates, astates$Transect),length)

		colnames(stateFreq)<-c("xstrmRnd","algaeState", "Transect","Freq")


	#melt stateFreq into wide format with each column a specific algal class, each row the xstrmRnd and the freq as the data

		library(reshape2)
	
		stateFreqw<-reshape(stateFreq, v.names = "Freq", timevar="algaeState" , idvar=c("xstrmRnd","Transect"), direction="wide")

		stateFreqw[is.na(stateFreqw)] = 0




# Calculate the Brillouin Index
	# used when species are not sampled randomly (http://www.pisces-conservation.com/sdrhelp/index.html?brillouind.htm)
	
	Br<-NULL
	for(i in 1:nrow(stateFreqw)){N<-sum(stateFreqw[i,3:12])
		Br[i]<-(lfactorial(N) - sum(lfactorial(stateFreqw[i,3:12])))/N
		}
	
	Brdata<-cbind(stateFreqw[,1:2],Br)

# Plot the Brillouin Index for each transect

	pBr <- ggplot(data= Brdata, aes(x=xstrmRnd, y=Br, factor=Transect))
	
	pBr + geom_bar(stat="identity") + facet_grid(.~Transect)


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


