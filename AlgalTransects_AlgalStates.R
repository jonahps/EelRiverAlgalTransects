# Visualize data to investigate how the algal species changes at individual transect points both inter and intra annually

# Code initialized by KBG July 22, 2013

# Call dataframe "AlgalTransects2" from "AlgalTransects_format_2013-07-19.r" script.


# Select data from April to September

	astates<-AlgalTransects2[which(as.numeric(AlgalTransects2$month) >= 5 & as.numeric(AlgalTransects2$month) <= 9),]
	
	#head(astates)
	#dim(astates)


# Round xstrm to nearest 0.5 meter

	astates$xstrm<-(round((astates$xstrm*2),0))/2

	#sort(unique(astates$xstrm))


# Reformat algaedom column 

	# Change NA to "bare"
		
		astates$algaedom<-factor(astates$algaedom, levels = c(levels(astates$algaedom),"bare"))
		
		b<-which(is.na(astates$algaedom)==TRUE)
		astates$algaedom[b]<-"bare"

		
	# Remove rows with algal classes we are not interested in, , and then drop the 		unused levels
	
		astates<-astates[which(astates$algaedom != "black crust" & astates$algaedom != "green skin" & astates$algaedom != "general blue-green algae" & astates$algaedom != "Cyanobac filaments" & astates$algaedom != "litter"),]

	
	# Combine some levels together
	
		# install.packages ("car") to use "recode" command
		library(car)
	
		astates$algaedom<-recode(astates$algaedom, " 'diatom skin' = 'Diatom skin' ")
		astates$algaedom<-recode(astates$algaedom, " 'Cladophora glomerata loose' = 'Cladophora glomerata attached' ")


	# Drop unused levels

		astates$algaedom<-droplevels(astates$algaedom)

	
	table(astates$algaedom,exclude=NULL)	


# Create palate for plots
	kbgPal1<-c("#ED2A5B", "#6E645A", "#3474AE", "#D6E29A", "#400B1E", "#F3C6D1", "#CD5A01", "#951475", "#4E8921", "#DF9DF1")


# Plot the frequency of each algal class at each point in transect 2, stream channel usually narrower than 15m
	
	p2 <- ggplot(data=astates[which(astates$Transect==2 & astates$xstrm <= 15),], aes(x=xstrm, factor=algaedom))
	
	p2 + geom_histogram(stat="bin", binwidth = 0.5) + facet_grid(~ algaedom, scales = "free_y") + plot_theme1 + ggtitle("Transect 2")

	p2 + geom_bar(stat="bin", binwidth = 0.5, aes(fill=algaedom)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ facet_grid(.~month) + ggtitle("Transect 2")


# Plot the frequency of each algal class at each point in transect 2.5, stream channel usually narrower than 20m
	
	p2.5 <- ggplot(data=astates[which(astates$Transect==2.5) & astates$xstrm <= 15,], aes(x=xstrm, factor=algaedom))
	
	p2.5 + geom_histogram(stat="bin", binwidth = 0.5) + facet_grid(~ algaedom, scales = "free_y") + plot_theme1 + ggtitle("Transect 2.5")

	p2.5 + geom_bar(stat="bin", binwidth = 0.5, aes(fill=algaedom)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ facet_grid(.~month) + ggtitle("Transect 2.5")

	p2.5 + geom_bar(stat="bin", binwidth = 0.5, aes(fill=algaedom)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1)) + ggtitle("Transect 2.5")


# Plot the frequency of each algal class at each point in transect 3, stream channel usually narrower than 29m
	
	p3 <- ggplot(data=astates[which(astates$Transect==3 & astates$xstrm <= 29),], aes(x=xstrm, factor=algaedom))
	
	p3 + geom_histogram(stat="bin", binwidth = 0.5) + facet_grid(~ algaedom, scales = "free_y") + plot_theme1 + ggtitle("Transect 3") 

	p3 + geom_bar(stat="bin", binwidth = 0.5, aes(fill=algaedom)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ facet_grid(.~month) + ggtitle("Transect 3")

	p3 + geom_bar(stat="bin", binwidth = 0.5, aes(fill=algaedom)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ ggtitle("Transect 3")
	
	
# Plot the frequency of each algal class at each point in transect 4, stream channel usually narrower than 30m
	
	p4 <- ggplot(data=astates[which(astates$Transect==4 & astates$xstrm <= 30),], aes(x=xstrm, factor=algaedom))
	
	p4 + geom_histogram(stat="bin", binwidth = 0.5) + facet_grid(~ algaedom, scales = "free_y") + plot_theme1 + ggtitle("Transect 4") 

	p4 + geom_bar(stat="bin", binwidth = 0.5, aes(fill=algaedom)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ facet_grid(.~month) + ggtitle("Transect 4")

	p4 + geom_bar(stat="bin", binwidth = 0.5, aes(fill=algaedom)) + plot_theme1 + scale_fill_manual(values = c(kbgPal1))	+ ggtitle("Transect 4")	
	



# Calculate the standard deviaion in algaedom class at each xstrm point for each transect

	stdv<-aggregate(data=astates, algaedom ~ Transect + xstrm, sd)

	# Plot the standard deviation of algaedom data

		pstdv <- ggplot(data= stdv, aes(x=xstrm, y=algaedom, group=Transect))

		pstdv + geom_bar(stat="identity", binwidth=-.5) +  plot_theme1 + facet_grid(.~ Transect)	+  ggtitle("Stdev of Algaedom")

	# Make a heatmap of the standard deviations

		phm<-ggplot(data=stdv, aes(x=xstrm, y=Transect,)) 

		phm + geom_tile(aes(fill=algaedom),color="white") + scale_fill_gradient(low = "white", high= "steel blue") + plot_theme1









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


