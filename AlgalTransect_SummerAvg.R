# This script will calculate the average summer light irradiance along Mary's 4 transects
# created by Keith Bouma-Gregson Oct. 2013

# Data from Collin Bode's LiDAR irradiance model
# units of light = watt hours / day

# Before using this script run the "LightModel_MaryTransects_inputscript.R" to format the data

# Dataframe = lld

	head(lld)
	sm<-lld

# Extract the weeks of the growing season April 1 - August 1 (approximately weeks 13 - 33)
	sml<-sm[which(sm$week >=13 & sm$week <=33),]
	
# Drop unused levels in transect factor (artifacts of raw data)
	sml$tx<-droplevels(sml$tx)
	
# Calculate the average watts per transect point for the entire summer
	wattavg<-aggregate(watts ~ transect + xstream, data=sml, FUN=mean)
	wattsd<-aggregate(watts ~ transect + xstream, data=sml, FUN=sd)
	
	wattavg<-cbind(wattavg,wattsd[,3])
	colnames(wattavg)[3:4]<-c("watt_avg","watt_stdev")
	
	head(wattavg)


# Estimate the xstream watt values for xstream integers not measured by the model


	# Eliminate xstream values that will not be modeled for cladophora growth

		# Range of xstreams to be included
			# 2	min=1	max=15
			# 2.5	min=0	max=19
			# 3	min=-1	max=29
			# 4 	min=2	max=23

		# Create a subset for each transect
			wattavg2<-wattavg[which(as.numeric(wattavg$transect)==1),]
			wattavg2.5<-wattavg[which(as.numeric(wattavg$transect)==2),]
			wattavg3<-wattavg[which(as.numeric(wattavg$transect)==3),]
			wattavg4<-wattavg[which(as.numeric(wattavg$transect)==4),]


	# Add additional xstream values to make the xstream interval 1m instead of 2m

		# Transect 2
 			a<-seq(0,14,by=2)
 			a<-a[-1]
 			z<-rep(2,7)
 			b<-matrix(c(z,a),nrow=7, ncol=4)
 	
 			for(i in 1:length(a)){
 				b[i,3]<-mean(c(wattavg2$watt_avg[i+1],wattavg2$watt_avg[i]))
 				b[i,4]<-mean(c(wattavg2$watt_stdev[i+1],wattavg2$watt_stdev[i]))
 			}


			# Format matrix as dataframe to bind with wattavg2
				b<-as.data.frame(b)
				colnames(b)<-colnames(wattavg2)
		
		
			# Add a value for xstream=1m because it can't be looped
				b[8,]<-c(2,1,b[1,3], b[1,4])


			# Bind new xstream values to wattavg2
				wattavg2<-rbind(wattavg2,b)
				head(wattavg2);tail(wattavg2)


		# Transect 2.5
		 		head(wattavg2.5,11)
		 		head(a2.5,11)
 				b2.5
 		
 				a2.5<-seq(from=1,to=19,by=2)
 				z<-rep(2.5,10)
 				b2.5<-matrix(c(z,a2.5),nrow=10, ncol=4)
 	
 				for(i in 1:length(a2.5)){
 					b2.5[i,3]<-mean(c(wattavg2.5$watt_avg[i+1],wattavg2.5$watt_avg[i]))
 					b2.5[i,4]<-mean(c(wattavg2.5$watt_stdev[i+1],wattavg2.5$watt_stdev[i]))
 				}


			# Format matrix as dataframe to bind with wattavg2
				b2.5<-as.data.frame(b2.5)
				colnames(b2.5)<-colnames(wattavg2.5)
		

			# Bind new xstream values to wattavg2
				wattavg2.5<-rbind(wattavg2.5,b2.5)
				head(wattavg2.5);tail(wattavg2.5)


		# Transect 3
 				head(wattavg3,15)
 				head(a3,15)
 				b3
 		
				a3<-seq(from=1,to=27,by=2)
 				z3<-rep(3,14)
 				b3<-matrix(c(z3,a3),nrow=14, ncol=4)
 	
 				for(i in 1:length(a3)){
 				b3[i,3]<-mean(c(wattavg3$watt_avg[i+1],wattavg3$watt_avg[i]))
 				b3[i,4]<-mean(c(wattavg3$watt_stdev[i+1],wattavg3$watt_stdev[i]))
 				}

			# Format matrix as dataframe to bind with wattavg2
				b3<-as.data.frame(b3)
				colnames(b3)<-colnames(wattavg3)
		

			# Bind new xstream values to wattavg2
				wattavg3<-rbind(wattavg3,b3)
				head(wattavg3);tail(wattavg3)

		# Transect 4
 				head(wattavg4,20)
				head(a3,15)
 				b4
 		
 				a4<-seq(from=3,to=21,by=2)
 				z4<-rep(4,10)
 				b4<-matrix(c(z4,a4),nrow=10, ncol=4)
 	
 				for(i in 1:length(a4)){
 					b4[i,3]<-mean(c(wattavg4$watt_avg[i+1],wattavg4$watt_avg[i]))
					b4[i,4]<-mean(c(wattavg4$watt_stdev[i+1],wattavg4$watt_stdev[i]))
 				}

			# Format matrix as dataframe to bind with wattavg2
				b4<-as.data.frame(b4)
				colnames(b4)<-colnames(wattavg4)
		
			# Bind new xstream values to wattavg2
				wattavg4<-rbind(wattavg4,b4)
				head(wattavg4);tail(wattavg4)

	# Combine all 4 datasets back into a larger set

		wattavgf<-rbind(wattavg2,wattavg2.5,wattavg3,wattavg4)
		colnames(wattavgf)[1:2]<-c("Transect","xstrm")
		head(wattavgf);tail(wattavgf)
		dim(wattavgf)


	# Constrain the xstream values to the range above
		wattavg2<-wattavg2[which(wattavg2$xstream >=1 & wattavg2$xstream <=15),]
		wattavg2.5<-wattavg2.5[which(wattavg2.5$xstream >=0 & wattavg2.5$xstream <=19),]
		wattavg3<-wattavg3[which(wattavg3$xstream >=-1 & wattavg3$xstream <=29),]
		wattavg4<-wattavg4[which(wattavg4$xstream >=2 & wattavg4$xstream <=23),]
	






## Plots to visualize data

	# My prefered ggplot theme
	plot_theme1<-theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_text(colour="black"), axis.line = element_line(colour="black"))

	# Plot of averageg watts for each transect
		p<-ggplot(data=wattavg, aes(x=xstream, y=watt_avg, group=transect))
		
		p + geom_line() + geom_ribbon(aes(ymin=watt_avg - watt_stdev, ymax=watt_avg + watt_stdev),fill="blue", alpha=0.2) + facet_grid(.~transect) + plot_theme1
		
	
	





