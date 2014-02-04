# Script creates plots to visualize the relationship between maximum Clad length at each point across the transects and 1) light 2) substrate 3) flow 4) depth.

# created by K. Bouma-Gregson Oct. 2013


# Input max clad data (file = AlgalTransects_PointCladMaxHeight.csv)
# created from "AlgalTransects_CladLightFlood.r" script

		cml<-read.csv(file.choose())
		head(cml)
		str(cml)

# Remove depth outliers

		#cml[which(cml$depth > 500),]
		#cml<-cml[-723,]
				

  	
# Plots of the data

	library(ggplot2)
	library(RColorBrewer)
	#install.packages("gridExtra")
	library(gridExtra)
	
	
	# Set the plotting aesthetics
	
		plot_themeCML <-theme(panel.grid = element_blank(),  panel.background = element_rect(color="gray5"), panel.border = element_blank(), axis.text = element_text(colour="black"), axis.line = element_line(colour="black"), legend.background = element_rect(size=0.25, color="black"), legend.key = element_blank(),strip.background=element_blank(),strip.text.y=element_text(angle=0))
	
	
	# Create labels for the facets
	
	t_labeller <-function(var,value){
		value<-as.character(value)
		if (var=="Transect"){
			value[value=="2"] <- "Transect 2"
			value[value=="2.5"] <- "Transect 2.5"
			value[value=="3"] <- "Transect 3"
			value[value=="4"] <- "Transect 4"
		}
		if (var=="Flood"){
			value[value=="flood"] <- "flood"
			value[value=="no flood"] <-"no flood"
		}
		return(value)
	}
	


# Plot of Clad Height as a function of Irradiance		
	pl<-ggplot(data=cml, aes(x=watt_avg,y=CladInt,group=Transect))
  
  	pl + geom_point(size=1) + facet_grid(Flood~Transect, scales='free_y', labeller=t_labeller) + labs(x="Irradiance (Avg. Watt Hours / Day)", y="Max. Clad Height (cm)") + ggtitle("Max. Clad. Height as a Function of Irradiance") + plot_themeCML
  

# Plot of Clad Height and Irradiance across Channel Cross Section	
	px<-ggplot(data=cml, aes(x=xstrm,y=CladInt,group=Transect))
 
 	px + geom_point() + geom_line(aes(x=xstrm,y=watt_avg/10), color="red") + 
 geom_point(aes(x=xstrm,y=depth*2, alpha=0.5), color="blue") +	
 	facet_grid(Transect~Flood, scales='free_y') + plot_themeCML
 	
# Depth and flow

	pdf<-ggplot(data=cml, aes(x=depth,y=flow,group=Transect))
	
	pdf + geom_point() + facet_grid(Transect~Flood, scales='free_y') + plot_themeCML


# Density plot of depth

		pd<-ggplot(data=cml,aes(x=depth,group=Transect))
		
		pd + geom_density() + facet_grid(Transect~Flood, scales='free_y') + plot_themeCML
