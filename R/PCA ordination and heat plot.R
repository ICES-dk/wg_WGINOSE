#' @title  PCA based Integrated Ecological analysis of the North Sea
#' @description  Script used by WGINOSE for 2016 report
#' @description  Based on earlier script developed by Andrew Kerry, CEFAS, UK        
#' and produce a 'shade' plot  of the original data matrix (sorted against PC1)
# new line here (4. mai)


#' LIBRARIES
#' ---------------------
library (lattice)
library(dplyr)
library(tidyr)
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library(ggplot2)
library(RColorBrewer)

#' SETTING WORKING DIRECTORIES
#' ---------------------
#' setwd("/TEMP/WGINOSE 2016")
setwd("~/ownCloud/Research/WGINOSE/PCA IEA analysis") #erik local files


#' looping through all WGINOSE AREAS
#' ---------------------

area<-c(1:11)
area.names<-c("Orkney-Shetland", "Fladen", "Utsira", "Long Forties", "Dogger Bank", "Norfolk Banks", "German Bight", "Oyster Ground", "Southern Bight", "Skagerrak", "Kattegat" )

#select only areas w benthos data
area<-c(7:9)
area.names<-c("German Bight", "Oyster Ground", "Southern Bight")

# Add area names to the plots and file names

for (i in 1:length(area)){ 

  #' IMPORTING  & TRANSFORMING DATA
  #' ---------------------
 # data <- as.matrix(read.csv (paste("wgi", area[i], "_bottom_surface_cpue.csv", sep=""), row.names=1, header=TRUE))
   data <- as.matrix(read.csv (paste("wgi", area[i], "_bottom_surface_cpue_benthos.csv", sep=""), row.names=1, header=TRUE))
  
  trans.std <- as.data.frame(scale(log10(data+1)))
  td<-trans.std
  td[c(ncol(td)+1)]<-area.names[i]
  colnames(td)[c(ncol(td))]<-c("area")
  
  if(i==1){ allTD <- td  
  NS.all.data<-allTD  
  NS.all.data$year<-rownames(NS.all.data)
  }
  
  if(i>1){
    td$year<-rownames(td)
    NS.all.data<-rbind.fill(NS.all.data, td)

    
  }
#}  
  

  
  
  #' ANALYSES
  #' ---------------------
  #' Perform PCA
  
  data.pca <- prcomp(trans.std, cor = F)
  summary(data.pca)
  
  
  
  #' Extract loadings (eigenvalues) for PC1 & PC2 vectors
  
  pc1 <- data.pca$rotation[,1]
  pc2 <- data.pca$rotation[,2]
  loadings <- data.frame(pc1,pc2)
  pc1.order <- order(pc1, na.last = TRUE, decreasing = TRUE)
  loadings.order <- loadings[pc1.order,]
  loadings.order
  
  #' add two columns with rank (based on absolute scores) in relation to PC1 and PC2
  loadings.order <- cbind(rownames(loadings.order), loadings.order[1:2])
  colnames(loadings.order)[1]<-c("variable")
  loadings.order <- tbl_df(loadings.order)
  loadings.order <- mutate(loadings.order, Rank1 = percent_rank(abs(pc1)) )
  loadings.order <- mutate(loadings.order, Rank2 = percent_rank(abs(pc2)) )
  
  write.csv(loadings.order, file = paste("WGINOSE16_Varloadings_",area.names[i], area[i], ".csv", sep=""))
  
  
  #' Plotordination using PC1 & PC2 scores
  
  #plot(data.pca$x[,1],data.pca$x[,2], xlab = "PC1", xlim = c(-8,11), ylab = "PC2", type = "both", main =paste("PCA", area.names[i]))
  #text(data.pca$x[,1], data.pca$x[,2], row.names(data), cex=0.9, pos=4, col="red") # add labels
  
  pdf(paste("WGINOSE16_PCA_ordniation_", area.names[i], area[i], ".pdf", sep=""))
  plot(data.pca$x[,1],data.pca$x[,2], xlab = "PC1", xlim = c(-10,11), ylab = "PC2", type = "both", main =paste("PCA", area.names[i]))
  text(data.pca$x[,1], data.pca$x[,2], row.names(data), cex=0.9, pos=4, col="red") # add labels
  dev.off()

  #' reorder the data matrix stnddata using the ranked order of PC1 loadings pc1.order
  
  trans.std.order <- as.matrix(trans.std[, pc1.order])
  
  #' Heat plot of PCA scores
  
 # levelplot(trans.std.order, col.regions = rainbow(100, start = 0, end = 0.325), scales = list(x=list(cex=0.8, rot = 90), y=list(cex = 0.7)), main=list(label=paste("PCA Heatplot", area.names[i]), cex = 2), xlab=list(label="Year",cex=2), ylab=list(label="Variables",cex=2), aspect = 1.25)
  
  pdf(paste("WGINOSE16_PCA_heat", area.names[i], area[i], ".pdf", sep=""))
  #png(paste("WGINOSE16_PCA_heat", area.names[i], area[i], ".png", sep=""))
  p1<-levelplot(trans.std.order, col.regions = rainbow(100, start = 0, end = 0.325), scales = list(x=list(cex=0.8, rot = 90), y=list(cex = 0.4)), main=list(label=paste("PCA Heatplot", area.names[i]), cex = 1), xlab=list(label="Year",cex=1), ylab=list(label="Variables",cex=1), aspect = 1.25)
  print(p1)
  dev.off()
}

 
#' PCA Analysis of all areas at once
#' ---------------------------------

NS.all.data<-cbind(NS.all.data[19:20], NS.all.data[1:18], NS.all.data[21:32])
NS.all.select<-cbind(NS.all.data[c(5,7,10,12,13,14,16)])

NSall.pca<-princomp(NS.all.select)
summary(NSall.pca)
NSall.scores<-cbind(NS.all.data$area, NS.all.data$year, as.data.frame(NSall.pca$scores))
colnames(NSall.scores)[1]<-c("area")
colnames(NSall.scores)[2]<-c("year")

#' PCA plot of scores, colored by area
pca.plot<-qplot(Comp.1, Comp.2, data=NSall.scores, colour=area) + geom_point() + scale_color_brewer(palette="Set3") +theme_bw() + ggtitle("PCA entire North Sea, common variables - AREAS")
pca.plot
ggsave("PCA_allNS_areas.png")

#' PCA plot of scores, colored by year
pca.plot2<-qplot(Comp.1, Comp.2, data=NSall.scores, colour=year) + geom_point() + scale_color_discrete() +theme_bw() + ggtitle("PCA entire North Sea, common variables- YEARS")
pca.plot2
ggsave("PCA_allNS_year.png")

#' for the 1984 - 2014 data the effect of year seems more important than the effect of sub-areas. Perhaps this should be analyzed using a GAM?
#' 

#' CLUSTER ANALYSIS (dendrogram)

RankClust1 <- hclust(dist(NS.all.data[3:32]))

# very simple dendrogram
png("All NS - Dedrogram PC1.png")
plot(RankClust1, hang = -1, ann=FALSE)
title("Dendrogram of PC1 scores - All NS")
dev.off()

