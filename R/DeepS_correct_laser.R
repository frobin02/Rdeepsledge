#' Laser Detection Correction Function
#'
#' This function performs corrections on the dataset produced by `DeepS_find_laser()` for double green lasers on a towed benthic sledge.
#'
#' @param RESULT A dataset produced by the `DeepS_find_laser()` function.
#' @return A backup file is created in the user's `Documents` directory.
#' @export
#'
#' @examples
#' result_data <- DeepS_find_laser(MP4_file)
#' corrected_data <- correct_laser_detection(result_data)

DeepS_correct_laser<-function(RESULT){
  library(ggplot2)
  library(raster)
  library(stringi)
LASER<-list()
ggplot(RESULT,aes(x=X,y=Y))+geom_point()
RESULT$Nframe<-do.call(rbind,strsplit(RESULT$frame,c("_")))[,2]
RESULT$Nframe<-as.numeric(gsub(".png","",RESULT$Nframe))
RESULT2<-droplevels(subset(RESULT,Nlaser%in%2))
library(dplyr)

RESULT2 %>%
  arrange(desc(Nframe),desc(X) ) -> RESULT2
RESULT2$seq<-c(1,2)
maxX<-mean(RESULT2$X[RESULT2$seq==1])
minX  <-  mean(RESULT2$X[RESULT2$seq==2])
mid<- mean(c(minX,maxX))


A<- ggplot(RESULT,aes(y=Nframe,x=Y))+geom_point()
B<- ggplot(RESULT,aes(y=Nframe,x=X))+geom_point()+geom_vline(xintercept = c(maxX,mid,minX))
library(ggpubr)
first<- ggarrange(A,B,ncol=2)

RESULT$laserposition= ifelse(RESULT$X>=mid,"right","left")
RL <-droplevels(subset(RESULT,laserposition%in% "left"))
RL$DISMEAN<- abs(RL$X-minX)
RR <-droplevels(subset(RESULT,laserposition%in% "right"))
RR$DISMEAN<- abs(RR$X -maxX)

statsRR <-  boxplot.stats(RR$DISMEAN)
statsRR <-  statsRR$stats[5]


RR$COL<-ifelse(RR$X>=maxX-statsRR & RR$X<=maxX+statsRR,"green","red")

statsRL <- boxplot.stats(RL$DISMEAN)
statsRL <- statsRL$stats[5]
RL$COL<-ifelse(RL$X>=minX-statsRL & RL$X<=minX+statsRL,"green","red")

RESULT<-rbind(RL,RR)
B<- ggplot(RESULT,aes(y=Nframe,x=X,color=COL))+
  geom_point()+geom_vline(xintercept = c(maxX,mid,minX))
A<- ggplot(RESULT,aes(y=Nframe,x=Y,color=COL))+
  geom_point()
ggarrange(A,B,ncol=2)

ggplot(RESULT,aes(x=X,y=Y,color=COL))+geom_point()

r <- raster(nrows=1100/4, ncols=1800/4, xmn=0, xmx=1800, ymn=0, ymx=1100)
r[] <- NA
points_df <- data.frame(x = RESULT$X, y = RESULT$Y, value = 1)
r <- rasterize(points_df[, c("x", "y")], r, field=points_df$value, fun=sum)
r[r<=1] <-NA
r[r>=1] <-1
buffered_raster <- buffer(r, width=1, maskNA=FALSE)
polygons <- rasterToPolygons(buffered_raster, dissolve=TRUE)
sel<- extract(polygons,points_df[,c(1,2)])
RESULT$Validity<- sel[2]
RESULT$Validity<-ifelse(is.na(RESULT$Validity) ,"red","green")


B<- ggplot(RESULT,aes(y=Nframe,x=X,color=Validity))+
  geom_point()+geom_vline(xintercept = c(maxX,mid,minX))
A<- ggplot(RESULT,aes(y=Nframe,x=Y,color=Validity))+
  geom_point()
first<-  ggarrange(A,B,ncol=2)

namevideo <-gsub("\\\\","£",RESULT$origin [1])
namevideo <-as.vector(stri_split_fixed(namevideo,"£") )
namevideo<-namevideo[[1]][length(as.factor(namevideo[[1]]))]
allp<-  ggplot(RESULT,aes(x=X,y=Y,color=Validity))+geom_point()+ggtitle(namevideo)

vieww <- ggarrange(allp,first,ncol=1)

LASER$DATA<-RESULT
LASER$GRAPH<-vieww
return(LASER)
}


