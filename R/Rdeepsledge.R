#PACKAGE "Rdeepsledge"


#"Fonction de detection laser"
#' Cette fonction detect les doubles lasers verts sur une luge benthique tracté"
#'@param MP4_file est la vide assigné MP4_file <- file.choose()
#'@param para est une option de parallelisation  pour optiniser le temps de calcul
#'@param freq est la frequence des frames utilisés (elle doit etre supperieur a 40)
#'@return un fichier RESULT, est une sauvegarde dans user document

find_laser <- function(MP4_file,para,freq) {

  if (!requireNamespace("tictoc", quietly = TRUE)) {
    install.packages("tictoc")
  }

  library(raster)
  library(sp)
  library(sf)
  library(foreach)
  library(doParallel)
  library(terra)
  library(stringi)
  library(tictoc)

  # Créer le répertoire temporaire
  temp_dir <- tempfile()
  dir.create(temp_dir)
  setwd(temp_dir)
    videonames <-gsub("\\\\","£",MP4_file)
    videonames <-as.vector(stri_split_fixed(videonames,"£") )
    videonames<-videonames[[1]][length(as.factor(videonames[[1]]))]
    videonames <-as.vector(stri_split_fixed(videonames,".") )[[1]][1]

  home_path <- Sys.getenv("HOME")
  desktop_path <- file.path(home_path, "Desktop")

  # Encodage de la vidéo
  av::av_encode_video(MP4_file, output = 'Capture_%5d.png',vfilter = 'null')#, vfilter = 'fps=fps=1')#ici  ok ça marche

  # Traitement des images
  pic_files <- list.files(pattern=".png")

  staat<-c("contrast")

  RESULT<-data.frame(matrix(vector(), 0,6 ,dimnames=list(c(), c("X","Y","Nlaser","surface", "frame","origin") )))

  for ( i in seq(from =1, to=c(length(levels(as.factor(pic_files)))-40),by=freq)){
    setwd(temp_dir)
  cat(paste("\n",round(c(i/length(levels(as.factor(pic_files)))*100),1),"% \n ",sep=" "))

  sample1<- seq(from=1, to=30,by=5)
  raslist <-list()
  k=1
  raster_glcm<-function(ras){glcm::glcm(ras, window = c(7, 7),
                                        shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)),
                                        statistics = "contrast"
  )}
  tic()
  para = FALSE
  if (para==FALSE){

  raslist<-list()
  k=1
  for ( j in sample1) {
    setwd(temp_dir)
    pic1<- levels(as.factor(pic_files))[i+j]
    raslist[[k]] <- raster_glcm(raster::brick(pic1)[[1]])
    k=k+1}
  results <- raslist}
  if (para==TRUE){

    cat("paral. on")
  raslist<-list()
  k=1
  for ( j in sample1) {
    setwd(temp_dir)
    pic1<- levels(as.factor(pic_files))[i+j]
    raslist[[k]] <- raster::brick(pic1)[[1]]
    k=k+1}
  raster_files <- raslist


 ## Fonction de traitement d'une image

 process_raster <- function(ras_file) {
   # Charger l'image
   ras1<-ras_file
   ras_contrast<-raster_glcm(ras1)

   # Traitement de l'image (exemple : calculer la moyenne)

   # Retourner le résultat
   return(ras_contrast)
 }

 cl <-parallel::makeCluster(detectCores()-2)
 doParallel::registerDoParallel(cores = cl)
 results <- foreach(ras_file = raster_files) %dopar% {
   raster_glcm<-function(ras){glcm::glcm(ras, window = c(7, 7),
                                                shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)),
                                                statistics = "contrast"
   )}
   process_raster(ras_file)


 }
gc(); stopCluster(cl)
  }
  toc()

 results_df <- do.call(stack, results)
 plot(results_df)
 cumul_res<-results_df[[1]]
 rr<-getValues(results_df)
 # rr<-ifelse(rr<=5,0,1)
 # values(results_df)<-rr
 # # x11();plot(b)
 rr[rr==0]<-NA

#  rr[is.na(rr)]<-0
#  # rr[rr>0]<-1
# is.na(rr[rr==0])
 values(cumul_res)<-rowSums(rr,na.rm = FALSE)
 rr<-getValues(cumul_res)
 rr[rr<=max(rr,na.rm=T)*2/3]<-NA
 rr[rr>0]<-1
 values(cumul_res)<-rr
 rr[is.na(rr)]<-0
 stop_to_next<-max(rr)==0


 regions_to_polygons <- function(region_raster) {
   polygons <- rasterToPolygons(region_raster, dissolve = TRUE)
   return(polygons)
 }


 if(stop_to_next==FALSE){
   glcm.red5<-regions_to_polygons(cumul_res)
   glcm.red5 <-st_as_sf(glcm.red5)
   glcmbuf<-st_buffer(glcm.red5, dist=3)
   # test<-raster::buffer(glcm.red5, width=3)
   # plot(test)
   merged_polygons <- st_as_sf(glcmbuf)

   df_union_cast <- st_cast(merged_polygons, "POLYGON")
   library(dplyr)
   df_union_cast <- df_union_cast %>%     mutate(surface = st_area(.))

   centros <- df_union_cast %>%  st_centroid() %>% st_coordinates()

   result1<-as.data.frame(centros)
   result1$Nlaser<-dim(result1)[1]
   result1$surface<-df_union_cast$surface
   result1$frame<-pic1
   result1$origin<-MP4_file
   cat( paste(dim(result1)[1], "  points...\n",sep=""))

   RESULT<-rbind(RESULT,result1)
   names(RESULT)<-c("X","Y","Nlaser","surface", "frame","origin")

  setwd(home_path)
  write.table(RESULT,paste("laser_position_",videonames,".txt",sep=""))

 }

 if(stop_to_next==TRUE) {
   X<-NA
   Y<-NA
   Nlaser<-0
   result1$surface<-NA
   frame<-pic1
   origin<-MP4_file
   cat(" pas de points...\n")
   result1<-c(X,Y,frame,origin,Nlaser)
   RESULT<-rbind(RESULT,result1)
   names(RESULT)<-c("X","Y","Nlaser","surface", "frame","origin")
   setwd(home_path)
   write.table(RESULT,paste("laser_position_",videonames,".txt",sep=""))

 }



    #/////////////////////////////////////////////////////////////////////////////////
  }
  # Nettoyage
  setwd(temp_dir)
  unlink(temp_dir, recursive = TRUE)
}



# Rdeepsledge
# Exemple d'utilisation
# MP4_file <- file.choose()

# find_laser(MP4_file,FALSE,120)
#
# RESULT<-read.table('laser_position_Plume2022_P2ST16_GOPR_2022-07-18.txt')
# MPO_REFUGE_source<-list()
# MPO_REFUGE_source$LASER<-RESULT

# RESULT <-readRDS("MPO_REFUGE_source.R")

# [2] "laser_position_Plume2022_P4ST12_GOPR_2022-07-15.txt"

# RESULT <- file.choose()
RESCORR<-function(RESULT){
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
RESULT$layer<- sel[2]
RESULT$layer<-ifelse(is.na(RESULT$layer) ,"red","green")


B<- ggplot(RESULT,aes(y=Nframe,x=X,color=layer))+
  geom_point()+geom_vline(xintercept = c(maxX,mid,minX))
A<- ggplot(RESULT,aes(y=Nframe,x=Y,color=layer))+
  geom_point()
first<-  ggarrange(A,B,ncol=2)

namevideo <-gsub("\\\\","£",RESULT$origin [1])
namevideo <-as.vector(stri_split_fixed(namevideo,"£") )
namevideo<-namevideo[[1]][length(as.factor(namevideo[[1]]))]
allp<-  ggplot(RESULT,aes(x=X,y=Y,color=layer))+geom_point()+ggtitle(namevideo)

vieww <- ggarrange(allp,first,ncol=1)

LASER$DATA<-RESULT
LASER$GRAPH<-vieww

}
#
# NEWRES<-RESCORR(RESULT)
#
# devtools::create("Rdeepsledge")
# export("Rdeepsledge")
# use_git()
#
# # Pousser le package sur GitHub
# use_github()
#
# chemin <- system.file("MPO_REFUGE_source.R", package = "Rdeepsledge")
#
#
############################## creation d'un grille sur les images 




############################## creation des masque d'ombrage et surface 
