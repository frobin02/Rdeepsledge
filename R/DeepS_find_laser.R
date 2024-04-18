#PACKAGE "Rdeepsledge"


#"Fonction de detection laser"


#' This function detects double green lasers on a towed benthic sledge
#'@param MP4_file is the video file assigned MP4_file <- file.choose() for example
#'@param para is a parallelization option to optimize the computing time FALSE for this version
#'@param freq is the frame frequency used. A shorter number increases the computing time.
#'@return a result is saved also in user/documents directory as laser_position_$yourvideonames$.txt at each iteration.
#'@export

DeepS_find_laser <- function(MP4_file,para,freq) {
  freq<-as.numeric(freq)
  library(raster)
  library(sp)
  library(sf)
  library(foreach)
  library(doParallel)
  library(terra)
  library(stringi)
  library(tictoc)
  library(dplyr)
  
 if(para==TRUE){para<-TRUE}else{para<-FALSE} 
freq<-as.numeric(freq)
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
                                        statistics = "contrast" )}
  tic()
  para <- FALSE
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
 # plot(results_df)
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

# dev.off()
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

  return(RESULT)
  
  }
#/////////////////////////////////////////////////////////////////////////////////



