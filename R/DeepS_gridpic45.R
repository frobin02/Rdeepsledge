#PACKAGE "Rdeepsledge"


#"Creat grid with 45 noeud on picture"
#' This function creat grid on new picture to help to subsample picture"
#'@param RESULT Is the new pircture is produced by DeepS_gridpic45()"
#'@return   GRID dir is create in picture directory
#'@export


# mydir<-choose.dir()

DeepS_gridpic45<-function(mydir){
  
  library(imager)
  library(raster)
  library(sf)
  library(terra)
  library(tidyr)
  library(Hmisc)
  library(magick)
  library(utils)
  library(ggplot2)
  library(dplyr)
  
  setwd(mydir)
  dir.create("GRID")
  output=paste(mydir,"/","GRID",sep="")
  for (i in 1:length(list.files(pattern=".png"))){
    setwd(mydir)
    pic_files <- list.files(pattern=".png")
    # library(imager)
    # library(magick)
    
    imgi<-load.image(pic_files[i])
    img <- image_read(pic_files[i])
    start_lat <- dim(imgi)[2]/5/2
    start_lng <- dim(imgi)[2]/5/2
    griddf <- expand.grid(latcoords = seq(from = start_lat, by = dim(imgi)[2]/5, l = 5),
                          lngcoords = seq(from = start_lng, by = dim(imgi)[2]/5, l = 9))
    
    point_img <- image_draw(img)
    for(p in 1:dim(griddf)[1]){
      # points(griddf$lngcoords[p], griddf$latcoords[p], col = "red", pch = 1, cex = 10,lwd=5)}
      abline(v=griddf$lngcoords[p], col = "white")
      abline(h=griddf$latcoords[p], col = "white")}
    
    dev.off()
    nom<-gsub(".png","",pic_files[i])
    output_path <- paste(nom,"-GRID.png",sep="")  # Remplacez par le chemin de sauvegarde souhaité
    setwd(output)
    
    image_write(point_img, path = output_path, format = "png")
  }
}

# DeepS_gridpic45(mydir)