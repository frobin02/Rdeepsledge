#PACKAGE "Rdeepsledge"


#"find dark zone in picture"
#' This function creat grid count number of pixel in dark zones"
#'@param RESULT Is the number of not avalaible pixel is produced by DeepS_darckzone()"
#'@return   GRID dir is create in picture directory with picture and summary
#'@export


#mydir <- choose.dir( caption = "Select folder")

#sensibility <-10 

DeepS_darckzone <- function(mydir,sensibility,plot){
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
  
  
  
  datablack<-list(NULL)
  setwd(mydir)
  suppressWarnings({dir.create("DARKZONE")})
  result<-paste(mydir,"/DARKZONE",sep="")
  setwd(mydir)
  pic_files=list.files(pattern=".png")
  for (i in 1:length(list.files(pattern=".png"))){
    setwd(mydir)
    pic1<- levels(as.factor(pic_files))[i]
    
    img <- load.image(pic1)
    gray_img <- grayscale(img)
    pixel_matrix <- as.cimg(gray_img)
    black_raster <- as.raster(pixel_matrix)
    hex_to_grayscale <- function(hex_color) {
      # Convert hex color to RGB values
      rgb_vals <- col2rgb(hex_color)
      
      # Since the image is grayscale, R, G, and B will be the same
      # We can use the R component (or G or B, since they are the same)
      # Normalize to 0-1 range by dividing by 255
      grayscale_value <- rgb_vals[1, ] / 255
      
      return(grayscale_value)
    }
    black_raster_numeric <- apply(as.matrix(black_raster), 2, hex_to_grayscale)
    
    black_raster_numeric_raster <- raster(black_raster_numeric)
    
    
    #   plot(black_raster_numeric_raster)
    #   raslist <- suppressWarnings({raster::brick(black_raster)})
    #   
    # 
    # raslist <- suppressWarnings({raster::brick(pic1)[[1]]})
    ss= max(getValues(black_raster_numeric_raster))*sensibility/100
    rdata=getValues(black_raster_numeric_raster)
    rdata[rdata >= ss]=NA
    rdata[rdata <= ss]= 1
    values(black_raster_numeric_raster)=rdata
    plot(black_raster_numeric_raster)
    raslist=black_raster_numeric_raster
    pixval <-as.data.frame(raslist)
    pixval %>% summary()
    datablack[[i]]<-c(pic1,sum(is.na(pixval)))
    if(plot==TRUE){
      plot(raslist)
      r_aggregated <- aggregate(raslist, fact=6, fun=min)
      r_flipped <- flip(r_aggregated, direction = "y")
      polygon <- rasterToPolygons(r_flipped, dissolve = TRUE)
      # plot(polygon,add=T)
      img <- image_read(pic_files[i])
      point_img <- image_draw(img)
      lines(polygon,col="red")
      dev.off()
      output_path <- paste(pic_files[i],"_DARKZONE_",sensibility,".png",sep="")  # Remplacez par le chemin de sauvegarde souhaité
      setwd(result)
      image_write(point_img, path = output_path, format = "png")
    }
  }
  setwd(result)
  darkzone<-as.data.frame(do.call(rbind,datablack))
  
  write.csv(darkzone,"DARKZONE_IMAGE.csv")
}

#TODARCKPIC(mydir,sensibility,TRUE)

