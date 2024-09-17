#PACKAGE "Rdeepsledge"


#"merge picture names in vector txt to imput pictures list in BIIGLE"
#' This function creat txt file with all picture names of the directory"
#'@param RESULT Is the txt file produced by DeepS_PictoBIIGLE()"
#'@return   list.filetoBIIGLE.txt is produce in directory
#'@export

############################## creation du fichier d'import pour biigle 

# mydir <- choose.dir( caption = "Select folder")


DeepS_PictoBIIGLE <- function(mydir) {
  if (!is.null(input)) {
    setwd(input)
    pic_files <- list.files(pattern = ".png")
    l <- as.data.frame(t(pic_files))
    write.table(l, "list.filetoBIIGLE.txt", sep = ',', col.names = FALSE, row.names = FALSE, quote = FALSE)
    message("File list success_fully saved to list.filetoBIIGLE.txt")
  } else {
    message("No folder selected. Operation cancelled.")
  }
}

# DeepS_PictoBIIGLE(mydir)
