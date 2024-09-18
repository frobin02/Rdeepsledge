#PACKAGE "Rdeepsledge"
#' Merge Picture Names into a Text File for BIIGLE Input
#'
#' This function creates a text file containing the names of all the pictures in the specified directory. The text file is intended for use as an input list in BIIGLE.
#'
#' @param mydir A directory where the pictures are stored.
#' @return A text file named `list.filetoBIIGLE.txt` is created in the specified directory. This file contains the names of all the pictures.
#' @export 
#' 
#' @examples
#' mydir <- "path/to/your/image/directory"
#' rDeepS_PictoBIIGLE(mydir)




############################## creation du fichier d'import pour biigle 

# mydir <- choose.dir( caption = "Select folder")


rDeepS_PictoBIIGLE <- function(mydir) {
  library(reshape2)
  
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
