#' Merge Picture Names into a Text File for BIIGLE Input
#'
#' This function generates a text file containing the names of all pictures in the specified directory. This text file can be used as an input list for BIIGLE.
#'
#' @param mydir A character string specifying the directory where the pictures are stored.
#' @return A text file named `list.filetoBIIGLE.txt` is created in the specified directory. This file contains the names of all the pictures.
#' @export
#'
#' @examples
#' mydir <- "path/to/your/image/directory"
#' DeepS_PictoBIIGLE(mydir)




############################## creation du fichier d'import pour biigle 

# mydir <- choose.dir( caption = "Select folder")


DeepS_PictoBIIGLE <- function(mydir) {
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
