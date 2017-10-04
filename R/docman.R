###
###
###
###   Purpose:   Helpers for document management
###   started:   2017/09/20 (pvr)
###
### ############################################# ###

#' Clean up all files with a name that starts with _main
#' 
bookdown_cleanup <- function(pspath = "cn", pspattern = "_main", pbfull.names = TRUE) {
  bfrmresult <- file.remove(list.files(path = pspath, 
                                       pattern = pspattern, 
                                       full.names = pbfull.names))
  return(invisible(all(bfrmresult)))                          
}
