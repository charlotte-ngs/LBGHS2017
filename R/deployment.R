###
###
###
###   Purpose:   Skripts related to deployment of material
###   started:   2017/09/17 (pvr)
###
### ######################################################## ###

#' Deployment of course website
#' 
deploy_course_website <- function(pswebsitename = "index.html", 
                                  psfrom        = ".", 
                                  psto          = "../../gh-pages/LBGHS2017",
                                  pbrecursive   = TRUE                       ) {
  ### # check whether the source file exists, if not stop
  src_file <- file.path(psfrom, pswebsitename)
  if (!file.exists(src_file))
    stop("ERROR in deploy_course_website: CANNOT FIND source file: ", src_file)
  ### # if the target directory does not exist, create it
  if (!dir.exists(psto) & pbrecursive)
    dir.create(path = psto, recursive = pbrecursive)
  trg_file <- file.path(psto, pswebsitename)
  ### # deploy via file.rename
  bresult <- file.rename(from = src_file, to = trg_file)
  return(invisible(bresult))
}