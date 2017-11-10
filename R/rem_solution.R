###
###
###
###   Purpose:   Testing Removal of solution sections
###   started:   2017-11-09 (pvr)
###
### ################################################### ###
###
### #   Set input file and read content into vector
psFile <- "sol/w9/lbg_hs_2017_w9_sol8.Rmd"

#' Remove solutions from solution document
#'
#' The following diagram shows the placement of the solutions in 
#' the solution document. The aim of this function is at any step 
#' i to cut out the parts between s[i] and e[i]
#' 
#'  |=============|----------|==================|-------------|   .....
#'  1           s[1]        e[1]              s[2]           e[2] .....
#'  
rem_solution_section <- function(psSrcFile, psTrgFile = NULL, pbDebugOut = FALSE){
  if (pbDebugOut) 
    cat(" ==> Started rem_solution_section with arg: psSrcFile = ", psSrcFile, "\n")
  conIn <- file(psSrcFile)
  vecCont <- readLines(con = conIn)
  close(conIn)
  if (pbDebugOut) cat(" ==> Number of lines read: ", length(vecCont), 
                      "\n ==> head: \n", paste(head(vecCont), sep = "", collapse = "\n"),
                      "\n ==> tail: \n", paste(tail(vecCont), sep = "", collapse = "\n"), "\n")
  ### #   Initialize result vector
  vecSolStart <- grep(pattern = "<!-- solution -->", x = vecCont, fixed = TRUE)
  vecSolEnd <- grep(pattern = "<!-- /solution -->", x = vecCont, fixed = TRUE)
  if (pbDebugOut)
    cat(" ==> solution starts: ", paste(vecSolStart, sep = ", ", collapse = ", "),
        "\n ==> solution ends: ", paste(vecSolEnd, sep = ", ", collapse = ", "), "\n")
  ### #   consistency checks
  if (length(vecSolStart) != length(vecSolEnd)){
    stop(" *** * Might be a run-away solution section - starts: ", 
         paste(vecSolStart, sep = ", ", collapse = ", "), 
         " - ends: ", 
         paste(vecSolEnd, sep = ", ", collapse = ", "))
  }
  ### # loop over start vector and cut out the solutions
  vecResult <- vecCont[1:(vecSolStart[1]-1)]
  for (idx in 2:length(vecSolStart)){
    vecResult <- c(vecResult, vecCont[(vecSolEnd[idx-1]+1):(vecSolStart[idx]-1)])
  }
  ### # if there are parts after last solution, add them here
  nLastSolEnd <- vecSolEnd[length(vecSolEnd)]
  if (length(vecCont) > nLastSolEnd)
    vecResult <- c(vecResult, vecCont[(nLastSolEnd+1):length(vecCont)])
  
  ### # check whether we have to construct the target filename
  sTrgFile <- psTrgFile
  if (is.null(sTrgFile)){
    sTrgDir <- sub(pattern = "sol", replacement = "ex", x = dirname(psSrcFile), fixed = TRUE)
    sTrgFile <- file.path(sTrgDir, sub(pattern = "_sol", 
                                       replacement = "_ex", 
                                       x = basename(psSrcFile),
                                       fixed = TRUE))
  } else {
    sTrgDir <- dirname(sTrgFile)
  }

  ### # in case sTrgDir does not exist, create it  
  if (!dir.exists(sTrgDir)) {
    if (pbDebugOut)
      cat(" ==> Creating target directory: ", sTrgDir, "\n")
    dir.create(sTrgDir, recursive = TRUE)
  }
  ### # write output to sTrgFile
  if (pbDebugOut)
    cat(" ==> Writing result to target file: ", sTrgFile, "\n")
  cat(paste0(vecResult, collapse = "\n"), "\n", file = sTrgFile)
  
  return(invisible(TRUE))
}

rem_solution_section(psSrcFile = psFile, pbDebugOut = TRUE)