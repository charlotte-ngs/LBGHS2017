###
###
###
###   Purpose:   Enumeration of objects in document
###   started:   2017-11-07 (pvr)
###
### ################################################## ###

#' Generic enumeration function
#' 
#' Given a file and a pattern, all lines where the pattern is found, 
#' are prepended by a running number or that number is appended to 
#' the line
generic_enumerate <- function(psFile, psPattern, pnStartNumber = 1, pbIsNumberAfter = TRUE){
  ### # read the content of psFile as a vector
  conIn <- file(psFile)
  vecCont <- readLines(con = conIn)
  close(con = conIn)
  ### # results
  vecResult <- vecCont
  ### # lines on which pattern occurs
  vecLineIdx <- grep(pattern = psPattern, vecResult, fixed = TRUE)

  ### # loop over vecLineIdx and add numbers
  for (idx in seq_along(vecLineIdx)){
    cat("Index", idx, "\n")
    currentNumber <- pnStartNumber + idx - 1
    if (pbIsNumberAfter){
      vecResult[vecLineIdx[idx]] <- paste(vecResult[vecLineIdx[idx]], currentNumber)
    } else {
      vecResult[vecLineIdx[idx]] <- paste(currentNumber, vecResult[vecLineIdx[idx]])
    }
  }
  
  return(vecResult)   
}

task_enumerate <- function(psFile, psPattern = "## Aufgabe", pnStartNumber = 1, pbIsNumberAfter = TRUE){
  return(generic_enumerate(psFile = psFile, 
                           psPattern = psPattern, 
                           pnStartNumber = pnStartNumber,
                           pbIsNumberAfter = pbIsNumberAfter))
}