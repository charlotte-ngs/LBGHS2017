###
###
###
###   Purpose:   User management for exercise platform
###   started:   2017/09/21 (pvr)
###
### ################################################### ###

#' Extract usernames from a list of Email addresses
#' 
#' We assume that user information is stored in a 
#' tab-separated file called psuser_file. Email 
#' addresses are assumed to be found in a column 
#' with a column header called psemail_col_header.
#' 
#' The user info is read and the part of the email address
#' before the @-sign is returned as a username.
#' 
#' @param psuser_file name of file where user info can be found
#' @param psemail_col_header header of column where emails are stored
#' @examples 
#' sUserFile <- file.path("data","users_to_add.tsv")
#' sEmailColHeader <- "E.Mail"
#' vec_users <- get_user_names(psuser_file = sUserFile, psemail_col_header = sEmailColHeader)
#' dfusers <- read.delim2(file = sUserFile, stringsAsFactors = FALSE)
#' vec_users <- get_user_names(pdfusers=dfusers, psemail_col_header = sEmailColHeader)
#' @return vecuser_names vector with extracted usernames
get_user_names <- function(psuser_file = NULL, pdfusers = NULL, psemail_col_header){
  if (is.null(pdfusers)){
    if (is.null(psuser_file))
      stop("ERROR: function get_user_names must either be given a dataframe with user info or a filename")
    dfusers <- read.delim2(file = psuser_file, stringsAsFactors = FALSE)
  } else {
    dfusers <- pdfusers
  }
  vecemail <- dfusers[[psemail_col_header]]
  vecuser_names <- sapply(vecemail, 
                         function(x) 
                           return(unlist(strsplit(x, 
                                                  split = "@", 
                                                  fixed = TRUE))[1]), 
                         USE.NAMES = FALSE)
  return(vecuser_names)
}

#' Return a list of usernames and passwords
#' 
#' @param pvecusers vector of usernames
#' @param pvecalpha vector containing the alphabet
#' @param pnpwd_len length of passwords
#' @return liresult_usr_passwd list of users and passwords
#' @examples 
#' luser_passwd <- get_user_passwd(pvecusers = vec_users)
#' 
get_user_passwd <- function(pvecusers, pvecalpha = c(as.character(0:9), letters, LETTERS), pnpwd_len = 8){
  lresult_user_passwd <- NULL
  for (idx_user in seq_along(pvecusers)){
    cur_password <- paste0(pvecalpha[floor(length(pvecalpha)*runif(pnpwd_len))+1], collapse = "")
    if(is.null(lresult_user_passwd)){
      lresult_user_passwd <- list(username = pvecusers[idx_user],
                                  password = cur_password) 
    } else {
      lresult_user_passwd <- c(lresult_user_passwd, list(username = pvecusers[idx_user],
                                                         password = cur_password) )
    }
  }
  return(lresult_user_passwd)
}


#' Second version of password generator
#' 
#' @examples 
#' luser_passwd <- get_user_passwd2(pvecusers = vec_users)
#' 
get_user_passwd2 <- function(pvecusers, pvecalpha = c(as.character(0:9), letters, LETTERS), pnpwd_len = 8){
  ### # construct vector with passwords
  vec_passwd <- sapply(seq_along(pvecusers), 
                       function(x) 
                         return(paste0(pvecalpha[floor(length(pvecalpha)*runif(pnpwd_len))+1], 
                                       collapse = "")))
  ### # return list of usernames and passwords
  return(list(usernames = pvecusers, passwords = vec_passwd))
  
}


#' Get username and password as data.frame
#' 
#' @examples 
#' sBelegungFilename <- "20171013_BelegungenLerneinheit_751630500L_Herbstsemester_2017.txt"
#' sUserFile <- file.path("data",sBelegungFilename)
#' sEmailColHeader <- "E.Mail"
#' dfusers_passwd <- get_df_user_passwd(psuser_file = sUserFile, psemail_col_header = sEmailColHeader)
get_df_user_passwd <- function(psuser_file, 
                               psemail_col_header, 
                               pvecalpha = c(as.character(0:9), letters, LETTERS), 
                               pnpwd_len = 8) {
  dfusers <- read.delim2(file = psuser_file, stringsAsFactors = FALSE)
 # vec_users <- get_user_names(psuser_file = psuser_file, psemail_col_header = psemail_col_header)
  vec_users <- get_user_names(pdfusers=dfusers, psemail_col_header = psemail_col_header)
  luser_passwd <- get_user_passwd2(pvecusers = vec_users,
                                   pvecalpha = pvecalpha,
                                   pnpwd_len = pnpwd_len)
  return(data.frame(Familienname = dfusers$Familienname,
                    Vorname      = dfusers$Vorname,
                    Email        = dfusers$E.Mail,
                    Username     = luser_passwd$usernames,
                    Password     = luser_passwd$passwords))
}


#' Generate Email messages for students based on a template text and on a student db
#' 
#' The student db is just a dataframe containing all information that must be entered
#' into the template to become individual email messages.
#' 
#' @param pstempl_file name of the template file
#' @param pvec_templ_text template text as a vector
#' @param pdfstudent_db dataframe with student information
#'
#' @examples  
#' Given, we have a dataframe with information about users, store it in a file
#' sdata_dir <- "data"
#' sfinal_userpasswd_file <- paste(format(Sys.time(), "%Y%m%d%H%M%S"), "lbghs2017_student_db.csv", sep = "_")
#' write.csv2(dfusers_passwd, file = file.path(sdata_dir,sfinal_userpasswd_file), row.names = FALSE, quote = FALSE)
#' dfstudent_db <- read.csv2(file = file.path(sdata_dir, sfinal_userpasswd_file), stringsAsFactors = FALSE)
#' passwd_templ_file <- "passwd_email_templ.txt"
#' generate_user_email(pstempl_file  = file.path(sdata_dir, passwd_templ_file),
#'                     pdfstudent_db = dfstudent_db,
#'                     psdata_dir = "data")
generate_user_email <- function(pstempl_file = NULL,
                                pvec_templ_text = NULL,
                                pdfstudent_db,
                                psdata_dir){
  ### # read template email text either from file or take it 
  ### #  from function argument
  if (is.null(pvec_templ_text)){
    if (is.null(pstempl_file))
      stop("ERROR: generate_user_email either needs a template file or template vector")
    conemail_templ_file <- file(pstempl_file)
    vec_templ_text <- readLines(con = conemail_templ_file)
    close(conemail_templ_file)
  } else {
    vec_templ_text <- pvec_templ_text
  }
  
  ### # loop over rows in pdfstudent_db and generate 
  ### # email messages
  ncol_dfstudent_db <- ncol(pdfstudent_db)
  for (nstudent_db_row_idx in 1:nrow(pdfstudent_db)){
    vecemail_result <- vec_templ_text
    for (idx in 1:ncol_dfstudent_db){
      spat <- paste0("[", names(pdfstudent_db)[idx], "]", collapse = "")
      srep <- pdfstudent_db[nstudent_db_row_idx,idx]
      cat("pattern: ", spat, "\n")
      cat("repl: ", srep, "\n")
      vecemail_result <- gsub(pattern = spat, 
                              replacement = srep, 
                              x = vecemail_result, 
                              fixed = TRUE)
    }
    cat(paste0(vecemail_result, collapse = "\n"), 
        file = file.path(psdata_dir, 
                         paste(format(Sys.time(), "%Y%m%d%H%M%S"), 
                               pdfstudent_db[nstudent_db_row_idx, "Username"],
                               sep = "_")))
    
  }
  return(invisible(TRUE))
}


#' Distribute content among students
#' 
# test_bash_cmd <- 'for student in "vrohrp"
# cp_bash_cmd <- 'for student in "ernstt" "martinfe" "dpisoni" "weberan" "wysss";
# do 
# echo $student;
# sudo cp -R project/GitHub/LBGHS2017 ../${student};
# sudo chown -R ${student}:${student} ../${student};
# sleep 2;
# done'
# system(cp_bash_cmd)

# alternative using git

test_bash_cmd <- 'PROJDIR=LBGHS2017
ADMIN=`whoami`
CURWD=/home/${ADMIN}
echo " * Project dir: $PROJDIR"
echo " * Current working dir: $CURWD"
for student in "vrohrp"
do 
echo $student
cd /home/${student}
sudo mv ${PROJDIR} ${PROJDIR}.${student}
sudo git clone -b r4tea-platform https://github.com/charlotte-ngs/LBGHS2017.git
sudo cp ${PROJDIR}.${student}/ex/w1/UmfrageAntworten.Rmd ${PROJDIR}/ex/w1
sudo chown -R ${student}:${student} .
sudo mv ${PROJDIR}.${student} ${CURWD}/${PROJDIR}.${student}
cd ${CURWD}
sudo chown -R ${ADMIN}:${ADMIN} ${PROJDIR}.${student}
sleep 2
done'

cp_bash_cmd <- 'PROJDIR=LBGHS2017
ADMIN=`whoami`
CURWD=/home/${ADMIN}
echo " * Project dir: $PROJDIR"
echo " * Current working dir: $CURWD"
for student in "ernstt" "martinfe" "dpisoni" "weberan" "wysss"
do 
echo $student
cd /home/${student}
sudo mv ${PROJDIR} ${PROJDIR}.${student}
sudo git clone -b r4tea-platform https://github.com/charlotte-ngs/LBGHS2017.git
sudo cp ${PROJDIR}.${student}/ex/w1/UmfrageAntworten.Rmd ${PROJDIR}/ex/w1
sudo chown -R ${student}:${student} .
sudo mv ${PROJDIR}.${student} ${CURWD}/${PROJDIR}.${student}
cd ${CURWD}
sudo chown -R ${ADMIN}:${ADMIN} ${PROJDIR}.${student}
sleep 2
done'

clone_bash_cmd <- 'PROJDIR=LBGHS2017
ADMIN=`whoami`
CURWD=/home/${ADMIN}
echo " * Project dir: $PROJDIR"
echo " * Current working dir: $CURWD"
for student in "ulmanns"
do 
echo $student
cd /home/${student}
sudo git clone -b r4tea-platform https://github.com/charlotte-ngs/LBGHS2017.git
sudo chown -R ${student}:${student} .
cd ${CURWD}
sleep 2
done'


test_pull_bash_cmd <- 'COPATH=ex/w2/lbg_hs_2017_w2_sol1.Rmd
COPATH=ex/w5/lbg_hs_2017_w5_ans4.Rmd
PROJDIR=LBGHS2017
ADMIN=`whoami`
CURWD=/home/${ADMIN}
echo " * Project dir: $PROJDIR"
echo " * Current working dir: $CURWD"
for student in "vrohrp"
do 
echo $student
cd /home/${student}/${PROJDIR}
#sudo git pull
sudo git fetch
sudo git checkout origin/r4tea-platform ${COPATH}
sudo chown -R ${student}:${student} .
cd ${CURWD}
sleep 2
done'

pull_bash_cmd <- 'PROJDIR=LBGHS2017
ADMIN=`whoami`
CURWD=/home/${ADMIN}
echo " * Project dir: $PROJDIR"
echo " * Current working dir: $CURWD"
for student in "vrohrp" "ernstt" "martinfe" "dpisoni" "weberan" "wysss" "ulmanns"
do 
echo $student
cd /home/${student}/${PROJDIR}
sudo git pull
sudo chown -R ${student}:${student} .
cd ${CURWD}
sleep 2
done'

fetch_bash_cmd <- 'COPATH=ex/w5/lbg_hs_2017_w5_ans4.Rmd
COPATH=ex/w2/lbg_hs_2017_w2_sol1.Rmd

ex/w6/lbg_hs_2017_w6_sol5.Rmd
ex/w9/lbg_hs_2017_w9_ans8.Rmd

PROJDIR=LBGHS2017
ADMIN=`whoami`
CURWD=/home/${ADMIN}
echo " * Project dir: $PROJDIR"
echo " * Current working dir: $CURWD"
#for COPATH in "ex/w6/lbg_hs_2017_w6_sol5.Rmd" "ex/w9/lbg_hs_2017_w9_ans8.Rmd"
#for COPATH in "ex/w7/lbg_hs_2017_w7_sol6.Rmd" "ex/w10/lbg_hs_2017_w10_ans9.Rmd"
for COPATH in "ex/w7/lbg_hs_2017_w7_sol6.Rmd"
do
echo $COPATH
for student in "vrohrp" "ernstt" "martinfe" "dpisoni" "weberan" "wysss" "ulmanns"
#for student in "vrohrp"
do 
echo $student
cd /home/${student}/${PROJDIR}
sudo git fetch
sudo git checkout origin/r4tea-platform ${COPATH}
sudo chown -R ${student}:${student} .
cd ${CURWD}
sleep 2
done
done
'
