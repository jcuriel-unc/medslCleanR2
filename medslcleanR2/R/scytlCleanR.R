#' Compile and clean the elections data from Scytl websites
#' 
#' This function takes the wide excel datasets downloaded from Scytl within a working directory and transforms them into long dataframes
#' by county, with temporary data saved into a secondary working directory. 
#' @param file_list1 The vector of file names within the working directory. Can easily be accomplished with the list.files(wd1) cmd
#' @param wd1 The string of the working directory where the scytl data are saved as excel files by county 
#' @param wd2 The string of the working directory where the long precinct results by county will be saved 
#' @param xlsx True or false statement as to whether the excel files are in xlsx documents. 
#' @return files saved into the secondary working directory 
#' }
#' 
#' @export
#' @examples 
#' ky_wd1 <- "F:/MEDSL/2018-precincts/KY/scytl"
#' ky_wd2 <- "F:/MEDSL/2018-precincts/KY/cleaned_data"
#' ky_scytl_list <- list.files(ky_wd1)
#' scytl_cleaner(ky_scytl_list,ky_wd1,ky_wd2, xlsx = TRUE)

scytl_cleaner <- function(file_list1,wd1,wd2,xlsx=TRUE){
  list.of.packages <- c("stringr","stringi","rlang","readxl","tidyr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(rlang)
  for(pkg in (list.of.packages)){
    eval(bquote(library(.(pkg))))
  }
  for(j in 1:length(file_list1)){
    svMisc::progress(j/length(file_list1))
    setwd(wd1) #setting the working directory to the raw scytl files 
    toc <- readxl::read_excel(file_list1[j], skip=4 ) # read in the toc and find out the number of sheets 
    toc_race <- readxl::read_excel(file_list1[j], skip=3 ) # want to get the name of the race next 
    toc_race <- toc_race$Contest # can pull in lower via u 
    last_sheet <- max(toc[,1]) #find the last sheet number 
    df_precincts <- data.frame(stringsAsFactors = FALSE) # set the df to store the results, which will store a county's data, save it,
    #and then remove 
    state_temp_df <- data.frame(stringsAsFactors = FALSE)
    if(xlsx==TRUE){
      ttt <- str_remove(file_list1[j], ".xlsx")
    }else{
      ttt <- str_remove(file_list1[j], ".xls")
    }
    #df_precincts <- data.frame(stringsAsFactors = FALSE)
    for(u in 2:last_sheet){ # will want to change the starting point as necessary, though it seems to be the case that most scytl sites start
      # with the name of the first returns as "2", even though it is technically the third sheet 
      sh_name <- as.character(u) # converts u to a character, in which it will then read in the sheet by name, not position
      cand_names <- readxl::read_excel(file_list1[j], skip=1, sheet = sh_name ) # skips the header
      cand_names <- as.vector(colnames(cand_names))
      # the goal of these cand names arguments is to pull out the number of candidates. This shall in turn tell the loop how many times to 
      #go through a single sheet so as to pull the data 
      cand_names <- gsub("[^a-zA-Z]", "", cand_names)
      cand_names <- na.omit(cand_names)
      cand_names <- unique(na.omit(unlist(strsplit(unlist(cand_names), "[^a-zA-Z]+"))))
      cand_names <- gsub('([[:upper:]])', ' \\1', cand_names)
      cand_names <- trimws(cand_names) # now have the candidate names. The next thing we want to do is get the cols
      #cand_names
      df_temp <- readxl::read_excel(file_list1[j], skip=2, sheet = sh_name ) #now will read in the actual data 
      #try(rm(y,z))
      print(str_remove(file_list1[j], ".xlsx")) # will inform the county presently on
      for(i in 1:length(cand_names)){
        #note: the issues seems to be that there are different numbers of modes per county. Therefore, I'll want to find the length of the 
        #modes, and then use the length to modify the range of y and z 
        #the temp_colnames will tell how many modes there are, which will be used to grab the data 
        temp_colnames <- colnames(df_temp) # grab the name. Then get rid of first two cols 
        temp_end <- length(temp_colnames) - 1
        temp_colnames <- temp_colnames[3:temp_end]
        temp_colnames <- gsub("[^[:alnum:] ]", "", temp_colnames)
        temp_colnames <- gsub('[[:digit:]]+', "", temp_colnames)
        temp_colnames <- sort(unique(temp_colnames))
        #temp_colnames # good. Now we will want to incorporate such info 
        #print(i)
        {if(i == 1){
          y <- 3
          z <- y+ (length(temp_colnames)-1)
        }else if(i > 1 ){
          y <- z + 1
          z <- y+(length(temp_colnames)-1)} }
        #print(y)
        #print(z)
        df_temp2 <- df_temp[,c(1,y:z)]
        df_temp3 <- tidyr::gather(df_temp2, key="mode",value="votes", 2:ncol(df_temp2))
        df_temp3$mode <- gsub("[^[:alnum:] ]", "", df_temp3$mode)
        df_temp3$mode <- gsub('[[:digit:]]+', '', df_temp3$mode)
        df_temp3$candidate <- cand_names[i]
        df_temp3$jurisdiction <- ttt
        df_temp3$race <- toc_race[u]
        #print(u/last_sheet)
        colnames(df_temp3)[1] <- "precinct"
        {if(nrow(state_temp_df)==0){
          state_temp_df <- df_temp3
        }else{
          state_temp_df <- rbind(state_temp_df, df_temp3)
        } }
        colnames(state_temp_df)[1] <- "precinct"
      }
      #print("Moving to second loop")
      {if(nrow(df_precincts)==0){
        df_precincts <- state_temp_df
      }else{
        df_precincts <- rbind(df_precincts, state_temp_df)
      } } 
    }
    ###saving the data 
    temp_name1 <- paste0(ttt,sep="_", "precincts",sep=".","Rdata")
    setwd(wd2)
    saveRDS(df_precincts, temp_name1)
    rm(df_precincts,state_temp_df)
  }
}
