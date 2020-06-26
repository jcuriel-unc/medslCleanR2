#' Compile the scytl results by county into a a single long data frame 
#' 
#' This function takes the long by county precinct results from the scytl_cleaner function in a working directory, then binds them into a 
#' single long dataframe 
#' @param file_list1 The vector of file names within the working directory. Can easily be accomplished with the list.files(wd) cmd
#' @param wd1 The string of the working directory where the scytl_cleaner function saved the cleaned data 
#' @return The long dataframe of the compiled county level precinct data 
#' }
#' 
#' @export
#' @examples 
#' ky_wd1 <- "F:/MEDSL/2018-precincts/KY/scytl"
#' ky_wd2 <- "F:/MEDSL/2018-precincts/KY/cleaned_data"
#' ky_scytl_list <- list.files(ky_wd1)
#' scytl_cleaner(ky_scytl_list,ky_wd1,ky_wd2, xlsx = TRUE)
#' ky_cleaned_list <- list.files(ky_wd2)
#' ky_precincts_all <- scytl_compiler(ky_cleaned_list,ky_wd2)
#' 
scytl_compiler <- function(file_list,wd1){
  precincts_all <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(file_list)){
    setwd(wd1)
    temp_df <- readRDS(file_list[i])
    if(nrow(precincts_all)==0){
      precincts_all <- temp_df
    }else{
      precincts_all <- rbind(precincts_all,temp_df)
    }
  }
  return(precincts_all)
}