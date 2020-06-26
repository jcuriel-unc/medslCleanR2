#' Identify the names that are similar and likely matches
#' 
#' This function takes the long datasets for other election information and computes the Herfindahl index measure of competition so that 
#' one can compare it to the raw precinct results 
#' @param df The long dataframe with the election results of interest
#' @param name_field The name of the character/string field of interest that should be matched with similar strings 
#' @param group_fields The name of the field that the name field should be subsetted by in order to constrain which names are compared against
#' each other (i.e. county, office, dataverse, etc. )  
#' @param max_distance The 0 - 1 Levenshtein distance that is used to identify matches, where values closer to 0 are more similar, and 1 
#' completely different. The default value is 0.1, the recommended 
#' @return The list with matched names that are scored as being similar
#' }
#' 
#' @export
#' @examples 
#' house_all <- read.csv("2016-precinct-house.csv")
#' name_match <- name_cleaner(house_all, "candidate", "county", max_distance=0.05) )
#' 
name_cleaner <- function(df, name_field, group_fields, max_distance = 0.1){
  df <- as.data.frame(df)
  name_position <- match(name_field, names(df))
  group_positions <- match(group_fields, names(df))
  cols_ext <- c(name_position,group_positions)
  group_list1 <- list()  
  match_list <- list()
  ###we will want to group by the fields 
  df2 <- df %>% group_by_at(vars(cols_ext)) %>% dplyr::summarise()
  df2 <- as.data.frame(df2)
  group_positions2 <- match(group_fields, names(df2))
  name_position2 <- match(name_field, names(df2))
  for(j in 1:length(group_positions)){
    group_list1[[j]] <- sort(unique(df[,group_positions[j]] ))
    for(t in 1:length(group_list1[[j]])){
      temp2 <- df2[df2[,group_positions2[j]]==group_list1[[j]][t], ] 
      name_vec <- sort(unique(temp2[, name_position2] ))
      name_vec = name_vec[name_vec!="YES"]
      name_vec = name_vec[name_vec!="NO"]
      name_vec = name_vec[name_vec!="FOR"]
      for(u in 1:length(name_vec)){
        temp_store1 <-  name_vec[u]
        temp_store2 <- name_vec[-u]
        for(v in 1:length(temp_store2)){
          temp_results <- agrep(temp_store1,temp_store2[v], max.distance = max_distance)
          if(length(temp_results)>0){
            #print(temp_store1)
            #print(temp_store2[j])
            match_list[[length(match_list)+1]] <- c(temp_store1,temp_store2[v])
          }
        }
        
      }
    }
  }
  return(match_list)
}