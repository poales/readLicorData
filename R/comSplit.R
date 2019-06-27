#'Comment split
#'
#'Divide a tibble into a list of tibbles by comments. If you dermarcate your runs with comments, this will divide your data into runs.
#'@param df The data you wish to divide, containing a Comments column
#'@examples licorData(loc,makeCommentsCol=T) %>% comSplit()
#'@name comSplit
#'@export

comSplit <- function(df){
  indices <- which(!is.na(df$Comments))
  mylist <- list()
  for(i in 1:(length(indices)-1)){
    mylist[[i]] <- df[indices[i]:(indices[i+1]-1),]
  }
  mylist[[length(indices)]] <-  df[indices[length(indices)]:nrow(df),]
  return(mylist)
}