#'Comment split
#'
#'Divide a tibble into a list of tibbles by comments. If you dermarcate your runs with comments, this will divide your data into runs.
#'@param df The data you wish to divide, containing a Comments column
#'@name comSplit
#'@export

comSplit <- function(df){
  indices <- which(!is.na(df$Comments))
  mylist <- list()
  if(indices[1]>1){
    mylist[[1]] <- df[1:(indices[1]-1)]
  }
  for(i in 1:(length(indices)-1)){
    mylist[[length(mylist)+1]] <- df[indices[i]:(indices[i+1]-1),]
  }
  mylist[[length(mylist)+1]] <-  df[dplyr::last(indices):nrow(df),]
  return(mylist)
}
