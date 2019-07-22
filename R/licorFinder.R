#' Pick the first column that matches what we want
#'
#' Given a dataframe and a column name, find the first column index that contains the chosen name.
#' @param df A dataframe.
#' @param colChoose The column we want to find
#' @name licorFinder

licorFinder <- function(df,colChoose){
  found <- first(which(grepl(pattern = colChoose,x = colnames(df))==TRUE))
  if(is.na(found)){
    cat("The chosen column name was not found in df.\n")
    return(NA)
  }
  else return(found)
}
