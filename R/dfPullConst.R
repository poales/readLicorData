#' Find where constants are changed, and gather those changes into a data frame to return.
#' 
#' @param df The dataframe to search through
#' @param label What to search the dataframe for
#' @param limit_search If TRUE, will limit search to only the first few columns (recommended for large datasets)
#' @name dfPullConst

dfPullConst <- function(df, label,limit_search=T){
  if(limit_search==TRUE){
    constlocs <- tibble::as_tibble(which(arr.ind=T,df[,1:4]==label))
  }else{
    constlocs <- tibble::as_tibble(which(arr.ind=T,df==label))
  }
  if(nrow(constlocs)>0){
    constlocs[,2]<- constlocs[,2]+1
    vals <- c()
    for(i in 1:nrow(constlocs)){
      id <- constlocs[i,]
      vals <- rbind(vals,df[id$row,id$col])
    }
    names(vals) <- "value"
    constlocs <- dplyr::bind_cols(constlocs,vals)
  }
  return(constlocs)
}