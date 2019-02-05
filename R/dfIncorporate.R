#' Incorporate a pulled constant into the dataframe
#' 
#' @param df The dataframe to incorporate into
#' @param newDat The new dataframe to add in, with "value"s and "row"s
#' @param label The name of the new column
#' @name dfIncorporate

dfIncorporate <- function(df, newDat, label){
  dat <- rep(newDat$value[1],length(df[,1]))
  if(length(newDat$value)>1){
    #for loop increments over the indices with changed oxygen values and inserts them into the dataframe
    for(i in 2:length(oxynewDat$value)){
      dat[newDat$row[i]:length(dat)] <- newDat$value[i]
    }
  }
  return(
    tibble::add_column(df, !!(label):= dat)
  )
}