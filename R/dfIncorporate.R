#' Incorporate a pulled constant into the dataframe
#' 
#' @param df The dataframe to incorporate into
#' @param newDat The new dataframe to add in, with "value"s and "row"s
#' @param label The name of the new column
#' @name dfIncorporate

dfIncorporate <- function(df, newDat, label){
  #Create a vector of the initialization value
  dat <- rep(newDat$value[1],nrow(df))
  #If the pulled constant never changes, we do not need to incorporate the changes.
  if(length(newDat$value)>1){
    #loop through all the new values of the constant.
    for(i in 2:length(newDat$value)){
      #each time, set the value of everything beneath - so, iteratively change the "bottom" part of the vector
      dat[newDat$row[i]:length(dat)] <- newDat$value[i]
      #the other approach, setting the value for only newDat$row[i]:newDat$row[i+1],
      #works if you leave the loop before the last one and execute code similar to above.
      #but this is pretty parsimonious
    }
  }
  return(
    #label is a variable, must unquote it. := is necessary for unquoted "assignments"
    tibble::add_column(df, !!(label):= dat)
  )
}