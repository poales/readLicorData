#' Read licor single flash data jsons
#' a package, rjson, reads the json data
#' @param location The filepath of the json data
#' @name licorFlashRead
#' @export
licorFlashRead <- function(location) {
  #require(tidyverse)
  #require(magrittr)
  #require(rjson)
  #like the gas exchange data, it automatically adds a blank column to the other end.
  a <- rjson::fromJSON(file=location)[c("CODE","SECS","FLUOR","DC","PFD","REDMODAVG")] 
  b <- tibble::bind_cols(a)
  return(tibble::add_column(b,run="FLUOR"))
  #I add a column called "run" where every entry is "FLUOR".  This is useful for if you want to plot fluorescence
  #and gas exchange in the same plot, on different facets using ggplot2::facet_wrap()
}
