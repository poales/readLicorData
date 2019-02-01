#' A trivial function to read in the RCRDG Li-cor traces
#'
#' Only works for traces - called "RCRDG"
#' This data is presented tidy, while single flashes are given true json formatting
#' @param location location of the file
#' @name licorFluorReadRCRDG
#' @export
licorFluorReadRCRDG <- function(location) {
  #require(tidyverse)
  #require(magrittr)
  #like the gas exchange data, it automatically adds a blank column to the other end.
  dat <- readr::read_delim(file = location,delim = "\t")
  return(tibble::add_column(dplyr::select(dat,1:9),run="FLUOR"))
  #I add a column called "run" where every entry is "FLUOR".  This is useful for if you want to plot fluorescence
  #and gas exchange in the same plot, on different facets using ggplot.facet_wrap()
}
