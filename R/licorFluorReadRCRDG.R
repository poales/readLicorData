#' A trivial function to read in the RCRDG Li-cor traces
#'
#' Only works for traces - called "RCRDG"
#' This data is presented tidy, while single flashes are given true json formatting
#' @param location location of the file
#' @name licorFluorReadRCRDG
#' @export
licorFluorReadRCRDG <- function(location) {
  #like the gas exchange data, it automatically adds a blank column to the other end.
  dat <- readr::read_delim(file = location,delim = "\t")
  return(tibble::add_column(dplyr::select(dat,1:9),run="FLUOR"))
}
