#' Read licor single flash data jsons
#' a package, rjson, reads the json data
#' @param location The filepath of the json data
#' @name licorFlashRead
#' @export
licorFlashRead <- function(location) {
  #like the gas exchange data, it automatically adds a blank column to the other end.
  a <- rjson::fromJSON(file=location)[c("CODE","SECS","FLUOR","DC","PFD","REDMODAVG")]
  b <- dplyr::bind_cols(a)
  return(tibble::add_column(b,run="FLUOR"))
}
