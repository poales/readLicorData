#' Merge two licor datasets based on time
#'
#' Given two licor datasets, will correct elapsed column based on differences in the hhmmss time.
#' @param df1 The first li-cor data set, from readLicorData::licorData
#' @param df2 The second li-cor data set, from readLicorData::licorData
#' @name licorMerge
#' @export
licorMerge <- function(df1, df2){
  dfRet <- NA
  end <- df1[nrow(df1),licorFinder(df1,"hhmmss")]
  start <- df2[1,licorFinder(df2,"hhmmss")]
  end <- strptime(as.character(end[[1]]),format="%H:%M:%S")
  start <- strptime(as.character(start[[1]]),format="%H:%M:%S")
  diff <- as.numeric(difftime(start[[1]],end[[1]],units="s"))
  df2$elapsed <- df2$elapsed - df2$elapsed[1] + df1$elapsed[nrow(df1)] + diff
  dfRet <- tryCatch(expr = {
    dplyr::bind_rows(df1,df2)
  },error = function(e){
    df1 <- df1[,-c(which(grepl(pattern = "hhmmss",x = colnames(df1))==TRUE))]
    df2 <- df2[,-c(which(grepl(pattern = "hhmmss",x = colnames(df2))==TRUE))]
    dplyr::bind_rows(df1,df2)
  },finally = {
    print("Removed hhmmss columns to allow merging dataframes")
  })
  return(dfRet)
}
