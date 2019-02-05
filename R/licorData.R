#' Read in data from the li-6800
#'
#' This function accepts a licor data file location (xlsx or no-extension (tsv)), and returns a tibble of all the data without the chaff
#' @param location the path to the data file
#' @param returnImportant Causes the function to return a list of the full data tibble and another tibble with just a few hand-picked variables - useful if all you want is to do an A/Ci curve
#' @param purgeComments Removes comments from the file recommended to leave this TRUE - will still work with FALSE but there will be issues
#' @param makeConstCol turns S and Oxygen constants into a column
#' @name licorData
#' @export

licorData <- function(location, returnImportant = F, purgeComments = T, makeConstCol = T, makeCommentsCol=T){
  #require(tidyverse)
  #require(stringr)
  #require(magrittr)
  #determine if its an xlsx...
  excel <- regexpr(".xlsx$",location)>=0
  if(excel){  #NOT PREFERED
    #paste("This function only works on the no-extension tsv, not on xlsx files (R does not know how to read in formulas that include \"IF\" statements)")
    #return()
    suppressMessages(data <- readxl::read_excel(path = location,sheet = 1,col_names = F))
    maxCols <- length(data)
    data <- tibble::as_tibble(data)
    #makeconstcol currently does not function with excel imports
    makeConstCol <- F

  } else {
    maxCols <- max(count.fields(file=location,sep = "\t",quote = ""))
    data <- read.table(location,sep="\t",quote="",dec=".",stringsAsFactors = FALSE,blank.lines.skip = FALSE,skipNul = FALSE,comment.char="",row.names = NULL,col.names = 1:maxCols)
    if(is.na(data[1,length(data)])){
      data <- data[,-length(data)]
      maxCols <- maxCols -1
    }
    data <- tibble::as_tibble(data)
  }
  #the maxCols variable is the number of variables, which we use when reading in the data

  #this if statement deletes a blank column at the end
  #by default, the licor data tsv has a \t at the end of every line, even though there is no data afterwards
  #so this creates a blank column


  #first find where the sysconst lines are for creating columns for constant changes

  #store in the variables which change
  if(makeConstCol){
    oxylocs <- dfPullConst(df=data,label = "SysConst:Oxygen")
    slocs <- dfPullConst(df=data,label = "Const:S")
  }

  #this while loop eliminates all the non-data at the top of the page.
  counter <- 0
  #the purpose of the counter variable is to track how many lines of text we delete
  #for adjustments to the sysconst columns later
  tester <- F

  while (!tester){
    if(is.na(data[1,maxCols]) | data[1,maxCols]==""){
      data <- data[-1,]
      counter <- counter+1
    } else {
      tester <- T
      data <- data[-1,]
      counter <- counter+1
    }
  }

  colnames(data)<-as.character(unlist(data[1,])) #turn the header line into column names
  data<- data[-1,]
  #delete the header line (it's saved as colnames) then delete the units line
  data<- data[-1,]
  data <- tibble::set_tidy_names(data,quiet = T) #there are like 5 columns all called "time." this renames them to time..2,time..3 etc
  counter <- counter+2 #increment counter for the header and unit lines
  #use counter var to adjust the "row" level on the slocs and oxylocs
  if(makeConstCol){
    slocs$row <- slocs$row - counter
    oxylocs$row <- oxylocs$row - counter
    if(nrow(oxylocs)>0){
      data <- dfIncorporate(df = data, newDat = oxylocs, label = "Oxygen")
    }
    if(nrow(slocs)>0){
      data <- dfIncorporate(df = data, newDat = slocs, label = "Leaf Area")
    }
  }


  if(purgeComments & !excel){
    #remove comments from the dataframe if requested (default TRUE)
    counter <- 1
    comlocs <- c()
    coms <- c()
    comtimes<- c()
    while(counter < length(data$obs)+1){
      if(data[counter,maxCols]==""){ #this is how we know it's a comment
        #all comments are stored in the first 2cells
        if(makeCommentsCol){
          comlocs <- rbind(comlocs,counter-1)
          comtimes <- rbind(comtimes,data[counter,1])
          #we actually want to save the comment and put it in a new column
          coms <- rbind(coms,data[counter,2])
        }
        data <- data[-counter,] #remove the row with the comment
        #deleting the line essentially advances the counter, so we don't want to advance it again
      }else
      {
        counter <- counter+1
      }
    }

    #if there are no comments, you get errors with setting names and zero index for loops
    if(!is.null(comlocs)){
      commentdf <- data.frame("hhmmss" = comtimes, "Comments" = coms,stringsAsFactors = F)
      commentdf <- magrittr::set_colnames(commentdf, c("hhmmss", "Comments")) #it tries to preserve the name for whatever reason...
      colnames(data)[grep("hhmmss",colnames(data))[1]]<- "hhmmss" #rename first instance of hhmmss to just hhmmss for sorting
      data <- tibble::add_column(data,"Comments"=NA,.before=2)
      counter <- length(comlocs)
      while(counter>= 1){ #we have to loop backwards in order to get all the positioning right
        data <- tibble::add_row(data,Comments = commentdf[counter,2],hhmmss=commentdf[counter,1],.after = comlocs[counter])
        counter <- counter-1
      }

    }
    suppressMessages(data <- readr::type_convert(data))

  }
  if(makeCommentsCol & excel){
    colnames(data)[grep("hhmmss",colnames(data))[1]]<- "hhmmss" #rename first instance of hhmmss to just hhmmss for sorting
    data2 <- readxl::read_excel(path = location,sheet = 2,col_names = F) #on the xlsx comments are stored on page 2
    commentlocs <- grep(pattern = "[0:9]{2}",data2$..1) #only the comments have got numbers at the front of them on page 2
    #that seems questionable - not 100% sure this is the case
    comments <- data2$..2[commentlocs]
    data <- tibble::add_column(data,"Comments"=NA,.before=2)
    comdf <- data.frame("Comments" = comments,"hhmmss" = data2$..1[commentlocs],stringsAsFactors = F)
    data <- dplyr::bind_rows(data,comdf)

    #sort by time... not a perfect solution, if they're taken in the afternoon and yous tarted in the morning they will be positioned wrong
    #need to fix this at some point
    data <- data[order(as.numeric(strptime(data$hhmmss,format="%H:%M:%S"))),] #strptime converts the hhmmss numerics into a sortable time

  }

  if(returnImportant){
    important_data <- dplyr::select(data,c("CO2_r_sp", "A", "Ci", "gsw", "elapsed","CO2_r","CO2_s","Qin","CO2_%","ETR"))
    #this while loop eliminates comments and non-data.
    counter <- 1
    while(counter < length(important_data$CO2_r_sp)){
      if(is.na(important_data$CO2_r_sp[counter])){
        #deleting the line essentially advances the counter, so we don't want to advance it again
        important_data <- important_data[-counter,]
      }else
      {
        counter <- counter+1
      }
    }

    #if you don't do this you end up with a bunch of points where ETR is 0, not NA
    #I have no idea why this is.  All I know is it works.
    for(i in 1:length(important_data$ETR)){
      if(is.na(important_data$ETR[i])){
        important_data$ETR[i]<- ""
      }
    }
    important_data <-  dplyr::mutate_at(important_data,vars(1:10),as.numeric)
    important_data$ETR <- important_data$ETR/4
    return(list(data,important_data))
  } else{
    return(data)
  }
}
