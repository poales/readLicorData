#' Read in data from the li-6800
#'
#' This function accepts a licor data file location (xlsx or no-extension (tsv)), and returns a tibble of all the data without the chaff
#' @param location the path to the data file
#' @param returnImportant Causes the function to return a list of the full data tibble and another tibble with just a few hand-picked variables - useful if all you want is to do an A/Ci curve
#' @param purgeComments Removes comments from the file recommended to leave this TRUE - will still work with FALSE but there will be issues
#' @param makeConstCol turns S and Oxygen constants into a column. Currently doesn't work with .xlsx files
#' @name licorData
#' @export

licorData <- function(location, returnImportant = F, purgeComments = T, makeConstCol = F, makeCommentsCol=T){
  excel <- regexpr(".xlsx$",location)>=0
  if(excel){
    suppressMessages(data <- readxl::read_excel(path = location,sheet = 1,col_names = F))
    maxCols <- length(data)
    data <- tibble::as_tibble(data)
    makeConstCol <- F #makeconstcol currently does not function with excel imports

  } else {
    maxCols <- max(count.fields(file=location,sep = "\t",quote = "")) #This logic is needed to get around the long, narrow header at the top of the TSV
    data <- read.table(location,sep="\t",quote="",dec=".",stringsAsFactors = FALSE,blank.lines.skip = FALSE,skipNul = FALSE,comment.char="",row.names = NULL,col.names = 1:maxCols)
    if(is.na(data[1,length(data)])){  #Terminal \t at the end of every line
      data <- data[,-length(data)]
      maxCols <- maxCols -1
    }
    data <- tibble::as_tibble(data)
  }
  if(makeConstCol){ #Store in where and what changes (sysconsts)
    oxylocs <- dfPullConst(df=data,label = "SysConst:Oxygen")
    slocs <- dfPullConst(df=data,label = "Const:S")
  }
  counter <- 0 #track how many lines of text we delete for adjustments to the sysconst columns later
  tester <- F
  while (!tester){ #this while loop eliminates all the non-data at the top of the page.
    if(is.na(data[1,maxCols]) | data[1,maxCols]==""){
      data <- data[-1,]
      counter <- counter+1
    } else {
      tester <- T
      data <- data[-1,] #delete the top row, which is not useful to us (category)
      counter <- counter+1
    }
  }

  colnames(data)<-as.character(unlist(data[1,]))
  data<- data[-1,]
  data<- data[-1,] #delete header and unit lines
  counter <- counter+2
  data <- tibble::set_tidy_names(data,quiet = T) #there are like 5 columns all called "time." this renames them to time..2,time..3 etc
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
      if(data[counter,maxCols]==""){ #this is how we know it's a comment (only has data in the first two columns)
        if(makeCommentsCol){#we actually want to save the comment and put it in a new column
          comlocs <- rbind(comlocs,counter-1)
          comtimes <- rbind(comtimes,data[counter,1])
          coms <- rbind(coms,data[counter,2])
        }
        data <- data[-counter,] #remove the row with the comment
      }else
      {
        counter <- counter+1
      }
    }

    if(!is.null(comlocs)){
      commentdf <- data.frame("hhmmss" = comtimes, "Comments" = coms,stringsAsFactors = F)
      colnames(commentdf) <- c("hhmmss", "Comments")
      colnames(data)[grep("hhmmss",colnames(data))[1]]<- "hhmmss" #rename first instance of hhmmss to just hhmmss for sorting
      data <- tibble::add_column(data,"Comments"=NA,.before=2)
      counter <- length(comlocs)
      while(counter>= 1){ #we have to loop backwards in order to get all the positioning right
        data <- tibble::add_row(data,Comments = commentdf[counter,2],hhmmss=commentdf[counter,1],.after = comlocs[counter])
        counter <- counter-1
      }
    }
    suppressMessages(data <- readr::type_convert(data)) #removed comments means former char cols can be made into numbers
  }
  if(makeCommentsCol & excel){
    colnames(data)[grep("hhmmss",colnames(data))[1]]<- "hhmmss" #rename first instance of hhmmss for sorting
    data2 <- suppressMessages(readxl::read_excel(path = location,sheet = 2,col_names = F)) #in the xlsx comments are stored on page 2
    commentlocs <- grep(pattern = "^[\\d]{2}",unlist(data2[,1]),perl=T) #only the comments have got numbers at the front of them on page 2
    comments <- unlist(unname(data2[,2][commentlocs,]))
    data <- tibble::add_column(data,"Comments"=NA,.before=2)
    comdf <- data.frame("Comments" = comments,"hhmmss" = unlist(data2[,1])[commentlocs],stringsAsFactors = F)
    data <- dplyr::bind_rows(data,comdf)
    data <- data[order(as.numeric(strptime(data$hhmmss,format="%H:%M:%S"))),] #strptime converts the hhmmss numerics into a sortable time

  }

  if(returnImportant){
    important_data <- dplyr::select(data,c("CO2_r_sp", "A", "Ci", "gsw", "elapsed","CO2_r","CO2_s","Qin","ETR"))
    #this while loop eliminates comments and non-data.
    counter <- 1
    while(counter < length(important_data$CO2_r_sp)){
      if(is.na(important_data$CO2_r_sp[counter])){
        important_data <- important_data[-counter,]
      }else
      {
        counter <- counter+1
      }
    }

    #if you don't do this you end up with a bunch of points where ETR is 0, not NA - graphs very poorly
    for(i in 1:length(important_data$ETR)){
      if(is.na(important_data$ETR[i])){
        important_data$ETR[i]<- ""
      }
    }
    important_data <- dplyr::mutate_at(important_data,vars(1:8),as.numeric)
    return(important_data)
  } else{
    return(data)
  }
}
