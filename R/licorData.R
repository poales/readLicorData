#' Read in data from the li-6800
#'
#' This function accepts a licor data file location (xlsx or no-extension (tsv)), and returns a tibble of all the data without the chaff
#' @param location the path to the data file
#' @param purgeComments Removes comments from the file recommended to leave this TRUE - will still work with FALSE but there will be issues
#' @param makeConstCol turns S and Oxygen constants into a column. Currently doesn't work with .xlsx files
#' @param xlsxIndex which sheet to read on an xlsx
#' @name licorData
#' @export

licorData <- function(location, purgeComments = T, makeConstCol = F, makeCommentsCol=T,xlsxIndex=1){
  excel <- regexpr(".xlsx$",location)>=0
  if(excel){
    suppressMessages(data <- readxl::read_excel(path = location,sheet = xlsxIndex,col_names = F))
    maxCols <- ncol(data)
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
  #cat("start of sysconst check ",Sys.time(),"\n")
  if(makeConstCol){ #Store in where and what changes (sysconsts)
    oxylocs <- dfPullConst(df=data,label = "SysConst:Oxygen")
    slocs <- dfPullConst(df=data,label = "Const:S")
  }
  #cat("end of sysconst check ",Sys.time(),"\n")
  counter <- 0 #track how many lines of text we delete for adjustments to the sysconst columns later
  tester <- F
  #cat("start of header elim ",Sys.time(),"\n")
  #what follows is the old method of header elimination, but since it recreates the data multiple times it's a huge issue.
  
  # while (!tester){ #this while loop eliminates all the non-data at the top of the page.
  #   #I am going to use maxCols-2 just to eliminate issues with a really stupid bug I found once.
  #   #the bug is on LICOR's side, but we can work around it to an extent here.
  #   if(is.na(data[1,maxCols-2]) | data[1,maxCols-2]==""){
  #     data <- data[-1,]
  #     counter <- counter+1
  #   } else {
  #     tester <- T
  #     data <- data[-1,] #delete the top row, which is not useful to us (category)
  #     counter <- counter+1
  #   }
  #   if(nrow(data) == 0) tester <- T
  # }
  rnum <- first(which(!(is.na(data[,maxCols-10]) | data[,maxCols-10]=="")))
  if(!is.na(rnum)){
    data <- data[-c(1:rnum),] #also deletes the category row
    counter <- counter+rnum
  } else{
    stop("There is no data here.")
  }
  #cat("end of header elim ",Sys.time(),"\n")
  # if(nrow(data) <3){
  #   stop("There is no data here.")
  # }
  colnames(data)<-as.character(unlist(data[1,]))
  data<- data[-1,]
  data<- data[-1,] #delete header and unit lines
  counter <- counter+2
  data <- tibble::set_tidy_names(data,quiet = T) #there are like 5 columns all called "time." this renames them to time..2,time..3 etc
  #cat("start of incorporating constant changes ",Sys.time(),"\n")
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

  #cat("start of comment removal ",Sys.time(),"\n")
  if(purgeComments & !excel){
    #remove comments from the dataframe if requested (default TRUE)
    counter <- 1
    comlocs <- c()
    coms <- c()
    comtimes<- c()
    #I bet we can speed this up by vectorizing
    comlocs <- which(data[,maxCols]=="")-1
    if(makeCommentsCol & length(comlocs)!=0){
      comtimes <- data[comlocs+1,1]
      coms <- data[comlocs+1,2]
    }
    
    # while(counter < length(data$obs)+1){
    #   if(data[counter,maxCols]==""){ #this is how we know it's a comment (only has data in the first two columns)
    #     if(makeCommentsCol){#we actually want to save the comment and put it in a new column
    #       comlocs <- rbind(comlocs,counter-1)
    #       comtimes <- rbind(comtimes,data[counter,1])
    #       coms <- rbind(coms,data[counter,2])
    #     }
    #      #remove the row with the comment
    #   }
    #   counter <- counter+1
    # }
    if(length(comlocs)!=0){
      data <- data[-c(comlocs+1),]
    }
    

    if(length(comlocs)!=0 & makeCommentsCol){
      commentdf <- data.frame("hhmmss" = comtimes, "Comments" = coms,stringsAsFactors = F)
      colnames(commentdf) <- c("hhmmss", "Comments")
      colnames(data)[grep("hhmmss",colnames(data))[1]]<- "hhmmss" #rename first instance of hhmmss to just hhmmss for sorting
      data <- tibble::add_column(data,"Comments"=NA,.before=2)
      counter <- 1
      while(counter<= length(comlocs)){ 
        data <- tibble::add_row(data,Comments = commentdf[counter,2],hhmmss=commentdf[counter,1],.after = comlocs[counter])
        counter <- counter+1
      }
    }
    #suppressMessages(data <- readr::type_convert(data)) #removed comments means former char cols can be made into numbers
  }
  #cat("End of comment removal ",Sys.time(),"\n")
  
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
  
  if("ETR" %in% colnames(data)){
    for(i in 1:length(data$ETR)){
      if(is.na(data$ETR[i])){
        data$ETR[i]<- ""
      }
    }
  }
  suppressMessages(data <- readr::type_convert(data))
  return(data)
}
