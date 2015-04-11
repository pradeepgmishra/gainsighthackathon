library(lubridate)
library(jsonlite)
library(dplyr)
library(tidyr)

path <-  "/Users/pradeepmishra/GAINSIGHT/Prob1/json_format/json/"
cormethod = "kendall"

features <- c("rooms_rt","value_rt","location_rt","frontdesk_rt","service_rt","buss_service_rt","clean_rt")

positive = F
negative = F
single <- F

getsampleids <- function(noofsamples){
  #file <-read.table(paste(path,"fileids.csv",sep=""),header=F)  
  ids <- as.integer(readLines(file(paste(path,"fileids.csv",sep=""))))  
  sampleids <- sample(ids, noofsamples)  
}
corByHotels <- function(noofsamples){
  
  corDF <- NA
  
  for(id in getsampleids(noofsamples)){
    if(is.na(corDF)){
      corDF <- ratingCor(hotelid=id,NULL)
    } else {
      corDF <- rbind(corDF,ratingCor(id,NULL))
    }    
  }
  corDF
}

sampleAcrossHotelData <- function(noofsamples){
  dataDF <- NA
  
  for(id in getsampleids(noofsamples)){
        
    if(is.na(dataDF)){
      try(dataDF <- readData(id))
      if(positive){
        try(dataDF <- dataDF[which(dataDF$overall_rt %in% c(4,5)),])
      }
      if(negative){
        try(dataDF <- dataDF[which(dataDF$overall_rt %in% c(1,2)),])
      }
      if(nrow(dataDF) == 0){
        dataDF <- NA
      }
    } else {
      try(dataDFTmp <- readData(id),T)
      if(positive){
        try(dataDFTmp <- dataDFTmp[which(dataDFTmp$overall_rt %in% c(4,5)),])
      }
      if(negative){
        try(dataDFTmp <- dataDFTmp[which(dataDFTmp$overall_rt %in% c(1,2)),])
      }
      if(nrow(dataDFTmp) > 0){
        dataDF <- rbind(dataDF,dataDFTmp)
      }      
    }    
  }
  dataDF
}


readDataMany <- function(listofids){
  dataDF <- NA
  
  for(id in listofids){
    
    if(is.na(dataDF)){
      try(dataDF <- readData(id))
      if(positive){
        try(dataDF <- dataDF[which(dataDF$overall_rt %in% c(4,5)),])
      }
      if(negative){
        try(dataDF <- dataDF[which(dataDF$overall_rt %in% c(1,2)),])
      }
      if(nrow(dataDF) == 0){
        dataDF <- NA
      }
    } else {
      try(dataDFTmp <- readData(id),T)
      if(positive){
        try(dataDFTmp <- dataDFTmp[which(dataDFTmp$overall_rt %in% c(4,5)),])
      }
      if(negative){
        try(dataDFTmp <- dataDFTmp[which(dataDFTmp$overall_rt %in% c(1,2)),])
      }
      if(nrow(dataDFTmp) > 0){
        dataDF <- rbind(dataDF,dataDFTmp)
      }      
    }    
  }
  dataDF
}

corAcrossHotels <- function(noofsamples){  
  ratingCor(NULL,data <- sampleAcrossHotelData(noofsamples))
}

ratingCor <- function(hotelid,data){
  if(is.na(data) || is.null(data)){
    data <- readData(hotelid)  
  }  
  
  if(nrow(data) < 2){
   
   service <- NA
   buss_service <- NA
   clean <- NA
   location <- NA
   frontdesk <- NA
   value <- NA
   rooms <- NA
   
  } else {
  
  #print(head(data))  
  service        <- if(is.null(data$service_rt) || sum(!is.na(data$service_rt)) == 0) { NA } else cor(data$overall_rt,data$service_rt,use="complete",method=cormethod)
  buss_service   <- if(is.null(data$buss_service_rt) || sum(!is.na(data$buss_service_rt)) == 0) { NA } else cor(data$overall_rt,data$buss_service_rt,use="complete",method=cormethod)
  clean          <- if(is.null(data$clean_rt) || sum(!is.na(data$clean_rt)) == 0) { NA } else cor(data$overall_rt,data$clean_rt,use="complete",method=cormethod)
  location       <- if(is.null(data$location_rt) || sum(!is.na(data$location_rt)) == 0) { NA } else cor(data$overall_rt,data$location_rt,use="complete",method=cormethod)
  frontdesk      <- if(is.null(data$frontdesk_rt) || sum(!is.na(data$frontdesk_rt)) == 0) { NA } else cor(data$overall_rt,data$frontdesk_rt,use="complete",method=cormethod)
  value          <- if(is.null(data$value_rt) || sum(!is.na(data$value_rt)) == 0) { NA } else cor(data$overall_rt,data$value_rt,use="complete",method=cormethod)
  rooms          <- if(is.null(data$rooms_rt) || sum(!is.na(data$rooms_rt)) == 0) { NA } else cor(data$overall_rt,data$rooms_rt,use="complete",method=cormethod)
  
  }
    if(is.null(hotelid)){
    corDF <- data.frame(data$hotel[1],buss_service,clean ,frontdesk ,value,rooms ,location )  
  } else {
    corDF <- data.frame(hotelid,service,buss_service,clean ,frontdesk ,value,rooms ,location )
  }
  
}
readData <- function(hotelid){
  file = paste(path,hotelid,".json",sep="")
  #print(file)
  tmpdata <- fromJSON(file)  
  reviews <- as.data.frame(tmpdata)
  ratings <- as.data.frame(reviews$Reviews.Ratings)
  
  #print(reviews[1,])
  reviews <- reviews[,c("Reviews.Date","Reviews.ReviewID","Reviews.Author","HotelInfo.HotelID")]    
  reviews <- tbl_df(cbind(reviews,ratings))
  coln <- names(reviews)
  
  reviews <- rename(reviews, hotel=HotelInfo.HotelID,date=Reviews.Date,
                    reviewid = Reviews.ReviewID,
                    author = Reviews.Author)
  
  if("Service" %in% coln){ reviews <- rename(reviews, service_rt = Service) }                    
  if("Cleanliness" %in% coln){ reviews <- rename(reviews, clean_rt = Cleanliness) }                    
  if("Overall" %in% coln){ reviews <- rename(reviews, overall_rt = Overall) }
  if("Value" %in% coln){ reviews <- rename(reviews, value_rt = Value) }
  if("Rooms" %in% coln){ reviews <- rename(reviews, rooms_rt = Rooms) }
  if("Location" %in% coln){ reviews <- rename(reviews, location_rt = Location) }
  
                    
  cols <- colnames(reviews)
  #print(cols)
  for(n in 1:length(cols)){
    #print(substring(cols[n],1,8))
    if(substring(cols[n],1,8)=="Business"){
      cols[n] <- "buss_service_rt"
    }
    if(substring(cols[n],1,5)=="Check"){
      cols[n] <- "frontdesk_rt"
    }
  }
  colnames(reviews) <- cols
  
  for(name in cols){
    col <- reviews[,name]
    col[col==-1] <- NA    
  
    if(substring(name,nchar(name)-2) == "_rt"){      
      col <- as.numeric(unlist(col))      
    }    
    reviews[,name] <- col
  }
  
  rows = nrow(reviews)
  
  hotel = if(length(reviews$hotel)==0){ rep(NA,rows) } else reviews$hotel
  date=if(length(reviews$date)==0){ rep(NA,rows) } else reviews$date
  reviewid = if(length(reviews$reviewid)==0){ rep(NA,rows) } else reviews$reviewid
  author = if(length(reviews$author)==0){ rep(NA,rows) } else reviews$author
  buss_service_rt = if(length(reviews$buss_service_rt)==0){ rep(NA,rows) } else reviews$buss_service_rt                    
  frontdesk_rt = if(length(reviews$frontdesk_rt)==0){ rep(NA,rows) } else reviews$frontdesk_rt                    
  service_rt = if(length(reviews$service_rt)==0){ rep(NA,rows) } else reviews$service_rt                    
  clean_rt = if(length(reviews$clean_rt)==0){ rep(NA,rows) } else reviews$clean_rt             
  overall_rt = if(length(reviews$overall_rt)==0){ rep(NA,rows) } else reviews$overall_rt
  value_rt = if(length(reviews$value_rt)==0){ rep(NA,rows) } else reviews$value_rt
  rooms_rt = if(length(reviews$rooms_rt)==0){ rep(NA,rows) } else reviews$rooms_rt
  location_rt = if(length(reviews$location_rt)==0){ rep(NA,rows) } else reviews$location_rt
  
  reviewsNew <- data.frame(hotel,
                           date,
                           reviewid,
                           author,
                           service_rt, 
                           buss_service_rt,
                           frontdesk_rt,
                           clean_rt,                    
                           overall_rt,
                           value_rt,
                           rooms_rt,
                           location_rt)
  reviewsNew
}

lmmultipleregression <- function(data,multiple){
  df = data.frame(a=c(TRUE,FALSE))
  fet = c();
  comb <- combn(1:length(features),multiple)
  
  #print(comb)

  for(n in 1:ncol(comb)){  
    #print(comb[,n])
    newfeature = "";      
    for(a in comb[,n]){
      if(newfeature == ""){
        newfeature = features[a]
      } else {
        newfeature = paste(newfeature,features[a],sep=" + ")  
      }      
    }
    fet=c(fet,newfeature)          
  }
  #print(fet)      
  
  df <- data.frame(fet)
  slope = c()  
  for(feature in fet){        
    try(slope <- c(slope,coef(lm(paste("overall_rt ~ ",feature,sep=""),data=data))[2]),T)     
  }
  
  if(single){
    df <- data.frame(slope)
  } else {
    df <- cbind(df,slope)
  }
  
  #print(df)
  df <- df[order(df$slope),]
  
}

lmregression <- function(data){
  df = data.frame(features)
  slope = c()
  for(feature in features){
    slope = c(slope,coef(lm(paste("overall_rt ~ ",feature,sep=""),data=data))[2])
  }
  df <- cbind(df,slope)  
}
getLinearRegressionByHotel <- function(id){
  data=readData(id)
  lmregression(data)
}
getMultipleRegressionByHotel <- function(id,multiple){
  single <<- T
  data=readData(id)
  lmmultipleregression(data,multiple)
}

getLinearRegressionAcrossHotels <- function(noofsamples){
  data=sampleAcrossHotelData(noofsamples)  
  lmregression(data)
}
getMultipleRegressionAcrossHotels <- function(noofsamples,multiple){
  data=sampleAcrossHotelData(noofsamples)
  lmmultipleregression(data,multiple)
}

explore <- function(id){
  if(is.null(id)){
    id <- getsampleids(1)
  }
  df <- readData(id)
  pairs(df)
}

exploreSample <- function(noofsamples){  
  df <- sampleAcrossHotelData(noofsamples)  
  pairs(df[,-c(1:3)])
}

writeGraph <- function(){
  ids <- as.integer(readLines(file(paste(path,"fileids.csv",sep=""))))  
  n <- 1;
  first <- T
  while(n < length(ids)){
    #print(ids[n])
    dataDF <- NA
    try(dataDF <- readData(ids[n]),T)
    #print(head(dataDF))    
    try(FF <- ratingCor(NULL,dataDF),T);
    #print(head(FF))
    if(first){
      try(write.table(FF, file = "corbyhotels.csv", sep = ",", 
                      col.names = TRUE, append=TRUE),T)
      first <- F
    }else{
      try(write.table(FF, file = "corbyhotels.csv", sep = ",", 
                      col.names = FALSE, append=TRUE),T)
    }
    
    n <- n+1;
  }
  
}

writeGraph2 <- function(){
  ids <- as.integer(readLines(file(paste(path,"fileids.csv",sep=""))))  
  n <- 1;
  first <- T
  while(n < length(ids)){
    #print(ids[n])
    dataDF <- NA
    #try(dataDF <- readData(ids[n]),T)
    #print(head(dataDF))    
    try(FF <- getLinearRegressionByHotel(ids[n]));
    #print(head(FF))
    if(first){
      try(write.table(FF, file = "regbyhotels.csv", sep = ",", 
                      col.names = TRUE, append=TRUE),T)
      first <- F
    }else{
      try(write.table(FF, file = "regbyhotels.csv", sep = ",", 
                      col.names = FALSE, append=TRUE),T)
    }
    
    n <- n+1;
  }
  
}
