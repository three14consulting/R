# Overview ----------------------------------------------------------------
# This script takes an array of searchIDs from SpareRoom.co.uk as inputs and
# outputs a boxplot of all the results returned from each searchID. Since the 
# results are rarely normally distributed, median values of "PCM Rent (£)" 
# are taken and returned in the array "medians"
#
# The input can be found by making a search on SpareRoom.co.uk and taking the
# numbers right of the "search_id=" term in the URL.

# Start the clock! --------------------------------------------------------

ptm <- proc.time()


# Library Packages --------------------------------------------------------

library(rvest)
library(boot)
library(plyr)
library(miscTools)

# Initializations ---------------------------------------------------------

searchArray <- list("1045519243","1045529875")
k = 1


# Web Scrape --------------------------------------------------------------

while (k <= length(searchArray))
{
  #Initialize index pageno
  pageno = 1
  
  #Fecth URL
  pagehtml <- read_html(paste0("http://www.spareroom.co.uk/flatshare/?offset=",
                                (pageno-1)*10,"&search_id=",searchArray[k],
                                "&sort_by=age&mode=list"))
  
  
  #Fetch searchName
  searchName <- pagehtml %>% 
    html_element(css = '#mainheader > div:nth-child(2) > h1') %>% 
    html_text()
  
  
  # How many listings
  no_results <- (pagehtml %>% 
    html_element(xpath = '//*[@id="results_header"]') %>% 
    html_elements("strong") %>%
    html_text2())[2] %>%
    as.numeric()
  
  # If the no_results > 1000 then no_results scrape returns "NA_real_" to resolve this:
  if(is.na(no_results) == TRUE){
    no_results = 1000
  }
  
  
  # Create empty dataframe as placeholder
  data <- data.frame(X=as.character((raw(length = no_results))), stringsAsFactors=FALSE)
  
  
  # How many listings in multiples of 10
  y <- ceiling(no_results / 10)
  remainder <- no_results %% 10
  
  
  #Populate dataframe
  while(pageno <= y){
    pagehtml <- read_html(paste0("http://www.spareroom.co.uk/flatshare/?offset=",
                                  (pageno - 1)*10,"&search_id=",searchArray[k],
                                  "&sort_by=age&mode=list"))
    
    vector <- pagehtml %>%
      html_nodes("article:nth-child(1) strong.listingPrice") %>%
      html_text()
    n <- length(vector)
    
    if(pageno == y){
      data[(((pageno - 1)*10)+1):((pageno-1)*10+remainder),1] <- c(vector[seq(n) %% 2 == 1])
    }
    else{
      data[(((pageno - 1)*10)+1):(pageno*10),1] <- c(vector[seq(n) %% 2 == 1])
    }
    pageno <- pageno +1
  }
  
  
  #Update no of results - THis is needed since data can update while program is running
  no_results <- (pagehtml %>% 
                   html_element(xpath = '//*[@id="results_header"]') %>% 
                   html_elements("strong") %>%
                   html_text2())[2] %>%
                   as.numeric()
  
  # If the no_results > 1000 then no_results scrape returns "NA_real_" to resolve this:
  if(is.na(no_results) == TRUE){
    no_results = 1000
  }
  
  
  #Preserve raw data
  data$RAW <- data$X
  
  
  #Clean dataframe by removing first two characters
  data$X <- gsub("£","",data$X)
  
  
  #Place the per calendar month (pcm) & per weekly (pw) identifier into a separate column
  data$Ident <- rep(0, length(data$X))
  data$Ident[grep("pw",data$X)] <- "pw"
  data$Ident[grep("pcm",data$X)] <- "pcm"
  
  
  #Scrub indentifier from data
  data$X <- gsub("pcm","",data$X)
  data$X <- gsub("pw","",data$X)
  
  
  #If there exists listings that have a range of values then...
  indices <- grep("-",data$X)
  for (z in indices) {
    
  }
  if(length(grep("-",data$X))>0){
    
    # Find all those listings that specify a range of values
    # and take the median of the min and max
    temp <- data.frame(do.call('rbind', strsplit(as.character(data$X),'-',fixed=TRUE)))
    data$X1 <- temp$X1
    data$X2 <- temp$X2
    data$X <- NULL
    rm(temp)
    
    
    #remove trailing spaces in preparation to convert strings to numerics
    data$X1 <- gsub(" ","", data$X1)
    data$X1 <- gsub(",","", data$X1)
    data$X2 <- gsub(" ","", data$X2)
    data$X2 <- gsub(",","", data$X2)
    
    
    #Convert to numeric
    data$X1 <- as.numeric(data$X1)
    data$X2 <- as.numeric(data$X2)
    
    
    #Median
    data$X <- (data$X1 + data$X2)/2
    
    
    #remove dummy columns
    data$X1 <- NULL
    data$X2 <- NULL
  }
    
  #convert pw to pcm
  i = 1
  while(i <= length(data$X)){
    if(data$Ident[i]=="pw"){
      data$X[i] = data$X[i] * (52/12)
      data$Ident[i] = "pcm"
    }
    i = i+1
  }
  
  # Remove columns
  data$RAW <- NULL
  data$Ident <- NULL
  
  
  
  # Add change column name in dataframe to reflect search
  colnames(data) <- paste0(searchName)
  
  
  # Add data to a list for future aggregating
  searchArray[k] <- list(data)
  
  
  #Remove unecessary variables and update indices
  rm(i,y, no_results, searchName, pagehtml)
  k = k+1
}


# Aggregating Data --------------------------------------------------------
{
  #Combine data into 1 data set
  combinedData <- as.data.frame(do.call(rbind.fill, searchArray))
  
  
  #Sort by ascending column median
  ColumnIndex <- colMedians(combinedData, na.rm = TRUE)
  SortedColIndex <- order(ColumnIndex)
  combinedData <- combinedData[,SortedColIndex]
  
  
  #Return data frame of medians of newly sorted combined dataframe
  medians <- data.frame("Median PCM Rent" = colMedians(combinedData, na.rm = TRUE), 
                        row.names = names(combinedData))
  
  #Add total array column for mean and median lines to use in boxplot
  totalArry <- as.data.frame(stack(combinedData)[,1])
  totalArry <- totalArry[is.na(totalArry)==FALSE]
  
  
  # n total number of results from queries
  n <- as.numeric(length(totalArry))
  
  
  #Boxplot
  boxplot(combinedData, ylab = "Rent PCM (£)")
  # Add median line in blue
  abline(h=median(totalArry), col="blue")
  # Add mean line in red
  abline(h=mean(totalArry), col="red")
  
  print(summary(combinedData))
}



# Remove Trash ------------------------------------------------------------
rm(ColumnIndex,data,k,no_results,searchName,SortedColIndex)



# Stop the clock! ---------------------------------------------------------
proc.time() - ptm