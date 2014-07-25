## Download file from

##  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",UCI_Har_Dataset.zip, method="curl")
## unzip("UCI_Har_Dataset.zip")

## Environment setup

wdir <- getwd()
#newdir <- setwd(paste(wdir,"/","UCI\ Har\ Dataset", sep=""))
setwd("./UCI Har Dataset")
newdir <- getwd()
print(as.character(newdir))

## URL Data Source
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

UCI_dir_files <- list.files(newdir)
UCI_dir_files

## Read Features features.txt
features <- read.table(paste(newdir, "/", UCI_dir_files[2], sep = ""))
features <- as.vector(features$V2)

## Get the Labels activity_labels.txt
labels <- read.table(paste(newdir, "/", UCI_dir_files[2], sep = ""))
labels <- as.vector(labels$V2)

## mergedata() function
mergedata <- function(datafile) {
  ## Set datafile directory
  setwd(paste(newdir, "/", datafile, sep = ""))
  
  ## List files in Working Directory
  Datafiles <- list.files(getwd())
  X <- Datafiles[3]
  Y <- Datafiles[4]
  Z <- Datafiles[2]
  library(plyr)
  ## Extracting data files and merging data
  Data <- do.call("cbind", lapply(c(Z, Y, Y, X), function(dn) data.frame(read.table(dn))))
  Activity <- Data[, 3]
  ## Linking Activities to Activity_labels
  for (i in Activity) {
    Activity[Activity == i] = labels[i]
  }
  Data[, 3] <- Activity
  ## Blocks column to identify subject groupings
  Blocks <- rep(paste(datafile, "set", sep = ""), nrow(Data))
  Data <- cbind(data.frame(Blocks), Data)
  Data
}

##mergeUCIdata
mergeUCIdata <- function(Data1, Data2) {
  Data1 <- mergedata(Data1)
  Data2 <- mergedata(Data2)
  UCIDATA <- rbind(Data1, Data2)
  cols <- c("Blocks", "Subject", "Activity_label", "Activity", features)
  colnames(UCIDATA) <- cols
  UCIDATA
}

UCIDATA <- mergeUCIdata("test", "train")

##Dimension of Test Data
Test_data <- mergedata("test")
dim(Test_data)

## Dimension of Train Data
Train_data <- mergedata("train")
dim(Train_data)

##Merged Dataset Dimension UCI
dim(UCIDATA)

## pattern to match
pattern <- c("mean", "std")

## match algorithm
matches <- unique(grep(paste(pattern, collapse = "|"), colnames(UCIDATA), value = TRUE))
length(matches)

## Get Mean and Standard Deviation
Mean_Std <- UCIDATA[, matches]

## Dimension of Mean and SD
dim(Mean_Std)

## Mean Variables and SD
names(Mean_Std)

## Split subset by 30 Level Factor List
G <- split(UCIDATA[, 5:565], list(UCIDATA$Activity, UCIDATA$Subject))
names(G)

## Store Summary
Summarydata <- sapply(G, colMeans)

## Dimension Summary data
dim(Summarydata)

##Write summary uncomment or execute this to your console
#write.csv(Summarydata,"Summarydata.csv")

## Restore directory
setwd(wdir)