library(lubridate) ## as.Date

Day <- Sys.Date()

oneTable <- read.csv("D:/code/PTA_dataPlant/OldData/Choice_PTA2016-07-29.csv",skip=0,strip.white=TRUE)
oneTable <- oneTable[seq(-1,-3,-1), ]
### repalce special chars
oneTable[,1] <- gsub("Äê","-",oneTable[,1])
oneTable[,1] <- gsub("ÔÂ","-",oneTable[,1])
oneTable[,1] <- gsub("ÈÕ","",oneTable[,1])
oneTable[,1] <- gsub("/","-",oneTable[,1])
for(j in 2:ncol(oneTable)) oneTable[,j] <- gsub(",","",oneTable[,j])

### date order and format
date <- as.Date(oneTable[,1])
oneTable[,1] <- paste(year(date),month(date),day(date),sep="-")
oneTable <- oneTable[order(date),  ]
write.csv(oneTable, file=paste("OldData/ChoiceData",Day,".csv",sep=""),row.names=FALSE,quote=FALSE)

#### choice old data

choiceData <- read.csv("D:/code/PTA_dataPlant/OldData/ChoiceData2016-07-29.csv")
choiceData[,1] <- as.character(as.Date(choiceData[,1]))

#### merge data 
Day <- '2016-07-28'
validDays <- read.csv("D:/code/PTA_dataPlant/OldData/ValidDays2002_2016.csv")[,1]
validDays <- validDays[1:which(validDays==Day)]

OldData <- matrix(,length(validDays),0)
OldData <- cbind(OldData,choiceData[match(validDays, choiceData[,1]),  ])

### delete repeat Time columns
OldData[, 1] <- validDays 

### delete NA columns, delete constant columns
tmpsubs1 <- sapply(1:ncol(OldData), function(i) !all(is.na(OldData[,i])) )
tmpsubs2 <- sapply(1:ncol(OldData), function(i) sd( as.numeric( OldData[!is.na(OldData[,i]),i] ) ) > 0 )
tmpsubs2[1] <- TRUE
tmpsubs2[is.na(tmpsubs2)] <- FALSE
OldData <- OldData[, tmpsubs1 & tmpsubs2]

OldData <- rbind(colnames(OldData),OldData)
colnames(OldData) <- 1:ncol(OldData)
rownames(OldData) <- 1:nrow(OldData)
OldData <- as.data.frame(OldData)

#### write into Mysql database
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
dbRemoveTable(con,"hengyi_pta_choicedata")
dbWriteTable(con, "hengyi_pta_choicedata", OldData)
dbDisconnect(con)
