
### write old data into Mysql database

# Day <- as.character(Sys.Date())
# 
# ##### read and clean weather data
# path <-  "D:/data/恒逸/data/ZSHC-Weather"
# filenames <- list.files(path,pattern = ".txt",full.names = TRUE)
# ### filename is consisted with the time order
# allWea  <- c()
# for(i in 1:length(filenames)){
#         tmp <- read.csv(filenames[i])
#         allWea <- rbind(allWea,tmp)
# }
# write.csv(allWea,file=paste(path,"/","Weather2002_2016",Day,".csv",sep=""),row.names = FALSE,quote = FALSE)
# write.csv(allWea,file=paste("D:/code/PTA_dataPlant/OldData","/","Weather2002_2016",Day,".csv",sep=""),row.names = FALSE,quote = FALSE)
# 
# 
# #### wind old data 
# library(WindR)
# w.start()
# w_edb_data<-w.edb('S0031529,S0070127,S0031714,S0031715,S0031716,S0031712,S0028007,S0028008,S0027203,S0027204,S5414121,S0027187,S0027188','2002-01-01','2016-07-13','')
# newData <- w_edb_data$Data
# write.csv(newData,file=paste("D:/code/PTA_dataPlant/OldData/WindOld.csv",sep=""),row.names=FALSE,quote=FALSE)
# w.stop()
# 
# windDic <- read.csv("UpdateData/WindDictionary.csv")
# WindTable <- read.csv(paste("OldData/WindOld.csv",sep=""))
# colnames(WindTable) <- c("Time", windDic[match(colnames(WindTable)[-1], colnames(windDic) )])
# write.csv(WindTable,file=paste("D:/code/PTA_dataPlant/OldData/WindOld",Day,".csv",sep=""),row.names=FALSE,quote=FALSE)
# 
# #### choice old data
# library(lubridate) ## as.Date
# oneTable <- read.csv("D:/code/PTA_dataPlant/OldData/Choice_PTA2016-07-14.csv",skip=0,strip.white=TRUE)
# oneTable <- oneTable[seq(-1,-3,-1), ]
# ### repalce special chars
# oneTable[,1] <- gsub("年","-",oneTable[,1])
# oneTable[,1] <- gsub("月","-",oneTable[,1])
# oneTable[,1] <- gsub("日","",oneTable[,1])
# oneTable[,1] <- gsub("/","-",oneTable[,1])
# for(j in 2:ncol(oneTable)) oneTable[,j] <- gsub(",","",oneTable[,j])
# 
# ### date order and format
# date <- as.Date(oneTable[,1])
# oneTable[,1] <- paste(year(date),month(date),day(date),sep="-")
# oneTable <- oneTable[order(date),  ]
# write.csv(oneTable, file=paste("OldData/ChoiceData",Day,".csv",sep=""),row.names=FALSE,quote=FALSE)



library(lubridate)
Weathers <- read.csv("D:/code/PTA_dataPlant/OldData/Weather2002_20162016-07-14.csv")
rownames(Weathers) <- as.character(as.Date(Weathers[,1]))
Weathers <- Weathers[, -1]

WindData <- read.csv("D:/code/PTA_dataPlant/OldData/WindOld2016-07-14.csv")
rownames(WindData) <- WindData[,1]
WindData <- WindData[, -1]

#### choice old data
## D:/code/PTA_dataPlant/OldData/Choice_PTA2016-07-14.csv
choiceData <- read.csv("D:/code/PTA_dataPlant/OldData/ChoiceData2016-07-14.csv")
choiceData[,1] <- as.character(as.Date(choiceData[,1]))

#### merge data 
Day <- '2016-07-13'
validDays <- read.csv("D:/code/PTA_dataPlant/OldData/ValidDays2002_2016.csv")[,1]
validDays <- validDays[1:which(validDays==Day)]

OldData <- matrix(,length(validDays),0)
OldData <- cbind(OldData,choiceData[match(validDays, choiceData[,1]),  ])
OldData <- cbind(OldData,Weathers[match(validDays, rownames(Weathers)),  ])
OldData <- cbind(OldData,WindData[match(validDays, rownames(WindData)),  ])
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
OldData <- as.data.frame(OldData)

#### write into Mysql database
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
#dbRemoveTable(con,"hengyi_pta_data")
dbWriteTable(con, "hengyi_pta_data", OldData)
dbDisconnect(con)
