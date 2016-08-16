### Update Choice
setwd("D:/code/PTA_dataPlant")
options(stringsAsFactors = FALSE)
Day <- Sys.Date() # '2016-08-15' #

if(file.exists("UpdateData/Choice_PTA.csv")) file.rename("UpdateData/Choice_PTA.csv", paste("UpdateData/Choice_PTA",Day,".csv",sep=""))
if(file.exists("UpdateData/Choice_PTA.xls")) file.remove("UpdateData/Choice_PTA.xls")

library(lubridate) ## as.Date
oneTable <- as.matrix(read.csv(paste("UpdateData/Choice_PTA",Day,".csv",sep=""),skip=0,strip.white=TRUE))
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

#choiceDay <- oneTable[as.Date(oneTable[,1])==Day, ,drop=FALSE] #!!!!!!! Day or Day-1
write.csv(oneTable,file="UpdateData/ChoicetmpOneday.csv",quote=FALSE,row.names = FALSE)

tmp <- read.csv("UpdateData/ChoicetmpOneday.csv",header = FALSE)
colnames(tmp) <- 1:ncol(tmp)


#### write into Mysql database
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
#dbSendQuery(con,'SET NAMES GBK')
dbRemoveTable(con,"hengyi_pta_choicedata")
dbWriteTable(con, "hengyi_pta_choicedata",  as.data.frame(tmp), row.names=FALSE)
#dbWriteTable(con, "hengyi_pta_choicedata", as.data.frame(tmp), append=TRUE,row.names=FALSE, overwrite=FALSE,col.names=FALSE)
dbDisconnect(con)

