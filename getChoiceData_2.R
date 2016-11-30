### Update Choice
setwd("D:/code/PTA_dataPlant")
library(lubridate) ## as.Date
options(stringsAsFactors = FALSE)
Day <- Sys.Date()
if(file.exists("UpdateData/Choice_PTA_hangye.csv")) file.rename("UpdateData/Choice_PTA_hangye.csv", paste("UpdateData/Choice_PTA_hangye",Day,".csv",sep=""))
if(file.exists("UpdateData/Choice_PTA_shangpin.csv")) file.rename("UpdateData/Choice_PTA_shangpin.csv", paste("UpdateData/Choice_PTA_shangpin",Day,".csv",sep=""))

readcsv_choice <- function(filename){
        
        oneTable <- as.matrix(read.csv(filename,skip=0,strip.white=TRUE))
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
        
        oneTable
}

oneT1 <- readcsv_choice(paste("UpdateData/Choice_PTA_hangye",Day,".csv",sep=""))
oneT2 <- readcsv_choice(paste("UpdateData/Choice_PTA_shangpin",Day,".csv",sep=""))
oneT1 <- oneT1[oneT1[,1] <= Day, ]
oneT2 <- oneT2[oneT2[,1] <= Day, ]

dd <- c(as.Date(oneT1[,1]),as.Date(oneT2[,1]))
dd <- dd[!duplicated(dd)]
unionDs <- dd[order(dd)]
n.col <- ncol(oneT1)+ncol(oneT2)-1
oneTable <- matrix(NA,length(unionDs),n.col)
tmpcol <- c(colnames(oneT1),colnames(oneT2)[-1])
tmpcol <- gsub(":","",tmpcol)
tmpcol <- gsub("\\.","",tmpcol)
tmpcol <- gsub("\\(","",tmpcol)
tmpcol <- gsub("\\)","",tmpcol)

for(i in 1:length(tmpcol)){
        n <- nchar(tmpcol[i])
        if(n>10) tmpcol[i] <- paste(substr(tmpcol[i],1,5),substr(tmpcol[i],(n-4),n),sep="")
}
tmpcol <- paste(tmpcol,1:length(tmpcol),sep="")
colnames(oneTable) <- tmpcol

oneTable[,1] <- as.character(unionDs)
oneTable[match(as.character(as.Date(oneT1[,1])),oneTable[,1]), 2:ncol(oneT1)] <- oneT1[,-1]
oneTable[match(as.character(as.Date(oneT2[,1])),oneTable[,1]), (ncol(oneT1)+1):n.col] <- oneT2[,-1]
write.csv(oneTable,file="UpdateData/ChoiceRest113Oneday.csv",quote=FALSE,row.names = FALSE)



tmp <- read.csv("UpdateData/ChoiceRest113Oneday.csv",header = FALSE)
colnames(tmp) <- 1:ncol(tmp)


#### write into Mysql database
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
dbRemoveTable(con,"hengyi_pta_choicedatav2")
dbWriteTable(con, "hengyi_pta_choicedatav2",  as.data.frame(tmp), row.names=FALSE)
dbDisconnect(con)
