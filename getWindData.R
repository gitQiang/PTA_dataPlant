
library(WindR)
w.start()
day <- as.character(Sys.Date())
w_edb_data<-w.edb('S0031529,S0070127,S0031714,S0031715,S0031716,S0031712,S0028007,S0028008,S0027203,S0027204,S5414121,S0027187,S0027188','2002-01-01',day,'')
newData <- w_edb_data$Data #[w_edb_data$Data[,1]== day, ,drop=FALSE]
write.csv(newData,file=paste("E:/HQ/PTA_dataPlant/UpdateData/Wind",day,".csv",sep=""),row.names=FALSE,quote=FALSE)
w.stop()

options(stringsAsFactors = FALSE)
windDic <- read.csv("E:/HQ/PTA_dataPlant/UpdateData/WindDictionary.csv")
tmp <- read.csv(paste("E:/HQ/PTA_dataPlant/UpdateData/Wind",day,".csv",sep=""),header = FALSE)
tmp[1, 2:ncol(tmp)] <- windDic[match(tmp[1,2:ncol(tmp)],names(windDic))]
#colnames(tmp) <- paste("V",1:ncol(tmp),sep="")
colnames(tmp) <- 1:ncol(tmp)

## write to mySQL database
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
dbRemoveTable(con,"hengyi_pta_winddata")
dbWriteTable(con, "hengyi_pta_winddata", as.data.frame(tmp),row.names=FALSE)
#dbWriteTable(con, "hengyi_pta_winddata", as.data.frame(tmp), append=TRUE,row.names=FALSE, overwrite=FALSE)
dbDisconnect(con)
