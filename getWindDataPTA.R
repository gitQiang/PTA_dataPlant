options(stringsAsFactors = FALSE)
Ids <- unlist(read.table("E:/HQ/PTA_dataPlant/PTA-new/WindIndexsV2.txt"))
strid1 <- paste(Ids[1:100],sep="",collapse = ",")
strid2 <- paste(Ids[101:200],sep="",collapse = ",")
strid3 <- paste(Ids[201:300],sep="",collapse = ",")
strid4 <- paste(Ids[301:387],sep="",collapse = ",")
print("Got id strings")

library(WindR)
w.start()
print("Start WindR")
day <- as.character(Sys.Date())

w_edb_data<-w.edb(strid1,'2002-01-01',day,'')
newData1 <- w_edb_data$Data
write.csv(newData1,file=paste("E:/HQ/PTA_dataPlant/PTA-new/UpdateData/Wind",day,"1.csv",sep=""),row.names=FALSE,quote=FALSE)

w_edb_data<-w.edb(strid2,'2002-01-01',day,'')
newData2 <- w_edb_data$Data
write.csv(newData2,file=paste("E:/HQ/PTA_dataPlant/PTA-new/UpdateData/Wind",day,"2.csv",sep=""),row.names=FALSE,quote=FALSE)

w_edb_data<-w.edb(strid3,'2002-01-01',day,'')
newData3 <- w_edb_data$Data
write.csv(newData3,file=paste("E:/HQ/PTA_dataPlant/PTA-new/UpdateData/Wind",day,"3.csv",sep=""),row.names=FALSE,quote=FALSE)

w_edb_data<-w.edb(strid4,'2002-01-01',day,'')
newData4 <- w_edb_data$Data
write.csv(newData4,file=paste("E:/HQ/PTA_dataPlant/PTA-new/UpdateData/Wind",day,"4.csv",sep=""),row.names=FALSE,quote=FALSE)

print("Got Update Data")
w.stop()


print("Connected to database")
## write to mySQL database
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
print("Write to Database")

for(i in 1:4){
        tmp <- read.csv(paste("E:/HQ/PTA_dataPlant/PTA-new/UpdateData/Wind",day,i,".csv",sep=""))
        dbRemoveTable(con,paste("hengyi_pta_winddataV2_",i,sep=""))
        dbWriteTable(con, paste("hengyi_pta_winddataV2_",i,sep=""), as.data.frame(tmp),row.names=FALSE)
}

dbDisconnect(con)
print("Done")
