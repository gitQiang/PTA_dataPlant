setwd("E:/HQ/PTA_dataPlant")
options(stringsAsFactors = FALSE)
# library(WindR)
# w.start()
# w_edb_data<-w.edb('S0031529,S0070127,S0031714,S0031715,S0031716,S0031712,S0028007,S0028008,S0027203,S0027204,S5414121,S0027187,S0027188','2002-01-01','2016-07-27','')
# newData <- w_edb_data$Data
# write.csv(newData,file=paste("E:/HQ/PTA_dataPlant/OldData/WindOld2016-07-27.csv",sep=""),row.names=FALSE,quote=FALSE)
# w.stop()

windDic <- read.csv("UpdateData/WindDictionary.csv")
tmp <- read.csv("OldData/WindOld2016-07-27.csv",header=FALSE)
tmp[1, 2:ncol(tmp)] <- windDic[match(tmp[1,2:ncol(tmp)],names(windDic))]


library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
#Tnames <- dbListTables(con)
#dbRemoveTable(con,"hengyi_pta_winddata")
dbWriteTable(con, "hengyi_pta_winddata", as.data.frame(tmp),row.names=FALSE)
dbDisconnect(con)
