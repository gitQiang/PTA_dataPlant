library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
OldData <- dbReadTable(con,"hengyi_pta_data")

Day <- as.character(Sys.Date())
oneDay <- as.matrix(read.csv(paste("UpdateData/UpdateData",Day,".csv",sep="")))
colnames(tmp) <- 1:ncol(OldData) #colnames(OldData)
tmp <- matrix(oneDay[1,unlist(OldData[1,])],nrow=1)

dbWriteTable(con, "hengyi_pta_data", as.data.frame(tmp), append=TRUE,row.names=FALSE, overwrite=FALSE)

dbDisconnect(con)
