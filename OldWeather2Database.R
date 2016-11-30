# ##### read and clean weather data
path <-  "D:/data/ºãÒİ/data/ZSHC-Weather"
filenames <- list.files(path,pattern = ".txt",full.names = TRUE)
Day <- Sys.Date()

### filename is consisted with the time order
allWea  <- c()
for(i in 1:length(filenames)){
        tmp <- read.csv(filenames[i])
        allWea <- rbind(allWea,tmp)
}
write.csv(allWea,file=paste(path,"/","Weather2002_2016",Day,".csv",sep=""),row.names = FALSE,quote = FALSE)
write.csv(allWea,file=paste("D:/code/PTA_dataPlant/OldData","/","Weather",Day,".csv",sep=""),row.names = FALSE,quote = FALSE)

#### write into Mysql database
tmp <- allWea
tmp <- rbind(colnames(allWea),tmp)


library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
#Tnames <- dbListTables(con)
dbRemoveTable(con,"hengyi_pta_weatherdata")
dbWriteTable(con, "hengyi_pta_weatherdata", as.data.frame(tmp),row.names=FALSE)
dbDisconnect(con)
