setwd("D:/code/PTA_dataPlant")
options(warn=-1)

# Weather columns
weaName <- c("CST", "Max.TemperatureC", "Mean.TemperatureC", "Min.TemperatureC", "Dew.PointC", "MeanDew.PointC", "Min.DewpointC", "Max.Humidity", "Mean.Humidity", "Min.Humidity", "Max.Sea.Level.PressurehPa", "Mean.Sea.Level.PressurehPa", "Min.Sea.Level.PressurehPa", "Max.VisibilityKm", "Mean.VisibilityKm", "Min.VisibilitykM", "Max.Wind.SpeedKm.h", "Mean.Wind.SpeedKm.h", "Max.Gust.SpeedKm.h", "Precipitationmm", "CloudCover", "Events", "WindDirDegrees")

Day <- as.character(Sys.Date())
wea <- read.csv(paste("D:/code/PTA_dataPlant/UpdateData/Weather",Day,".csv",sep=""))

ind3 <- c("TemperatureC","Dew.PointC","Humidity", "Sea.Level.PressurehPa","VisibilityKm","Wind.SpeedKm.h")
ind3Sum <- c()
for(i in 1:length(ind3)){
        tmp <- c(max(wea[,ind3[i]]), mean(wea[,ind3[i]]), min(wea[,ind3[i]]))
        ind3Sum <- c(ind3Sum,tmp)
}
ind3Sum <- ind3Sum[- length(ind3Sum)]
indO <- c(max(wea[,"Gust.SpeedKm.h"]), max(wea[,"Precipitationmm"]), names(table(wea[,"Conditions"]))[1], max(as.numeric(wea[,"Events"])), mean(wea[,"WindDirDegrees"]))
oneDay <- matrix(c(Day, ind3Sum, indO),nrow=1)
colnames(oneDay) <- weaName
write.csv(oneDay,file="UpdateData/WeathertmpOneday.csv",quote=FALSE,row.names = FALSE)

tmp <- read.csv("UpdateData/WeathertmpOneday.csv")

library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
#OldData <- dbReadTable(con,"hengyi_pta_weatherdata")
dbWriteTable(con, "hengyi_pta_weatherdata", as.data.frame(tmp), append=TRUE,row.names=FALSE, overwrite=FALSE)
dbDisconnect(con)
