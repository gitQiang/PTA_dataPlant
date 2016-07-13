
setwd("D:/code/PTA_dataPlant")
options(warn=-1)

# Weather columns
weaName <- c("CST", "Max.TemperatureC", "Mean.TemperatureC", "Min.TemperatureC", "Dew.PointC", "MeanDew.PointC", "Min.DewpointC", "Max.Humidity", "Mean.Humidity", "Min.Humidity", "Max.Sea.Level.PressurehPa", "Mean.Sea.Level.PressurehPa", "Min.Sea.Level.PressurehPa", "Max.VisibilityKm", "Mean.VisibilityKm", "Min.VisibilitykM", "Max.Wind.SpeedKm.h", "Mean.Wind.SpeedKm.h", "Max.Gust.SpeedKm.h", "Precipitationmm", "CloudCover", "Events", "WindDirDegrees")

Day <- as.character(Sys.Date())
wea <- read.csv(paste("UpdateData/Weather",Day,".csv",sep=""))

ind3 <- c("TemperatureC","Dew.PointC","Humidity", "Sea.Level.PressurehPa","VisibilityKm","Wind.SpeedKm.h")
ind3Sum <- c()
for(i in 1:length(ind3)){
        tmp <- c(max(wea[,ind3[i]]), mean(wea[,ind3[i]]), min(wea[,ind3[i]]))
        ind3Sum <- c(ind3Sum,tmp)
}
ind3Sum <- ind3Sum[- length(ind3Sum)]
indO <- c(max(wea[,"Gust.SpeedKm.h"]), max(wea[,"Precipitationmm"]), names(table(wea[,"Conditions"]))[1], max(as.numeric(wea[,"Events"])), mean(wea[,"WindDirDegrees"]))
oneDay <- matrix(c(Day, ind3Sum, indO),ncol=1)
rownames(oneDay) <- weaName


# wind distionary
windDic <- read.csv("UpdateData/WindDictionary.csv")
WindTable <- read.csv(paste("UpdateData/Wind",Day,".csv",sep=""))
if(nrow(WindTable)==0){
        WindData = rep(0,ncol(WindTable)-1)
}else{
        WindData = WindTable[WindTable[,1]==Day,-1,drop=FALSE]        
}
WindData <- matrix(WindData,ncol=1)
rownames(WindData) <- windDic[match(colnames(WindTable)[-1], colnames(windDic) )]


### Update Choice
library(lubridate) ## as.Date
oneTable <- read.csv("UpdateData/Choice_PTA.csv",skip=0,strip.white=TRUE)
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

##!!!!! only for test
oneTable[nrow(oneTable), 1] <- Day
### !!!!!!!!!!!!!!!!!!!!!!!!!!!

choiceDay <- oneTable[oneTable[,1]==Day,-1,drop=FALSE]
choiceDay <- t(choiceDay)

#file.rename("UpdateData/Choice_PTA.xls", paste("UpdateData/Choice_PTA",Day,".xls",sep=""))
file.rename("UpdateData/Choice_PTA.csv", paste("UpdateData/Choice_PTA",Day,".csv",sep=""))
if(file.exists("UpdateData/Choice_PTA.xls")) file.remove("UpdateData/Choice_PTA.xls")


oneData <- rbind(oneDay,WindData,choiceDay)
write.csv(t(oneData), file=paste("UpdateData/UpdateData",Day,".csv",sep=""),row.names=FALSE,quote=FALSE)


