### 读出wind, choice  data 和 天气数据
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")

choiceData <- dbReadTable(con,"hengyi_pta_choicedata")
WindData <- dbReadTable(con,"hengyi_pta_winddata")
WeatherData <- dbReadTable(con,"hengyi_pta_weatherdata")

dbDisconnect(con)

windDic <- c("时间","现货价:原油(中国大庆):环太平洋","产量:服装:当月值","中国棉花价格指数:328","中国棉花价格指数:527","中国棉花价格指数:229","Cotlook:A指数","产量:布:当月值","产量:布:当月同比","产量:聚酯:当月值","产量:聚酯:当月同比","产量:聚酯:累计值","产量:化学纤维:当月值","产量:化学纤维:当月同比")
Wind1 <- WindData
colnames(Wind1) <- windDic
Wind1 <- Wind1[-1, ]

Weather1 <- WeatherData[ , -22]
colnames(Weather1) <- Weather1[1, ]
Weather1 <- Weather1[-1, ]

choice1 <- choiceData
colnames(choice1) <- choice1[1, ]
choice1 <- choice1[-1, ]

days1 <- as.character(as.Date(choice1[,1]))
days2 <- as.character(as.Date(Weather1[,1]))
days3 <- as.character(as.Date(Wind1[,1]))
timeDays <- intersect(days1,intersect(days2,days3)) 

Newdata <- cbind(choice1[match(timeDays, days1), ], Weather1[match(timeDays,days2), -1], Wind1[match(timeDays, days3), -1])


### 重新写到数据库表里
tmp <- rbind(colnames(Newdata),Newdata)
colnames(tmp) <- paste("V",1:ncol(tmp),sep="_")

library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
dbWriteTable(con, "hengyi_pta_data", as.data.frame(tmp), row.names=FALSE)
dbDisconnect(con)
