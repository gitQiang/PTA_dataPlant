source("D:/code/PTA_dataPlant/misc.R")
library(lubridate) 
cost <- csv_clean("data_v2/PTA成本.csv")
Price <- csv_clean("data_v1/PTAPriceIn.csv")

oDays <- intersect(cost[,1],Price[,1])
aa <- cbind(oDays, Price[match(oDays,Price[,1]),2], cost[match(oDays,cost[,1]),2])
aa <- aa[order(as.Date(aa[,1])), ]
write.csv(aa,file="data_v2/PTAPrice_cost.csv",row.names = FALSE, quote = FALSE)


source("D:/code/src_scripts/xls2csv_hq.R")
filenames <- list.files("D:/data/恒逸/恒逸共享/调研数据整理/data_v2",".xls",full.names = TRUE)

xls2csv_hq(filenames)
