setwd("D:/code/PTA_dataPlant")

name_trans <- function(oldnames){
        oldnames <- gsub(":","",oldnames)
        oldnames <- gsub("\\(","",oldnames)
        oldnames <- gsub("\\)","",oldnames)
        oldnames <- gsub("\\.","",oldnames) 
        oldnames
}


load("data_2002_2015")

oldnames <- colnames(data)
#oldnames <- name_trans(oldnames)

aa <- read.csv("UpdateData/Choice_PTA2016-09-05.csv",skip=0,strip.white=TRUE)
bb <- read.csv("UpdateData/WeathertmpOneday.csv")
cc <- c("时间","现货价:原油(中国大庆):环太平洋","产量:服装:当月值","中国棉花价格指数:328","中国棉花价格指数:527","中国棉花价格指数:229","Cotlook:A指数","产量:布:当月值","产量:布:当月同比","产量:聚酯:当月值","产量:聚酯:当月同比","产量:聚酯:累计值","产量:化学纤维:当月值","产量:化学纤维:当月同比")

new0 <- c(colnames(aa),colnames(bb),cc)
#new0 <- name_trans(new0)

oneT1 <- read.csv("UpdateData/Choice_PTA_MAC2016-09-05.csv")
oneT2 <- read.csv("UpdateData/Choice_PTA_EIA2016-09-05.csv")
new1 <- colnames(oneT1)
new2 <- colnames(oneT2)
#new1 <- name_trans(colnames(oneT1))
#new2 <- name_trans(colnames(oneT2))

newnames <- c(new0,new1,new2)

write.table(setdiff(oldnames,newnames),file="oldnames_newnames_diff_tmp.txt")
#setdiff(oldnames,newnames)


