test <- function(){
        rm(list=ls())
        gc()
        
        setwd("D:/code/PTA_dataPlant")
        # library and source files
        source("misc.R")
        source("PTA_prediction.R")
        library(lubridate) ## as.Date
        library(zoo) ## na.approx
        library(forecast) ##CV
        library(TTR) #WMA
        library(vars) #VAR
        
        source("Data_Pre.R")
}

pcaAna <- function(){

        #PTA_data <- getUpdateData_v1()
        PTA_data <- getUpdateData()
        tsub <- which(colnames(PTA_data)=="市场价PTA对苯二甲酸")
        PTA_data <- PTA_data[!is.na(PTA_data[,tsub]), ]
        
        
        colwea <- c("MeanVisibilityKm","MaxVisibilityKm","MinVisibilitykM")
        tmp <- as.matrix(PTA_data[,colwea])
        tmp[as.numeric(tmp) <= 0] <- NA
        PTA_data[ ,colwea] <- tmp
        data <- data_filling(PTA_data)
        
        rownames(data) <- data[ ,1]
        data <- data[, -1]
        mode(data) <- "numeric"
        
        # specific setting to PTA Price
        target <- "市场价PTA对苯二甲酸"
        sub <- which(colnames(data)==target)
        data <- data[,-sub]
        
        a <- 1:ncol(data)
        for(i in 1:ncol(data)){
                a[i] <- sd(data[,i])
        }
        data <- data[,a>0]
        
        
        # load("data_2002_2015")
        # data <- data_filling(data)
        # data <- data[!is.na(data[,1]), ]
        
        tmpdata <- groupPredict(data,i=1)
        pr <- prcomp(tmpdata, scale = TRUE)
        vars <- apply(pr$x, 2, var)  
        props <- vars / sum(vars)
        cumsum(props)[1:100]
        
        
        a <- 1:ncol(data)
        for(i in 1:ncol(data)){
                a[i] <- sd(data[,i])/mean(data[,i])
        }
        plot(a)
        
        # 141 -> 32
        
        # 328 -> 15
        
        # 总结： 新收集的数据具有很高的冗余性，所以数据用之前应该进行重复删除、异常值分析、缺失值填补或者主成分转化。

}

name_trans <- function(oldnames){
        oldnames <- gsub(":","",oldnames)
        oldnames <- gsub("\\(","",oldnames)
        oldnames <- gsub("\\)","",oldnames)
        oldnames <- gsub("\\.","",oldnames) 
        oldnames
}

dataRep <- function(){

        data1 <- getUpdateData_v1()
        data0 <- getUpdateData()
        load("data_2002_2015")
        cc <- colnames(data)
        cc <- name_trans(cc)
        # cs <- c(colnames(data1),colnames(data0),cc)
        # cs <- unique(cs)
        # cs <- sort(cs)
        # write.table(cs,file="ChoiceAllCols.txt",col.names = FALSE,row.names=FALSE,quote = FALSE)
        
        data0[,"time"] <- as.character(as.Date(data0[,"time"]))
        intT <- intersect(data1[,"time"],data0[,"time"])
        intT <- intersect(intT,rownames(data))
        
        tmp <- data0[match(intT,data0[,"time"]), ]
        tmp <- cbind(tmp,data1[match(intT,data1[,"time"]), ])
        tmp <- cbind(tmp,data[match(intT,rownames(data)), ])
        coltmp <- colnames(tmp)
        coltmp <- name_trans(coltmp)
        colnames(tmp) <- coltmp
        
        mancs <- unlist(read.table("ChoiceAllColsUnique.txt"))
        tmp1 <- tmp[,mancs]
        cs1 <- dataRepOne(tmp1)
        cs1 <- unique(cs1)
        length(cs1)
        
        a1 <- setdiff(cs1,colnames(data1))
        a2 <- setdiff(a1,colnames(data0))
        write.table(a2,file="NotIncludeNowCols.txt", col.names = FALSE,row.names=FALSE,quote = FALSE)
        write.table(setdiff(cs1,a2),file="HaveNowCols.txt",col.names = FALSE,row.names=FALSE,quote = FALSE)
        
        #cs1 <- dataRepOne(tmp)
        #cs1 <- name_trans(cs1)
        #cs1 <- unique(cs1)
        #length(cs1)

        # col1 <- dataRepOne(data1)
        # col0 <- dataRepOne(data0)
        # colc <- dataRepOne(data)
        # colc <- name_trans(colc)
        # cs <- unique(c(col1,col0,colc))
}

dataRepOne <- function(data){
        
        n <- ncol(data)
        consub <- 1:n
        ss <- 1:n
        for(i in 1:n){
                tmp <- as.numeric(data[,i])
                consub[i] <- sd(tmp[!is.na(tmp)])==0
                ss[i] <- paste(data[,i],sep="",collapse = "_")
        }
        consub[is.na(consub)] <- FALSE
        depsub <- duplicated(ss)
        
        colnames(data)[!(consub | depsub)]
}


##先删除常数项，在删除重复项，（填补、清洗）在删除冗余项,指标转化与归一化，相关性分析或者主成分分析降维（特征选择）。 数据项的聚类与层次结构分析！！！！！

collectedIndexs <- function(){
        
        ###add old cols========
        setwd("//Bjb-155/hq/PTA_dataPlant/PTA-new")
        filenames <- list.files(path=".",".xls$")
        
        library(xlsx)
        options(warn=-1)
        
        indexIDM <- c()
        
        for(i in 1:length(filenames)){
                print(i)
                aa <- read.xlsx2(filenames[i],1,as.data.frame = TRUE, header=FALSE, colClasses="character")
                aa <- as.matrix(aa)
                tmp <- aa[c(3,6), -1]
                indexIDM <- cbind(indexIDM,tmp)
        }
        indexIDM <- t(indexIDM)
        indexIDM <- indexIDM[!duplicated(indexIDM[,2]), ]
        indexIDM[,1] <- gsub("【计算用】","",indexIDM[,1])
        indexIDM <- indexIDM[nchar(indexIDM[,2]) > 3,  ]
        
        write.table(indexIDM,file="D:/code/PTA_dataPlant/WindIndexRest113.txt",sep="\t",col.names = FALSE,row.names = FALSE,quote = FALSE) 
        
        
        
        ###all Wind Indexs=======
        setwd("//Bjb-155/hq/PTA_dataPlant/PTA-new")
        filenames <- list.files(path=".",".txt$")
        indexs <- c()
        maps <- c()
        for(i in 1:length(filenames)){
                tmp <- read.delim(filenames[i],header = FALSE)
                maps <- rbind(maps,tmp[,1:2])
                indexs <- union(indexs,tmp[,2])
        }
        indexs <- sort(unique(indexs))
        tmp <- paste(maps[,1],maps[,2],sep="_")
        maps <- maps[!duplicated(tmp), ]
        write.table(maps,file="WindIndexMaps.txt",sep="\t",col.names = FALSE,row.names = FALSE,quote = FALSE) 
        write.table(indexs,file="WindIndexsV2.txt",sep="\t",col.names = FALSE,row.names = FALSE,quote = FALSE) 
        
        
}

addOldCols <- function(){
        
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
        

}
