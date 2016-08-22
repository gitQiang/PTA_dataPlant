#' PTA Price Prediction based on linear regression and ARIMA.
#' 
#' More details please contact: Qiang Huang. 
#' Copyright reserved by Qiang Huang.
#' 
#'====================================================
#' Qiang Huang, 2016/07/07, huangqiang@3golden.com.cn
#'====================================================
PTA_Model_training <- function(filenames=NULL,trace1=0,trans=0){
        # options
        #   filenames: filenames of add new indexes.
        #   trace1: print or not the running trace, 1 print, 0 not print
        #   trans: transformation for target index, 0 raw price, 1 diff(log,1)
        
        # output
        #   by now, we directly ouput to shiny web site.
        
        #==========================================================
        # library and source files
        source("misc.R")
        source("Models.R")
        jobid <- jobTrace(11,trace1);
        jobid <- jobTrace(12,trace1);
        library(lubridate) ## as.Date
        library(zoo) ## na.approx
        library(forecast) ##CV
        library(TTR) #WMA
        library(vars) #VAR
        
        PTA_data <- getUpdateData()
        #PTA_data <- getUpdateData_v1()
        colwea <- c("MeanVisibilityKm","MaxVisibilityKm","MinVisibilitykM")
        tmp <- as.matrix(PTA_data[,colwea])
        tmp[as.numeric(tmp) <= 0] <- NA
        PTA_data[ ,colwea] <- tmp
        data <- data_filling(PTA_data)
        #data <- data_fillingNew(PTA_data)
        
        #==========================================================
        # ? upload new indexes
        if(!is.null(filenames)){dataM <- addNewIndex(filenames,useYears);}else{dataM <- c();}
        
        jobid <- jobTrace(1,trace1)
        rownames(data) <- data[ ,1]
        data <- data[, -1]
        data <- cbind(data,dataM)
        mode(data) <- "numeric"

        # specific setting to PTA Price
        target <- "市场价PTA对苯二甲酸"
        sub <- which(colnames(data)==target)
        tmp <- data[,-sub]
        data <- cbind(data[,sub],tmp)
        colnames(data)[1] <- "Target"
        
        #==========================================================
        ## one day, one week, one month and one quarter predictions
        fres <- c(1,5,12,4)
        pers <- c(20,20,30,20)
        precs <- list()
        backprec <- list()
        results <- list()
        k <- 1
        plot=TRUE
        i=4;j=4
        
        for(i in 2){
                jobtmp <- jobTrace(i+1,trace1)
                tmpdata1 <- groupPredict(data,i)
                #tmpdata <- groupPredict(data,i)
                #tmpdata1 <- sapply(1:ncol(tmpdata), function(i) na.approx(tof(tmpdata[,i]),maxgap=5,na.rm=FALSE))
                #colnames(tmpdata1) <- colnames(tmpdata)
                #rownames(tmpdata1) <- rownames(tmpdata)
                #tmpdata1 <- fraction_NA(tmpdata1,pNA=0.5)
                
                #ntmp <- nrow(tmpdata1)
                #if(ntmp > 1500) tmpdata1 <- tmpdata1[(ntmp-1043):ntmp, ]
                
                for(j in c(1,2,3,4,5,6,7,8)){
                        results[[k]] <- oneDimPredict(tmpdata1,targetIndex=1,fre=fres[i],per=pers[i],sflag=i,model=j) 
                        ### 1) fractions of residuals are smaller than p=0.05; 2) predicted trend corrected; both 1) and 2); 4) residual summary; 5)R2 summary
                        precs[[k]] <- precision_pred(results[[k]][[1]],p=0.05)
                        backprec[[k]] <- precision_pred(results[[k]][[2]],p=0.05)
                        if(plot) plot_testing(results[[k]][[1]]$obs,results[[k]][[1]]$preds,results[[k]][[1]]$labs)
                        
                        
                        k <- k+1
                        print(i)
                        print(j)
                }
        }
        
        jobid <- jobTrace(10,trace1)
  
}


PTA_Model_Predicting <- function(filenames=NULL,trace1=0,trans=0){
        # options
        #   filenames: filenames of add new indexes.
        #   trace1: print or not the running trace, 1 print, 0 not print
        #   trans: transformation for target index, 0 raw price, 1 diff(log,1)
        
        #==========================================================
        # library and source files
        source("misc.R")
        source("Models.R")
        jobid <- jobTrace(11,trace1);
        jobid <- jobTrace(12,trace1);
        library(lubridate) ## as.Date
        library(zoo) ## na.approx
        library(forecast) ##CV
        
        #PTA_data <- getUpdateData()
        PTA_data <- getUpdateData_v1()
        colwea <- c("MeanVisibilityKm","MaxVisibilityKm","MinVisibilitykM")
        tmp <- as.matrix(PTA_data[,colwea])
        tmp[as.numeric(tmp) <= 0] <- NA
        PTA_data[ ,colwea] <- tmp
        data <- data_filling(PTA_data)
        
        #==========================================================
        # ? upload new indexes
        if(!is.null(filenames)){dataM <- addNewIndex(filenames,useYears);}else{dataM <- c();}
        
        jobid <- jobTrace(1,trace1)
        rownames(data) <- data[ ,1]
        data <- data[, -1]
        
        data <- cbind(data,dataM)
        mode(data) <- "numeric"
        
        # specific setting to PTA Price
        target <- "市场价PTA对苯二甲酸"
        sub <- which(colnames(data)==target)
        tmp <- data[,-sub]
        data <- cbind(data[,sub],tmp)
        colnames(data)[1] <- "Target"

        #==========================================================
        ## one day, one week, one month and one quarter predictions
        fres <- c(1,5,12,4)
        results <- list()
        k <- 1
        for(i in 1:4){
                jobtmp <- jobTrace(i+1,trace1)
                tmpdata1 <- groupPredict(data,i)
                #tmpdata <- groupPredict(data,i)
                #tmpdata1 <- sapply(1:ncol(tmpdata), function(i) na.approx(tof(tmpdata[,i]),maxgap=5,na.rm=FALSE))
                #colnames(tmpdata1) <- colnames(tmpdata)
                #rownames(tmpdata1) <- rownames(tmpdata)
                #tmpdata1 <- fraction_NA(tmpdata1,pNA=0.5)
        
                for(j in 9:16){
                        results[[k]] <- PredictPTA(tmpdata1,targetIndex=1,fre=fres[i],per=fres[i],sflag=i,model=j) 
                        k <- k+1
                        
                        print(i)
                        print(j)
                }
        }
        jobid <- jobTrace(10,trace1)
        
}



### 预留接口，用于获取实时数据
getUpdateData <- function(){
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
        
        timeDays <- intersect( as.character(as.Date(Wind1[,1])), intersect( as.character(as.Date(Weather1[,1])), as.character(as.Date(choice1[,1])) ) ) 
        
        Newdata <- cbind(choice1[match(timeDays, as.character(as.Date(choice1[,1])) ), ], Weather1[match(timeDays,as.character(as.Date(Weather1[,1]))), -1], Wind1[match(timeDays, as.character(as.Date(Wind1[,1]))), -1])
        
        
        tmpcols <- c("time",colnames(choice1)[-1],colnames(Weather1)[-1],colnames(Wind1)[-1])
        tmpcols <- gsub(":","",tmpcols)
        tmpcols <- gsub("\\(","",tmpcols)
        tmpcols <- gsub("\\)","",tmpcols)
        tmpcols <- gsub("\\.","",tmpcols)
        colnames(Newdata) <- tmpcols
        Newdata <- as.matrix(Newdata)
        Newdata <- Newdata[,!duplicated(colnames(Newdata))]
        
        Newdata
}

getUpdateData_v1 <- function(){
        ### 读出wind, choice  data 和 天气数据
        library(RMySQL)
        library(DBI)
        con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
        choiceData <- dbReadTable(con,"hengyi_pta_choicedata")
        choiceDataV1 <- dbReadTable(con,"hengyi_pta_choicedatav1")
        WindData <- dbReadTable(con,"hengyi_pta_winddata")
        WeatherData <- dbReadTable(con,"hengyi_pta_weatherdata")
        dbDisconnect(con)
        
        windDic <- c("时间","现货价:原油(中国大庆):环太平洋","产量:服装:当月值","中国棉花价格指数:328","中国棉花价格指数:527","中国棉花价格指数:229","Cotlook:A指数","产量:布:当月值","产量:布:当月同比","产量:聚酯:当月值","产量:聚酯:当月同比","产量:聚酯:累计值","产量:化学纤维:当月值","产量:化学纤维:当月同比")
        Wind1 <- WindData
        colnames(Wind1) <- windDic
        Wind1 <- Wind1[-1, ]
        n.wind <- ncol(Wind1)
        
        Weather1 <- WeatherData[ , -22]
        colnames(Weather1) <- Weather1[1, ]
        Weather1 <- Weather1[-1, ]
        n.weat <- ncol(Weather1)
        
        choice1 <- choiceData
        colnames(choice1) <- choice1[1, ]
        choice1 <- choice1[-1, ]
        n.choi1 <- ncol(choice1)
        
        choice2 <- choiceDataV1
        colnames(choice2) <- choice2[1, ]
        choice2 <- choice2[-1, ]
        n.choi2 <- ncol(choice2)
        
        dd <- c(as.Date(Wind1[,1]), as.Date(Weather1[,1]), as.Date(choice1[,1]), as.Date(choice2[,1]))
        dd <- dd[!duplicated(dd)]
        unionDs <- as.character(dd[order(dd)])
        t1 <- as.character(as.Date(choice1[,1]))
        t2 <- as.character(as.Date(choice2[,1]))
        t3 <- as.character(as.Date(Weather1[,1]))
        t4 <- as.character(as.Date(Wind1[,1]))
        
        Newdata <- matrix(NA,length(unionDs),(n.wind+n.weat+n.choi1+n.choi2-3))
        Newdata[,1] <- unionDs
        Newdata[match(t1,Newdata[,1]), 2:n.choi1] <- as.matrix(choice1[ ,-1])
        Newdata[match(t2,Newdata[,1]), (n.choi1+1):(n.choi1+n.choi2-1)] <- as.matrix(choice2[ ,-1])
        Newdata[match(t3,Newdata[,1]), (n.choi1+n.choi2):(n.choi1+n.choi2+n.weat-2)] <- as.matrix(Weather1[, -1])
        Newdata[match(t4,Newdata[,1]), (n.choi1+n.choi2+n.weat-1):ncol(Newdata)] <- as.matrix(Wind1[, -1])
        
        tmpcols <- c("time",colnames(choice1)[-1],colnames(choice2)[-1],colnames(Weather1)[-1],colnames(Wind1)[-1])
        tmpcols <- gsub(":","",tmpcols)
        tmpcols <- gsub("\\(","",tmpcols)
        tmpcols <- gsub("\\)","",tmpcols)
        tmpcols <- gsub("\\.","",tmpcols)
        colnames(Newdata) <- tmpcols
        
        sub <- min(which(grepl("2004",Newdata[,1])))
        Newdata <- Newdata[sub:nrow(Newdata), ]
        Newdata <- Newdata[,!duplicated(colnames(Newdata))]
        
        Newdata
}


### 预留接口，用于增加新的数据指标
addNewIndex <- function(filenames=NULL,useYears=2002:2016){
        
        startYear <- useYears[1]
        endYear <- useYears[length(useYears)]
        
        useDates <- seq.Date(as.Date(paste(startYear,"-1-1",sep="")),as.Date(paste(endYear,"-12-31",sep="")),by="day")
        useMonths <- paste(year(useDates),month(useDates),sep="-")
        dataM <- c()
        factors <- c()
        
        for(i in 1:length(filenames)){
                tmpTable <- csv_clean(filenames[i])
                tmpDate <- as.Date(tmpTable[,1])
                tmpTable <- tmpTable[year(tmpDate) %in% useYears,  ]
                tmpTable <- fill_NA(tmpTable,len=4)
                
                if(ncol(tmpTable) >= 2){
                        tmpDate <- as.Date(tmpTable[,1])
                        tmpName <- colnames(tmpTable)
                        
                        for(j in 2:ncol(tmpTable)){
                                onex <- rep(NA,length(useDates))
                                onex[match(tmpDate,useDates)] <- tmpTable[,j]
                                onex <- fill_onex(useMonths,tof(onex))
                                onex <- na.approx(onex,maxgap=62,na.rm=FALSE)
                                dataM <- cbind(dataM,onex)
                                factors <- c(factors,colnames(tmpTable)[j])
                        }
                }
                
        }
        rownames(dataM) <- as.character(useDates)
        colnames(dataM) <- factors
        dataM
}
