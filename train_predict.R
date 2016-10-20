#' PTA Price Prediction based on linear regression and ARIMA.
#' 
#' More details please contact: Qiang Huang. 
#' Copyright reserved by Qiang Huang.
#' 
#'====================================================
#' Qiang Huang, 2016/07/07, huangqiang@3golden.com.cn
#'====================================================
#'

source("DataClean.R")
source("miscNew.R")
source("Models.R")
library(lubridate) ## as.Date
library(zoo) ## na.approx
library(forecast) ##CV
library(TTR) #WMA
library(vars) #VAR

##两个问题： 时间周为单位和时间月度为单位的。
## 预测的时候至少要考虑一个时间点 滞后性， 整体最优的时间滞后点的选择

PTA_Model_training <- function(){
       
        
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
                        precs[[k]] <- precision_pred(results[[k]][[1]],p=-1,sflag=i)
                        backprec[[k]] <- precision_pred(results[[k]][[2]],p=-1,sflag=i)
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
