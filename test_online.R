test_online <- function(){
        rm(list=ls())
        gc()
        
        # library and source files
        source("misc.R")
        source("Models.R")
        source("PTA_prediction.R")
        source("test_online.R")
        library(lubridate) ## as.Date
        library(zoo) ## na.approx
        library(forecast) ##CV
        library(TTR) #WMA
        library(vars) #VAR
        
        
        PTA_data <- getUpdateData()
        #PTA_data <- getUpdateData_v1()
        subs8 <- which(grepl("2016-8",PTA_data[,1]))
        gdate <- groupDate(PTA_data[,1])
        
        
        ### 8yue everyday
        preds <- rep(0,length(subs8))
        i=1
        k <- 1
        for(kk in subs8){
                day1 <- as.numeric(unlist(strsplit(gdate[kk,1],"-"))[3])
                day2 <- as.numeric(unlist(strsplit(gdate[kk-1,1],"-"))[3])
                if((day1-day2)==1){ kk1 <- kk-2;}else{ kk1 <- kk-1;}
                
                onedata <- PTA_data[1:kk1, ]
                trainBest <- PTA_training(onedata,i,per=1)
                j <- which.min(trainBest) + 8
                tmp <- PTA_Predicting(onedata,i,j,per=1)      
                preds[k] <- tmp[[1]][[1]]$preds
                k <- k+1
        }
        plot_testing(PTA_data[subs8,3],preds,labs=PTA_data[subs8,1])

        ### 8yue five weeks
        w8 <- c("2016-31","2016-32","2016-33","2016-34","2016-35")
        preds <- rep(0,length(w8))
        i=2
        for(kk in 1:length(w8)){
                kk1 <- match(w8[kk],gdate[,2]) - 1
                
                onedata <- PTA_data[1:kk1, ]
                trainBest <- PTA_training(onedata,i,per=1)
                j <- which.min(trainBest) + 8
                tmp <- PTA_Predicting(onedata,i,j,per=1)      
                preds[kk] <- tmp[[1]][[1]]$preds[1]
        }
        
        obs <- aggregate(as.numeric(PTA_data[subs8,3]),by=list(gdate[subs8,2]), mean)[,2]
        plot_testing(obs,preds,labs=w8)
        
        
        ### 8yue one month
        preds <- 0
        i=3
        kk1 <- subs8[1]-1
        onedata <- PTA_data[1:kk1, ]
        trainBest <- PTA_training(onedata,i,per=1)
        j <- which.min(trainBest) + 8
        tmp <- PTA_Predicting(onedata,i,j,per=1)      
        preds <- tmp[[1]][[1]]$preds
        mean(as.numeric(PTA_data[subs8,3]))
        
}


PTA_training <- function(PTA_data,i,per=1){
        
        colwea <- c("MeanVisibilityKm","MaxVisibilityKm","MinVisibilitykM")
        tmp <- as.matrix(PTA_data[,colwea])
        tmp[as.numeric(tmp) <= 0] <- NA
        PTA_data[ ,colwea] <- tmp
        data <- data_filling(PTA_data)
        #data <- data_fillingNew(PTA_data)

        rownames(data) <- data[ ,1]
        data <- data[, -1]
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
        precs <- list()
        backprec <- list()
        results <- list()
        k <- 1
       
        tmpdata1 <- groupPredict(data,i)
               
        
        trainBest <- 1:8   
        for(j in 1:8){
                results[[k]] <- oneDimPredict(tmpdata1,targetIndex=1,fre=fres[i],per=per,sflag=i,model=j) 
                precs[[k]] <- precision_pred(results[[k]][[1]],p=0.03,sflag=i)
                backprec[[k]] <- precision_pred(results[[k]][[2]],p=0.03,sflag=i)
                
                trainBest[j] <- precs[[k]]$s4[3]
                k <- k+1
        }

        trainBest
}


PTA_Predicting <- function(PTA_data,i,j,per=1){

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
        tmp <- data[,-sub]
        data <- cbind(data[,sub],tmp)
        colnames(data)[1] <- "Target"
        
        #==========================================================
        ## one day, one week, one month and one quarter predictions
        fres <- c(1,5,12,4)
        results <- list()
        k <- 1
       
        tmpdata1 <- groupPredict(data,i)
                
        results[[k]] <- PredictPTA(tmpdata1,targetIndex=1,fre=fres[i],per=fres[i],sflag=i,model=j) 
        
        results             
}