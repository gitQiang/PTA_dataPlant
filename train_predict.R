#' PTA Price Prediction on week and month dimension.
#' 
#' More details please contact: Qiang Huang. 
#' Copyright reserved by Qiang Huang.
#' 
#'====================================================
#' Qiang Huang, 2016/10/07, huangqiang@3golden.com.cn
#'====================================================
#'

## 两个问题： 时间周为单位和时间月度为单位的。
## 预测的时候至少要考虑一个时间点 滞后性， 整体最优的时间滞后点的选择
## 特征选择的最优化原则是什么？？？
## SARIMA模型与多元线性回归模型整合的模型直接一个函数就可以实现了，不用分开残差项了？？？

PTA_Models <- function(){
       
        ## step1 : data cleaning
        source("DataClean.R")
        data0 <- FirstCheck()
        colnames(data0)[1] <- "Target"
        Y <- data0[,1]
        x <- data0[, -1]
        xtrain <- x[-nrow(x), ] ## train --> train and test
        xpred <- x[nrow(x), ,drop=FALSE]
        ytrain <- Y[-1]
        
        
        ## step2: training and predicting by models
        source("miscNew.R")
        source("Models.R")
        library(lubridate) ## as.Date
        library(forecast) ##CV
        library(TTR) #WMA
        library(vars) #VAR
        
        #==========================================================
        ## one day, one week, one month and one quarter predictions
        fres <- c(52,12,4) ## 53 weeks (one without data)
        pers <- c(30,20,10)
        
        precs <- list()
        backprec <- list()
        results <- list()
        k <- 1
        plot=TRUE
        
        i=2; ## i=2: week prediction;i=3: month prediction; i=4: season prediction
        j=1; ## model: 1, ARIMA; 2, SARIMA; 3, HW; 4, MLR; 5, MLR_SARIMA; 6, MLR_HW;
        
        #for(i in 2:4){
                #for(j in 1:8){
                        
                        ## prepare train data set
                        
                        
        
        #for(j in 1:8){               ## training
                model <- j
                sflag <- i
                perform <- list();
                if(model <= 7){ onetrain <- cbind(ytrain,xtrain);
                }else{ onetrain <- cbind(ytrain,xtrain[,1:2]);}
                perform[[1]] <- Models(model,sflag,onetrain,sub=1,per=pers[sflag],fre=fres[sflag])
                perform[[2]] <- pseudoPredict(onetrain,pers[sflag],1)
                results[[k]] <- perform
                k <- k+1
        #}
        
        for(k in 1) print(mean(abs(results[[k]][[1]]$residuals)))
        mean(abs(results[[1]][[2]]$residuals))
        
        
                        ## access models
                        
                        
                        # ## predicting
                        # 
                        # results[[k]] <- oneDimPredict(tmpdata1,targetIndex=1,fre=fres[i],per=pers[i],sflag=i,model=j) 
                        # precs[[k]] <- precision_pred(results[[k]][[1]],p=-1,sflag=i)
                        # backprec[[k]] <- precision_pred(results[[k]][[2]],p=-1,sflag=i)
                        # if(plot) plot_testing(results[[k]][[1]]$obs,results[[k]][[1]]$preds,results[[k]][[1]]$labs)
                        # 
                        # 
                        # ### one prediction
                        # k <- k+1
               # }
        #}
        
        ##  step3: best model selection and visualization
        
        
}

