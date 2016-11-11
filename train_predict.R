#' PTA Price Prediction on week and month dimension.
#' 
#' More details please contact: Qiang Huang. 
#' Copyright reserved by Qiang Huang.
#' 
#'====================================================
#' Qiang Huang, 2016/10/07, huangqiang@3golden.com.cn
#'====================================================
#'


options(warn = -1)
## source and library
source("DataClean.R")
source("miscNew.R")
source("Models.R")

library(RMySQL)
library(DBI)
library(lubridate)
library(TTR)
library(vars)
library(zoo)
library(FSelector)
library(forecast) ##CV

## loading data 
Newdata <- getUpdateData_v2()
Newdata[Newdata==""] <- NA
data0 <- delRep(Newdata)

## given parameters about data dimension
## i=2: week prediction;i=3: month prediction; i=4: season prediction
i=4
dims <- c("day","week","month","season")
fres <- c(255,52,12,4) ## 53 weeks (one without data)
pers <- c(50,30,20,10)

## step1 : data cleaning
data0 <- FirstCheck(data0,dims[i])
colnames(data0)[1] <- "Target"
Y <- data0[,1]
x <- data0[, -1]
xtrain <- x[-nrow(x), ] ## train --> train and test
xpred <- x[nrow(x), ,drop=FALSE]
ytrain <- Y[-1]

## step2: training models
precs <- list()
backprec <- list()
results <- list()
k <- 1
plot=TRUE

precis <- matrix(0,8,9)
for(j in 1:8){
        ## training
        model <- j
        sflag <- i
        perform <- list();
        if(model <= 7){ onetrain <- cbind(ytrain,xtrain);
        }else{ onetrain <- cbind(ytrain,xtrain[,1:2]);}
        perform[[1]] <- Models(model,sflag,onetrain,sub=1,per=pers[sflag],fre=fres[sflag])
        perform[[2]] <- pseudoPredict(onetrain,pers[sflag],1)
        results[[k]] <- perform
        
        ## access models
        print(summary(abs(results[[k]][[1]]$residuals)))
        precs[[k]] <- precision_pred(results[[k]][[1]],p=-1,ytrain)
        backprec[[k]] <- precision_pred(results[[k]][[2]],p=-1,ytrain)
        if(plot) plot_testing(results[[k]][[1]]$obs,results[[k]][[1]]$preds,results[[k]][[1]]$labs)
        precis[j, ] <- unlist(precs[[k]])[1:9]
        
        k <- k+1
}

##  step3: best model selection and visualization
tmp <- sapply(1:8,function(ii) (precis[ii,6]*precis[ii,7]*precis[ii,8])^(1/3))
tmp <- 1/tmp
if( sum(tmp==max(tmp)) >1 ){
        n1sub <- which(tmp==max(tmp))
        n2sub <- which.max(precis[n1sub,2])
        bestM <- n1sub[n2sub]
}else{
        bestM <- which.max(tmp)    
}
print(precis[bestM, ])


## step 4: predicting
model1 <- bestM + 8
print(model1)

for(model1 in 9:16){
        
        if(model1 <= 15){ 
                onetrain <- cbind(ytrain,xtrain)
                onepred <- rbind(onetrain,c(NA,xpred))
        }else{  onetrain <- cbind(ytrain,xtrain[,1:2])
                onepred <- rbind(onetrain,c(NA,xpred[,1:2]))
        }
        
        predR <- Models(model1,sflag,onepred,sub=1,per=1,fre=fres[sflag])$preds
        print(predR)
        
}
