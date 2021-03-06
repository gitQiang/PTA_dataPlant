#' PTA Price Prediction based on eight different models.
#' 
#' More details please contact: Qiang Huang. 
#' Copyright reserved by Qiang Huang.
#' 
#'====================================================
#' Qiang Huang, 2016/07/07, huangqiang@3golden.com.cn
#'====================================================
#'
Models <- function(model,sflag,tmpnewdata,sub=1,per=per,fre=fre){
### input 
        # model: 1, ARIMA; 2, SARIMA; 3, HW; 4, MLR; 5, MLR_SARIMA; 6, MLR_HW;
        # sflag: 1, day; 2, week; 3, month; 4, season;
        # tmpnewdata: data
        # sub: target index
        # per: number of train or predict points
        # fre: the frequency of time-series
        
        #funs <- c("ARIMA_T","SARIMA_T","HW_T","MLR_T","MLR_SARIMA_T","MLR_HW_T","WMA_T","VAR_T","ARIMA_P","SARIMA_P","HW_P","MLR_P","MLR_SARIMA_P","MLR_HW_P","WMA_P","VAR_P") 
        #tmp <- as.function(funs[model], tmpnewdata=tmpnewdata,sub=sub,per=per,fre=fre,sflag=sflag)
        
        if(model==1) tmp <- ARIMA_T(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==2) tmp <- SARIMA_T(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==3) tmp <- HW_T(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==4) tmp <- MLR_T(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==5) tmp <- MLR_SARIMA_T(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==6) tmp <- MLR_HW_T(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==7) tmp <- WMA_T(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==8) tmp <- VAR_T(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        
        n <- 8
        if(model==n+1) tmp <- ARIMA_P(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==n+2) tmp <- SARIMA_P(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==n+3) tmp <- HW_P(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==n+4) tmp <- MLR_P(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==n+5) tmp <- MLR_SARIMA_P(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==n+6) tmp <- MLR_HW_P(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==n+7) tmp <- WMA_P(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        if(model==n+8) tmp <- VAR_P(tmpnewdata,sub,per=per,fre=fre,sflag=sflag)
        
        tmp        
}


######## training models
ARIMA_T <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        residuals <- 1:per
        para <- c()
        #### input info
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        for(n1 in (n2-per):(n2-1)){
                pdq <- arima_paraNew(tmpnewdata[1:n1,sub],fre=fre)
                stsr <- arima(ts(tmpnewdata[1:n1,sub],frequency = pdq[4]),order=pdq[1:3])

                #### one step prediction
                preds[n1-(n2-per-1)] <- predict(stsr, n.ahead=1)$pred[1]
                para <- cbind(para,pdq)
                R2[n1-(n2-per-1)] <- R_squared_hq(tmpnewdata[1:n1,sub],fitted(stsr))
                residuals[n1-(n2-per-1)] <- tmpnewdata[n1+1,sub] - preds[n1-(n2-per-1)]
                #print(n1)
        }
        
        list(obs=tmpnewdata[(n2-per+1):n2,sub],R2=R2,preds=preds,residuals=residuals,para=para,labs=rownames(tmpnewdata)[(n2-per+1):n2])
}


SARIMA_T <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        residuals <- 1:per
        para <- c()
        #### input info
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        for(n1 in (n2-per):(n2-1)){
                pdq <- sarima_paraNew(tmpnewdata[1:n1,sub],fre=fre)
                stsr <- arima(ts(tmpnewdata[1:n1,sub],frequency = pdq[7]),order=pdq[1:3],seasonal = list(order=pdq[4:6],period=pdq[7]))
                
                #### one step prediction
                preds[n1-(n2-per-1)] <- predict(stsr, n.ahead=1)$pred[1]
                para <- cbind(para,pdq)
                R2[n1-(n2-per-1)] <- R_squared_hq(tmpnewdata[1:n1,sub],fitted(stsr))
                residuals[n1-(n2-per-1)] <- tmpnewdata[n1+1,sub] - preds[n1-(n2-per-1)]
                #print(n1)
        }
        
        list(obs=tmpnewdata[(n2-per+1):n2,sub],R2=R2,preds=preds,residuals=residuals,para=para,labs=rownames(tmpnewdata)[(n2-per+1):n2])
}


HW_T <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        residuals <- 1:per
        para <- c()
        #### input info
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        for(n1 in (n2-per):(n2-1)){
                stsr <- HoltWinters(ts(tmpnewdata[1:n1,sub],frequency = fre),gamma=FALSE)
                res <- tmpnewdata[1:n1,sub] - c(tmpnewdata[1:2,sub], stsr$fitted[,"xhat"])
                
                #### one step prediction
                preds[n1-(n2-per-1)] <- predict(stsr,1)[1]
                para <- cbind(para,c(stsr$alpha,stsr$beta,stsr$gamma,stsr$coefficients))
                R2[n1-(n2-per-1)] <- R_squared_hq(tmpnewdata[1:n1,sub], c(tmpnewdata[1:2,sub], stsr$fitted[,"xhat"]))
                residuals[n1-(n2-per-1)] <- tmpnewdata[n1+1,sub] - preds[n1-(n2-per-1)]
                #print(n1)
        }
        
        list(obs=tmpnewdata[(n2-per+1):n2,sub],R2=R2,preds=preds,residuals=residuals,para=para,labs=rownames(tmpnewdata)[(n2-per+1):n2])
}


MLR_T <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        residuals <- 1:per
        para <- c()
        
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        for(n1 in (n2-per):(n2-1)){
                tmpdata <- as.data.frame(tmpnewdata[1:n1,])
                xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                mlmr <- lm( paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=tmpdata )
                
                #### one step prediction
                preds[n1-(n2-per-1)] <- sum(c(1,tmpnewdata[n1+1,xnew]) * tof(as.vector(mlmr$coefficients),na.rm=FALSE,fill=TRUE,f="zero"))
                para <- cbind(para,mlmr$coefficients)
                R2[n1-(n2-per-1)] <- R_squared_hq(tmpnewdata[1:n1,sub],fitted(mlmr))
                residuals[n1-(n2-per-1)] <- tmpnewdata[n1+1,sub] - preds[n1-(n2-per-1)]
        }

        list(obs=tmpnewdata[(n2-per+1):n2,sub],R2=R2,preds=preds,residuals=residuals,para=para,labs=rownames(tmpnewdata)[(n2-per+1):n2])
}


MLR_SARIMA_T <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=TRUE,trace1,trans=0){
        
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        residuals <- 1:per
        para <- c()
        
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        labs0 <- rownames(tmpnewdata)      
        
        if(trans==0){
                if(sflag==1) pdq <- c(1,1,2,0,0,0,1) #day
                if(sflag==2) pdq <- c(1,1,1,2,1,0,7) #week
                if(sflag==3) pdq <- c(0,1,1,0,1,1,11) #month
                if(sflag==4) pdq <- c(0,1,0,0,1,0,4) #season
                if(sflag==5) pdq <- c(1,0,1,0,1,1,10) #month to season
        }else if(trans==1){
                if(sflag==1) pdq <- c(1,0,1,0,0,0,1) #day
                if(sflag==2) pdq <- c(1,0,3,0,1,0,7) #week
                if(sflag==3) pdq <- c(1,0,1,0,1,1,10) #month
                if(sflag==4) pdq <- c(0,0,0,0,1,0,4) #season
        }
        
        for(n1 in (n2-per):(n2-1)){
                if(sflag<4){
                        # pdq <- sarima_paraNew(tmpnewdata[1:n1,sub],fre=fre)
                        stsr <- arima(ts(tmpnewdata[1:n1,sub],frequency = pdq[7]),order=pdq[1:3],seasonal = list(order=pdq[4:6],period=pdq[7]))
                        tmpdata <- tmpnewdata[1:n1,]
                        tmpdata[,sub] <- stsr$residuals
                        xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                        mlmr <- lm(paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=as.data.frame(tmpdata))
                        res1 <- mlmr$residuals
                }
                
                if(sflag==4){
                        tmpdata <- as.data.frame(tmpnewdata[1:n1,])
                        xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                        mlmr <- lm( paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=tmpdata )
                        res <- ts(mlmr$residuals,frequency = fre)
                        pdq <- sarima_paraNew(res,fre=fre)
                        stsr <- arima(res,order=pdq[1:3],seasonal = list(order=pdq[4:6],period=pdq[7]))
                        res1 <- stsr$residuals
                }
                
                if(sflag <= 4){
                        #### one step prediction
                        preds[n1-(n2-per-1)] <- sum(c(1,tmpnewdata[n1+1,xnew]) * tof(as.vector(mlmr$coefficients),na.rm=FALSE,fill=TRUE,f="zero"))  +  predict(stsr, n.ahead=1)$pred[1]
                        oneP <- pdq
                        para <- cbind(para,oneP)
                        R2[n1-(n2-per-1)] <- R_squared_hq(tmpnewdata[1:n1,sub],tmpnewdata[1:n1,sub]-res1)
                        residuals[n1-(n2-per-1)] <- tmpnewdata[n1+1,sub] - preds[n1-(n2-per-1)]
                }
        
        }
       
        list(obs=tmpnewdata[(n2-per+1):n2,sub],R2=R2,preds=preds,residuals=residuals,para=para,labs=rownames(tmpnewdata)[(n2-per+1):n2])
        
}


MLR_HW_T <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=TRUE,trace1,trans=0){
        
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        residuals <- 1:per
        para <- c()
        
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        ### H-W model gamma 
        gamma = FALSE;
        for(n1 in (n2-per):(n2-1)){
                if(sflag<4){
                        stsr <- HoltWinters(ts(tmpnewdata[1:n1,sub],frequency = fre),gamma=gamma)
                        tmpdata <- tmpnewdata[1:n1,]
                        
                        tmpdata[,sub] <- tmpdata[,sub] -  c(tmpdata[1:2,sub], stsr$fitted[,"xhat"])
                        xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                        mlmr <- lm(paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=as.data.frame(tmpdata))
                        res1 <- mlmr$residuals
                }
                
                if(sflag==4){
                        tmpdata <- as.data.frame(tmpnewdata[1:n1,])
                        xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                        mlmr <- lm( paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=tmpdata )
                        res <- ts(mlmr$residuals,frequency = fre)
                        stsr <- HoltWinters(res,gamma=gamma)
                        res1 <- res - c(0,0,stsr$fitted[,"xhat"])
                }
                
                
                #### one step prediction
                preds[n1-(n2-per-1)] <- sum(c(1,tmpnewdata[n1+1,xnew]) * tof(as.vector(mlmr$coefficients),na.rm=FALSE,fill=TRUE,f="zero"))  +  predict(stsr,1)[1]
                para <- cbind(para,c(stsr$alpha,stsr$beta,stsr$gamma,stsr$coefficients))
                R2[n1-(n2-per-1)] <- R_squared_hq(tmpnewdata[1:n1,sub],tmpnewdata[1:n1,sub]-res1)
                residuals[n1-(n2-per-1)] <- tmpnewdata[n1+1,sub] - preds[n1-(n2-per-1)]
        }
        
        list(obs=tmpnewdata[(n2-per+1):n2,sub],R2=R2,preds=preds,residuals=residuals,para=para,labs=rownames(tmpnewdata)[(n2-per+1):n2])
}


WMA_T <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        library(TTR)
        n2 <- nrow(tmpnewdata)
        n1 <- n2-per
        #### output 
        ts <- tmpnewdata[-n2,1]
        #wts <- WMA(ts,2)
        wts <- EMA(ts,2)
        if(sflag==1){ preds <- wts[(n1-1):(n2-2)]; }else{preds <- wts[n1:(n2-1)];}
        
        residuals <- tmpnewdata[(n1+1):n2,sub] - preds
        R2 <- sapply( (n1+1):n2, function(kk) R_squared_hq(tmpnewdata[3:kk,sub],wts[2:(kk-1)]) )
        para <- rep(2,per)
        
        list(obs=tmpnewdata[(n2-per+1):n2,sub],R2=R2,preds=preds,residuals=residuals,para=para,labs=rownames(tmpnewdata)[(n2-per+1):n2])
}


VAR_T <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        ### fix moving windows
        n.limit <- 700
        ntmp <- nrow(tmpnewdata)
        if(ntmp > n.limit) tmpnewdata <- tmpnewdata[(ntmp-n.limit+1):ntmp, ]
        
        ######
        library(vars)
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        residuals <- 1:per
        para <- c()
        #### input info
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        if(sflag==1){ diffdata <- tmpnewdata[3:n2, ] - tmpnewdata[1:(n2-2), ];}else{diffdata <- tmpnewdata[2:n2, ] - tmpnewdata[1:(n2-1), ];}   
        
        L <- nrow(diffdata)
        for(n1 in (L-per):(L-1)){
                vmdl <- VAR(diffdata[1:n1,],p=1,type='const')
                oneP <- predict(vmdl,n.ahead=1)
                preds[n1-L+per+1] <- oneP$fcst[[1]][1]+tmpnewdata[n1+1,sub]
                para <- cbind(para,oneP$fcst[[1]][2:4])
                R2[n1-L+per+1] <- R_squared_hq(tmpnewdata[(n2-L+2):(n1+n2-L),sub],fitted(vmdl)[,sub]+tmpnewdata[2:n1,sub])
                residuals[n1-L+per+1] <- tmpnewdata[n1+1+n2-L,sub] - preds[n1-L+per+1]
        }
        
        list(obs=tmpnewdata[(n2-per+1):n2,sub],R2=R2,preds=preds,residuals=residuals,para=para,labs=rownames(tmpnewdata)[(n2-per+1):n2])
}


######## predicting models
ARIMA_P <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        per <- fre
        n1 <- min(which(is.na(tmpnewdata[,1]))) - 1
        
        pdq <- arima_paraNew(tmpnewdata[1:n1,sub],fre=fre)
        stsr <- arima(ts(tmpnewdata[1:n1,sub],frequency = pdq[4]),order=pdq[1:3])
        preds <- predict(stsr, n.ahead=per)$pred
        R2 <- R_squared_hq(tmpnewdata[1:n1,1],fitted(stsr))
        

        list(R2=R2,preds=preds,para=pdq)
}


SARIMA_P <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        per <- fre
        n1 <- min(which(is.na(tmpnewdata[,1]))) - 1
        
        pdq <- sarima_paraNew(tmpnewdata[1:n1,sub],fre=fre)
        stsr <- arima(ts(tmpnewdata[1:n1,sub],frequency = pdq[7]),order=pdq[1:3],seasonal = list(order=pdq[4:6],period=pdq[7]))
        preds <- predict(stsr, n.ahead=per)$pred
        R2 <- R_squared_hq(tmpnewdata[1:n1,sub],fitted(stsr))

        list(R2=R2,preds=preds,para=pdq)
}


HW_P <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        per <- fre
        n1 <- min(which(is.na(tmpnewdata[,1]))) - 1
        
        stsr <- HoltWinters(ts(tmpnewdata[1:n1,sub],frequency = fre),gamma=FALSE)
        preds <- predict(stsr,per)
        para <- c(stsr$alpha,stsr$beta,stsr$gamma,stsr$coefficients)
        R2 <- R_squared_hq(tmpnewdata[1:n1,sub], c(tmpnewdata[1:2,sub], stsr$fitted[,"xhat"]))
        
        list(R2=R2,preds=preds,para=para)
}


MLR_P <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        per <- sum(is.na(tmpnewdata[,sub]))
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        para <- c()
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        for(n1 in (n2-per+1):n2){
                tmpdata <- as.data.frame( tmpnewdata[1:(n2-per), c(TRUE, !is.na(tmpnewdata[n1, -sub]))])
                
                xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                mlmr <- lm( paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=tmpdata )
                
                #### one step prediction
                preds[n1-(n2-per)] <- sum(c(1,tmpnewdata[n1,xnew]) * tof(as.vector(mlmr$coefficients),na.rm=FALSE,fill=TRUE,f="zero"))
                para <- cbind(para,mlmr$coefficients)
                R2[n1-(n2-per)] <- R_squared_hq(tmpnewdata[1:(n2-per),sub],fitted(mlmr))
        }
        
        list(R2=R2,preds=preds,para=para)
}


MLR_SARIMA_P <- function(tmpnewdata,sub=1,per=1,fre=fre,sflag=sflag,plotP=TRUE,trace1,trans=0){
        
        per <- sum(is.na(tmpnewdata[,sub]))
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        para <- c()
        
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        
        if(trans==0){
                if(sflag==1) pdq <- c(1,1,2,0,0,0,1) #day
                if(sflag==2) pdq <- c(1,1,1,2,1,0,7) #week
                if(sflag==3) pdq <- c(0,1,1,0,1,1,11) #month
                if(sflag==4) pdq <- c(0,1,0,0,1,0,4) #season
                if(sflag==5) pdq <- c(1,0,1,0,1,1,10) #month to season
        }else if(trans==1){
                if(sflag==1) pdq <- c(1,0,1,0,0,0,1) #day
                if(sflag==2) pdq <- c(1,0,3,0,1,0,7) #week
                if(sflag==3) pdq <- c(1,0,1,0,1,1,10) #month
                if(sflag==4) pdq <- c(0,0,0,0,1,0,4) #season
        }
        
        for(n1 in (n2-per+1):n2){
                if(sflag<4){
                        # pdq <- sarima_paraNew(tmpnewdata[1:n1,sub],fre=fre)
                        stsr <- arima(ts(tmpnewdata[1:(n2-per),sub],frequency = pdq[7]),order=pdq[1:3],seasonal = list(order=pdq[4:6],period=pdq[7]))
                        tmpdata <- tmpnewdata[1:(n2-per),  c(TRUE, !is.na(tmpnewdata[n1, -sub]))]
                        tmpdata[,sub] <- stsr$residuals
                        xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                        mlmr <- lm(paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=as.data.frame(tmpdata))
                        res1 <- mlmr$residuals
                }
                
                if(sflag==4){
                        tmpdata <- as.data.frame( tmpnewdata[1:(n2-per), c(TRUE, !is.na(tmpnewdata[n1, -sub]))] )
                        xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                        mlmr <- lm( paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=tmpdata )
                        res <- ts(mlmr$residuals,frequency = fre)
                        pdq <- sarima_paraNew(res,fre=fre)
                        stsr <- arima(res,order=pdq[1:3],seasonal = list(order=pdq[4:6],period=pdq[7]))
                        res1 <- stsr$residuals
                }
                
                if(sflag <= 4){ #### one step prediction
                        preds[n1-(n2-per)] <- sum(c(1,tmpnewdata[n1,xnew]) * tof(as.vector(mlmr$coefficients),na.rm=FALSE,fill=TRUE,f="zero"))  +  predict(stsr, n.ahead=n1-n2+per)$pred[n1-n2+per]
                        para <- cbind(para,pdq)
                        R2[n1-(n2-per)] <- R_squared_hq(tmpnewdata[1:(n2-per),sub],tmpnewdata[1:(n2-per), sub]-res1)
                }
        }
        
        list(R2=R2,preds=preds,para=para)
        
}


MLR_HW_P <- function(tmpnewdata,sub=1,per=1,fre=fre,sflag=sflag,plotP=TRUE,trace1,trans=0){
        
        per <- sum(is.na(tmpnewdata[,sub]))
        #### output 
        R2 <- 1:per ### R^2 values
        preds <- 1:per ### predict values
        para <- c()
        
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        ### H-W model gamma 
        gamma = FALSE;
        for(n1 in (n2-per+1):n2){
                if(sflag<4){
                        stsr <- HoltWinters(ts(tmpnewdata[1:(n2-per),sub],frequency = fre),gamma=gamma)
                        tmpdata <- tmpnewdata[1:(n2-per), c(TRUE, !is.na(tmpnewdata[n1, -sub])) ]
                        
                        tmpdata[,sub] <- tmpdata[,sub] -  c(tmpdata[1:2,sub], stsr$fitted[,"xhat"])
                        xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                        mlmr <- lm(paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=as.data.frame(tmpdata))
                        res1 <- mlmr$residuals
                }
                
                if(sflag==4){
                        tmpdata <- as.data.frame(tmpnewdata[1:(n2-per), c(TRUE, !is.na(tmpnewdata[n1, -sub])) ])
                        xnew <-stepCV_hq(tmpdata,sub,cvf=1,dir="forward")
                        mlmr <- lm( paste(colnames(tmpdata)[sub]," ~ ",paste(xnew,sep="",collapse = "+"),sep=""), data=tmpdata )
                        res <- ts(mlmr$residuals,frequency = fre)
                        stsr <- HoltWinters(res,gamma=gamma)
                        res1 <- res - c(0,0,stsr$fitted[,"xhat"])
                }
                
                #### one step prediction
                preds[n1-(n2-per)] <- sum(c(1,tmpnewdata[n1,xnew]) * tof(as.vector(mlmr$coefficients),na.rm=FALSE,fill=TRUE,f="zero"))  +  predict(stsr,n1-n2+per)[n1+per-n2]
                para <- cbind(para,c(stsr$alpha,stsr$beta,stsr$gamma,stsr$coefficients))
                R2[n1-(n2-per)] <- R_squared_hq(tmpnewdata[1:(n2-per),sub],tmpnewdata[1:(n2-per),sub]-res1)
        }
        
        list(R2=R2,preds=preds,para=para)
}


WMA_P <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        
        library(TTR)
        per <- fre
        n1 <- min(which(is.na(tmpnewdata[,1]))) - 1

        ts <- tmpnewdata[1:n1,sub]
        wts <- WMA(ts,2)
        R2 <- R_squared_hq(tmpnewdata[3:n1,1],wts[2:(n1-1)])
        
        preds <- 1:per
        for(i in 1:per){
                wts <- WMA(ts,2)
                #wts <- EMA(ts,2)
                preds[i] <- wts[n1+i-1]
                ts <- c(ts,preds[i])
        }
        para <- rep(2,per)
        
        
        list(R2=R2,preds=preds,para=para)
}


VAR_P <- function(tmpnewdata,sub=1,per=per,fre=fre,sflag=sflag,plotP=FALSE,trace1=0,trans=0){
        ### fix moving windows
        n.limit <- 700
        ntmp <- nrow(tmpnewdata)
        if(ntmp > (n.limit+fre)) tmpnewdata <- tmpnewdata[(ntmp-n.limit-fre+1):ntmp, ]
        
        ####
        library(vars)
        per <- fre
        n1 <- min(which(is.na(tmpnewdata[,1]))) - 1
        
        n2 <- n1
        if(sflag==1){ diffdata <- tmpnewdata[3:n2, ] - tmpnewdata[1:(n2-2), ];}else{diffdata <- tmpnewdata[2:n2,] - tmpnewdata[1:(n2-1), ];}   
        
        L <- nrow(diffdata)
        vmdl <- VAR(diffdata,p=1,type='const')
        oneP <- predict(vmdl,n.ahead=per)

        preds <- 1:per
        preds[1] <- oneP$fcst[[1]][1,1] + tmpnewdata[L+1,sub]
        if(per >= 2){
                for(i in 2:per) preds[i] <- oneP$fcst[[1]][i,1] + ifelse((L+i)>n1, preds[i-1], tmpnewdata[L+i,sub])
        }
        para <- oneP$fcst[[1]][2:4]
        R2 <- R_squared_hq(tmpnewdata[(n2-L+2):n1,sub],fitted(vmdl)[,sub]+tmpnewdata[2:L,sub])
        
        list(R2=R2,preds=preds,para=para)
}


