## best parameters for arima month data ==========

sub=1
per=20
fre=4
n2 <- length(ytrain)

resM <- matrix(0,9,8)
mea <- 1:9
ar <- c(0,1,2)
ma <- c(0,1,2)
k <- 1
for(i in 1:3){
        for(j in 1:3){
                preds <- 1:per ### predict values
                residuals <- 1:per
                
                ### best para for ARIMA
                for(n1 in (n2-per):(n2-1)){
                        
                        pdq <- c(ar[i],1,ma[j])
                        stsr <- arima(ts(ytrain[1:n1],frequency = fre),order=pdq)
                        res1 <- stsr$residuals
                        
                        
                        #### one step prediction
                        preds[n1-(n2-per-1)] <- predict(stsr, n.ahead=1)$pred[1]
                        residuals[n1-(n2-per-1)] <- ytrain[n1+1] - preds[n1-(n2-per-1)]
                }
                
                resM[k, ] <- c(summary(abs(residuals)),ar[i],ma[j])
                mea[k] <- (resM[k,3]*resM[k,4]*resM[k,5])^(1/3)
                k <- k+1
        }
}


### best para for SARMA ======= 
source("DataClean.R")
data0 <- FirstCheck()
colnames(data0)[1] <- "Target"
Y <- data0[,1]
x <- data0[, -1]
xtrain <- x[-nrow(x), ] ## train --> train and test
xpred <- x[nrow(x), ,drop=FALSE]
ytrain <- Y[-1]

onetrain <- cbind(ytrain,xtrain)


PDQM  <- matrix(c(0,1,0,1,1,0,
                  1,1,0,1,1,0,
                  2,1,0,1,1,0,
                  0,1,1,1,1,0,
                  1,1,1,1,1,0,
                  2,1,1,1,1,0,
                  0,1,2,1,1,0,
                  1,1,2,1,1,0,
                  2,1,2,1,1,0,
                  0,1,0,1,1,1,
                  1,1,0,1,1,1,
                  2,1,0,1,1,1,
                  0,1,1,1,1,1,
                  1,1,1,1,1,1,
                  2,1,1,1,1,1,
                  0,1,2,1,1,1,
                  1,1,2,1,1,1,
                  2,1,2,1,1,1,
                  0,1,0,0,1,1,
                  1,1,0,0,1,1,
                  2,1,0,0,1,1,
                  0,1,1,0,1,1,
                  1,1,1,0,1,1,
                  2,1,1,0,1,1,
                  0,1,2,0,1,1,
                  1,1,2,0,1,1,
                  2,1,2,0,1,1
),ncol = 6,byrow = TRUE)

fre=4
per=20
n2 <- nrow(onetrain)
resM <- matrix(Inf,27,per)
mea <- 1:27
for(ii in c(1:27)){
        preds <- 1:per ### predict values
        residuals <- 1:per
        pdq <- PDQM[ii, ]
        for(n1 in (n2-per):(n2-1)){
                stsr <- arima(ytrain[1:n1],order=pdq[1:3], seasonal = list(order=pdq[4:6], period=fre))
                res1 <- stsr$residuals
                
                #### one step prediction
                preds[n1-(n2-per-1)] <- predict(stsr, n.ahead=1)$pred[1]
                residuals[n1-(n2-per-1)] <- ytrain[n1+1] - preds[n1-(n2-per-1)]
        }
        print(summary(abs(residuals)))
        
        tmp <- summary(abs(residuals))
        mea[ii] <- (tmp[3]*tmp[4]*tmp[5])^(1/3)
        resM[ii, ] <- residuals
}

aa <- 1:27
for(ii in 1:27) aa[ii] <- quantile(abs(resM[ii,]),0.75)
PDQM[which.min(aa), ]


### best para for MLR + ARIMA=========
fre=4

source("DataClean.R")
data0 <- FirstCheck()
colnames(data0)[1] <- "Target"
Y <- data0[,1]
x <- data0[, -1]
xtrain <- x[-nrow(x), ] ## train --> train and test
xpred <- x[nrow(x), ,drop=FALSE]
ytrain <- Y[-1]

onetrain <- cbind(ytrain,xtrain)
colnames(onetrain) <- colnames(data0)
mlmr <- lm( "Target ~ .", data=as.data.frame(onetrain) )
res <- ts(mlmr$residuals,frequency = fre)
acf2(res)

bestARIMA <- function(ytrain,fre=12,per=20){
        sub=1
        n2 <- length(ytrain)
        
        resM <- matrix(Inf,9,8)
        mea <- rep(Inf,9)
        ar <- c(0,1,2)
        ma <- c(0,1,2)
        k <- 1
        for(i in 1:3){
                for(j in 1:3){
                        #if(!(i==2 & j==3)){
                        residuals <- 1:per
                        ### best para for ARIMA
                        for(n1 in (n2-per):(n2-1)){
                                pdq <- c(ar[i],1,ma[j])
                                stsr <- arima(ts(ytrain[1:n1],frequency = fre),order=pdq)
                                res1 <- stsr$residuals
                                
                                residuals[n1-(n2-per-1)] <- ytrain[n1+1] - predict(stsr, n.ahead=1)$pred[1]
                        }
                        
                        resM[k, ] <- c(summary(abs(residuals)),ar[i],ma[j])
                        mea[k] <- (resM[k,3]*resM[k,4]*resM[k,5])^(1/3)
                        #}
                        k <- k+1
                }
        }
        
        resM[which.min(mea), 7:8]
}

bestARIMA(res,4,20)

