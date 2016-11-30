sub=1
tmpnewdata <- onetrain
per=20
n2 <- nrow(tmpnewdata)
preds <- 1:per ### predict values
residuals <- 1:per
source("miscNew.R")

for(n1 in (n2-per):(n2-1)){
        
        tmpdata <- as.data.frame(tmpnewdata[1:n1,])
        xnew <- colnames(tmpdata)[-sub]
        mlmr <- lm( paste(colnames(tmpdata)[sub]," ~ .",sep=""), data=tmpdata )
        res <- ts(mlmr$residuals,frequency = fre)
        pdq <- c(0,1,1)
        stsr <- arima(res,order=pdq)
        res1 <- stsr$residuals
        
        
        #### one step prediction
        preds[n1-(n2-per-1)] <- sum(c(1,tmpnewdata[n1+1,xnew]) * tof(as.vector(mlmr$coefficients),na.rm=FALSE,fill=TRUE,f="zero"))  +  predict(stsr, n.ahead=1)$pred[1]
        residuals[n1-(n2-per-1)] <- tmpnewdata[n1+1,sub] - preds[n1-(n2-per-1)]
}

summary(abs(residuals))


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


per=30
n2 <- nrow(onetrain)
resM <- matrix(Inf,27,per)

for(ii in c(1:10,12:27)){
        preds <- 1:per ### predict values
        residuals <- 1:per
        pdq <- PDQM[ii, ]
        for(n1 in (n2-per):(n2-1)){
                stsr <- arima(ytrain[1:n1],order=pdq[1:3], seasonal = list(order=pdq[4:6], period=52))
                res1 <- stsr$residuals
                
                #### one step prediction
                preds[n1-(n2-per-1)] <- predict(stsr, n.ahead=1)$pred[1]
                residuals[n1-(n2-per-1)] <- ytrain[n1+1] - preds[n1-(n2-per-1)]
        }
        print(summary(abs(residuals)))
        
        resM[ii, ] <- residuals
}

aa <- 1:27
for(ii in 1:27) aa[ii] <- quantile(abs(resM[ii,]),0.75)
PDQM[which.min(aa), ]
save(PDQM, file="bestParaSARIMA")

