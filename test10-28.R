sub=1
tmpnewdata <- onetrain
per=20
n2 <- nrow(tmpnewdata)
preds <- 1:per ### predict values
residuals <- 1:per
source("miscNew.R")


### best para for ARIMA
for(n1 in (n2-per):(n2-1)){

        pdq <- c(2,1,3)
        stsr <- arima(ytrain[1:n1],order=pdq)
        res1 <- stsr$residuals

        
        #### one step prediction
        preds[n1-(n2-per-1)] <- predict(stsr, n.ahead=1)$pred[1]
        residuals[n1-(n2-per-1)] <- ytrain[n1+1] - preds[n1-(n2-per-1)]
}

summary(abs(residuals))


## add MLR
source("miscNew.R")
pdq <- c(2,1,3)
stsr <- arima(ytrain,order=pdq)
res1 <- stsr$residuals
subs <- FSel(xtrain,res1)
xtrain1 <- xtrain[ ,subs]

for(n1 in (n2-per):(n2-1)){
        
        pdq <- c(2,1,3)
        stsr <- arima(ytrain[1:n1],order=pdq)
        res <- stsr$residuals
        
        tmpdata <- cbind(res,xtrain1[1:n1, ])
        mlmr <- lm( paste(colnames(tmpdata)[1]," ~ .",sep=""), data=tmpdata )
        res1 <- mlmr$residuals
      
        #### one step prediction
        preds[n1-(n2-per-1)] <- sum(c(1,xtrain1[n1+1,]) * tof(as.vector(mlmr$coefficients),na.rm=FALSE,fill=TRUE,f="zero"))  +  predict(stsr, n.ahead=1)$pred[1]
        residuals[n1-(n2-per-1)] <- ytrain[n1+1] - preds[n1-(n2-per-1)]
}


summary(abs(residuals))

