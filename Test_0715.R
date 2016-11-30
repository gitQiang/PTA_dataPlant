### test moving average method for day prediction ==========
setwd("D:/code/PTA_dataPlant")
rm(list=ls())
gc()
library(TTR)
load("data_2002_2015")

sub = 11
ts <- as.ts(data[!is.na(data[,sub]),sub])

L=500
n <- (length(ts)-L):length(ts)

errM <- matrix(-1,3,20)
for(i in 2:21){
        a1 <- SMA(ts,i)
        a2 <- EMA(ts,i)
        a3 <- WMA(ts,i)
        errM[1, i-1] <- sqrt(sum((ts[n]-a1[n-2])^2))
        errM[2, i-1] <- sqrt(sum((ts[n]-a2[n-2])^2))
        errM[3, i-1] <- sqrt(sum((ts[n]-a3[n-2])^2))
}
errM <- errM/L
which(errM==min(errM),arr.ind = TRUE)

## chose WMA and length = 2

### ==========================================================================
source("D:/code/PTA_dataPlant/misc.R")
library(lubridate) 
cost <- csv_clean("data_v2/PTA成本.csv")
Price <- csv_clean("data_v1/PTAPriceIn.csv")

oDays <- intersect(cost[,1],Price[,1])
aa <- cbind(oDays, Price[match(oDays,Price[,1]),2], cost[match(oDays,cost[,1]),2])
aa <- aa[order(as.Date(aa[,1])), ]
write.csv(aa,file="data_v2/PTAPrice_cost.csv",row.names = FALSE, quote = FALSE)


source("D:/code/src_scripts/xls2csv_hq.R")
filenames <- list.files("D:/data/恒逸/恒逸共享/调研数据整理/data_v2",".xls",full.names = TRUE)

xls2csv_hq(filenames)


source("D:/code/PTA_dataPlant/misc.R")
library(lubridate) 
inMEGPrice <- csv_clean("D:/data/恒逸/恒逸共享/调研数据整理/data_v2/MEG内盘.csv")
PTAPrice <- csv_clean("D:/data/恒逸/恒逸共享/调研数据整理/data_v1/PTAPriceIn.csv")

InterDays <- intersect(inMEGPrice[,1],PTAPrice[,1])
y <- as.numeric(PTAPrice[match(InterDays,PTAPrice[,1]), 2])
x <- as.numeric(inMEGPrice[match(InterDays,inMEGPrice[,1]), 2])

fit <- lm(y~x,data=data.frame(y=y,x=x))
plot(fit$residuals,type="l")

x0 <- 1:length(x)
plot(x0,y,col=1,type="l",xlab="",ylab="")
lines(x0,fitted(fit),col=2,type="l")
legend("topright",legend=c("PTA Price","Fitted"),col=1:2,lwd=1)



setwd("D:/data/恒逸/恒逸共享/调研数据整理/PTAPrices")
source("D:/code/PTA_dataPlant/misc.R")
library(lubridate) 
Price <- csv_clean("D:/data/恒逸/恒逸共享/调研数据整理/data_v1/PTAPriceIn.csv")
Hengyi <- csv_clean("PTA出厂价逸盛石化1.csv")
oDays <- intersect(Hengyi[,1],Price[,1])

aa <- cbind(oDays, Price[match(oDays,Price[,1]),2], Hengyi[match(oDays,Hengyi[,1]),2])
colnames(aa) <- c("Days","Price","Hengyi")
aa <- aa[order(as.Date(aa[,1])), ]
write.csv(aa,file="PTAPrice_Hengyi.csv",row.names = FALSE, quote = FALSE)


rm(list=ls())
gc()
setwd("D:/code/PTA_dataPlant")
source("PTA_prediction.R")
filenames=NULL
trace1=1
trans=0


data <- tmpdata1;#pers=fres
targetIndex=1;fre=fres[i];per=pers[i];sflag=i;model=j;sub=1;

data <- tmpdata1;#pers=fres
targetIndex=1;fre=fres[i];per=1;sflag=i;model=j;sub=1;trace1=1

####aaaaa===========================================
source('D:/code/PTA_dataPlant/misc.R')
source('D:/code/PTA_dataPlant/PTA_prediction.R')
a1 <- csv_clean("D:/data/恒逸/data/ChoiceData/原油库存量原油和石油产品不包括.csv")
PTA_data <- getUpdateData()
a2 <- PTA_data[,c("指标名称","现货价.原油.中国大庆..环太平洋")]

ts <- intersect(a1[,1],a2[,1])
b1 <- as.numeric(a1[,2])
b2 <- as.numeric(a2[,2])

b1 <- b1[match(ts,a1[,1])]
b2 <- b2[match(ts,a2[,1])]
#b2 <- b2 * 10000
b21 <- b2[!is.na(b2)]

b1 <- (b1-min(b1))/(max(b1)-min(b1))
b2 <- (b2-min(b21))/(max(b21)-min(b21))

plot(1:length(b1), b1,type="l")
lines(1:length(b2), b2,col=2,type="l")

rm(list=ls())
gc()
source('D:/code/PTA_dataPlant/misc.R')
source('D:/code/PTA_dataPlant/PTA_prediction.R')

PTA_data <- getUpdateData_v1()
data <- data_filling(PTA_data)


## 
per=50
#lv <- c(3154,2154,1154,600,300)
lv <- c(458,358,258,158,58)
res <- list()
tmpnewdata0 <- tmpnewdata

for(kk in 1:length(lv)){
        tmpnewdata <- tmpnewdata0[(nrow(tmpnewdata0)-lv[kk]+1):nrow(tmpnewdata0), 1,drop=FALSE]

        #### output 

        preds <- 1:per ### predict values
        residuals <- 1:per
        #### input info
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        for(n1 in (n2-per):(n2-1)){
                pdq <- arima_paraNew(tmpnewdata[1:n1,sub],fre=fre)
                stsr <- arima(ts(tmpnewdata[1:n1,sub],frequency = pdq[4]),order=pdq[1:3])
                
                #### one step prediction
                preds[n1-(n2-per-1)] <- predict(stsr, n.ahead=1)$pred[1]
                residuals[n1-(n2-per-1)] <- tmpnewdata[n1+1,sub] - preds[n1-(n2-per-1)]
                print(n1)
        }
        
        res[[kk]] <- residuals
}

plot(density(abs(res[[1]])),col="black")
lines(density(abs(res[[2]])),col="red")
lines(density(abs(res[[3]])),col="blue")
lines(density(abs(res[[4]])),col="green")
lines(density(abs(res[[5]])),col="yellow")

#load("ARIMA_Len")
for(kk in 1:5) print(median(abs(res[[kk]])))
for(kk in 1:5) print(sd(abs(res[[kk]])))

#save(res,file="ARIMA_LenWeek")

## VAR Len
per=50
#lv <- c(3154,2154,1154,600,300)
lv <- c(458,358,258,158,58)
res <- list()
tmpnewdata0 <- tmpnewdata

for(kk in 1:length(lv)){
        tmpnewdata <- tmpnewdata0[(nrow(tmpnewdata0)-lv[kk]+1):nrow(tmpnewdata0), ,drop=FALSE]
        
        library(vars)
        #### output 
        preds <- 1:per ### predict values
        residuals <- 1:per
 
        #### input info
        n2 <- nrow(tmpnewdata) ### input tmpnewdata length
        diffdata <- tmpnewdata[2:n2, ] - tmpnewdata[1:(n2-1), ];
        
        L <- nrow(diffdata)
        for(n1 in (L-per):(L-1)){
                vmdl <- VAR(diffdata[1:n1,],p=1,type='const')
                oneP <- predict(vmdl,n.ahead=1)
                preds[n1-L+per+1] <- oneP$fcst[[1]][1]+tmpnewdata[n1+1,sub]
                residuals[n1-L+per+1] <- tmpnewdata[n1+1+n2-L,sub] - preds[n1-L+per+1]
        }
        
        res[[kk]] <- residuals
}

save(res,file="VAR_Len")


aa <- c()
for(i in 1:332){
        if(sd(tmpdata1[,i]) > 3*mean(tmpdata1[,i])) aa <- c(aa,i)
}

