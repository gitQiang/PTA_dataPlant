rm(list=ls())
gc()

setwd("D:/code/PTA_dataPlant")
source("misc.R")
source("PTA_prediction.R")
library('vars')
library('tsDyn')
library(lubridate)
library(zoo)

## PTA, PET(plastic), Oil, Cotton, 
PTA_data <- getUpdateData()
PTA_OIL <- PTA_data[,c("指标名称","市场价.PTA.对苯二甲酸.","现货价.原油.中国大庆..环太平洋")]
PET <- csv_clean("D:/data/恒逸/data/ChoiceData/PET恒逸出厂价.csv")
Cotton <- csv_clean("D:/data/恒逸/data/ChoiceData/棉花价格/棉花CotlookA指数.csv")
timeDays <- intersect(PTA_OIL[,1],intersect(PET[,1],Cotton[,1]))
data2 <- cbind(PTA_OIL[match(timeDays,PTA_OIL[,1]),2:3],PET[match(timeDays,PET[,1]), 2],Cotton[match(timeDays,Cotton[,1]), 2])
data2 <- as.matrix(data2)
colnames(data2) <- c("PTA","OIL",'PET','Cotton')
rownames(data2) <- as.character(timeDays)
mode(data2) <- "numeric"
for(j in 1:ncol(data2)) data2[,j] <- na.approx(data2[,j],maxgap=4,na.rm=FALSE)
for(j in 1:ncol(data2)) data2[is.na(data2[,j]), j] <- mean(data2[!is.na(data2[,j]),j])


plot(data2[,1],type='l',col=1,xlab='date',ylab='price',ylim=c(min(data2)-1,max(data2)+1))
lines(data2[,2],type='l',col=2)
lines(data2[,3],type='l',col=3)
lines(data2[,4],type='l',col=4)
legend('topright',legend=c("PTA","OIL",'PET','Cotton'),col=1:4,lwd=3)

# VAR model
diffdata <- data2[3:nrow(data2), ] - data2[1:(nrow(data2)-2), ]#diff(data2) #!!!!
diffdata <- diffdata[,-3]

n.index <- ncol(diffdata)
L<-nrow(diffdata)
totalfcst<-c()
for(i in (L-50):(L-1)){
  vmdl<-VAR(diffdata[1:i,],p=1,type='const')
  res<-predict(vmdl,n.ahead=1)
  fcst<-c()
  for(j in 1:n.index)fcst[j]<-res$fcst[[j]][1]+data2[i+1,j]
  totalfcst<-rbind(totalfcst,fcst)
}
indexname<-c('PTAPrice',"OILPrice","PETPrice","CottonPrice")
#for(j in 1:4) plot_testing(data2[(L-48):(L+1),j],totalfcst[,j],labs=rownames(data2)[(L-47):(L+2)],main=indexname[j])
j=1;plot_testing(data2[(L-48):(L+1),j],totalfcst[,j],labs=rownames(data2)[(L-47):(L+2)],main=indexname[j])

tmp <- list()
tmp$obs <- data2[(L-48):(L+1),1]
tmp$preds <- totalfcst[,1]
tmp$residuals <- tmp$obs - tmp$preds
tmp$R2 <- runif(50,0,1)
precision_pred(tmp)



#VECM model
# totalfcst<-c()
# for(i in (L-49):L){
#   mdl<-VECM(data2,lag=0,include='both',estim='2OLS')
#   fcst <-predict(mdl,n.ahead=1)
#   totalfcst<-rbind(totalfcst,fcst)
# }
# for(j in 1:4) plot_testing(data2[(L-48):L+1,j],totalfcst[,j],labs=rownames(data2)[(L-48):(L+1)],main=indexname[j])

