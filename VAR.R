source("misc.R")
library('vars')
library('tsDyn')
load('data_2002_2015')
targetlist=c(11,118,121,124)
data<-cbind(data[,targetlist],data[,-targetlist])

#tmpdata <- groupPredict(data,i)
data1<-sapply(1:ncol(data), function(i) na.approx(tof(data[,i]),maxgap=5,na.rm=FALSE))
colnames(data1)<-colnames(data)
rownames(data1)<-rownames(data)
noNAindex<-delete_NA2(data1[,1:4])
data2<-data1[noNAindex,1:4]
data3 <- fraction_NA(data1,pNA=0.5)
plot(data2[,1],type='l',col=1,xlab='date',ylab='price',ylim=c(min(data2)-1,max(data2)+1))
lines(data2[,2],type='l',col=2)
lines(data2[,3],type='l',col=3)
lines(data2[,4],type='l',col=4)
legend('topright',legend=c("PTA","POY",'DTY','FDY'),col=1:4,lwd=3)

# VAR model
diffdata<-diff(data2)
L=length(diffdata[,1])
totalfcst<-c()
for(i in (L-50):(L-1)){
  vmdl<-VAR(diffdata[1:i,],p=1,type='const')
  res<-predict(vmdl,n.ahead=1)
  fcst<-c()
  for(j in 1:4)fcst[j]<-res$fcst[[j]][1]+data2[i+1,j]
  totalfcst<-rbind(totalfcst,fcst)
}
indexname<-c('PTAPrice',"POYPrice","DTYPrice","FDYPrice")
for(j in 1:4){
  plot_testing(data2[(L-48):(L+1),j],totalfcst[,j],labs=rownames(data2)[(L-48):(L+1)])
        print(indexname[j])
}
#VECM model
totalfcst<-c()
for(i in (L-49):L){
  mdl<-VECM(data2,lag=0,include='both',estim='2OLS')
  res<-predict(mdl,n.ahead=1)
  fcst<-c()
  for(j in 1:4)fcst[j]<-res[j]
  totalfcst<-rbind(totalfcst,fcst)
}
for(j in 1:4){
  plot_testing(data2[(L-48):L+1,j],totalfcst[,j],labs=rownames(data2)[(L-48):(L+1)])
        print(indexname[j])
}
