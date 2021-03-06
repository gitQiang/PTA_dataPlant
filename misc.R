csv_clean <- function(filename,wfile=""){
        
        ## file format: date and other indexs
        oneTable <- read.csv(filename,strip.white=TRUE)
        
        ### repalce special chars
        oneTable[,1] <- gsub("年","-",oneTable[,1])
        oneTable[,1] <- gsub("月","-",oneTable[,1])
        oneTable[,1] <- gsub("日","",oneTable[,1])
        oneTable[,1] <- gsub("/","-",oneTable[,1])
        for(j in 2:ncol(oneTable)) oneTable[,j] <- gsub(",","",oneTable[,j])
        
        ### date order and format
        n.unit <- length(unlist(strsplit(oneTable[1,1],"-")))
        if(n.unit==1){ ## in year unit
                oneTable[,1] <- paste(oneTable[,1],"-1-1",sep="")   
        }else if(n.unit==2){
                oneTable[,1] <- paste(oneTable[,1],"1",sep="")          
        }
        date <- as.Date(oneTable[,1])
        oneTable[,1] <- paste(year(date),month(date),day(date),sep="-")
        oneTable <- oneTable[order(date), ]
        
        if(wfile!="") write.csv(oneTable,file=wfile,row.names=FALSE,quote = FALSE)
        
        oneTable
        
}

data_filling <- function(PTA_data){
        library(zoo)
        oneM <- PTA_data[,-1]
        oneM[oneM==""] <- NA
        mode(oneM) <- "numeric"
        
        ### delete all NA columns
        subf <- sapply(1:ncol(oneM), function(i) all(is.na(oneM[,i])))
        oneM <- oneM[,!subf]
        n <- nrow(oneM)

        dataM <- sapply(1:ncol(oneM), function(i){
                if(anyNA(oneM[,i])){
                        subna <- which(!is.na( oneM[ ,i] ))
                        if(subna[1] > 1) oneM[1:(subna[1]-1),i] <- oneM[subna[1],i]    
                        j=length(subna);
                        if(subna[j] < n) oneM[(subna[j]+1):n,i] <- oneM[subna[j],i]
                        oneM[,i] <- na.approx(oneM[,i],maxgap=n,na.rm=FALSE)
                }
        })
        dataM <- cbind(PTA_data[,1],dataM)
        
        dataM
}

groupPredict <- function(data,i){
        
        useDates <- as.Date(rownames(data))
        if(i==1){
                tmpdata <- data
        }else if(i==2){
                gs <- paste(year(useDates),week(useDates),sep="-")
                ws <- unique(gs)
        }else if(i==3){
                gs <- paste(year(useDates),month(useDates),sep="-")
        }else if(i==4){
                gs <- paste(year(useDates),quarter(useDates),sep="-")
        }
        
        if(i > 1){
                tmpdata <- sapply(1:ncol(data), function(i)  sapply(unique(gs), function(ix) mean(data[gs==ix,i])) )
                colnames(tmpdata) <- colnames(data)                
        }
        
        if(i==2) rownames(tmpdata) <- ws
        
        tmpdata
}

oneDimPredict <- function(data,targetIndex,fre,per,sflag,model,trace1=1,trans=0){
        
        jobid <- 6
        jobid <- jobTrace(jobid,trace1)
        
        if(model <= 7){
                sdv <- sapply(1:ncol(data), function(i) sd(data[!is.na(data[,i]),i])) ## delete constant columns
                data <- data[, sdv!=0]
                newdata <- timelag_data(data,targetIndex,fre=fre)$newdata ## time lags for all factors
                sub <- sapply(2:ncol(newdata), function(i) max(diff(which(!is.na(newdata[,i])),1)) <=1 ) ## delete discontinuous factors
                newdata <- newdata[ , c(TRUE,sub),drop=FALSE]
                
                ### add time variable
                tmp <- colnames(newdata)[-1]
                newdata <- cbind(newdata[,1],1:nrow(newdata),newdata[,-1])
                colnames(newdata) <- c("Target","Time",tmp)
                
                jobid <- jobTrace(jobid,trace1) # 2: the regression model with arima errors
                
                newdata <- newdata[!is.na(newdata[,1]), ]
                vNA <- rowSums(is.na(newdata))
                vNA <- vNA[1:(which(vNA==min(vNA))[1]-1)]
                dNA <- sort(unique(vNA),decreasing = TRUE)
                
                i=1
                startT <- which(vNA==dNA[i])[1]
                tmpnewdata <- newdata[startT:nrow(newdata), !is.na(newdata[startT, ])]
                endT <- nrow(tmpnewdata) #which(rownames(newdata)=="p1")
                tmpnewdata <- tmpnewdata[1:(endT-1), !is.na(tmpnewdata[endT-1, ])]
        }else{
               subcols <- c("Target","Cotlook.A指数","现货价.原油.中国大庆..环太平洋") 
               tmpnewdata <- data[,subcols]
               tmpnewdata <- tmpnewdata[!apply(tmpnewdata,1,FUN = anyNA), ]
               jobid <- jobid+1
        }
        

        jobid <- jobTrace(jobid,trace1)
        ### output
        perform <- list();k=1;
        perform[[k]] <- Models(model,sflag=sflag,tmpnewdata,sub=targetIndex,per=per,fre=fre)
        k <- k+1
        pseudoR <- pseudoPredict(tmpnewdata,per,targetIndex)
        perform[[k]] <- pseudoR
        
        perform
}

PredictPTA <- function(data,targetIndex,fre,per,sflag,model,trace1=1,trans=0){
        
        jobid <- 6
        jobid <- jobTrace(jobid,trace1)
        
        if(model<=15){
                sdv <- sapply(1:ncol(data), function(i) sd(data[!is.na(data[,i]),i])) ## delete constant columns
                data <- data[, sdv!=0]
                newdata <- timelag_data(data,targetIndex,fre=fre)$newdata ## time lags for all factors
                sub <- sapply(2:ncol(newdata), function(i) max(diff(which(!is.na(newdata[,i])),1)) <=1 ) ## delete discontinuous factors
                newdata <- newdata[ ,c(TRUE,sub)]
                
                ### add time variable
                tmp <- colnames(newdata)[-1]
                newdata <- cbind(newdata[,1],1:nrow(newdata),newdata[,-1])
                colnames(newdata) <- c("Target","Time",tmp)
                
                jobid <- jobTrace(jobid,trace1) # 2: the regression model with arima errors
                
                vNA <- rowSums(is.na(newdata))
                vNA <- vNA[1:(which(vNA==min(vNA))[1]-1)]
                dNA <- sort(unique(vNA),decreasing = TRUE)
                i=1
                startT <- max(which(vNA==dNA[i])[1], min(which(!is.na(newdata[,1]))))
                tmpnewdata <- newdata[startT:nrow(newdata), !is.na(newdata[startT, ])]
        }else{
                subcols <- c("Target","Cotlook.A指数","现货价.原油.中国大庆..环太平洋") 
                tmpnewdata <- data[,subcols]
                tmpnewdata <- tmpnewdata[!is.na(tmpnewdata[,1]), ]
                tmpnewdata <- tmpnewdata[rowSums(is.na(tmpnewdata)) <= 2, ]
                if(any(is.na(tmpnewdata))){
                        tmpsubs <- which(is.na(tmpnewdata),arr.ind = TRUE)
                        tmpsubs <- tmpsubs[order(tmpsubs[,1]), ]
                        for(kk in 1:nrow(tmpsubs)) tmpnewdata[tmpsubs[kk,1],tmpsubs[kk,2]] <- tmpnewdata[tmpsubs[kk,1]-1,tmpsubs[kk,2]]
                }
                tmpnewdata <- rbind(tmpnewdata,matrix(NA,fre,3))
                jobid <- jobid + 1
        }
        
        
        jobid <- jobTrace(jobid,trace1)
        ### output
        perform <- list();k=1;
        perform[[k]] <- Models(model,sflag=sflag,tmpnewdata,sub=1,per=per,fre=fre)
        k <- k+1
        
        if(model %in% c(9,10,11,15,16)){per=fre;}else{per <- sum(is.na(tmpnewdata[,1]));}
        pseudoR <- pseudoPredict(tmpnewdata,per=per,targetIndex,2)
        perform[[k]] <- pseudoR
        
        perform
}

plot_testing <- function(obs,preds,labs,main=""){
        ylab="Price";xlab="";ord="topleft";
        ymax <- 1.1*max(c(obs,preds))
        ymin <- 0.9*min(c(obs,preds))
        par(mai=c(1.2,1.2,1,1))
        plot(obs,col=1,type="b",ylim=c(ymin,ymax),ylab=ylab,xlab=xlab,xaxt="n",lwd=3,main=main)
        lines(preds,col=2,type="b",lwd=3)
        axis(1,at=1:length(obs),labels =FALSE)  
        
        pos <- 1:length(obs)-length(obs)/100
        if(length(pos) > 20){pos <- pos[seq(1,length(pos),length.out=20)];labs <- labs[seq(1,length(labs),length.out=20)];}
        text(pos, par("usr")[3]-0.11*(ymax-ymin), labels = labs, srt = 90, pos = 1, xpd = TRUE)
        legend(ord,legend=c("Observed","Prediction"),col=1:2,lwd=2)         
}

precision_pred <- function(tmp,p=0.03){
        n <- length(tmp$preds)
        dobs <- diff(tmp$obs,1)
        dpred <- diff(tmp$preds,1)
        sub1 <- abs(tmp$residuals)/tmp$obs < p
        sub2 <- dobs * dpred >= 0
        
        s1 <- sum(sub1)/n  ### error smaller than p
        s2 <- sum(sub2)/(n-1) ### trend correct
        s3 <- sum(sub1 & c(TRUE,sub2) )/n ### both
        s4 <- as.vector(summary(abs(tmp$residuals))) ### residual summary
        s5 <- as.vector(summary(tmp$R2)) ### R2 summary
        
        list(s1=s1,s2=s2,s3=s3,s4=s4,s5=s5)
}

pseudoPredict <- function(tmpdata,per,targetIndex=1,flag=1){
        R2 <- 1:per
        preds <- 1:per
        residuals <- 1:per
        n2 <- nrow(tmpdata)
        if(flag==2){
                tmpdata[is.na(tmpdata[,targetIndex]), targetIndex] <- tmpdata[max(which(!is.na(tmpdata[,targetIndex]))),targetIndex]       
        }
        
        for(n1 in (n2-per):(n2-1)){
                R2[n1-(n2-per-1)] <- R_squared_hq(tmpdata[2:n1,targetIndex],tmpdata[1:(n1-1),targetIndex])
                preds[n1-(n2-per-1)] <- tmpdata[n1,targetIndex]
                residuals[n1-(n2-per-1)] <- tmpdata[n1+1,targetIndex] - preds[n1-(n2-per-1)]
        }
       
        list(obs=tmpdata[(n2-per+1):n2,targetIndex],R2=R2,preds=preds,residuals=residuals,para=-1,labs=rownames(tmpdata)[(n2-per+1):n2])
}

sarima_paraNew <- function(x,fre=10,nlag=0){
        
        if(!is.ts(x)) x <- as.ts(x,frequency=fre)
        if(nlag==0) nlag <- min(length(x) - fre+1,200)
        ## reference: http://people.duke.edu/~rnau/arimrule.htm
        sd=1; ## one fixed value !!!!
        maxd <- 5
        
        tmp <- acf2_hq(x,max.lag=nlag)
        d=0;
        if(all(tmp[1:5,"ACF"]>0)){
                sdd <- sapply(1:maxd, function(i) sd(diff(x,i)))
                d <- which.min(sdd)
        }
        
        if(d>0){
                dx <- diff(x,d)
                tmp <- acf2_hq(dx,max.lag = min(nlag,length(dx)-2))
        }else{ dx <- x; }
        
        dashV <- -1/length(dx) + 1.96/sqrt(length(dx))
        p <- which(abs(tmp[,"PACF"])<dashV)[1] - 1
        p <- ifelse(p<0,0,p)
        
        q <- which(abs(tmp[,"ACF"])<dashV)[1] - 1
        q <- ifelse(q<0,0,q)
        
        peak0 <- which(tmp[,"ACF"] > dashV)
        dpeak0 <- diff(peak0,1)
        fres0 <- dpeak0[dpeak0 > fre/2 & dpeak0 < 1.5*fre]
        sma <- length(fres0)
        peak1 <- which(tmp[,"PACF"] > dashV)
        dpeak1 <- diff(peak1,1)
        fres1 <- dpeak1[dpeak1 > fre/2 & dpeak1 < 1.5*fre]
        sar <- length(fres1)
        fres <- unique(c(fres0,fres1))
        
        if(fre==1){sar=0;sma=0;fres=1;}
        
        minBIC <- Inf
        pdq <- c(p,d,q,0,sd,0,fre)
        for(f in fres){
                tmp <- auto.arima(ts(x,frequency = f), d=d, D=sd, max.p=min(max(p,1),3), max.q=min(max(q,1),3), max.P=min(max(sar,1),2), max.Q=min(max(sma,1),2), max.d=d, max.D=sd, start.p=0, start.q=0, start.P=0, start.Q=0, seasonal=TRUE, ic="bic",stepwise = FALSE)
                onebic <- BIC(tmp)
                if(onebic < minBIC){ minBIC <- onebic; pdq <- tmp$arma[c(1,6,2,3,7,4,5)]; }
        }
        
        pdq
}

arima_paraNew <- function(x,fre=10,nlag=0){
        
        #if(!is.ts(x)) x <- as.ts(x,frequency=fre)
        if(nlag==0) nlag <- min(length(x) - fre+1,200)
        
        d=1
        fres <- max((fre-2),1):(fre+2)
        minBIC <- 1e9
        pdq <- c(0,1,0,fre)
        for(f in fres){
                tmp <- auto.arima(ts(x,frequency = f), d=d, max.p=3, max.q=3, max.d=d, start.p=0, start.q=0, seasonal=FALSE, ic="bic",stepwise = FALSE)
                onebic <- BIC(tmp)
                if(onebic < minBIC){ minBIC <- onebic; pdq <- c(tmp$arma[c(1,6,2)],f); }
        }
        
        pdq
}

stepCV_hq <- function(data,sub=1,cvf=1,dir="backward"){
        ### which measure is used: 1:CV;2:AIC;3:AICc;4:BIC;5:AdjR2 
        
        Y <- colnames(data)[sub]
        X <- colnames(data)[-sub]
        CVs <- sapply(X, function(i) CV(lm( paste(Y,"~",i,sep=""),  data=as.data.frame(data) ))[cvf])
        if(cvf==5){CVs = 1-CVs;}
        
        
        if(dir=="backward"){
                fit0 <- lm(paste(Y,"~.",sep=""), data=as.data.frame(data))
                print(CV(fit0))
                CV0 <- ifelse(cvf<=4,CV(fit0)[cvf],1-CV(fit0)[cvf])
                tmp <- sort(CVs,decreasing = TRUE, index.return=TRUE)
                X <- X[tmp$ix]
                Xnew <- X;
                for(i in 1:length(X)){
                        tmpfit <-  lm(paste(Y," ~ ",paste(setdiff(Xnew,X[i]),sep="",collapse=" + "),sep=""), data=as.data.frame(data))
                        tmpCV <- ifelse(cvf<=4,CV(tmpfit)[cvf],1-CV(tmpfit)[cvf])
                        print(tmpCV)
                        print(CV0)
                        if(tmpCV < CV0){fit0 <- tmpfit; CV0 <- tmpCV; Xnew <- setdiff(Xnew,X[i]);}
                }
        }
        
        if(dir=="forward"){
                tmp <- sort(CVs, index.return=TRUE)
                X <- X[tmp$ix]
                Xnew <- X[1];
                fit0 <-  lm(paste(Y," ~ ",X[1],sep=""), data=as.data.frame(data))
                CV0 <- ifelse(cvf<=4,CV(fit0)[cvf],1 - CV(fit0)[cvf])
                for(i in 2:length(X)){
                        tmpfit <-  lm(paste(Y," ~ ",paste(c(Xnew,X[i]),sep="",collapse=" + "),sep=""), data=as.data.frame(data))
                        tmpCV <- ifelse(cvf<=4,CV(tmpfit)[cvf],1-CV(tmpfit)[cvf])
                        if(tmpCV < CV0){fit0 <- tmpfit; CV0 <- tmpCV; Xnew <- c(Xnew,X[i]);}
                }
        }
        
        Xnew
}

timelag_data <- function(data,targetIndex,per=20,fre=12,n.model=3){
        newdata <- c(data[,targetIndex],rep(NA,fre))
        lags <- c()
        newNames <- c()
        
        cNames <- colnames(data)
        xcols <- (1:ncol(data))[-targetIndex]
        for(i in xcols){
                tmpsubs <- delete_NA(tof(data[,targetIndex]),tof(data[,i]))
                tmpdata <- data[tmpsubs,c(targetIndex,i)]
                
                tmpccf <- ccf(tmpdata[,2],tmpdata[,1],lag.max=min(30,nrow(data)),plot=FALSE,type="correlation")
                lag <- tmpccf$lag[which.max(abs(tmpccf$acf[tmpccf$lag <= (fre+1)]))]
                
                if(length(lag)==0) lag=0
                Xone <- data[,i,drop=FALSE]
                None <- cNames[i]
                
                if(lag==0){
                        lag = -1
                }else if(lag > 0){
                        lag <- lag - fre
                        if(lag==0) lag <- -fre
                        if(lag>0) lag <- -1
                }
                
                if(fre==1 & lag==-1) lag  <- -2  ## 日度数据至少滞后两天
                
                tmp <- c(rep(NA,-lag), Xone)
                if(length(tmp) < (nrow(data)+fre)) tmp <- c(tmp,rep(NA,(nrow(data)+fre)-length(tmp)))
                newdata <- cbind(newdata,tmp[1:(nrow(data)+fre)]);
                newNames <- c(newNames,None)
                lags <- c(lags,lag)
        }
        colnames(newdata) <- c("Target",newNames)
        rownames(newdata) <- c(rownames(data), paste("p",1:fre,sep=""))
        tmp <- fraction_NA(t(newdata),pNA=1)
        newdata <- t(tmp)
        lags <- cbind(xcols,lags)
        
        list(newdata=newdata,lags=lags)    
}

delete_NA <- function(oneV,twoV=NULL){
        if(length(twoV)==0){
                oneV <- tof(oneV)
                tmpsubs <- which(is.na(oneV))
                tmpsubs <- c(0,tmpsubs,length(oneV)+1)
                n.na <- length(tmpsubs)
                onesub <- which.max(tmpsubs[2:n.na]-tmpsubs[1:(n.na-1)])
                (tmpsubs[onesub]+1):(tmpsubs[onesub+1]-1)
        }else{
                tmpsubs <- which(is.na(oneV) | is.na(twoV))
                tmpsubs <- c(0,tmpsubs,length(oneV)+1)
                n.na <- length(tmpsubs)
                onesub <- which.max(tmpsubs[2:n.na]-tmpsubs[1:(n.na-1)])
                (tmpsubs[onesub]+1):(tmpsubs[onesub+1]-1)
        }
}

delete_NA2 <- function(data){
        subindex <- which(apply(data,1,FUN = anyNA))
        subindex <- c(0,subindex,length(subindex)+1)
        start <- which.max(diff(subindex))
        (subindex[start]+1):(subindex[start+1]-1)
}

tof <- function(oneV,na.rm=FALSE,fill=FALSE,f="mean"){
        ## replace function of as.numeric
        options(warn=-1)
        oneV <- as.numeric(oneV)
        options(warn=0)
        
        if(na.rm) oneV <- oneV[!is.na(oneV)]
        if(fill){
                if(f=="mean") oneV[is.na(oneV)] <- mean(oneV[!is.na(oneV)])
                if(f=="min") oneV[is.na(oneV)] <- min(oneV[!is.na(oneV)])
                if(f=="median") oneV[is.na(oneV)] <- median(oneV[!is.na(oneV)])
                if(f=="zero") oneV[is.na(oneV)] <- 0
        }
        oneV
}

R_squared_hq <- function(y,y1){
        y_ <- mean(y)
        1-(sum((y-y1)^2)/sum((y-y_)^2))
}

acf2_hq <- function(series, max.lag = NULL){
        
        num = length(series)
        if (num > 49 & is.null(max.lag)) 
                max.lag = ceiling(10 + sqrt(num))
        if (num < 50 & is.null(max.lag)) 
                max.lag = floor(5 * log10(num))
        if (max.lag > (num - 1)) 
                stop("Number of lags exceeds number of observations")
        ACF = stats::acf(series, max.lag, plot = FALSE)$acf[-1]
        PACF = stats::pacf(series, max.lag, plot = FALSE)$acf
        
        ACF <- round(ACF, 2)
        PACF <- round(PACF, 2)
        return(cbind(ACF, PACF))
}

#=======job tracing and write log file
jobTrace <- function(job,trace1=0){
        
        logFile = "PTArunning.log"
        jobNames <- c("Data Loading ... ...","Start Day Prediction ... ...","Start Week Prediction ... ...","Start Month Prediction ... ...", "Start Season Prediction ... ...", "Start Data integrating ... ...","Start Multiple factor selection ... ...","Start PTA-PPM module ... ...","End PTA-PPM module ... ...","End One Run ... ...","Start One Run ... ...","Initialization ... ...")
        
        if(trace1!=0) print(jobNames[job])
        printstr <- paste(Sys.time(), Sys.info()["user"],jobNames[job],sep="\t")
        cat(printstr, file=logFile, append=TRUE, sep = "\n")
        
        if(job==10){
                cat("", file=logFile, append=TRUE, sep = "\n")
                cat("", file=logFile, append=TRUE, sep = "\n")
        }
        
        job <- job+1
        job
}
