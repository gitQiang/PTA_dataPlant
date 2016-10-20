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
                #sub <- sapply(2:ncol(newdata), function(i) max(diff(which(!is.na(newdata[,i])),1)) <=1 ) ## delete discontinuous factors
                #newdata <- newdata[ , c(TRUE,sub),drop=FALSE]
                
                ### add time variable
                tmp <- colnames(newdata)[-1]
                newdata <- cbind(newdata[,1],1:nrow(newdata),newdata[,-1])
                colnames(newdata) <- c("Target","Time",tmp)
                
                jobid <- jobTrace(jobid,trace1) # 2: the regression model with arima errors
                
                #newdata <- newdata[!is.na(newdata[,1]), ]
                #vNA <- rowSums(is.na(newdata))
                #vNA <- vNA[1:(which(vNA==min(vNA))[1]-1)]
                #dNA <- sort(unique(vNA),decreasing = TRUE)
                
                #i=1
                #startT <- which(vNA==dNA[i])[1]
                #tmpnewdata <- newdata[startT:nrow(newdata), !is.na(newdata[startT, ])]
                #endT <- nrow(tmpnewdata) #which(rownames(newdata)=="p1")
                #tmpnewdata <- tmpnewdata[1:(endT-1), !is.na(tmpnewdata[endT-1, ])]
                tmpnewdata <- newdata[!is.na(newdata[,1]), ]
                tmpnewdata <- tmpnewdata[-c(1:max(fre,2)), ]
        }else{
                subcols <- c("Target","CotlookA指数","现货价原油中国大庆环太平洋") 
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
                #sub <- sapply(2:ncol(newdata), function(i) max(diff(which(!is.na(newdata[,i])),1)) <=1 ) ## delete discontinuous factors
                #newdata <- newdata[ ,c(TRUE,sub)]
                
                ### add time variable
                tmp <- colnames(newdata)[-1]
                newdata <- cbind(newdata[,1],1:nrow(newdata),newdata[,-1])
                colnames(newdata) <- c("Target","Time",tmp)
                
                jobid <- jobTrace(jobid,trace1) # 2: the regression model with arima errors
                
                # vNA <- rowSums(is.na(newdata))
                # vNA <- vNA[1:(which(vNA==min(vNA))[1]-1)]
                # dNA <- sort(unique(vNA),decreasing = TRUE)
                # i=1
                # startT <- max(which(vNA==dNA[i])[1], min(which(!is.na(newdata[,1]))))
                # tmpnewdata <- newdata[startT:nrow(newdata), !is.na(newdata[startT, ])]
                tmpnewdata <- newdata[-c(1:max(fre,2)), ]
        }else{
                subcols <- c("Target","CotlookA指数","现货价原油中国大庆环太平洋") 
                tmpnewdata <- data[,subcols]
                tmpnewdata <- tmpnewdata[!is.na(tmpnewdata[,1]), ]
                # tmpnewdata <- tmpnewdata[rowSums(is.na(tmpnewdata)) <= 2, ]
                # if(any(is.na(tmpnewdata))){
                #         tmpsubs <- which(is.na(tmpnewdata),arr.ind = TRUE)
                #         tmpsubs <- tmpsubs[order(tmpsubs[,1]), ]
                #         for(kk in 1:nrow(tmpsubs)) tmpnewdata[tmpsubs[kk,1],tmpsubs[kk,2]] <- tmpnewdata[tmpsubs[kk,1]-1,tmpsubs[kk,2]]
                # }
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

good_p <- function(){
        library(RMySQL)
        library(DBI)
        con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
        choiceData <- dbReadTable(con,"hengyi_pta_choicedata")
        dbDisconnect(con)
        
        choice1 <- choiceData
        colnames(choice1) <- choice1[1, ]
        choice1 <- choice1[-1, ]
        
        ### based on the recent one year history price
        library(lubridate)
        day <- Sys.Date()
        day1 <- gsub(year(day),year(day)-1,day)
        
        tmp <- choice1[, c(1,which(colnames(choice1)=="市场价.PTA.对苯二甲酸."))]
        tmp <- tmp[as.Date(tmp[,1]) >= day1, ]
        tmpG <- groupDate(tmp[,1])
        
        pV <- sapply(1:ncol(tmpG), function(i){
                x <- as.numeric(tmp[,2])
                a1 <- aggregate(x[!is.na(x)],by=list(tmpG[!is.na(x),i]),median)[,2]
                median(abs(diff(a1)))/median(a1)
        })
        
        pV
}

groupDate <- function(date1){
        
        useDates <- as.Date(date1)
        
        w1 <- as.numeric(week(useDates))
        w1[w1<10] <- paste(0,w1[w1<10],sep="")
        m1 <- as.numeric(month(useDates))
        m1[m1<10] <- paste(0,m1[m1<10],sep="")
        
        gs1 <- paste(year(useDates),w1,sep="-")
        gs2 <- paste(year(useDates),m1,sep="-")
        gs3 <- paste(year(useDates),quarter(useDates),sep="-")
        
        useDates <- as.character(useDates)
        useDates <- cbind(useDates,gs1,gs2,gs3)
        useDates
}

precision_pred <- function(tmp,p=-1,sflag=1){
        if(p<0) p <- good_p()[sflag]
        
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
        if(dir=="inter" & ncol(data) > nrow(data)) dir="forward"
        
        
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
                        if(is.na(tmpCV)) tmpCV <- CV0+1000
                        if(tmpCV < CV0){fit0 <- tmpfit; CV0 <- tmpCV; Xnew <- c(Xnew,X[i]);}
                        if(length(Xnew) >= (nrow(data)-1)) break;
                        if(sum(abs(fitted(tmpfit)-data[,Y])) < 0.0001*mean(data[,Y]) ) break;
                }
        }
        
        if(dir=="inter"){
                inds <- colnames(data)
                inds0 <- inds
                while(TRUE){
                        fit <- lm( paste(Y," ~ .",sep=""), data=as.data.frame(data[,inds]))
                        sfit <- summary(fit)
                        if(all(sfit$coefficients[-1,4] < 0.001)){
                                Xnew <- setdiff(rownames(sfit$coefficients),"(Intercept)")
                                break
                        }
                        sels <- rownames(sfit$coefficients)[sfit$coefficients[,4] < 0.001]
                        if(length(sels)==0){
                                Xnew <- setdiff(rownames(sfit$coefficients)[rank(sfit$coefficients[,4]) < 5],"(Intercept)")
                                break
                        }
                        sels <- setdiff(sels,"(Intercept)")
                        inds <- c(Y,sels)
                        if(setequal(inds,inds0)){
                                Xnew <- inds
                                break
                        }else{ inds0 <- inds; }
                }
        }
        
        Xnew
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
