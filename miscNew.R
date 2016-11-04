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

precision_pred <- function(tmp,p=-1,ytrain){
        if(p<0) p <- good_p(ytrain)
        
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

good_p <- function(ytrain, n.obs=100){
        
        ### based on the recent 20 observations 
        n <- length(ytrain)
        a1 <- ytrain[max(1,(n-n.obs+1)):n]
        
        # median(abs(diff(a1)))/median(a1)
        mean(abs(diff(a1)))/mean(a1)
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

