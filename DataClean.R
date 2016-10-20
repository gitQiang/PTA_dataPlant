getUpdateData_v2 <- function(){
        ### 读出wind, choice  data 和 天气数据
        library(RMySQL)
        library(DBI)
        con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
        
        tabs <- c("hengyi_pta_choicedata","hengyi_pta_choicedatav1","hengyi_pta_choicedatav2","hengyi_pta_winddatav2_1","hengyi_pta_winddatav2_2","hengyi_pta_winddatav2_3","hengyi_pta_winddatav2_4")
        tabList <- list()
        for(i in 1:length(tabs)){
                tabList[[i]] <- dbReadTable(con,tabs[i])
                
                if(i<=3){
                        tmp <- tabList[[i]]
                        colnames(tmp) <- tmp[1, ]
                        tmp <- tmp[-1, ]
                        tabList[[i]] <- tmp
                }
        }
        
        WindData <- dbReadTable(con,"hengyi_pta_winddata")
        WeatherData <- dbReadTable(con,"hengyi_pta_weatherdata")
        
        dbDisconnect(con)
        
        windDic <- c("时间","现货价:原油(中国大庆):环太平洋","产量:服装:当月值","中国棉花价格指数:328","中国棉花价格指数:527","中国棉花价格指数:229","Cotlook:A指数","产量:布:当月值","产量:布:当月同比","产量:聚酯:当月值","产量:聚酯:当月同比","产量:聚酯:累计值","产量:化学纤维:当月值","产量:化学纤维:当月同比")
        Wind1 <- WindData
        colnames(Wind1) <- windDic
        Wind1 <- Wind1[-1, ]
       
        Weather1 <- WeatherData[ , -22]
        colnames(Weather1) <- Weather1[1, ]
        Weather1 <- Weather1[-1, ]
        
        tabList[[i+1]] <- Wind1
        tabList[[i+2]] <- Weather1
        
        dd <- c()
        n.col <- 1
        cols <- c()
        for(i in 1:length(tabList)){
                dd <- c(dd,as.character(as.Date(tabList[[i]][,1])))
                n.col <- n.col + ncol(tabList[[i]]) - 1
                cols <- c(cols,colnames(tabList[[i]])[-1])
        }
        dd <- dd[!duplicated(dd)]
        unionDs <- as.character(dd[order(as.Date(dd))])
        Newdata <- matrix(NA,length(unionDs), n.col)
        Newdata[,1] <- unionDs
        k <- 1
        for(i in 1:length(tabList)){
                ttmp <- as.character(as.Date(tabList[[i]][,1]))
                ntmp <- ncol(tabList[[i]]) - 1
                Newdata[match(ttmp,Newdata[,1]), (k+1):(k+ntmp)] <- as.matrix(tabList[[i]][,-1])
                k <- k+ntmp
        }
        
        tmpcols <- c("time",cols)
        tmpcols <- gsub(":","",tmpcols)
        tmpcols <- gsub("\\(","",tmpcols)
        tmpcols <- gsub("\\)","",tmpcols)
        tmpcols <- gsub("\\.","",tmpcols)
        colnames(Newdata) <- tmpcols
        
        sub <- min(which(grepl("2010",Newdata[,1])))
        Newdata <- Newdata[sub:nrow(Newdata), ]
        ###Newdata <- Newdata[,!duplicated(colnames(Newdata))]
        
        Newdata
}

FirstCheck <- function(){
        
        source("DataClean.R")
        Newdata <- getUpdateData_v2()
        Newdata[Newdata==""] <- NA
        data0 <- delRep(Newdata)
   
        data1 <- trans2Week(data0)
        #data2 <- delRep(data1)
        ysub <- which(colnames(data1)=="市场价PTA对苯二甲酸")
        Y <- data1[,ysub]
        yna <- is.na(Y)
        Y <- Y[!yna]
        data2 <- data1[!yna, -ysub]
        
        ## outliers and NA indexs analysis
        data3 <- outlierAna(data2)
        # print(sum(is.na(data3))/(nrow(data3)*ncol(data3)))
        
        ## missing value fill
        data4 <- data_filling(data3)
        
        ## data normalization
        data5 <- dataNormal(data4)
        
        ## delete redantant factors
        data6 <- delRandant(data5,Y)
        
        data6

        # library(randomForest)
        # ## randomForest methods
        # fit.rf <- randomForest(Class ~ ., data=a1, importance=TRUE, proximity=TRUE)
        # fit.rf
        # plot(density(abs(y-fit.rf$predicted) ) )

}

delRep <- function(data){
        
        n <- ncol(data)
        nasub <- rep(FALSE,n)
        consub <- 1:n
        ss <- 1:n
        for(i in 1:n){
                tmp <- as.numeric(data[,i])
                consub[i] <- sd(tmp[!is.na(tmp)])==0
                ss[i] <- paste(data[,i],sep="",collapse = "_")
                nasub[i] <- all(is.na(data[,i]))
        }
        consub[is.na(consub)] <- FALSE
        depsub <- duplicated(ss)
        
        data <- data[,!(consub | depsub | nasub)]
        
        
        data
}

trans2Week <- function(data0){
        
        rownames(data0) <- data0[,1]
        data0 <- data0[ , -1]
        mode(data0) <- "numeric"
        
        useDates <- as.Date(rownames(data0))
        gs <- paste(year(useDates),week(useDates),sep="-")
        ws <- unique(gs)

        tmpdata <- sapply(1:ncol(data0), function(i)  sapply(ws, function(ix) median(data0[gs==ix,i],na.rm = TRUE)) )
        colnames(tmpdata) <- colnames(data0)
        rownames(tmpdata) <- ws
        
        tmpdata
}

outlierAna <- function(data2){
        ## 手动迭代的过程
        colwea <- c("MeanVisibilityKm","MaxVisibilityKm","MinVisibilitykM","MaxWindSpeedKmh","MeanWindSpeedKmh","MaxGustSpeedKmh")
        tmp <- data2[ ,colwea]
        tmp[tmp < 0] <- NA
        data2[ ,colwea] <- tmp
        data2[data2[,"S5409599"] > 300, "S5409599"] <- 300
        data2[data2[,"S5410166"] > 300, "S5410166"] <- 300
        
        # n <- ncol(data2)
        # tmp <- 1:n
        # for(i in 1:n){
        #         a1 <- data2[!is.na(data2[,i]),i]
        #         #tmp[i] <- sd(a1)/mean(abs(a1)) 
        #         tmp[i] <- max(abs(a1))/median(abs(a1))
        # }
        # plot(tmp)
        # 
        # none <- 662
        # colnames(data2)[none]
        # min(data2[!is.na(data2[,none]),none])
        # max(data2[!is.na(data2[,none]),none])
        # median(data2[!is.na(data2[,none]),none])
        
        tmp <- 1:ncol(data2)
        for(i in 1:ncol(data2)) tmp[i] <- sum(is.na(data2[,i]))
        # naRate <- 1:10
        # rs <- seq(1,0.1,-0.1)
        # for(i in 1:10){
        #         data3 <- data2[, tmp/nrow(data2) <= rs[i]]
        #         naRate[i] <- sum(is.na(data3))/(nrow(data3)*ncol(data3))
        # }
        # plot(naRate,type="b")
        
        data3 <- data2[, tmp/nrow(data2) <= 0.7]
        
        data3
}

data_filling <- function(data3){
        library(zoo)
        n <- nrow(data3)
        data4 <- sapply(1:ncol(data3), function(i){
                if(anyNA(data3[,i])){
                        subna <- which(!is.na( data3[ ,i] ))
                        if(subna[1] > 1) data3[1:(subna[1]-1),i] <- data3[subna[1],i]    
                        j=length(subna);
                        if(subna[j] < n) data3[(subna[j]+1):n,i] <- data3[subna[j],i]
                        data3[,i] <- na.approx(data3[,i],maxgap=n,na.rm=FALSE)
                }
                return(data3[,i])
        })
        colnames(data4) <- colnames(data3)
        
        data4
}

dataNormal <-  function(data4){
        # n <- ncol(data4)
        # ps <- 1:n
        # for(i in 1:n) ps[i] <- shapiro.test(data4[,i])$p.value
        
        # http://bbs.pinggu.org/thread-3352047-1-1.html
        # shapiro.test();#Shapiro-Wilk检验
        # library("nortest");
        # lillie.test() #Kolmogorov-Smirnov检验
        # ad.test() #Anderson-Darling正态性检验
        # cvm.test() #Cramer-von Mises正态性检验
        # pearson.test() #Pearson卡方正态性检验
        # sf.test() #Shapiro-Francia正态性检验    
        
        data5 <- data4
        for(i in 1:ncol(data5)) data5[,i] <- data5[,i]/max(abs(data5[,i]))
        #for(i in 1:n) data5[,i] <- (data5[,i]-mean(data5[,i]))/sd(data5[,i])
        
        data5
}

delRandant <- function(data5,Y){
        
        # pr <- prcomp(data5, scale = FALSE)
        # vars <- apply(pr$x, 2, var)
        # props <- vars / sum(vars)
        # tsub <- which(cumsum(props)>0.95)[1]
        # data6 <- pr$x[,1:tsub]
        
        
        x <- data5
        y <- Y
        a1 <- data.frame(x,y)
        colnames(a1) <- c(paste("X",1:(ncol(a1)-1),sep=""),"Class")
        rownames(a1) <- 1:nrow(a1)
        
        library(FSelector)
        w0 <- unlist(information.gain(Class ~., a1))
        # w1 <- unlist(chi.squared(Class~., a1))
        # subset <- cfs(Class ~ ., a1)
        subs <- which(w0 > 1)
        data6 <- data5[, subs]
        
        # tmp <- data5
        # colnames(tmp) <- 1:ncol(data5)
        # 
        # d0 <- dist(t(tmp))
        # h2 <- hclust(d0) 
        # plot(h2)
        # 
        # d <- 1 - abs(cor(tmp))
        # h1 <- hclust(as.dist(d)) 
        # plot(h1)
        
        data6
}
