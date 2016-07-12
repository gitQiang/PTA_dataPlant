
        # wind 实时数据获取, 以天为基础
        library(WindR)
        w.start()
        #w_edb_data<-w.edb('S0031529,S0070127','2015-07-13','2016-07-12','')
        day <- as.character(Sys.Date())
        w_edb_data<-w.edb('S0031529,S0070127,S0031714,S0031715,S0031716,S0031712,S0028007,S0028008,S0027203,S0027204,S5414121,S0027187,S0027188',day,day,'')
        newData <- w_edb_data$Data[w_edb_data$Data[,1]== day, ,drop=FALSE]
        write.csv(newData,file=paste("D:/code/PTA_dataPlant/UpdateData/Wind",day,".csv",sep=""),row.names=FALSE,quote=FALSE)
        w.stop()
        

