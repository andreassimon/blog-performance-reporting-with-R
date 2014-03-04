# region read-table
responseTimes<-read.table("response-times.csv",header=TRUE,sep=",")
# endregion read-table

png(width=1200,height=900,filename="response-times.png")
# region test-time-latency
plot((responseTimes$timeStamp-min(responseTimes$timeStamp))/1000/60,
     responseTimes$Latency,
     xlab="Test time [min]",
     ylab="Latency [ms]",
     col="dodgerblue4",
     pch=18
)
# endregion test-time-latency
dev.off()

png(width=1200,height=800,filename="response-codes.png")
# region response-codes
par(lwd=4)
pie(summary(responseTimes$responseCode),
    clockwise=TRUE,
    col=c("steelblue3", "tomato2", "tomato3"),
    border=c("steelblue4","tomato4","tomato4"),
    main="HTTP Response Codes"
)
# endregion response-codes
dev.off()

png(width=1200,height=800,filename="quantiles.png")
# region cumulative-relative-frequencies
plot(ecdf(responseTimes$Latency),
     main="Cumulative relative frequency of response times",
     xlab="Latency [ms]",
     ylab="Quantiles",
     pch=18
)
# endregion cumulative-relative-frequencies

# region usability-barriers
axis(1,at=c(100),labels=c("100"),col="green4")
instantResponse = cumsum(table(cut(responseTimes$Latency,c(0,100))))/nrow(responseTimes)
segments(100,-1,100,instantResponse,col="palegreen3",lty="dashed",lwd=2)
points(c(100),instantResponse,col="green4",pch=19)
text(100,instantResponse,paste(format(instantResponse*100,digits=3), "%"),col="green4",adj=c(1.1,-.3))

fastResponse = cumsum(table(cut(responseTimes$Latency,c(0,1000))))/nrow(responseTimes)
segments(1000,-1,1000,fastResponse,col="palegreen3",lty="dashed",lwd=2)
points(c(1000),fastResponse,col="green4",pch=19)
text(1000,fastResponse,paste(format(fastResponse*100,digits=3), "%"),col="green4",adj=c(1.1,-.3))
# endregion usability-barriers

# region ninetyfive-quantile
axis(2,at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,1),labels=FALSE)
axis(2,at=c(.95),labels=c("95%"),col="tomato3")
ninetyfiveQuantile = quantile(responseTimes$Latency,c(0.95))
segments(-10000, 0.95, ninetyfiveQuantile, .95, col="tomato1",lty="dashed",lwd=2)
points(ninetyfiveQuantile,c(0.95),col="red3",pch=19)
text(ninetyfiveQuantile,c(0.95),paste(format(ninetyfiveQuantile),"ms"),col="red3",adj=c(-.1,1.3))
# endregion ninetyfive-quantile

dev.off()
