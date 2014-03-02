# responseTimes<-read.table("response-times.csv",header=T,sep=",")
# write.table("response-times.out.csv",responseTimes)
plot((responseTimes$timeStamp-min(responseTimes$timeStamp))/1000/60,responseTimes$Latency,xlab="Test time [min]",ylab="Latency [ms]",col=2,pch=18)
dev.print(device=png,width=1200,height=900,"response-times.png")

pie(summary(responseTimes$responseCode),col=c("steelblue3", "tomato2", "tomato3"), main="Relative numbers of HTTP Response Codes")
dev.print(device=png,width=1200,height=800,"response-codes.png")

plot(ecdf(responseTimes$Latency),main="Cumulative relative frequency of response times",xlab="Latency [ms]",ylab="Quantiles",pch=18)
axis(2,at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,1),labels=FALSE)
axis(2,at=c(.95),labels=c("95%"),col="tomato3")
axis(1,at=c(100),labels=c("100"),col="green4")

abline(h=c(0.95),col="tomato1",lty="dashed",lwd=2)
ninetyfiveQuantile = quantile(responseTimes$Latency,c(0.95))
points(ninetyfiveQuantile,c(0.95),col="red3",pch=19)
text(ninetyfiveQuantile,c(0.95),paste(format(ninetyfiveQuantile),"ms"),col="red3",adj=c(-.1,1.3))

abline(v=c(100),col="palegreen3",lty="dashed",lwd=2)
instantResponse = cumsum(table(cut(responseTimes$Latency,c(0,100))))/nrow(responseTimes)
points(c(100),instantResponse,col="green4",pch=19)
text(100,instantResponse,paste(format(instantResponse*100,digits=2), "%"),col="green4",adj=c(1.1,-.3))

abline(v=c(1000),col="palegreen3",lty="dashed",lwd=2)
fastResponse = cumsum(table(cut(responseTimes$Latency,c(0,1000))))/nrow(responseTimes)
points(c(1000),fastResponse,col="green4",pch=19)
text(1000,fastResponse,paste(format(fastResponse*100,digits=3), "%"),col="green4",adj=c(1.1,-.3))
dev.print(device=png,width=1200,height=800,"quantiles.png")
