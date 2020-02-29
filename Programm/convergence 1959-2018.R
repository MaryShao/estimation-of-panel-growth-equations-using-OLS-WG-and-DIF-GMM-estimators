GDP<-read.csv('GDP China.csv',header=T)

GDP1<-log(GDP$GDP[1:19])
GDP2<-log(GDP$GDP[20:41])
GDP3<-log(GDP$GDP[22:length(GDP)])

m1<-lm((GDP1[2:19]-GDP1[1:18])~GDP1[1:18])
summary(m1)
m2<-lm((GDP2[2:length(GDP2)]-GDP2[1:(length(GDP2)-1)])~GDP2[1:(length(GDP2)-1)])
summary(m2)
m3<-lm((GDP3[2:length(GDP3)]-GDP3[1:(length(GDP3)-1)])~GDP3[1:(length(GDP3)-1)])
summary(m3)