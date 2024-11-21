rm(list = ls())

library(ggplot2)
library(data.table)
library(openair)

df = fread('./AMF_CA-CF3_BASE_HH_1-5.csv',na.strings = c('-9999'))

df$date = as.POSIXct(x = as.character(df$TIMESTAMP_END),format = '%Y%m%d%H%M',tz = 'UTC')
df = subset(df,df$date > as.POSIXct('2022-07-01'))
df$dou

library(bigleaf)


av = timeAverage(mydata = df,avg.time = '1 day',data.thresh = 50,statistic = 'mean')
gs = subset(av,av$TA > 5 & av$PPFD_IN > 0)
gs = gs[complete.cases(gs$PPFD_IN),]
gs = gs[complete.cases(gs$FC),]

gs = subset(df,df$TA > 8 & df$PPFD_IN > 0)
gs = gs[complete.cases(gs$PPFD_IN),]
gs = gs[complete.cases(gs$FC),]

lr = light.response(data = gs,NEE = 'FC',Reco = 'RECO_PI',PPFD = 'PPFD_IN',PPFD_ref = 1500)
lr
fit = lr$m$fitted()


ggplot(data = gs)+
  geom_point(aes(PPFD_IN,GPP_PI_F,col=TA))+
  geom_line(aes(PPFD_IN,fit),col='red')

ggplot(data = gs)+
  geom_point(aes(PPFD_IN,FC,col=TA))+
  geom_line(aes(PPFD_IN,-fit),col='red')

gs = df[complete.cases(df$PPFD_IN),]
gs = gs[complete.cases(gs$FC),]

lr = light.response(data = gs,NEE = 'FC',Reco = 'RECO_PI',PPFD = 'PPFD_IN',PPFD_ref = 1500)
lr
fit = lr$m$fitted()


ggplot(data = df)+
  geom_point(aes(PPFD_IN,GPP_PI_F,col=TA))+
  geom_line(aes(PPFD_IN,fit),col='red')

ggplot(data = gs)+
  geom_point(aes(PPFD_IN,FC,col=TA))+
  geom_line(aes(PPFD_IN,-fit),col='red')

#−NEE = αPPFD/(1−(PPFD/PPFDref)+αPPFD/GPPref)−Reco
α = 0.007827
PPFDref = 1500
GPPref = 3.225959

lr.line = predict(lr)

plot(lr.line)
fit = lr$m$fitted()

lr$
ggplot(data = gs)+
  geom_point(aes(PPFD_IN,FC,col=TA))+
  geom_smooth(formula = y ~ α*x / (1−(x/PPFDref) + α*x/GPPref) - RECO_PI)
