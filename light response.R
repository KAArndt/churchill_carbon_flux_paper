rm(list = ls())

library(ggplot2)
library(data.table)
library(openair)
library(bigleaf)
library(dplyr)

df = fread('./AMF_CA-CF3_BASE_HH_1-5.csv',na.strings = c('-9999'))

df$date = as.POSIXct(x = as.character(df$TIMESTAMP_END),format = '%Y%m%d%H%M',tz = 'UTC')
df = subset(df,df$date > as.POSIXct('2022-07-01'))

############################################################################################
av = timeAverage(mydata = df,avg.time = '1 day',data.thresh = 50,statistic = 'mean')
gs = subset(av,av$TA > 5 & av$PPFD_IN > 0)
gs = gs[complete.cases(gs$PPFD_IN),]
gs = gs[complete.cases(gs$FC),]

# growing season control based on light response curve #######################
gs = subset(df,df$TS_3_3_1 > 10 & df$PPFD_IN > 1)
gs = gs[complete.cases(gs$PPFD_IN),]
gs = gs[complete.cases(gs$FC),]

lr = light.response(data = gs,NEE = 'FC',Reco = 'RECO_PI',PPFD = 'PPFD_IN',PPFD_ref = 1500)

fit = lr$m$fitted()

ggplot(data = gs)+
  geom_point(aes(PPFD_IN,FC,col=TA))+
  geom_line(aes(PPFD_IN,fit*-1),col='red')


ggplot(data = gs,aes(fit*-1,FC))+
  geom_point()+
  geom_smooth(method = 'lm')

lr.mod = lm(gs$FC ~ fit*-1)
summary(lr.mod)

##########################
df$doy = as.numeric(format(df$date,'%j'))
av = timeAverage(mydata = df,avg.time = '1 day',data.thresh = 50,statistic = 'mean')
shs = subset(av,av$TS_3_4_1 < 1 | av$PPFD_IN < 100)
shs = subset(df,df$TS_3_4_1 < 1 | df$PPFD_IN < 25)
#shs = subset(df,df$TS_3_4_1 < 1)

# shs = shs[complete.cases(shs$TS_3_4_1),]
# shs = shs[complete.cases(shs$FC),]


ggplot(data = shs)+
  geom_point(aes(TA,FC,col=doy))


av = timeAverage(mydata = shs,avg.time = '1 day',data.thresh = 50,statistic = 'mean')

ggplot(data = subset(av,av$FC>0))+
  geom_point(aes(TA,log10(FC),col=doy))+
  geom_smooth()

t.mod = lm(log(shs$FC) ~ shs$TS_3_1_1)
summary(t.mod)

ggplot(data = shs)+
  geom_point(aes(TA,FC,col=PPFD_IN))
ggplot(data = shs)+
  geom_point(aes(TA,FC,col=doy))

ggplot(data = shs,aes(fit*-1,FC))+
  geom_point()+
  geom_smooth(method = 'lm')








df2 = subset(df,df$PPFD_IN > 1 & df$TS_3_3_1 > 5)

ggplot(data = df2,aes(PPFD_IN,FC,col=TS_3_3_1))+
  geom_point()+
  geo
  geom_smooth(method = 'lm')

summary(lm(formula = df2$FC ~ df2$PPFD_IN))


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
