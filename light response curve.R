rm(list = ls())

library(ggplot2)
library(data.table)
library(openair)
library(bigleaf)
library(dplyr)

#load in the CF3 Ameriflux file
df = fread('./AMF_CA-CF3_BASE_HH_1-5.csv',na.strings = c('-9999'))

#create a "date" column so it works with Time Average
df$date = as.POSIXct(x = as.character(df$TIMESTAMP_END),format = '%Y%m%d%H%M',tz = 'UTC')
df = subset(df,df$date > as.POSIXct('2022-07-01')) #subset to rough beginning of dataset

############################################################################################
# play with conditions where light response is most valid, i.e., after greenup during the growing season
gs = subset(df,df$TS_3_3_1 > 10 & df$PPFD_IN > 1)
gs = gs[complete.cases(gs$PPFD_IN),]
gs = gs[complete.cases(gs$FC),]

#run the light. response curve in bigleaf package
lr = light.response(data = gs,NEE = 'FC',Reco = 'RECO_PI',PPFD = 'PPFD_IN',PPFD_ref = 1500)
fit = lr$m$fitted() #add fitted data
lr$m$fitted()

#plot the fit data vs the real PAR, NEE relationship
ggplot(data = gs)+
  geom_point(aes(PPFD_IN,FC,col=TA))+
  geom_line(aes(PPFD_IN,fit*-1),col='red')

#see how it compares to the real data
ggplot(data = gs,aes(fit*-1,FC))+
  geom_point()+
  geom_smooth(method = 'lm')

#how good did this model perform?
lr.mod = lm(gs$FC ~ fit*-1)
summary(lr.mod)

#approximately what doy is here?
gs$doy = as.numeric(format(gs$date,'%j'))

summary(gs$doy)

ggplot(data = gs,aes(date,FC))+
  geom_point()+
  geom_smooth(method = 'lm')

#−NEE = αPPFD/(1−(PPFD/PPFDref)+αPPFD/GPPref)−Reco
lr

α = 0.01637
GPPref = 6.14171 
PPFD_ref = 1500
df$light.response.nee = (α*df$PPFD_IN/(1-(df$PPFD_IN/PPFD_ref)+α*df$PPFD_IN/GPPref)-df$RECO_PI)*-1

plot(df$date,df$light.response.nee)
points(df$date,df$FC,col='red')

plot(df$light.response.nee,df$FC)


#lets look at just light control #########################################
df$doy = as.numeric(format(df$date,'%j'))
av = timeAverage(mydata = df,avg.time = '1 day',data.thresh = 50,statistic = 'mean')
plot(av$PPFD_IN,av$FC)
shs = subset(av,av$TS_3_4_1 < 1 | av$PPFD_IN < 125)
#shs = subset(df,df$TS_3_4_1 < 1 | df$PPFD_IN < 25)

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
