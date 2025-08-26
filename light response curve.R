
library(ggplot2)
library(data.table)
library(openair)
library(bigleaf)
library(dplyr)

#load in the CF3 Ameriflux file
df = fread('./data/AMF_CA-CF3_BASE_HH_1-5.csv',na.strings = c('-9999'))

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

#−NEE = αPPFD/(1−(PPFD/PPFDref)+αPPFD/GPPref)−Reco
lr

# α = 0.01637
# GPPref = 6.14171 
# PPFD_ref = 1500
# df$light.response.nee = (α*df$PPFD_IN/(1-(df$PPFD_IN/PPFD_ref)+α*df$PPFD_IN/GPPref)-df$RECO_PI)*-1
# 
# plot(df$date,df$light.response.nee)
# points(df$date,df$FC,col='red')
# 
# plot(df$light.response.nee,df$FC)


#lets look at just light control #########################################
df$doy = as.numeric(format(df$date,'%j'))
av = timeAverage(mydata = df,avg.time = '3 day',data.thresh = 50,statistic = 'mean')

shs = subset(av,av$TA > 1)


ggplot(data = shs,aes(TA,FC,col=doy))+
  geom_point()+
  geom_smooth(method = 'lm')


light.mod = lm(shs$FC ~ shs$NETRAD)
summary(light.mod)





