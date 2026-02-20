#Load in required packages
library(data.table)
library(ggplot2)
library(zoo)
library(randomForest)
library(dplyr)
Sys.setenv(TZ = 'UTC')

#Load in data
df = fread('./CA-CF3_HH_INTERNAL_202208062330_202601010000.csv',na.strings = c('-9999'))

df$date = as.POSIXct(as.character(df$TIMESTAMP_END),format='%Y%m%d%H%M')
summary(df)

#create a sub data frame of the variables you want for the random forest
df2 = data.frame(df$date,
                 df$FC,
                 df$FCH4,
                 df$AIRT_ERA5,
                 df$DEW_ERA5,
                 df$ST2_ERA5,
                 df$VWC1_ERA5,
                 df$RAD_ERA5,
                 df$H_ERA5,
                 df$LE_ERA5,
                 df$PRES_ERA5)

#rename for easier names
names(df2) = c('date','nee',"fch4",'tair','dew','tsoil','vwc','rg','h','le','pres')

#calculate VPD from air t and RH
df2$rh = 100*(exp((17.625*df2$dew)/(243.04+df2$dew))/exp((17.625*df$AIRT_ERA5)/(243.04+df$AIRT_ERA5)))
svp = 610.7*10^((7.5*df2$tair)/(237.3+df2$tair))
df2$vpd = ((100 - df2$rh)/100)*svp  

# NEE #################################################################################################
#get to gap filling
cc = df2[complete.cases(df2$nee),]

#set up parallel processing
library(foreach)
library(doParallel)
library(doSNOW)

samp = sample(c(TRUE, FALSE), nrow(cc), replace=TRUE, prob=c(0.8,0.2))
train  = cc[samp, ]
test   = cc[!samp, ]

#setup parallel back end to use many processors
cores = detectCores()   #detect the number of cores
cl = makeCluster(10)     #assign number of cores
{orig = Sys.time()      #start the clock for timing the process
  registerDoSNOW(cl)    #register the cores
  
  rf.model.results = foreach (i = 1:100, #i or any letter for each iteration that's parallel
                              .verbose = T,     #verbose means to show the processing while it's going
                              .combine = cbind, #how to combine the data, this will essentially make a data frame
                              .packages = c('randomForest')) %dopar% {
    #train each random forest on each matrix training data set
  rfnee = randomForest(formula = nee ~ tair + rh + rg + tsoil + vpd + le + pres + h,
                       data = train, ntree = 75)
    #predict from each of these random forests over the whole data set
  predict(object = rfnee,newdata = df2)
      }
  stopCluster(cl) #stop the clusters
  Sys.time() - orig}

#pre-create median and standard deviations
median.rf.nee = vector(length = nrow(rf.model.results))
sd.rf.nee     = vector(length = nrow(rf.model.results))

#use a for loop to calculate stats across iterations
for (i in 1:nrow(rf.model.results)) {
  median.rf.nee[i] = median(x = rf.model.results[i,])
  sd.rf.nee[i]     = sd(x = rf.model.results[i,])
}

#add them into the original data frame
df$median.rf.nee = median.rf.nee
df$sd.rf.nee     = sd.rf.nee

df$rf1 = rf.model.results[,1]
df$rf2 = rf.model.results[,2]
df$rf3 = rf.model.results[,3]
df$rf4 = rf.model.results[,4]
df$rf5 = rf.model.results[,5]
df$rf6 = rf.model.results[,6]
df$rf7 = rf.model.results[,7]
df$rf8 = rf.model.results[,8]
df$rf9 = rf.model.results[,9]
df$rf10 = rf.model.results[,10]
df$rf11 = rf.model.results[,11]
# df$rf12 = rf.model.results[,12]
# df$rf13 = rf.model.results[,13]
# df$rf14 = rf.model.results[,14]
# df$rf15 = rf.model.results[,15]
# df$rf16 = rf.model.results[,16]
# df$rf17 = rf.model.results[,17]
# df$rf18 = rf.model.results[,18]
# df$rf19 = rf.model.results[,19]
# df$rf20 = rf.model.results[,20]
# df$rf21 = rf.model.results[,21]
# df$rf22 = rf.model.results[,22]
# df$rf23 = rf.model.results[,23]
# df$rf24 = rf.model.results[,24]
# df$rf25 = rf.model.results[,25]

limit = subset(df,df$date >= as.POSIXct("2025-01-01") & df$date <= as.POSIXct('2026-01-01'))

#Time series plots
ggplot(data = limit)+theme_bw()+
  geom_point(aes(date,rf1,col='RF1'),alpha=0.5)+
  geom_point(aes(date,rf2,col='RF2'),alpha=0.5)+
  geom_point(aes(date,rf3,col='RF3'),alpha=0.5)+
  geom_point(aes(date,rf4,col='RF4'),alpha=0.5)+
  geom_point(aes(date,rf5,col='RF5'),alpha=0.5)+
  geom_line(aes(date,FC,col='Real'),alpha=0.8)+
#  scale_x_datetime(limits = as.POSIXct(c("2024-06-01","2024-07-30")))+
  scale_y_continuous(limits = c(-8,5))

#Time series plots
ggplot(data = limit)+theme_bw()+
  geom_ribbon(aes(x = date,y = median.rf.nee,
                  ymin = median.rf.nee - sd.rf.nee*2, ymax = median.rf.nee + sd.rf.nee*2,col='RF'),alpha=0.2)+
  geom_line(aes(date,FC,col='Real'),alpha=0.8)+
  scale_x_datetime(limits = as.POSIXct(c("2025-07-01","2025-07-14")))+
  scale_y_continuous(limits = c(-6,8))

#merge with the test data set left out of model training
test.final = merge(test,df,by = c('date'))

#plot the 1:1 plot
ggplot(data = test.final)+theme_bw()+
  geom_abline(slope = 1,intercept = 0,lty=2)+
  geom_point(aes(FC,median.rf.nee))+
  scale_x_continuous(limits = c(-7,7))+
  scale_y_continuous(limits = c(-7,7))

#calculate the linear model and residuals from the model for error calculatons
rf.mod.nee = lm(median.rf.nee~FC,data = test.final)
hist(rf.mod.nee$residuals)

#calculate error metrics to apply to gap-filled points
rmse.nee = sqrt(sum(rf.mod.nee$residuals^2)/length(rf.mod.nee$residuals))
mae.nee  = sum(abs(test.final$median.rf.nee - test.final$FC))/length(test.final$FC)
be.nee   = sum(test.final$median.rf.nee - test.final$FC)/length(test.final$FC)


#create complete cases of factors
df$FC_F            = ifelse(complete.cases(df$FC),df$FC,df$median.rf.nee)
df$FC_F.rand.error = ifelse(complete.cases(df$FC),df$RAND_ERR_CO2_FLUX,df$sd.rf.nee)
df$count.nee       = ifelse(is.na(df$FC),1,0)
df$rmse.nee        = ifelse(is.na(df$FC),rmse.nee,NA)


# CH4 #####################################################################################
#get to gap filling
cc.ch4 = df2[complete.cases(df2$fch4),]

#set up parallel processing
library(foreach)
library(doParallel)
library(doSNOW)

samp = sample(c(TRUE, FALSE), nrow(cc.ch4), replace=TRUE, prob=c(0.8,0.2))
train  = cc.ch4[samp, ]
test   = cc.ch4[!samp, ]

#setup parallel back end to use many processors
cores = detectCores()   #detect the number of cores
cl = makeCluster(10)     #assign number of cores
{orig = Sys.time()      #start the clock for timing the process
  registerDoSNOW(cl)    #register the cores
  
  rf.model.results = foreach (i = 1:100, #i or any letter for each iteration that's parallel
                              .verbose = T,     #verbose means to show the processing while it's going
                              .combine = cbind, #how to combine the data, this will essentially make a data frame
                              .packages = c('randomForest')) %dopar% {
                                #train each random forest on each matrix training data set
                                rfch4 = randomForest(formula = fch4 ~ tair + rh + rg + tsoil + vpd + le + pres + h,
                                                     data = train, ntree = 75)
                                #predict from each of these random forests over the whole data set
                                predict(object = rfch4,newdata = df2)
                              }
  stopCluster(cl) #stop the clusters
  Sys.time() - orig}

#pre-create median and standard deviations
median.rf.ch4 = vector(length = nrow(rf.model.results))
sd.rf.ch4     = vector(length = nrow(rf.model.results))

#use a for loop to calculate stats across iterations
for (i in 1:nrow(rf.model.results)) {
  median.rf.ch4[i] = median(x = rf.model.results[i,])
  sd.rf.ch4[i]     = sd(x = rf.model.results[i,])
}

#add them into the original data frame
df$median.rf.ch4 = median.rf.ch4
df$sd.rf.ch4     = sd.rf.ch4

df$rf1 = rf.model.results[,1]
df$rf2 = rf.model.results[,2]
df$rf3 = rf.model.results[,3]
df$rf4 = rf.model.results[,4]
df$rf5 = rf.model.results[,5]
df$rf6 = rf.model.results[,6]
df$rf7 = rf.model.results[,7]
df$rf8 = rf.model.results[,8]
df$rf9 = rf.model.results[,9]
df$rf10 = rf.model.results[,10]
df$rf11 = rf.model.results[,11]
# df$rf12 = rf.model.results[,12]
# df$rf13 = rf.model.results[,13]
# df$rf14 = rf.model.results[,14]
# df$rf15 = rf.model.results[,15]
# df$rf16 = rf.model.results[,16]
# df$rf17 = rf.model.results[,17]
# df$rf18 = rf.model.results[,18]
# df$rf19 = rf.model.results[,19]
# df$rf20 = rf.model.results[,20]
# df$rf21 = rf.model.results[,21]
# df$rf22 = rf.model.results[,22]
# df$rf23 = rf.model.results[,23]
# df$rf24 = rf.model.results[,24]
# df$rf25 = rf.model.results[,25]

limit = subset(df,df$date >= as.POSIXct("2025-01-01") & df$date <= as.POSIXct('2026-01-01'))

#Time series plots
ggplot(data = limit)+theme_bw()+
  geom_point(aes(date,rf1,col='RF1'),alpha=0.5)+
  geom_point(aes(date,rf2,col='RF2'),alpha=0.5)+
  geom_point(aes(date,rf3,col='RF3'),alpha=0.5)+
  geom_point(aes(date,rf4,col='RF4'),alpha=0.5)+
  geom_point(aes(date,rf5,col='RF5'),alpha=0.5)+
  geom_line(aes(date,FCH4,col='Real'),alpha=0.8)
#  scale_x_datetime(limits = as.POSIXct(c("2024-06-01","2024-07-30")))+
#  scale_y_continuous(limits = c(-8,5))

#Time series plots
ggplot(data = limit)+theme_bw()+
  geom_ribbon(aes(x = date,y = median.rf.ch4,
                  ymin = median.rf.ch4 - sd.rf.ch4*2, ymax = median.rf.ch4 + sd.rf.ch4*2,col='RF'),alpha=0.2)+
  geom_line(aes(date,FCH4,col='Real'),alpha=0.8)+
  scale_x_datetime(limits = as.POSIXct(c("2025-07-01","2025-07-14")))
#  scale_y_continuous(limits = c(-6,8))

#merge with the test data set left out of model training
test.final = merge(test,df,by = c('date'))

#plot the 1:1 plot
ggplot(data = test.final)+theme_bw()+
  geom_abline(slope = 1,intercept = 0,lty=2)+
  geom_point(aes(FCH4,median.rf.ch4))+
  scale_x_continuous(limits = c(-75,150))+
  scale_y_continuous(limits = c(-75,150))

#calculate the linear model and residuals from the model for error calculatons
rf.mod.ch4 = lm(median.rf.ch4~FCH4,data = test.final)
hist(rf.mod.ch4$residuals)

#calculate error metrics to apply to gap-filled points
rmse.ch4 = sqrt(sum(rf.mod.ch4$residuals^2)/length(rf.mod.ch4$residuals))
mae.ch4  = sum(abs(test.final$median.rf.ch4 - test.final$FCH4))/length(test.final$FCH4)
be.ch4   = sum(test.final$median.rf.ch4 - test.final$FCH4)/length(test.final$FCH4)

#create complete cases of factors
df$FCH4_F            = ifelse(complete.cases(df$FCH4),df$FCH4,df$median.rf.ch4)
df$FCH4_F.rand.error = ifelse(complete.cases(df$FCH4),df$RAND_ERR_CH4_FLUX,df$sd.rf.ch4)
df$count             = ifelse(is.na(df$FCH4),1,0)
df$rmse.ch4          = ifelse(is.na(df$FCH4),rmse.ch4,NA)

#annual statistics
df$year  = format(x = df$date,'%Y')
df$month = format(x = df$date,'%m')

#conversion factor so its g C m-2 30 mins
annual = df %>%
  group_by(year) %>%
  summarise(fch4 = sum(FCH4_F*60*30/10^9*12),
            ch4.rand.error = sqrt(sum((FCH4_F.rand.error*60*30/10^9*12)^2)),
            ch4.gf.error   = sqrt(sum((rmse.ch4*60*30/10^9*12)^2,na.rm=T)),
            ch4.total.error = ch4.rand.error + ch4.gf.error,
            nee = sum(FC_F*60*30/10^6*12),
            nee.rand.error = sqrt(sum((FC_F.rand.error*60*30/10^6*12)^2)),
            nee.gf.error = sqrt(sum((rmse.nee*60*30/10^6*12)^2,na.rm=T)),
            ch4.total.error = nee.rand.error+nee.gf.error)

annual


plot(df$rmse.ch4)
plot(df$rmse.nee)
plot(df$sd.rf.ch4)
plot(df$sd.rf.nee)

#
write.csv(x = df,file = './error_churchill.csv',row.names = F)


#sums of annual sums
sums = vector(length = 11)
for (i in 1:11) {
  sums[i] = sum(rf.model.results[,i]*60*30/10^6*12)
}

sd(sums)*2

sum(df$rf1)
sum(df$rf2)
sum(df$rf3)
sum(df$rf4)
sum(df$rf5)
sum(df$rf6)
sum(df$rf7)
sum(df$rf8)




