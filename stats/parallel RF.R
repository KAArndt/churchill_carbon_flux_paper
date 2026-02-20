#Load in required packages
library(data.table)
library(ggplot2)
library(zoo)
library(randomForest)
# library(reshape)
# library(caret)
# library(Boruta)
# library(tidyverse)
# library(patchwork)
Sys.setenv(TZ = 'UTC')

#Load in data
df = fread('./data/CA-CF3_HH_INTERNAL_202208062330_202601010000.csv',na.strings = c('-9999'))

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

#get to gap filling
cc = df2[complete.cases(df2$nee),]

library(foreach)
library(doParallel)
library(doSNOW)

test.sample = sample(c(TRUE, FALSE), nrow(cc), replace=TRUE, prob=c(0.8,0.2))

samp = matrix(nrow = (length(test.sample)),ncol = 25)
train = list()
test = list()

for (i in 1:25) {
  samp[,i] = sample(c(TRUE, FALSE), nrow(cc), replace=TRUE, prob=c(0.8,0.2))
  train[[i]]  = cc[samp[,i], ]
  test[[i]]   = cc[!samp[,i], ]
}

#remove data to free up memory
rm(samp)

#setup parallel back end to use many processors
cores = detectCores()        #detect the number of cores
cl = makeCluster(4) #assign number of cores, must be less than number of cores in above line
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  rf.model.results = foreach (i = 1:length(train), #i or any letter for each iteration that's parallel
                              .verbose = T,     #verbose means to show the processing while it's going
                              .combine = cbind, #how to combine the data, this will essentially make a data frame
                              .packages = c('randomForest')) %dopar% {
    #train each random forest on each matrix training dataset
  rfnee = randomForest(formula = nee ~ tair + rh + rg + tsoil + vpd + le + pres + h,
                       data = train[[i]], ntree = 100, importance = TRUE)
    #predict from each of these random forests over the whole data set
  predict(object = rfnee,newdata = df2)
      }
  stopCluster(cl) #stop the clusters
  Sys.time() - orig}


nrow(rf.model.results)

# Precreate median and st dev
median.rf = vector(length = nrow(rf.model.results))
sd.rf = vector(length = nrow(rf.model.results))

for (i in 1:nrow(rf.model.results)) {
  median.rf[i] = median(x = rf.model.results[i,])
  sd.rf[i] = sd(x = rf.model.results[i,])
}

df$median.rf = median.rf
df$sd.rf = sd.rf

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
df$rf12 = rf.model.results[,12]
df$rf13 = rf.model.results[,13]
df$rf14 = rf.model.results[,14]
df$rf15 = rf.model.results[,15]
df$rf16 = rf.model.results[,16]
df$rf17 = rf.model.results[,17]
df$rf18 = rf.model.results[,18]
df$rf19 = rf.model.results[,19]
df$rf20 = rf.model.results[,20]
df$rf21 = rf.model.results[,21]
df$rf22 = rf.model.results[,22]
df$rf23 = rf.model.results[,23]
df$rf24 = rf.model.results[,24]
df$rf25 = rf.model.results[,25]

limit = subset(df,df$date >= as.POSIXct("2025-07-01") & df$date <= as.POSIXct('2025-07-30'))

#Time series plots
ggplot(data = limit)+theme_bw()+
  geom_point(aes(date,rf1,col='RF1'),alpha=0.2)+
  geom_point(aes(date,rf2,col='RF2'),alpha=0.2)+
  geom_point(aes(date,rf3,col='RF3'),alpha=0.2)+
  geom_point(aes(date,rf4,col='RF4'),alpha=0.2)+
  geom_point(aes(date,rf5,col='RF5'),alpha=0.2)+
  geom_line(aes(date,FC,col='Real'),alpha=0.8)+
  scale_x_datetime(limits = as.POSIXct(c("2025-07-01","2025-07-15")))

#Time series plots
ggplot(data = limit)+theme_bw()+
  geom_ribbon(aes(x = date,y = median.rf,
                  ymin = median.rf - sd.rf, ymax = median.rf + sd.rf,col='RF'),alpha=0.2)+
  geom_line(aes(date,FC,col='Real'),alpha=0.8)+
  scale_x_datetime(limits = as.POSIXct(c("2025-07-01","2025-07-15")))

### Take budget
### Remove RAND_ERR_CO2_FLUX from cleaned data points
### St dev addition: sqrt(std1^2 + std2^2+...)













###### Copy and pasted from old file

#Validation

test.data = merge(test,cf,by = 'date',all.x = T)

co2.rf = ggplot(data = test.data,aes(rfnee,co2_flux.c))+theme_bw()+
  geom_hline(yintercept = 0,lty=2)+
  geom_vline(xintercept = 0,lty=2)+
  geom_point(alpha=0.2)+
  geom_abline(slope = 1,intercept = 0,col='red',lty=2)+
  scale_x_continuous(limits = c(-10,10),expression('Random Forest NEE ('*mu*mol~CO[2]~m^-2~s^-1*')'))+
  scale_y_continuous(limits = c(-10,10),expression('Eddy Covariance NEE ('*mu*mol~CO[2]~m^-2~s^-1*')'))+
  theme(text = element_text(size = 6))

co2.rf

summary(lm(test.data$co2_flux.c ~ test.data$rfnee))


Examine variable importance for methane

set.seed(123)
cc = ncf[complete.cases(ncf$fch4),]
# 
# boruta = Boruta(fch4 ~ tair + rh + rg + ws + tsoil + swc + vpd + le + pres + h,data = cc,doTrace = 2,maxRuns = 100)
# 
# plot(boruta,las = 2)
```

try random forest for CH4
```{r,error=FALSE,warning=FALSE}
#use 80% of data set as training set and 20% as test set, expanded to try to reprsent the larger points better
cc = ncf[complete.cases(ncf$fch4),]

sample = sample(c(TRUE, FALSE), nrow(cc), replace=TRUE, prob=c(0.8,0.2))
train  = cc[sample, ]
test   = cc[!sample, ]

hist(cc$fch4)
hist(train$fch4)
hist(test$fch4)

#extra step of the summary here to see which dataset got extremes
summary(cc$fch4)
summary(train$fch4)
summary(test$fch4)

rfch4 = randomForest(formula = fch4 ~ tair + rh + rg + ws + tsoil + vpd + swc + h + le,data = train,ntree = 100,importance = TRUE)

pch4 = predict(object = rfch4,newdata = ncf)

cf$rfch4 = pch4
```

Plots and Validation
```{r,error=FALSE,warning=FALSE}
ggplot(data = cf)+theme_bw()+
  geom_point(aes(date,rfch4,col='RF'),alpha=0.5)+
  geom_point(aes(date,ch4_flux.c,col='Real'),alpha=0.5)+
  scale_x_datetime(limits = as.POSIXct(c("2025-06-01","2025-06-30")))
# scale_y_continuous(limits = c(-0.05,0.05))

ggplot(data = cf)+theme_bw()+
  geom_point(aes(date,rfch4,col='RF'),alpha=0.2)+
  geom_point(aes(date,ch4_flux.c,col='Real'),alpha=0.2)
```

```{r,error=FALSE,warning=FALSE}
test.data = merge(test,cf,by = 'date',all.x = T)

ch4.rf = ggplot(data = test.data,aes(rfch4,ch4_flux.c))+theme_bw()+
  geom_hline(yintercept = 0,lty=2)+
  geom_vline(xintercept = 0,lty=2)+
  geom_point(alpha=0.2)+
  geom_abline(slope = 1,intercept = 0,col='red',lty=2)+
  scale_x_continuous(limits = c(-0.1,0.25),expression('Random Forest '*CH[4]~flux~" ("*mu*mol~CH[4]~m^-2~s^-1*')'))+
  scale_y_continuous(limits = c(-0.1,0.25),expression('Eddy Covariance '*CH[4]~flux~" ("*mu*mol~CH[4]~m^-2~s^-1*')'))+
  theme(text = element_text(size = 6))
ch4.rf

summary(lm(test.data$ch4_flux.c ~ test.data$rfch4))
```

```{r}

# png("./png/rf_validations.png",width = 5,height = 3,units = 'in',res = 1500)
# co2.rf + ch4.rf
# dev.off()

```


save off the data
```{r,error=FALSE,warning=FALSE}
write.csv(x = cf,file = "./outputs/gapfilling/churchill_2022_2025_gf.csv",quote = F,row.names = F)
```
