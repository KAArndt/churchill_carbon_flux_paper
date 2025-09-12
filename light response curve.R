
rm(list = ls())

library(ggplot2)
library(data.table)
library(openair)
library(bigleaf)
library(dplyr)
library(patchwork)

#load in the CF3 Ameriflux file
df = fread('./bothyears_HH.csv')

# #create a "date" column so it works with Time Average
df$date = df$TIMESTAMP
# df = subset(df,df$date > as.POSIXct('2022-07-01')) #subset to rough beginning of dataset

############################################################################################
# play with conditions where light response is most valid, i.e., after greenup during the growing season
gs = subset(df, df$season_name %in% c("Growing Season") & df$PPFD_IN > 0)
# gs$FC = ifelse(gs$FC < -5 & gs$PPFD_IN < 400,NA,gs$FC)

#gs = subset(df, df$TS_2_05_1 > 5  & df$PPFD_IN > 0)

gs = gs[complete.cases(gs$PPFD_IN),]

gs = gs[complete.cases(gs$FC), ]

#run the light. response curve in bigleaf package
lr = light.response(data = gs,NEE = 'FC',Reco = 'RECO',PPFD = 'PPFD_IN',PPFD_ref = 1500)
fit = lr$m$fitted() #add fitted data
lr$m$fitted()



#−NEE = αPPFD/(1−(PPFD/PPFDref)+αPPFD/GPPref)−Reco
lr_summary <- summary(lr)
lr_summary

#Save parameters for plotting
parameters <- lr_summary$parameters
alpha <- parameters["alpha", "Estimate"]
GPP_ref <- parameters ["GPP_ref", "Estimate"]


gs$FC = ifelse(gs$FC < -5 & gs$PPFD_IN < 400,NA,gs$FC)
gs$lrfit = fit

gs = gs[order(gs$PPFD_IN),]

α = rep(0.016,2682)
PPFDref = rep(1500,2682)
GPPref = rep(4.29,2682)
Reco = gs$RECO
ppfd = gs$PPFD_IN

line = α*ppfd/(1-(ppfd/PPFDref)+α*ppfd/GPPref)-mean(gs$RECO)

mean(gs$RECO)
#plot the fit data vs the real PAR, NEE relationship
lrc <- ggplot(data = gs)+
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), 
    axis.title.y = element_text(size = 14),axis.title.x = element_text(size = 14))+
  geom_hline(aes(yintercept = 0),col = 'black')+
  geom_point(aes(PPFD_IN,FC, col = SWC_2_2_1))+
    geom_line(aes(PPFD_IN,line*-1),col='red')+
  geom_smooth(formula = y ~ α*ppfd/(1-(ppfd/PPFDref)+α*ppfd/GPPref)-Reco,col='red',aes(PPFD_IN,FC),method = 'lm')+
#  geom_line(aes(PPFD_IN,lrfit*-1),col='red')+
  scale_color_viridis_c()+
  annotate("text", 
           x = Inf, y = Inf, 
           hjust = 1.1, vjust = 1.3,
           label = paste0("\u03B1 = ", round(alpha, 3), "\nGPP ref = ", round(GPP_ref, 2)),
           size = 5)+
  labs(
    y = expression('Half-hourly CO '[2] * ' Flux ('*mu*mol~m^-2~s^-1*')'),
    x = expression('PAR In ('*mu*'mol m'^-2*'s'^-1*')'),
    title = "Light Response Curve - Growing Season Only",
    color = expression("SWC (%)"))
lrc


#how good did this model perform?
lr.mod = lm(gs$FC ~ line*-1)
lr.mod_summary <- summary(lr.mod)
lr.mod_summary
hist(lr.mod_summary$residuals)

#Store r squared for plotting
r_sq <- lr.mod_summary$r.squared

#see how it compares to the real data
lm <- ggplot(data = gs,aes(line*-1,FC))+
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), 
    axis.title.y = element_text(size = 14),axis.title.x = element_text(size = 14))+
  geom_point(aes(color = TA))+
  geom_smooth(method = 'lm', color= "red")+
  scale_color_viridis_c()+
  annotate("text", 
           x = -Inf, y = Inf, 
           hjust = -0.3, vjust = 1.5,
           label = paste0("R² = ", round(r_sq, 2)),
           size = 5)+
  labs(
    y = expression('Half-hourly CO '[2] * ' Flux ('*mu*mol~m^-2~s^-1*')'),
    x = expression('Fit * -1'),
    title = "LRC Accuracy - 5cm Soil Temp > 5",
    color = expression("Air T ("*degree*"C)"))
lm


png(filename = './lrc_smooth.png',width = 10,height = 6.72,units = 'in',res = 2000)
lrc
dev.off()


#approximately what doy is here?
#gs$doy = as.numeric(format(gs$date,'%j'))

ggplot(data = df)+
  geom_point(aes(x = TIMESTAMP, y = SWC_2_2_1)) +
  scale_x_datetime(name = expression(""), limits = as.POSIXct(c('2024-07-01', '2024-08-20'))) +
  scale_y_continuous(name = expression(""), limits = c(70, 80))



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
#df$doy = as.numeric(format(df$date,'%j'))

av = timeAverage(mydata = df,avg.time = '1 day',data.thresh = 50,statistic = 'mean')

shs = subset(av,av$TA > 4)


ggplot(data = shs,aes(TA,FC,col=DOY))+
  geom_point()+
  geom_smooth(method = 'lm')


light.mod = lm(shs$FC ~ shs$NETRAD)
summary(light.mod)





