
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
#gs = subset(df, df$season_name %in% c("Growing Season") & df$PPFD_IN > 0)
gs = subset(df, df$TS_2_05_1 > 5  & df$PPFD_IN > 0)

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


#plot the fit data vs the real PAR, NEE relationship
lrc <- ggplot(data = gs)+
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), 
    axis.title.y = element_text(size = 14),axis.title.x = element_text(size = 14))+
  geom_hline(aes(yintercept = 0),col = 'black')+
  geom_point(aes(PPFD_IN,FC,color=TA))+
  geom_line(aes(PPFD_IN,fit*-1),col='red')+
  annotate("text", 
           x = Inf, y = Inf, 
           hjust = 1.1, vjust = 1.3,
           label = paste0("\u03B1 = ", round(alpha, 3), "\nGPP ref = ", round(GPP_ref, 2)),
           size = 5)+
  labs(
    y = expression('Half-hourly CO '[2] * ' Flux ('*mu*mol~m^-2~s^-1*')'),
    x = expression('PAR In ('*mu*'mol m'^-2*'s'^-1*')'),
    title = "Light Response Curve - 5cm Soil Temp > 5",
    color = expression("Air T ("*degree*"C)"))
lrc


#how good did this model perform?
lr.mod = lm(gs$FC ~ fit*-1)
lr.mod_summary <- summary(lr.mod)
lr.mod_summary

#Store r squared for plotting
r_sq <- lr.mod_summary$r.squared

#see how it compares to the real data
lm <- ggplot(data = gs,aes(fit*-1,FC))+
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), 
    axis.title.y = element_text(size = 14),axis.title.x = element_text(size = 14))+
  geom_point(aes(color = TA))+
  geom_smooth(method = 'lm', color= "red")+
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


png(filename = './lrc_v8.png',width = 14,height = 6.72,units = 'in',res = 2000)
lrc + lm + plot_layout(guides = "collect", axes = "collect")
dev.off()


#approximately what doy is here?
#gs$doy = as.numeric(format(gs$date,'%j'))





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





