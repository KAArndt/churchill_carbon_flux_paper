library(data.table)
library(ggplot2)
library(cowplot)
Sys.setenv(tz='UTC')

df = fread('./data/error_churchill_validation.csv')

co2 = subset(df,df$valco2 == 'validation')

co2mod = lm(co2$median.rf.nee ~ co2$FC)
summary(co2mod)

ch4 = subset(df,df$valch4 == 'validation')

ch4mod = lm(ch4$median.rf.ch4 ~ ch4$FCH4)
summary(ch4mod)

co2p = ggplot(data = co2)+theme_bw()+
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0)+
  geom_point(aes(FC,median.rf.nee),alpha=0.25)+
  geom_abline(slope = 1,intercept = 0,col='red',lty=2)+
  scale_x_continuous(limits = c(-8,10),expression("EC NEE ("*mu*mol~CO[2]~m^-2~s^-1*")"))+
  scale_y_continuous(limits = c(-8,10),expression("RF NEE ("*mu*mol~CO[2]~m^-2~s^-1*")"))+
  annotate(geom = 'text',x = 6,y = -6,label = expression(R^2~'='~0.86),size=2)+
  annotate(geom = 'text',x = 6,y = -5,label = 'slope = 0.81',size=2)+
  theme(text = element_text(size = 7))


ch4p = ggplot(data = ch4)+theme_bw()+
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0)+
  geom_point(aes(FCH4,median.rf.ch4),alpha=0.25)+
  geom_abline(slope = 1,intercept = 0,col='red',lty=2)+
  scale_x_continuous(limits = c(-70,150),expression("EC"~CH[4]~Flux~"("*nmol~CH[4]~m^-2~s^-1*")"))+
  scale_y_continuous(limits = c(-70,150),expression("RF"~CH[4]~Flux~"("*nmol~CH[4]~m^-2~s^-1*")"))+
  annotate(geom = 'text',x = 100,y = -50,label = expression(R^2~'='~0.85),size=2)+
  annotate(geom = 'text',x = 100,y = -40,label = 'slope = 0.83',size=2)+
  theme(text = element_text(size = 7))

png('./figures/rf_validation.png',width = 4,height = 3,units = 'in',res = 1500)
plot_grid(co2p,ch4p)
dev.off()
