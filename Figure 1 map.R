rm(list = ls())

library(ggmap)
library(jpeg)
library(grid)
library(cowplot)
library(FREddyPro)
library(data.table)

# df = fread('./AMF_CA-CF3_BASE_HH_1-5.csv',na.strings = c('-9999'))
# 
# df$ts = as.POSIXct(as.character(df$TIMESTAMP_END),format = '%Y%m%d%H%M',tz = 'UTC')
# 
# df = subset(df,df$ts >= as.POSIXct('2023-01-01') & df$ts <= as.POSIXct('2023-12-31'))
# 
# fp = Average(fetch = 300,height = 2.8,speed = df$WS,direction = df$WD,uStar = df$USTAR,zol = df$ZL,sigmaV = df$V_SIGMA)
# 
# plotFootprint(fp)

#personal API key, REMOVE BEFORE PUSHING TO GIT!!!!!!!!!!!!!!
register_google(key = '')
#tester
#coordinates of the churchill tower
coords = c(-93.830758,58.665705)

#get a zoomed in site photo
ch.site = get_map(location = c(coords),maptype = 'satellite',zoom = 12)

#get a regional image
can = get_map(location = c(coords),maptype = 'satellite',zoom = 3)

#read in the photo
image = readJPEG('PXL_20220924_001408385.jpg')
g     = rasterGrob(image, interpolate=T)

#north america plot
a = ggmap(ggmap = can)+
  geom_point(aes(coords[1],coords[2]),col='white',pch=24,fill='black',size=0.75)+
  theme(text = element_text(size = 6))

#site plot
b=ggmap(ggmap = ch.site)+
  geom_point(aes(coords[1],coords[2]),col='white',pch=24,fill='black',size=0.75)+
  theme(text = element_text(size = 6))
 # annotate(geom = 'text',label = 'CA-CF3',x = coords[1]+.0008,y = coords[2]+.0002,col='white')

ab = plot_grid(a,b,labels = c('a','b'),nrow = 2,label_size = 6)

jpeg(filename = './figure1.jpg',width = 4,height = 3,units = 'in',res = 1500)
plot_grid(ab,g,labels = c('','c'),label_size = 6,nrow = 1,rel_widths = c(0.4,0.6))
dev.off()
