library(ggmap)
library(jpeg)
library(grid)
library(cowplot)
library(FREddyPro)
library(data.table)
library(imager)

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
ch.site = get_map(location = c(coords),maptype = 'satellite',zoom = 10)

#get a regional image
can = get_map(location = c(coords),maptype = 'satellite',zoom = 4)

#read in the photo
# Load your JPEG image
img = load.image("./data/IMG_9051.jpg")

# Rotate the image by a specified angle (e.g., 45 degrees)
rotated_img = imrotate(img, angle = 90)

# Plot the rotated image
plot(rotated_img)
g     = rasterGrob(rotated_img, interpolate=T)

#north america plot
a = ggmap(ggmap = can)+
  geom_point(aes(coords[1],coords[2]),col='white',pch=24,fill='black',size=1)+
  theme(text = element_text(size = 6))

#site plot
b=ggmap(ggmap = ch.site)+
  geom_point(aes(coords[1],coords[2]),col='white',pch=24,fill='black',size=1)+
  theme(text = element_text(size = 6))
 # annotate(geom = 'text',label = 'CA-CF3',x = coords[1]+.0008,y = coords[2]+.0002,col='white')

ab = plot_grid(a,b,labels = c('a','b'),nrow = 2,label_size = 6)

jpeg(filename = './figures/figure1.jpg',width = 3,height = 2.5,units = 'in',res = 1500)
plot_grid(ab,g,labels = c('','c'),label_size = 6,nrow = 1,rel_widths = c(0.5,0.5))
dev.off()
