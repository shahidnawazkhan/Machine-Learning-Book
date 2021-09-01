library(tidyverse)

## make data
##dat <- matrix(rnorm(100, 3, 1), ncol=10)
dat <- read.csv("myData/reg_flow.csv", stringsAsFactors=FALSE)

## reshape data (tidy/tall form)

## plot data
p<- ggplot(subset(dat, orig_reg %in% c(100,200,300, 400,500,600,700,800,900,1000,1100,1200)), aes(orig_reg,dest_reg, fill =flow, label= flow))+
#ggplot(dat, aes(orig_reg, dest_reg)) +
  scale_x_discrete(name="X (Meters)", breaks= c(100,200,300, 400,500,600,700,800,900,1000,1100,1200), labels = c('100', '200', '300','400','500','600','700','800','900','1000','Radeloof','FHSZ')) +
  scale_y_discrete(name="Y (Meters)", breaks= c(1000,900,800,700,600,500,400,300,200,100,1100,1200), labels = c('1000','900','800','700','600','500','400','300','200','100','Radeloof','FHSZ')) +
  geom_tile(aes(fill = flow)) + 
  geom_text(aes(label = round(flow, 1))) +
  scale_fill_gradient(low = "white", high = "red")+coord_flip()
p

