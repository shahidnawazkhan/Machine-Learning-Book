library(tidyverse)

## make data

dat <- read.csv("myData/reg_flow.csv", stringsAsFactors=FALSE)

## plot data
p<- ggplot(dat, aes(orig_reg, dest_reg)) +
  scale_x_continuous(name="Distance (meters)") +
  scale_y_continuous(name="Distance (meters)") +
  #scale_x_discrete(name="X (Meters)", breaks= c(100,200,300, 400,500,600,700,800,900,1000,1100,1200), labels = c('100', '200', '300','400','500','600','700','800','900','1000','Radeloof','FHSZ')) +
  #scale_y_discrete(name="Y (Meters)", breaks= c(1000,900,800,700,600,500,400,300,200,100,1100,1200), labels = c('1000','900','800','700','600','500','400','300','200','100','Radeloof','FHSZ')) +
  geom_tile(aes(fill = Percent_Area)) + 
  geom_text(aes(label = round(Percent_Area, 1))) +
  scale_fill_gradient(low = "green", high = "red")+coord_flip()

ggsave("1 heatmap1.jpeg", p, height=10, width=14, limitsize= TRUE)

