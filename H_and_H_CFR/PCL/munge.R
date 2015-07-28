library(ggplot2)
library(dplyr)

data <-read.csv('../CFRssRaw.csv')
# data[,c('sub_num','class')] <- lapply(data[,c('sub_num','class')],factor)
data <- data %>% group_by(sub_num) %>% 
  mutate(abs_list =rep(1:12,as.vector(table(list,phase)[,2:1][table(list,phase)[,2:1]>0]))) %>% 
  group_by(sub_num, abs_list)   %>% 
  filter(score == 1 ) %>%
  mutate(CRT = cumsum(RT), 
         order = 1:n(),
         Nrecalled = sum(score)) %>% 
  arrange(desc(phase),practice) # dont need to arrange by subject since the data frame is grouped by it
dataFull <-expand.grid(class=levels(data$class), order= levels(factor(data$order)), time = seq(.1,90,.1),D=NA)
for (i in unique(dataFull$class)) {
  for(j in unique(dataFull$order)) {
    D <- density(data$CRT[data$class==i & data$order==j],bw=1,n=900,from=.1,to=90)$y
    D <-D/sum(D)
    D<-(sum(data$class==i & data$order==j)/(34*4)) * D
    dataFull$D[dataFull$class==i & dataFull$order==j] <- D
  }
}
dataFull <- dataFull %>% group_by(class) %>% 
  mutate(D = D/sum(D))

ggplot(dataFull,aes(x=time,y=D,color=class))+
  geom_line() +
  facet_grid(.~order) + 
  scale_x_continuous(limits=c(0,70)) + 
  scale_color_discrete("Condition",labels=c("No Practice", "Study", "Test"))
