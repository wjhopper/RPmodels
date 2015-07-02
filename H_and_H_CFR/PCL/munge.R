df <-read.csv('../CFRssRaw.csv')
library(ggplot2)
library(dplyr)
dedup <- function(a,b) { 
  if (any(is.na(a))) {
    a[!is.na(a)] <- 1:length(a[!is.na(a)])
  }
  return(a)
}
df[,c('sub_num','class')] <- lapply(df[,c('sub_num','class')],factor)
df <- df %>% group_by(sub_num) %>% 
  mutate(abs_list =rep(1:12,as.vector(table(list,phase)[,2:1][table(list,phase)[,2:1]>0])))
df <- df %>% group_by(sub_num, abs_list)   %>% 
  mutate(CRT = cumsum(RT[!is.nan(RT)]), 
         practice = practice[1],
         order = 1:length(order),
         Nrecalled = sum(score %in% c(0,1))) 
df$order[df$score ==2] <- NA
df <- df %>% group_by(sub_num,abs_list) %>%
  mutate(order = dedup(order,score)) %>% 
  ungroup() %>% 
  arrange(desc(phase),practice)
df$score <- factor(df$score, levels = c(0,1))
df <- df %>% ungroup() %>% arrange(sub_num,practice)
df2 <- df %>% filter(score %in% 1) %>% group_by(sub_num,abs_list) %>% 
  mutate(N=factor(1:n()),
         grpSz = n()) 
df3 <-expand.grid(class=levels(df2$class), N= levels(df2$N), time = seq(.1,90,.1),D=NA)
for (i in unique(df2$class)){
  for(j in unique(df2$N)){
  D <- density(df2$CRT[df2$class==i & df2$N==j],bw=1,n=900,from=.1,to=90)$y
  D <-D/sum(D)
  D<-(sum(df2$class==i & df2$N==j)/(34*4)) * D
  df3$D[df3$class==i & df3$N==j] <- D
  }
}
df3 <- df3 %>% group_by(class) %>% 
  mutate(D = D/sum(D))

ggplot(df3,aes(x=time,y=D,color=class))+
  geom_line() +
  facet_grid(.~N) + 
  scale_x_continuous(limits=c(0,70)) + 
  scale_color_discrete("Condition",labels=c("No Practice", "Study", "Test"))
# df4 <- df2 %>% group_by(class,N) %>% 
#   summarise(herp = n())
