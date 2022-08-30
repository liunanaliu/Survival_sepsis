#for temp of 4th survival
library(readxl)
X20220819survival_run4 <- read_excel("20220819survival_run4.xlsx", 
                                       sheet = "temp")

temp_4th <- X20220819survival_run4
temp_4th <- temp_4th[-c(32,48),]
#exclude the 2 non-sepsis outliers
temp_4th$Analgesic <- ifelse(temp_4th$Analgesic=="No","None",temp_4th$Analgesic)
temp_4th$Analgesic <- factor(temp_4th$Analgesic,
                           levels = c('None','Metapyrin','Melosus'))
temp_4th$Sex <- factor(temp_4th$Sex,
                     levels = c("F","M"))
temp_4th$Sex <- ifelse(temp_4th$Sex=="F","Female","Male")

colnames(temp_4th)

temp_4th[,9:24] <- (temp_4th[,9:24])/temp_4th$t0
save(temp_4th,file = "temp_4th.rda")


library(tidyverse)
temp_4th_1 <- temp_4th %>% gather(key = "Time",
                              value = "Temp",
                              "t0","t0,25","t0,75","t1,25","t1,75",
                              "t2,25","t2,75","t3,25","t3,75","t4,25","t4,75",
                              "t5,25","t5,75","t6,25","t6,75","t7") 
#duplicate 16 times for each time point
#from 58 samples to 522 samples
pi3a <- ggline(temp_4th_1,"Time","Temp",linetype = "Analgesic",
               color = "Analgesic", size = 1.5,
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0',' ','0.75',' ','1.75',
                              ' ','2.75',' ','3.75',
                              ' ','4.75',' ','5.75',
                              ' ','6.75',' '))+
  labs(y='Relative body temperautre (°C)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi3a
#Removed 293 rows containing non-finite values (stat_summary). 
#due to non-survival, the trend of change will be droped data as time went by

res_temp1 <- compare_means(Temp~Time,
                         group.by = "Analgesic",
                         data = temp_4th_1)
res_temp1
#360
res_temp1 <-  res_temp1 %>% filter(p.signif != "ns")
#145
res_temp1$y.position <- 0.8
fix(res_temp1)
#make manually later to save the next 2 comparision
save(res_temp1,file = "res_temp1.rda")
geom_bracket(data = res3,aes(xmin=time,
                             xmax=time,
                             label=p.signif),
             label.size = 8,
             y.position = res3$y.position,
             linetype  = "blank")
########################################################
pi3b <- ggline(temp_4th_1,"Time","Temp",linetype = "Analgesic",
               color = "Analgesic",
               facet.by = "Sex",
               size = 1.5,
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0',' ','0.75',' ','1.75',
                              ' ','2.75',' ','3.75',
                              ' ','4.75',' ','5.75',
                              ' ','6.75',' '))+
  labs(y='Relative body temperautre (°C)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=14,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi3b

res_temp2 <- compare_means(Temp~Time,
                         group.by = c("Analgesic","Sex"),
                         data = temp_4th_1)
#not enough (non-missing) 'x' observations
#test for female only
test_f <- temp_4th_1[temp_4th_1$Sex=="Female",]

res_temp2f <- compare_means(Temp~Time,
                          group.by = c("Analgesic"),
                          data = test_f)
res_temp2f
#354
res_temp2f <-  res_temp2f %>% filter(p.signif != "ns")
#111
res_temp2f$y.position <- 0.8
fix(res_temp2f)
save(res_temp2f,file = "res_temp2f.rda")
compare_means(Temp~Analgesic,
              group.by = c("Time"),
              data = test_f)

test_m <- temp_4th_1[temp_4th_1$Sex=="Male",]

compare_means(Temp~Time,
              group.by = c("Analgesic"),
              data = test_m)
#! not enough (non-missing) 'x' observations
tiff(filename = 'pl3_0828.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl3 <- ggarrange(pi3a,pi3b,ncol = 1,
                 common.legend = T,
                 font.label = list(size=20),
                 labels = c("A","B"))
pl3
dev.off()
