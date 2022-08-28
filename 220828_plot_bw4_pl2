#for bw of 4th survival
getwd()
library(readxl)
X20220819survival_run4 <- read_excel("20220819survival_run4.xlsx", 
                                      sheet = "bw")
bw_4th <- X20220819survival_run4
bw_4th <- bw_4th[-c(32,48),]
#exclude the 2 non-sepsis outliers
bw_4th$Analgesic <- ifelse(bw_4th$Analgesic=="No","None",bw_4th$Analgesic)
bw_4th$Analgesic <- factor(bw_4th$Analgesic,
                          levels = c('None','Metapyrin','Melosus'))
bw_4th$Sex <- factor(bw_4th$Sex,
                     levels = c("F","M"))
bw_4th$Sex <- ifelse(bw_4th$Sex=="F","Female","Male")

colnames(bw_4th)

bw_4th[,9:17] <- (bw_4th[,9:17])/bw_4th$t0
save(bw_4th,file = "bw_4th.rda")

library(tidyverse)
bw_4th_1 <- bw_4th %>% gather(key = "Time",
                              value = "Weight",
                              "t0","t0,75","t1,75",
                              "t2,75","t3,75","t4,75",
                              "t5,75","t6,75","t7") 
#duplicate 9 times for each time point
#from 58 samples to 522 samples
pi2a <- ggline(bw_4th_1,"Time","Weight",linetype = "Analgesic",
       color = "Analgesic", size = 1.5,
       palette = c("#DE1A1A","#29BF12","#00A5CF"),
       add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0','0.75','1.75',
                              '2.75','3.75',
                              '4.75','5.75',
                              '6.75','7'))+
  labs(y='Relative body weight (g)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi2a
#Removed 293 rows containing non-finite values (stat_summary). 
#due to non-survival, the trend of change will be droped data as time went by

res_bw1 <- compare_means(Weight~Time,
                         group.by = "Analgesic",
                         data = bw_4th_1)
res_bw1
#108
res_bw1 <-  res_bw1 %>% filter(p.signif != "ns")
#43
res_bw1$y.position <- 0.8
fix(res_bw1)
#make manually later to save the next 2 comparision
save(res_bw1,file = "res_bw1.rda")
  geom_bracket(data = res3,aes(xmin=time,
                               xmax=time,
                               label=p.signif),
               label.size = 8,
               y.position = res3$y.position,
               linetype  = "blank")
########################################################
pi2b <- ggline(bw_4th_1,"Time","Weight",linetype = "Analgesic",
                 color = "Analgesic",
               facet.by = "Sex",
               size = 1.5,
                 palette = c("#DE1A1A","#29BF12","#00A5CF"),
                 add = "mean_se",error.plot = "upper_errorbar")+
    scale_x_discrete(labels = c('0','0.75','1.75',
                                '2.75','3.75',
                                '4.75','5.75',
                                '6.75','7'))+
    labs(y='Relative body weight (g)',x='Time (d)')+
    theme(axis.title.y = element_text(size=18,face='bold'),
          axis.text.y = element_text(size = 14,face = 'bold'),
          axis.title.x = element_text(size = 18,face = 'bold'),
          axis.text.x = element_text(size=14,face='bold'),
          legend.text=element_text(size=14,face = 'bold'),
          legend.title=element_blank(),
          strip.text.x = element_text(size = 18,face = 'bold'))
pi2b

res_bw2 <- compare_means(Weight~Time,
                         group.by = c("Analgesic","Sex"),
                         data = bw_4th_1)
#not enough (non-missing) 'x' observations
#test for female only
test_f <- bw_4th_1[bw_4th_1$Sex=="Female",]

res_bw2f <- compare_means(Weight~Time,
              group.by = c("Analgesic"),
              data = test_f)
#106
res_bw2f <-  res_bw2f %>% filter(p.signif != "ns")
#40
res_bw2f$y.position <- 0.8
fix(res_bw2f)
save(res_bw2f,file = "res_bw2f.rda")
compare_means(Weight~Analgesic,
              group.by = c("Time"),
              data = test_f)

test_m <- bw_4th_1[bw_4th_1$Sex=="Male",]

compare_means(Weight~Time,
              group.by = c("Analgesic"),
              data = test_m)
#! not enough (non-missing) 'x' observations
tiff(filename = 'pl2_0828.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl2 <- ggarrange(pi2a,pi2b,ncol = 1,
                 common.legend = T,
                 font.label = list(size=20),
                 labels = c("A","B"))
pl2
dev.off()
