###compare the difference between run 1-3 and run 4
#temp for non-survival and survival
load("/Users/nanaliu/Desktop/temp_4th.rda")
temp_4th$Status <- ifelse(temp_4th$`Status             (0=live, 1 = dead)`==0,
                          "Survivor","Non-survivor")

temp_4th$as <- paste(temp_4th$Analgesic,temp_4th$Status,sep = ",")
table(temp_4th$as)

temp_4th$ss <- paste(temp_4th$Sex,temp_4th$Status,
                         sep = ",")
table(temp_4th$ss)

library(tidyverse)
temp_4th_1 <- temp_4th %>% gather(key = "Time",
                                  value = "Temp",
                                  "t0","t0,25","t0,75","t1,25","t1,75",
                                  "t2,25","t2,75","t3,25","t3,75","t4,25","t4,75",
                                  "t5,25","t5,75","t6,25","t6,75","t7") 
save(temp_4th_1,file = "temp_4thsurvival.rda")
#############################################
library(ggplot2)
library(ggplotify)
library(ggpubr)

pi3a_s <- ggline(temp_4th_1,"Time","Temp",#linetype = "Analgesic",
               color = "Status", size = 1.5,
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0',' ','',' ','1.75',
                              ' ','',' ','3.75',
                              ' ','',' ','5.75',
                              ' ','','7'))+
  labs(y='Relative body temperautre (°C)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi3a_s

pi3a_ss <- ggline(temp_4th_1,"Time","Temp",#linetype = "Analgesic",
                 color = "ss", size = 1.5,
                 palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                             "red","blue"),
                 add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0',' ','',' ','1.75',
                              ' ','',' ','3.75',
                              ' ','',' ','5.75',
                              ' ','','7'))+
  labs(y='Relative body temperautre (°C)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi3a_ss

pi3a_as <- ggline(temp_4th_1,"Time","Temp",#linetype = "Analgesic",
                  color = "as", size = 1.5,
                  palette = c("#DE1A1A","red",
                              "#29BF12","green","#00A5CF","blue" #maximum_red, kelly_green,rich_blue
                  ),
                  add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0',' ','',' ','1.75',
                              ' ','',' ','3.75',
                              ' ','',' ','5.75',
                              ' ','','7'))+
  labs(y='Relative body temperautre (°C)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi3a_as

pi3a_ass <- ggline(temp_4th_1,"Time","Temp",#linetype = "Analgesic",
                  color = "as", size = 1.5,
                  facet.by = "Sex",
                  palette = c("#DE1A1A","red",
                              "#29BF12","green","#00A5CF","blue" #maximum_red, kelly_green,rich_blue
                              ),
                  add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0',' ','',' ','1.75',
                              ' ','',' ','3.75',
                              ' ','',' ','5.75',
                              ' ','','7'))+
  labs(y='Relative body temperautre (°C)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi3a_ass


#############################################
pl3c <- ggarrange(pi3a_as,pi3a_ass,
                  common.legend = T,
                  #widths = c(1,1)
                  ncol = 1,
                  labels = c("B","D"))
pl3c
tiff(filename = 'pl3for_survival_0830.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl3 <- ggarrange(pi3a_s,pi3a_as,pi3a_ss,pi3a_ass,
                 #byrow=T,
                 #heights = c(0.5,0.5,1),
                 # common.legend = T,
                 font.label = list(size=20),
                 labels = c("A","B","C","D"))
pl3
dev.off()
#############################################
