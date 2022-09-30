#for pain of 4th survival
library(readxl)
X20220819survival_run4 <- read_excel("20220819survival_run4.xlsx", 
                                      sheet = "pain")

pain_4th <- X20220819survival_run4
pain_4th <- pain_4th[-c(32,48),]
#exclude the 2 non-sepsis outliers
pain_4th$Analgesic <- ifelse(pain_4th$Analgesic=="No","None",pain_4th$Analgesic)
pain_4th$Analgesic <- factor(pain_4th$Analgesic,
                            levels = c('None','Metapyrin','Melosus'))
pain_4th$Sex <- factor(pain_4th$Sex,
                      levels = c("F","M"))
pain_4th$Sex <- ifelse(pain_4th$Sex=="F","Female","Male")

(colnames(pain_4th))
pain_4th$...89 <- NULL
save(pain_4th,file = "pain_4th.rda")
#pain type: o,n,c,e,w
pain_4th_1 <- gather(pain_4th,key = "Time",value = "Pain",
       't0o','t0,25o','t0,75o','t1,25o','t1,75o',
       't2,25o','t2,75o','t3,25o','t3,75o',
       't4,25o','t4,75o','t5,25o','t5,75o',
       't6,25o','t6,75o','t7o',
       't0n','t0,25n','t0,75n','t1,25n','t1,75n',
       't2,25n','t2,75n','t3,25n','t3,75n',
       't4,25n','t4,75n','t5,25n','t5,75n',
       't6,25n','t6,75n','t7n',
       't0c','t0,25c','t0,75c','t1,25c','t1,75c',
       't2,25c','t2,75c','t3,25c','t3,75c',
       't4,25c','t4,75c','t5,25c','t5,75c',
       't6,25c','t6,75c','t7c',
       't0e','t0,25e','t0,75e','t1,25e','t1,75e',
       't2,25e','t2,75e','t3,25e','t3,75e',
       't4,25e','t4,75e','t5,25e','t5,75e',
       't6,25e','t6,75e','t7e',
       't0w','t0,25w','t0,75w','t1,25w','t1,75w',
       't2,25w','t2,75w','t3,25w','t3,75w',
       't4,25w','t4,75w','t5,25w','t5,75w',
       't6,25w','t6,75w','t7w')
pain_4th_1
pain_4th_1$type <- stringi::stri_sub(pain_4th_1$Time,-1)
table(pain_4th_1$type)
library(profvis)
library(tidyverse)
pain_4th_1$day <- stringi::stri_sub(pain_4th_1$Time,2,
                                    stringr::str_length(pain_4th_1$Time)-1)
table(pain_4th_1$day)
#####################################
colnames(pain_4th_1)
#this is for sum of pain,
pain_4th_2 <- pain_4th
colnames(pain_4th_2)
pain_4th_2$`t0s` <- rowSums(pain_4th_2[,c(9:13)])
pain_4th_2$`t0,25s` <- rowSums(pain_4th_2[,c(14:18)])
pain_4th_2$`t0,75s` <- rowSums(pain_4th_2[,c(19:23)])
pain_4th_2$`t1,25s` <- rowSums(pain_4th_2[,c(24:28)])
pain_4th_2$`t1,75s` <- rowSums(pain_4th_2[,c(29:33)])
pain_4th_2$`t2,25s` <- rowSums(pain_4th_2[,c(34:38)])
pain_4th_2$`t2,75s` <- rowSums(pain_4th_2[,c(39:43)])
pain_4th_2$`t3,25s` <- rowSums(pain_4th_2[,c(44:48)])
pain_4th_2$`t3,75s` <- rowSums(pain_4th_2[,c(49:53)])
pain_4th_2$`t4,25s` <- rowSums(pain_4th_2[,c(54:58)])
pain_4th_2$`t4,75s` <- rowSums(pain_4th_2[,c(59:63)])
pain_4th_2$`t5,25s` <- rowSums(pain_4th_2[,c(64:68)])
pain_4th_2$`t5,75s` <- rowSums(pain_4th_2[,c(69:73)])
pain_4th_2$`t6,25s` <- rowSums(pain_4th_2[,c(74:78)])
pain_4th_2$`t6,75s` <- rowSums(pain_4th_2[,c(79:83)])
pain_4th_2$`t7s` <- rowSums(pain_4th_2[,c(84:88)])
#pain_4th_2_2 <- pain_4th_2[,c(89:104)]

#pain type: o,n,c,e,w
pain_4th_1_2 <- gather(pain_4th_2,key = "Time",value = "Pain",
                     't0o','t0,25o','t0,75o','t1,25o','t1,75o',
                     't2,25o','t2,75o','t3,25o','t3,75o',
                     't4,25o','t4,75o','t5,25o','t5,75o',
                     't6,25o','t6,75o','t7o',
                     't0n','t0,25n','t0,75n','t1,25n','t1,75n',
                     't2,25n','t2,75n','t3,25n','t3,75n',
                     't4,25n','t4,75n','t5,25n','t5,75n',
                     't6,25n','t6,75n','t7n',
                     't0c','t0,25c','t0,75c','t1,25c','t1,75c',
                     't2,25c','t2,75c','t3,25c','t3,75c',
                     't4,25c','t4,75c','t5,25c','t5,75c',
                     't6,25c','t6,75c','t7c',
                     't0e','t0,25e','t0,75e','t1,25e','t1,75e',
                     't2,25e','t2,75e','t3,25e','t3,75e',
                     't4,25e','t4,75e','t5,25e','t5,75e',
                     't6,25e','t6,75e','t7e',
                     't0w','t0,25w','t0,75w','t1,25w','t1,75w',
                     't2,25w','t2,75w','t3,25w','t3,75w',
                     't4,25w','t4,75w','t5,25w','t5,75w',
                     't6,25w','t6,75w','t7w',
                     't0s','t0,25s','t0,75s','t1,25s','t1,75s',
                     't2,25s','t2,75s','t3,25s','t3,75s',
                     't4,25s','t4,75s','t5,25s','t5,75s',
                     't6,25s','t6,75s','t7s')
pain_4th_1_2
pain_4th_1_2$type <- stringi::stri_sub(pain_4th_1_2$Time,-1)
table(pain_4th_1_2$type)
#s is for the sum, 928 for each, *6 = 5568
library(profvis)
library(tidyverse)
pain_4th_1_2$day <- stringi::stri_sub(pain_4th_1_2$Time,2,
                                    stringr::str_length(pain_4th_1_2$Time)-1)
table(pain_4th_1_2$day)
save(pain_4th_1_2,file = "pain_4th_1_2.rda")
pain_4th_1_2s <- pain_4th_1_2[pain_4th_1_2$type=="s",]
#928
summary(pain_4th_1_2s)
summary(pain_4th)
data.frame(pain_4th_2[8,])
pi5a <- ggline(pain_4th_1_2s,'day','Pain',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar')+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time (d)',y='Cumulative grimace scale')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi5a


pi5b <- ggline(pain_4th_1_2s,'day','Pain',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "Sex")+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time (d)',y='Cumulative grimace scale')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi5b

colnames(pain_4th_1_2ns)
pain_4th_1_2ns <- pain_4th_1_2[pain_4th_1_2$type !="s",]
#4640
table(pain_4th_1_2ns$day)
sum(is.na(pain_4th_2$t0s))
#0
sum(is.na(pain_4th_2$`t0,25s`))
sum(is.na(pain_4th_2$`t0,75s`))
sum(is.na(pain_4th_2$`t1,25s`))
#6
sum(is.na(pain_4th_2$`t1,75s`))
#30
sum(is.na(pain_4th_2$`t2,25s`))
#36
sum(is.na(pain_4th_2$`t2,75s`))
#40
sum(is.na(pain_4th_2$`t3,25s`))
#42
sum(is.na(pain_4th_2$`t3,75s`))
#43
sum(is.na(pain_4th_2$`t4,25s`))
#44
sum(is.na(pain_4th_2$`t4,75s`))
#44
sum(is.na(pain_4th_2$`t5,25s`))
#44
sum(is.na(pain_4th_2$`t5,75s`))
#45
sum(is.na(pain_4th_2$`t6,25s`))
#45
sum(is.na(pain_4th_2$`t6,75s`))
#45
sum(is.na(pain_4th_2$t7s))
#46
n=c(58,58,58,52,28,22,18,16,15,14,14,14,13,13,13,12)
#the number of mice to be observed
pi5c <- ggplot(pain_4th_1_2ns,aes(x=day,y=Pain,fill=type))+
  theme_classic()+
  geom_bar(position = 'fill',stat = 'identity')+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time(d)',y='Grimace scale distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.position="top",
        legend.text=element_text(size=12,face = 'bold'))+
  annotate('text',
           x=1:length(table(pain_4th_1_2ns$day)),
           y=0.05,
           label=n,
           col='black',
           vjust=-1,
           size=4,
           fontface='bold')+
  scale_fill_discrete(labels=c('Cheek','Ear','Nose',
                               'Orbital','Whisker'))
pi5c

pain_4th_1_2ns$type <- recode(pain_4th_1_2ns$type,'c'='Cheek bulge', 
                              'e' ='Ear position', 
                              'n' ='Nose bulge',
                              'o'  ='Orbital tightening', 
                              'w' ='Whisker change')
pi5d <- ggline(pain_4th_1_2ns,'day','Pain',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "type")+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time (d)',y='Grimace scale distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=14,face = 'bold'))

pi5d

############################################
tiff(filename = 'pl5_0829.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl5 <- ggarrange(pi5a,pi5b,pi5c,pi5d,#ncol = 2,
                 font.label = list(size=20),
                 #widths = c(.6,1,.6,1),
                 labels = c('A','B','C','D'))

pl5
dev.off()
