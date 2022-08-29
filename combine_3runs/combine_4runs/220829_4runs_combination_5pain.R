##combine 4 runs regardless of severity
#pl4 for pain
load("/Users/nanaliu/Downloads/pain_overall_long.rda")
pain_overall_wide$SEK00 <- as.numeric(as.character(pain_overall_wide$SEK00))
summary(pain_overall_wide)

pain_overall_wide$Sex <- factor(pain_overall_wide$Sex,
                                 levels = c("Female","Male"))

pain_overall_wide$Analgesic <- factor(pain_overall_wide$Analgesic,
                                       levels = c('None','Metapyrin','Melosus'))

names(pain_overall_wide)[c(2,8,15)] <- c("SEK00-","day","s")

pain_overall_wide2 <- gather(pain_overall_wide,key = "type",value = "Pain",
       "c","e","n","o","w","s")
load("/Users/nanaliu/Desktop/pain_4th_1_2.rda")
names(pain_4th_1_3) 
names(pain_overall_wide2)
table(pain_4th_1_2$type)
pain_4th_1_2$round = 4
pain_4th_1_3 <- pain_4th_1_2[,-c(1,5,9)]
pain_overall_wide2$No <- NULL
pain_overall_wide2 <- pain_overall_wide2 %>% select("SEK00-","LabID",
                                      "Sex","Analgesic","Duration (days)",
                                      "Status             (0=live, 1 = dead)",
                                      "Pain","type",
                                      "day","round")

combined_pain5 <- rbind(pain_overall_wide2,pain_4th_1_3)
save(combined_pain5,file = "combined_pain5.rda")

################################################
table(combined_pain5$type)
combined_pain5s <- combined_pain5[combined_pain5$type=="s",]
#2208

pi5a <- ggline(combined_pain5s,'day','Pain',
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


pi5b <- ggline(combined_pain5s,'day','Pain',
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

################################################
table(combined_pain5$type)
combined_pain5ns <- combined_pain5[combined_pain5$type!="s",]
#11040

n1 <- c(30,30,30,30,29,24,24,24,21,19,19,19,19,19,19,19)
n2 <- c(31,31,31,31,27,27,24,23,23,23,23,18,18,18,17,17)
n3 <- c(19,19,19,19,18,18,17,16,16,16,16,16,16,16,16,16)
n4=c(58,58,58,52,28,22,18,16,15,14,14,14,13,13,13,12)
n=n1+n2+n3+n4

combined_pain5ns$type <- recode(combined_pain5ns$type,'c'='Cheek bulge', 
                                'e' ='Ear position', 
                                'n' ='Nose bulge',
                                'o'  ='Orbital tightening', 
                                'w' ='Whisker change')
pi5c <- ggplot(combined_pain5ns,aes(x=day,y=Pain,fill=type))+
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
           x=1:length(table(combined_pain5ns$day)),
           y=0.05,
           label=n,
           col='black',
           vjust=-1,
           size=4,
           fontface='bold')+
  scale_fill_discrete(labels=c('Cheek','Ear','Nose',
                               'Orbital','Whisker'))
pi5c


pi5d <- ggline(combined_pain5ns,'day','Pain',
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
tiff(filename = 'pl5for5_0829.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl5 <- ggarrange(pi5a,pi5b,pi5c,pi5d,#ncol = 2,
                 font.label = list(size=20),
                 #widths = c(.6,1,.6,1),
                 labels = c('A','B','C','D'))

pl5
dev.off()

