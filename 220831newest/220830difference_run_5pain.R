##compare the difference between run 1-3 and run 4
#pain
load("/Users/nanaliu/Desktop/combined_pain5.rda")
combined_pain5$run <- ifelse(combined_pain5$round==4,"Severe","Mild")
table(combined_pain5$run)

combined_pain5$run <- factor(combined_pain5$run,
                            levels = c("Mild","Severe"))
combined_pain5$sr <- paste(combined_pain5$Sex,combined_pain5$run,sep = ",")
combined_pain5$ar <- paste(combined_pain5$Analgesic,combined_pain5$run,
                          sep = ",")

################################################
table(combined_pain5$type)
combined_pain5s <- combined_pain5[combined_pain5$type=="s",]
#2208

pi5a <- ggline(combined_pain5s,'day','Pain',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar')+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='Cumulative grimace scale')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi5a

################################################
pi5b <- ggline(combined_pain5s,'day','Pain',
               #linetype = 'Analgesic',
               size = 1.5,
               color = 'ar',
               palette = c("#DE1A1A","red",
                           "#29BF12","green","#00A5CF","blue"),
               add ='mean',#error.plot = 'upper_errorbar',
               facet.by = "Sex")+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='Cumulative grimace scale')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi5b

pi5b2 <- ggline(combined_pain5s,'day','Pain',
               #linetype = 'Analgesic',
               size = 1.5,
               color = 'sr',
               palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                           "red","blue"),
               add ='mean',#error.plot = 'upper_errorbar',
               facet.by = "Analgesic")+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='Cumulative grimace scale')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi5b2
################################################
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
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
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
           size=3,
           fontface='bold')+
  scale_fill_discrete(labels=c('Cheek','Ear','Nose',
                               'Orbital','Whisker'))
pi5c

############################################
pi5d <- ggline(combined_pain5ns,'day','Pain',
               #linetype = 'Analgesic',
               size = 1.5,
               color = 'ar',
               palette = c("#DE1A1A","red",
                           "#29BF12","green","#00A5CF","blue"),
               add ='mean',#error.plot = 'upper_errorbar',
               facet.by = "type")+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='Grimace scale distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=14,face = 'bold'))

pi5d

pi5d2 <- ggline(combined_pain5ns,'day','Pain',
               #linetype = 'Analgesic',
               size = 1.5,
               color = 'sr',
               palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                           "red","blue"),
               add ='mean',#error.plot = 'upper_errorbar',
               facet.by = "type")+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='Grimace scale distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=14,face = 'bold'))

pi5d2

######################################################
pi5b_ar <- ggarrange(pi5b,pi5d,
                     common.legend = T)
pi5b_ar

pi5d_sr <- ggarrange(pi5b2,pi5d2,
                     common.legend = T)
pi5d_sr

tiff(filename = 'pl5for_run_0830.tiff', 
     units="cm",#scale = 1, 
     width=55, height=38, 
     res = 600, compression= "lzw")

pl5 <- ggarrange(pi5a,pi5b_ar,pi5c,pi5d_sr,
                 ncol = 2,nrow = 2,
                 font.label = list(size=20),
                 widths = c(.5,1,.5,1),
                 labels = c('A','B','C','D'))

pl5
dev.off()

