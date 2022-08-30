##compare the difference between run 1-3 and run 4
#css
load("/Users/nanaliu/Desktop/combined_css5.rda")
table(combined_css5$type)
#5
table(combined_css5$day)
#16
11040/138
#16*5
16*80*5
combined_css5[6400:6458,]
combined_css5[11000:11040,]
combined_css5$run <- "Mild"
combined_css5$run[(6401:11040)] <- "Severity"
table(combined_css5$run)
#80*80 80*58
combined_css5$run <- factor(combined_css5$run,
                            levels = c("Mild","Severity"))
combined_css5$sr <- paste(combined_css5$Sex,combined_css5$run,sep = ",")
combined_css5$ar <- paste(combined_css5$Analgesic,combined_css5$run,
                          sep = ",")
####################################################
table(combined_css5$type)
combined_css5o <- combined_css5[combined_css5$type=="o",]

pi4a <- ggline(combined_css5o,'day','CSS',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar')+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi4a

######################################################

pi4b <- ggline(combined_css5o,'day','CSS',
               #linetype = 'Analgesic',
               size = 1.5,
               color = 'ar',
               palette = c("#DE1A1A","red",
               "#29BF12","green","#00A5CF","blue"), #maximum_red, kelly_green,rich_blue
               #palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "Sex")+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi4b

######################################################
pi4b2 <- ggline(combined_css5o,'day','CSS',
               #linetype = 'Analgesic',
               size = 1.5,
               color = 'sr',
               palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                           "red","blue"),
               #palette = c("#DE1A1A","red",
               #            "#29BF12","green","#00A5CF","blue"), #maximum_red, kelly_green,rich_blue
               #palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "Analgesic")+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi4b2

######################################################
combined_css5no <- combined_css5[combined_css5$type !="o",]
#8832
table(combined_css5no$type)
library(dplyr)
combined_css5no$type <- recode(combined_css5no$type, 
                               'a' ='Activity','p' ='Posture',
                               'r'  ='Reaction','s' ='Symptomatic')

n1 <- c(30,30,30,30,29,24,24,24,21,19,19,19,19,19,19,19)
n2 <- c(31,31,31,31,27,27,24,23,23,23,23,18,18,18,17,17)
n3 <- c(19,19,19,19,18,18,17,16,16,16,16,16,16,16,16,16)
n4=c(58,58,58,52,28,22,18,16,15,14,14,14,13,13,13,12)
n=n1+n2+n3+n4
#the number of mice to be observed
pi4c <- ggplot(combined_css5no,aes(x=day,y=CSS,fill=type))+
  theme_classic()+
  geom_bar(position = 'fill',stat = 'identity')+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time(d)',y='CSS distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.position="top",
        legend.text=element_text(size=12,face = 'bold'))+
  annotate('text',
           x=1:length(table(combined_css5no$day)),
           y=0.05,
           label=n,
           col='black',
           vjust=-1,
           size=3,
           fontface='bold')
pi4c
######################################################

pi4d <- ggline(combined_css5no,'day','CSS',
               #linetype = 'Analgesic',size = 1.5,
               color = 'ar',
               palette = c("#DE1A1A","red",
                           "#29BF12","green","#00A5CF","blue"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "type")+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='CSS distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=14,face = 'bold'))

pi4d

pi4d2 <- ggline(combined_css5no,'day','CSS',
               #linetype = 'Analgesic',size = 1.5,
               color = 'sr',
               palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                           "red","blue"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "type")+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  labs(x='Time (d)',y='CSS distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=14,face = 'bold'))

pi4d2
######################################################
pi4b_ar <- ggarrange(pi4b,pi4d,
                     common.legend = T)
pi4b_ar

pi4d_sr <- ggarrange(pi4b2,pi4d2,
                     common.legend = T)
pi4d_sr

tiff(filename = 'pl4for_run_0830.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl4 <- ggarrange(pi4a,pi4b_ar,pi4c,pi4d_sr,
                 ncol = 2,nrow = 2,
                 font.label = list(size=20),
                 widths = c(.4,1,.4,1),
                 labels = c('A','B','C','D'))

pl4
dev.off()

