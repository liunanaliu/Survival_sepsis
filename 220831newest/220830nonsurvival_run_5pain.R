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

combined_pain5$Status <- ifelse(combined_pain5$`Status             (0=live, 1 = dead)`==0,
                                "Survivor","Non-survivor")
table(combined_pain5$Status)

combined_pain5$rs <- paste(combined_pain5$run,combined_pain5$Status,
                          sep = ",")
combined_pain5$as <- paste(combined_pain5$Analgesic,combined_pain5$Status,
                          sep = ",")
combined_pain5$ss <- paste(combined_pain5$Sex,combined_pain5$Status,
                          sep = ",")
save(combined_pain5,file = "combined_pain5survival.rda")
load("/Users/nanaliu/Desktop/220830combination_4runs/combined_pain5survival.rda")
#############################################
library(ggplot2)
library(ggplotify)
library(ggpubr)

table(combined_pain5$type)
combined_pain5s <- combined_pain5[combined_pain5$type=="s",]
#2208
pi5a_s <- ggline(combined_pain5s,'day','Pain',
                 #linetype = 'Analgesic',
                 size = 1.5,
                 color = 'Status',
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

pi5a_s

pi5a_ss <- ggline(combined_pain5s,'day','Pain',
                  #linetype = 'Analgesic',
                  size = 1.5,
                  color = 'ss',
                  palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                              "red","blue"),
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
        legend.text=element_text(size=18,face = 'bold'))+
  guides(colour=guide_legend(nrow=2,byrow = T))

pi5a_ss
######################################################

pi5b <- ggline(combined_pain5s,'day','Pain',
               #linetype = 'Analgesic',
               size = 1.5,
               color = 'as',
               palette = c("#DE1A1A","red",
                           "#29BF12","green","#00A5CF","blue"), #maximum_red, kelly_green,rich_blue
               #palette = c("#DE1A1A","#29BF12","#00A5CF"),
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

######################################################
pi5b2 <- ggline(combined_pain5s,'day','Pain',
                #linetype = 'Analgesic',
                size = 1.5,
                color = 'ss',
                palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                            "red","blue"),
                #palette = c("#DE1A1A","red",
                #            "#29BF12","green","#00A5CF","blue"), #maximum_red, kelly_green,rich_blue
                #palette = c("#DE1A1A","#29BF12","#00A5CF"),
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

######################################################
######################################################
table(combined_pain5$type)
combined_pain5ns <- combined_pain5[combined_pain5$type!="s",]
library(dplyr)
combined_pain5ns$type <- recode(combined_pain5ns$type,'c'='Cheek bulge', 
                                'e' ='Ear position', 
                                'n' ='Nose bulge',
                                'o'  ='Orbital tightening', 
                                'w' ='Whisker change')

n1 <- c(30,30,30,30,29,24,24,24,21,19,19,19,19,19,19,19)
n2 <- c(31,31,31,31,27,27,24,23,23,23,23,18,18,18,17,17)
n3 <- c(19,19,19,19,18,18,17,16,16,16,16,16,16,16,16,16)
n4=c(58,58,58,52,28,22,18,16,15,14,14,14,13,13,13,12)
n=n1+n2+n3+n4
#the number of mice to be observed
pi5c <- ggplot(combined_pain5ns,aes(x=day,y=Pain,fill=type))+
  theme_classic()+
  #facet_grid(.~Status)+
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
           fontface='bold')
pi5c
######################################################

pi5d <- ggline(combined_pain5ns,'day','Pain',
               #linetype = 'Analgesic',size = 1.5,
               color = 'as',
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
                #linetype = 'Analgesic',size = 1.5,
                color = 'ss',
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
pi5b_as <- ggarrange(pi5b,pi5d,
                     common.legend = T)
pi5b_as

pi5d_ss <- ggarrange(pi5b2,pi5d2,
                     common.legend = T)
pi5d_ss

tiff(filename = 'pl5for_survival_0830.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl5 <- ggarrange(pi5a_s,pi5b_as,pi5a_ss,pi5d_ss,
                 ncol = 2,nrow = 2,
                 font.label = list(size=20),
                 widths = c(.4,1,.4,1),
                 labels = c('A','B','C','D'))

pl5
dev.off()

#######################################
#0901 pain grouped by sex and analgesic
combined_pain5s <- combined_pain5[combined_pain5$type=="s",]

pi5a_s <- ggline(combined_pain5s,'day','Pain',
                 #linetype = 'Analgesic',
                 size = 1.5,
                 color = 'Sex',
                 palette = c("orange","blue"),
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
        legend.text=element_text(size=20,face = 'bold'))

pi5a_s

pi5b_a <- ggline(combined_pain5s,'day','Pain',
                 #linetype = 'Analgesic',
                 size = 1.5,
                 color = 'Sex',
                 palette = c("orange","blue"), #maximum_red, kelly_green,rich_blue
                 #palette = c("#DE1A1A","#29BF12","#00A5CF"),
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
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=20,face = 'bold'),
        strip.text.x = element_text(size = 18,face = 'bold'))

pi5b_a

pi5a <- ggarrange(pi5a_s,NULL,pi5b_a,nrow = 1,
                  widths = c(1,0.05,1),
                  common.legend = T,
                  labels = c("A","B"))
pi5a
######################################################

pi5a_a <- ggline(combined_pain5s,'day','Pain',
                 linetype = 'Analgesic',
                 size = 1.5,
                 color = 'Analgesic',
                 palette = c("gray28","cyan","magenta"),
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
        legend.text=element_text(size=20,face = 'bold'))

pi5a_a

##220921 change the plot for government
tiff(filename = 'pl45gov_0921.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=26, height=12, 
     res = 600, compression= "lzw")
pi45 <- ggarrange(pi4a_a,pi5a_a,
          font.label = list(size=20),
          labels = c("A","B"))
pi45
dev.off()



pi5b_s <- ggline(combined_pain5s,'day','Pain',
                 linetype = 'Analgesic',
                 size = 1.5,
                 color = 'Analgesic',
                 palette = c("gray28","cyan","magenta"), #maximum_red, kelly_green,rich_blue
                 #palette = c("#DE1A1A","#29BF12","#00A5CF"),
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
        legend.text=element_text(size=20,face = 'bold'),
        strip.text.x = element_text(size = 18,face = 'bold'))

pi5b_s
pi5b <- ggarrange(pi5a_a,NULL,pi5b_s,nrow = 1,
                  widths = c(1,0.05,1),
                  common.legend = T,
                  labels = c("C","D"))
pi5b

tiff(filename = 'pl5for5_0902.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")
pl5 <- ggarrange(pi5a,NULL,pi5b,ncol = 1,
                 heights = c(1,0.05,1))
pl5
dev.off()
######################################################


tiff(filename = 'pl5for5_0901.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl5 <- ggarrange(pi5a_s,pi5b_s,pi5a_a,pi5b_a,
                 ncol = 2,nrow = 2,
                 font.label = list(size=20),
                 widths = c(.6,1,.6,1),
                 labels = c('A','B','C','D'))

pl5
dev.off()

#############################################
#220914 add significance table
load("/Users/nanaliu/Desktop/220830combination_4runs/combined_pain5survival.rda")
combined_pain5s <- combined_pain5[combined_pain5$type=="s",]
#############################################

res5s <- compare_means(Pain~day,
                       group.by = "Sex",
                       data = combined_pain5s)
write_xlsx(res5s,"220914res5s.xlsx")

res5st <- compare_means(Pain~Sex,
                        group.by = c("day","Analgesic"),
                        data = combined_pain5s)
write_xlsx(res5st,"220914res5st.xlsx")


res5a <- compare_means(Pain~day,
                       group.by = "Analgesic",
                       data = combined_pain5s)
write_xlsx(res5a,"220914res5a.xlsx")

res5as <- compare_means(Pain~day,
                        group.by = c("Analgesic","Sex"),
                        data = combined_pain5s) 
write_xlsx(res5as,"220914res5as.xlsx")

res5at <- compare_means(Pain~Analgesic,
                        group.by = c("day","Sex"),
                        data = combined_pain5s)
write_xlsx(res5at,"220914res5at.xlsx")

res5att <- compare_means(Pain~Analgesic,
                         group.by = c("day","Sex","type"),
                         data = combined_pain5ns)
write_xlsx(res5att,"220914res5att.xlsx")
