###compare the difference between run 1-3 and run 4
#css for non-survival and survival
load("/Users/nanaliu/Desktop/combined_css5.rda")

combined_css5$run <- "Mild"
combined_css5$run[(6401:11040)] <- "Severity"
table(combined_css5$run)
#80*80 80*58
combined_css5$run <- factor(combined_css5$run,
                            levels = c("Mild","Severity"))
combined_css5$sr <- paste(combined_css5$Sex,combined_css5$run,sep = ",")
combined_css5$ar <- paste(combined_css5$Analgesic,combined_css5$run,
                          sep = ",")
combined_css5$Status <- ifelse(combined_css5$`Status             (0=live, 1 = dead)`==0,
                               "Survivor","Non-survivor")
table(combined_css5$Status)
combined_css5$rs <- paste(combined_css5$run,combined_css5$Status,
                          sep = ",")
combined_css5$as <- paste(combined_css5$Analgesic,combined_css5$Status,
                          sep = ",")
combined_css5$ss <- paste(combined_css5$Sex,combined_css5$Status,
                          sep = ",")
save(combined_css5,file = "combined_css5survival.rda")
load("/Users/nanaliu/Desktop/220830combination_4runs/combined_css5survival.rda")
#############################################
library(ggplot2)
library(ggplotify)
library(ggpubr)
load("/Users/nanaliu/Desktop/combined_css5survival.rda")
table(combined_css5$type)
combined_css5o <- combined_css5[combined_css5$type=="o",]

#####################
#add css levels
#change the font of axis
#220921
combined_css5o$css_level <- ifelse(combined_css5o$CSS<5, 1,
                                   ifelse(combined_css5o$CSS<9,2,
                                          ifelse(combined_css5o$CSS<13,3,4)))
table(combined_css5o$css_level)

################
pi4a_s <- ggline(combined_css5o,'day','CSS',
               #linetype = 'Analgesic',
               size = 1.5,
               color = 'Status',
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

pi4a_s

pi4a_ss <- ggline(combined_css5o,'day','CSS',
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
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))+
  guides(colour=guide_legend(nrow=2,byrow = T))

pi4a_ss
######################################################

pi4b <- ggline(combined_css5o,'day','CSS',
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
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi4b2

######################################################
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
  #facet_grid(.~Status)+
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
               color = 'as',
               palette = c("#DE1A1A","red",
                           "#29BF12","green","#00A5CF","blue"),
               add ='mean',#error.plot = 'upper_errorbar',
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
                color = 'ss',
                palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                            "red","blue"),
                add ='mean',#error.plot = 'upper_errorbar',
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
pi4b_as <- ggarrange(pi4b,pi4d,
                     common.legend = T)
pi4b_as

pi4d_ss <- ggarrange(pi4b2,pi4d2,
                     common.legend = T)
pi4d_ss

tiff(filename = 'pl4for_survival_0830.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl4 <- ggarrange(pi4a_s,pi4b_as,pi4a_ss,pi4d_ss,
                 ncol = 2,nrow = 2,
                 font.label = list(size=20),
                 widths = c(.4,1,.4,1),
                 labels = c('A','B','C','D'))

pl4
dev.off()


#########################################
#0901 group by sex and analgesic
pi4a_s <- ggline(combined_css5o,'day','CSS',
                 #linetype = 'Analgesic',
                 size = 1.5,
                 color = 'Sex',
                 palette = c("orange","blue"),
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
        legend.text=element_text(size=20,face = 'bold'))

pi4a_s

pi4b_a <- ggline(combined_css5o,'day','CSS',
                 #linetype = 'Analgesic',
                 size = 1.5,
                 color = 'Sex',
                 palette = c("orange","blue"),#maximum_red, kelly_green,rich_blue
                 #palette = c("#DE1A1A","#29BF12","#00A5CF"),
                 add ='mean',#error.plot = 'upper_errorbar',
                 facet.by = "Analgesic")+
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
        legend.text=element_text(size=20,face = 'bold'),
        strip.text.x = element_text(size = 18,face = 'bold'))

pi4b_a

pi4a <- ggarrange(pi4a_s,NULL,pi4b_a,nrow = 1,
                  widths = c(1,0.05,1),
                  common.legend = T,
                  labels = c("A","B"))
pi4a


pi4a_a <- ggline(combined_css5o,'day','css_level',
                 linetype = 'Analgesic',
                 size = 1.5,
                 color = 'Analgesic',
                 palette = c("gray28","cyan","magenta"),
                 add ='mean_se',error.plot = 'upper_errorbar')+
  scale_x_discrete(labels = c('0','','','','1.75',
                              '','','','3.75',
                              '','','','5.75',
                              '','','7'))+
  #labs(x='Time (d)',y='Cumulative CSS')+
  labs(x='Time (d)',y='Clinical Severity Score')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=20,face = 'bold'))

pi4a_a



pi4b_s <- ggline(combined_css5o,'day','CSS',
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
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=20,face = 'bold'),
        strip.text.x = element_text(size = 18,face = 'bold'))

pi4b_s

pi4b <- ggarrange(pi4a_a,NULL,pi4b_s,nrow = 1,
                  widths = c(1,0.05,1),
                  common.legend = T,
                  labels = c("C","D"))
pi4b

tiff(filename = 'pl4for5_0902.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")
pl4 <- ggarrange(pi4a,NULL,pi4b,ncol = 1,
                 heights = c(1,0.05,1))
pl4
dev.off()

tiff(filename = 'pl4for5_0901.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl4 <- ggarrange(pi4a_s,pi4b_s,pi4a_a,pi4b_a,
                 ncol = 2,nrow = 2,
                 font.label = list(size=20),
                 widths = c(.6,1,.6,1),
                 labels = c('A','B','C','D'))

pl4
dev.off()

#############################################
#220914 add significance table
load("/Users/nanaliu/Desktop/220830combination_4runs/combined_css5.rda")

combined_css5o <- combined_css5[combined_css5$type=="o",]
res4s <- compare_means(CSS~day,
                       group.by = "Sex",
                       data = combined_css5o)
write_xlsx(res4s,"220914res4s.xlsx")

res4st <- compare_means(CSS~Sex,
                        group.by = c("day","Analgesic"),
                        data = combined_css5o)
write_xlsx(res4st,"220914res4st.xlsx")


res4a <- compare_means(CSS~day,
                       group.by = "Analgesic",
                       data = combined_css5o)
write_xlsx(res4a,"220914res4a.xlsx")

res4as <- compare_means(CSS~day,
                        group.by = c("Analgesic","Sex"),
                        data = combined_css5o) 
write_xlsx(res4as,"220914res4as.xlsx")

res4at <- compare_means(CSS~Analgesic,
                        group.by = c("day","Sex"),
                        data = combined_css5o)
write_xlsx(res4at,"220914res4at.xlsx")

res4att <- compare_means(CSS~Analgesic,
              group.by = c("day","Sex","type"),
              data = combined_css5no)
write_xlsx(res4att,"220914res4att.xlsx")
