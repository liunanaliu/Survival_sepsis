##combine 4 runs regardless of severity
#pl4 for css
load("/Users/nanaliu/Downloads/combined_css_wide6.rda")
combined_css_wide6$SEK00 <- as.numeric(as.character(combined_css_wide6$SEK00))
summary(combined_css_wide6)

combined_css_wide6$Sex <- ifelse(combined_css_wide6$Sex=="f","Female","Male")
combined_css_wide6$Sex <- factor(combined_css_wide6$Sex,
                                 levels = c("Female","Male"))

combined_css_wide6$Analgesic <- factor(combined_css_wide6$Analgesic,
                                      levels = c('None','Metapyrin','Melosus'))


names(combined_css_wide6)[c(1,5,6,7)] <- c("SEK00-","day","type","CSS")
table(combined_css_wide6$type)

load("/Users/nanaliu/Desktop/css_4th_1_2.rda")
names(css_4th_1_2) 
css_4th_1_2 <- css_4th_1_2[,-c(1,3,5,7,9)]
table(css_4th_1_2$type)
css_4th_1_2 <- css_4th_1_2 %>% select("SEK00-","Sex","Analgesic",
                       "Status             (0=live, 1 = dead)",
                       "day","type","CSS")
combined_css5 <- rbind(combined_css_wide6,css_4th_1_2)
save(combined_css5,file = "combined_css5.rda")

####################################################
table(combined_css5$type)
combined_css5o <- combined_css5[combined_css5$type=="o",]

pi4a <- ggline(combined_css5o,'day','CSS',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar')+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi4a


pi4b <- ggline(combined_css5o,'day','CSS',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "Sex")+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi4b

######################################################
combined_css5no <- combined_css5[combined_css5$type !="o",]
#8832
table(combined_css5no$type)
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
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
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
           size=4,
           fontface='bold')
pi4c

pi4d <- ggline(combined_css5no,'day','CSS',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "type")+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time (d)',y='CSS distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=14,face = 'bold'))

pi4d

############################################
tiff(filename = 'pl4for5_0829.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl4 <- ggarrange(pi4a,pi4b,pi4c,pi4d,#ncol = 2,
                 font.label = list(size=20),
                 #widths = c(.6,1,.6,1),
                 labels = c('A','B','C','D'))

pl4
dev.off()
