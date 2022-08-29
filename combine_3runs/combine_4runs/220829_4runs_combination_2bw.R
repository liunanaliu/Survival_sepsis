#combine 4 runs regardless of severity
#for bw
load("/Users/nanaliu/Desktop/bw_4th.rda")
load("/Users/nanaliu/Downloads/combined_bw_wide2.rda")
summary(combined_bw_wide2)
combined_bw_wide2$Sex <- ifelse(combined_bw_wide2$Sex=="f","Female","Male")
combined_bw_wide2$Sex <- factor(combined_bw_wide2$Sex,
                                levels = c("Female","Male"))
combined_bw_wide2$Analgesic <- factor(combined_bw_wide2$Analgesic,
                                      levels = c('None','Metapyrin','Melosus'))

load("/Users/nanaliu/Desktop/bw_4th_re.rda")
names(bw_4th_re)
bw_4th_re2 <- bw_4th_re[,-c(1,3,5,7,9:17)]
names(combined_bw_wide2)[c(1,5,6)] <- c("SEK00-","Time","Weight")
bw_4th_re3 <- gather(bw_4th_re2,key = "Time",
                     value = "Weight",
                     "t0l","t0,75l","t1,75l",
                     "t2,75l","t3,75l","t4,75l",
                     "t5,75l","t6,75l","t7l")
bw_4th_re3$Time <- stringi::stri_sub(bw_4th_re3$Time,2,
                                     stringr::str_length(bw_4th_re3$Time)-1)

sum(combined_bw_wide2$`SEK00-`==bw_4th_re3$`SEK00-`)
is.numeric(bw_4th_re3$`SEK00-`)
combined_bw_wide2$`SEK00-` <- as.numeric(as.character(combined_bw_wide2$`SEK00-`))
summary(combined_bw_wide2)
summary(bw_4th_re3)
summary(combined_bw5)
combined_bw5 <- rbind(combined_bw_wide2,bw_4th_re3)
save(combined_bw5,file = "combined_bw5.rda")

#############################################
pi2a <- ggline(combined_bw5,"Time","Weight",linetype = "Analgesic",
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

######################################################
pi2b <- ggline(combined_bw5,"Time","Weight",linetype = "Analgesic",
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

######################################################
tiff(filename = 'pl2for5_0829.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl2 <- ggarrange(pi2a,pi2b,ncol = 1,
                 common.legend = T,
                 font.label = list(size=20),
                 labels = c("A","B"))
pl2
dev.off()




