##compare the difference between run 1-3 and run 4
#bw
load("/Users/nanaliu/Desktop/combined_bw5.rda")
table(combined_bw5$Time)
#combined_bw5$run <- seq("Mild",9*80,"Severity",9*58)
combined_bw5$run <- "Mild"
combined_bw5$run[(721:1242)] <- "Severity"
table(combined_bw5$run)
combined_bw5$run <- factor(combined_bw5$run,
                           levels = c("Mild","Severity"))

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
ggplot(combined_bw5,aes(Time,Weight))+
  geom_line(color= Analgesic,shape= run)
combined_bw5$sr <- paste(combined_bw5$Sex,combined_bw5$run,sep = ",")
table(combined_bw5$sr)

pi2b <- ggline(combined_bw5,"Time","Weight",#linetype = "Analgesic",
               color ="sr",
               #shape = "run",
               facet.by = "Analgesic",
               size = 1.5,
               palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                           "red","blue"),
               #c("#DE1A1A","red","#00A5CF","blue"),
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
combined_bw5$ar <- paste(combined_bw5$Analgesic,combined_bw5$run,sep = ",")
table(combined_bw5$ar)

pi2b2 <- ggline(combined_bw5,"Time","Weight",#linetype = "Analgesic",
               color ="ar",
               #shape = "run",
               facet.by = "Sex",
               size = 1.5,
               palette = c("#DE1A1A","#29BF12","#00A5CF", #maximum_red, kelly_green,rich_blue
                           "red","green","blue"),
                 #c("#DE1A1A","red","#00A5CF","blue"),
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
pi2b2

######################################################
tiff(filename = 'pl2for_run_0830.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl2 <- ggarrange(pi2a,pi2b2,pi2b,ncol = 1,
                 #widths = c(1,0.5,0.5),
                # common.legend = T,
                 font.label = list(size=20),
                 labels = c("A","B","c"))
pl2
dev.off()



