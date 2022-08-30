###compare the difference between run 1-3 and run 4
#bw for non-survival and survival
load("/Users/nanaliu/Desktop/combined_bw5.rda")
table(combined_bw5$Time)
#combined_bw5$run <- seq("Mild",9*80,"Severity",9*58)
combined_bw5$run <- "Mild"
combined_bw5$run[(721:1242)] <- "Severity"
table(combined_bw5$run)
combined_bw5$run <- factor(combined_bw5$run,
                           levels = c("Mild","Severity"))
combined_bw5$sr <- paste(combined_bw5$Sex,combined_bw5$run,sep = ",")
table(combined_bw5$sr)

combined_bw5$ar <- paste(combined_bw5$Analgesic,combined_bw5$run,sep = ",")
table(combined_bw5$ar)

combined_bw5$Status <- ifelse(combined_bw5$`Status             (0=live, 1 = dead)`==0,
                              "Survivor","Non-survivor")
table(combined_bw5$Status)
table(combined_bw5$`Status             (0=live, 1 = dead)`)

combined_bw5$rs <- paste(combined_bw5$run,combined_bw5$Status,
                         sep = ",")
table(combined_bw5$rs)
save(combined_bw5,file = "combined_bw5survival.rda")

#############################################
library(ggplot2)
library(ggplotify)
library(ggpubr)

pi2a_s <- ggline(combined_bw5,"Time","Weight",#linetype = "Analgesic",
                  color = "Status", size = 1.5,
                  palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                              "red","blue"),
                  add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0','','1.75',
                              '','3.75',
                              '','5.75',
                              '','7'))+
  labs(y='Relative body weight (g)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi2a_s


pi2a_rs <- ggline(combined_bw5,"Time","Weight",#linetype = "Analgesic",
               color = "rs", size = 1.5,
               palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                           "red","blue"),
               add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0','','1.75',
                              '','3.75',
                              '','5.75',
                              '','7'))+
  labs(y='Relative body weight (g)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi2a_rs

pi2a_sr <- ggline(combined_bw5,"Time","Weight",#linetype = "Analgesic",
               color = "sr", size = 1.5,
               palette = c("#DE1A1A","#00A5CF", #maximum_red, kelly_green,rich_blue
                           "red","blue"),
               facet.by = "Status",
               add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0','','1.75',
                              '','3.75',
                              '','5.75',
                              '','7'))+
  labs(y='Relative body weight (g)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi2a_sr

pi2a2_ar <- ggline(combined_bw5,"Time","Weight",#linetype = "Analgesic",
               color = "ar", size = 1.5,
               palette = c("#DE1A1A","#29BF12","#00A5CF", #maximum_red, kelly_green,rich_blue
                           "red","green","blue"),
               facet.by = "Status",
               add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0','','1.75',
                              '','3.75',
                              '','5.75',
                              '','7'))+
  labs(y='Relative body weight (g)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=14,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi2a2_ar
#############################################
tiff(filename = 'pl2for_survival_0830.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl2 <- ggarrange(pi2a_s,pi2a_rs,pi2a_sr,pi2a2_ar,ncol = 1,
                 #widths = c(1,0.5,0.5),
                 # common.legend = T,
                 font.label = list(size=20),
                 labels = c("A","B","C","D"))
pl2
dev.off()
#############################################
#############################################

