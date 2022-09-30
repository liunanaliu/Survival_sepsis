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
load("/Users/nanaliu/Desktop/220830combination_4runs/combined_bw5survival.rda")
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
#0901 grouped by sex and analgesic
pi2a_s <- ggline(combined_bw5,"Time","Weight",#linetype = "Analgesic",
                 color = "Sex", size = 1.5,
                 palette = c("orange","blue"),
                 add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0','','1.75',
                              '','3.75',
                              '','5.75',
                              '','7'))+
  labs(y='Relative body weight (%)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=20,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi2a_s


pi2a_sa <- ggline(combined_bw5,"Time","Weight",#linetype = "Analgesic",
                  color = "Sex", size = 1.5,
                  palette = c("orange","blue"),
                  facet.by = "Analgesic",
                  add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0','','1.75',
                              '','3.75',
                              '','5.75',
                              '','7'))+
  labs(y='Relative body weight (%)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=20,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi2a_sa



pi2a_a <- ggline(combined_bw5,"Time","Weight",linetype = "Analgesic",
                 color = "Analgesic", size = 1.5,
                 palette = c("gray28","cyan","magenta"),
                 add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0','','1.75',
                              '','3.75',
                              '','5.75',
                              '','7'))+
  labs(y='Relative body weight (%)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=20,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi2a_a

pi2a_sr <- ggline(combined_bw5,"Time","Weight",linetype = "Analgesic",
                  color = "Analgesic", size = 1.5,
                  palette = c("gray28","cyan","magenta"),
                  facet.by = "Sex",
                  add = "mean_se",error.plot = "upper_errorbar")+
  scale_x_discrete(labels = c('0','','1.75',
                              '','3.75',
                              '','5.75',
                              '','7'))+
  labs(y='Relative body weight (%)',x='Time (d)')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 16,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.text=element_text(size=20,face = 'bold'),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))
pi2a_sr


pi2a <- ggarrange(pi2a_s,NULL,pi2a_sa,nrow = 1,
                  widths = c(1,0.05,1),
                  common.legend = T,
                  labels = c("A","B"))
pi2a

pi2b <- ggarrange(pi2a_a,NULL,pi2a_sr,nrow = 1,
                  widths = c(1,0.05,1),
                  common.legend = T,
                  labels = c("C","D"))
pi2b

tiff(filename = 'pl2for5_0902.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")
pl2 <- ggarrange(pi2a,NULL,pi2b,ncol = 1,
                 heights = c(1,0.05,1))
pl2
dev.off()

###############
tiff(filename = 'pl2for5_0901.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl2 <- ggarrange(pi2a_s,pi2a_sr,pi2a_a,pi2a_sa,#ncol = 1,
                 widths = c(0.6,1,0.6,1),
                 # common.legend = T,
                 font.label = list(size=20),
                 labels = c("A","B","C","D"))
pl2
dev.off()
#############################################
#220914 add significance table
res2s <- compare_means(Weight~Time,
              group.by = "Sex",
              data = combined_bw5)
write_xlsx(res2s,"220914res2s.xlsx")

res2a <- compare_means(Weight~Time,
                      group.by = "Analgesic",
                      data = combined_bw5)
write_xlsx(res2a,"220914res2a.xlsx")

res2as <- compare_means(Weight~Time,
                        group.by = c("Analgesic","Sex"),
                        data = combined_bw5) 
write_xlsx(res2as,"220914res2as.xlsx")

res2at <- compare_means(Weight~Analgesic,
                       group.by = c("Time","Sex"),
                       data = combined_bw5)
write_xlsx(res2at,"220914res2at.xlsx")

res2st <- compare_means(Weight~Sex,
                        group.by = c("Time","Analgesic"),
                        data = combined_bw5)
write_xlsx(res2st,"220914res2st.xlsx")
