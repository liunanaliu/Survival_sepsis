#combine 4 runs regardless of severity
load("/Users/nanaliu/Desktop/surv4.rda")
load("/Users/nanaliu/Downloads/combined_surv.rda")

(colnames(surv4)==colnames(combined_sur))
surv4$round <-  4
summary(surv4)
summary(combined_sur)
combined_sur$Analgesic <- factor(combined_sur$Analgesic,
                                 levels = c('None','Metapyrin','Melosus'))
combined_sur$Sex <- factor(combined_sur$Sex,
                           levels = c("Female","Male"))
combined_sur$DOB <- surv4$DOB <- NULL
combined_sur5 <- rbind(combined_sur,surv4)

names(combined_sur5)[c(6,7)]<- c('Time','Status')
save(combined_sur5,file = "combined_sur5.rda")

load("/Users/nanaliu/Desktop/220830combination_4runs/combined_sur5.rda")
surv_a <-  survival::Surv(combined_sur5$Time,combined_sur5$Status)
surv_a
kmfit1 <- survival::survfit(surv_a~1,
                            data=combined_sur5)
kmfit1

library(survminer)
#grouped by gender
kmfit2 <- survival::survfit(surv_a~combined_sur5$Sex,
                            data=combined_sur5)
kmfit2

tiff(filename = 'pi1a_0829.tiff', #pi1a, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")
#https://stackoverflow.com/questions/59041065/how-do-i-change-the-font-for-all-elements-in-ggsurvplot
pi1a <- ggsurvplot(kmfit2,
                     data = combined_sur5,
                     #fun = 'pct',
                     #legend='bottom',
                     size=1.8,
                     palette = c("orange","blue"),
                     font.legend = c(20, "bold"),
                     font.x=c(18,'bold'),
                     font.y=c(18,'bold'),
                     font.caption=c(14,'bold'),
                     font.tickslab=c(16,'bold'),#for x,and y axis
                     legend.title='',
                     legend.labs=c('Female','Male'),
                     break.time.by=1,
                     xlim=c(0,7),
                     #conf.int = T,
                     pval = T,pval.method = T,
                     pval.size=5,
                     tables.height = .4,
                     #risk.table = T,ncensor.plot=F,
                     #risk.table.pos='in', https://github.com/kassambara/survminer/issues/245
                     risk.table = 'abs_pct',
                     risk.table.title='Number at risk by Sex : n (%)',
                     surv.scale='percent',
                     #ggtheme = theme_bw(),
                     #linetype = 'strata',
                     #title='Survival plot by analgesic',
                     xlab='Time (d)')
pi1a
dev.off()

#########################################
#grouped by analgesia
kmfit3 <-  survival::survfit(surv_a~combined_sur5$Analgesic)
kmfit3

pi1b <- ggsurvplot(kmfit3,
                    data = combined_sur5,
                    #fun = 'pct',
                    #legend='bottom',
                    xlim=c(0,7),
                    palette = c("magenta","cyan","gray28"),
                    size=1.8,
                    font.legend = c(20, "bold"),
                    font.x=c(18,'bold'),
                    font.y=c(18,'bold'),
                    font.caption=c(14,'bold'),
                    font.tickslab=c(16,'bold'),#for x,and y axis
                    legend.title='',
                    legend.labs=c('Melosus','Metapyrin','None'),
                    #legend="bottom",
                    break.time.by=1,
                    #conf.int = T,
                    pval = T,pval.method = T,
                    pval.size=5,
                    #risk.table = T,ncensor.plot=F,
                    #risk.table.pos='in', https://github.com/kassambara/survminer/issues/245
                    risk.table = 'abs_pct',
                    risk.table.title='Number at risk by Analgesic : n (%)',
                    surv.scale='percent',
                    #ggtheme = theme_bw(),
                    #linetype = 'strata',
                    #title='Survival plot by analgesic',
                    xlab='Time (d)')
pi1b


#################################################
library(forestmodel)
set.seed(1234)
pi1c <- forest_model(coxph(surv_a~Sex+Analgesic,combined_sur5))
pi1c

#################################################
library(survival)
kmfit2_1 <- survfit(surv_a ~ Sex+Analgesic,combined_sur5)
kmfit2_1
pi1s <- ggsurvplot(kmfit2_1,
                   palette = c("gray28","cyan","magenta",
                               "gray28","cyan","magenta"),
                   #legend.labs=c('Female, None',
                   #              'Female, Metapyrin',
                    #             'Female, Melosus',
                     #            'Male, None',
                      #           'Male, Metapyrin',
                       #          'Male, Melosus'),
                   size=1.8,xlim=c(0,7),
                   font.legend = c(10, "bold"),
                   font.x=c(18,'bold'),
                   font.y=c(18,'bold'),
                   font.caption=c(14,'bold'),
                   font.tickslab=c(16,'bold'),
                   legend.title='',
                   #legend="none",
                   pval = F,pval.method = T,
                   pval.size=3,
                   #risk.table = T,
                   ncensor.plot=F,
                   #risk.table = 'abs_pct',
                   #risk.table.title='Number at risk by gender: n',
                   surv.scale='percent',
                   #ggtheme = theme_bw(),
                   #linetype = 'strata',
                   # title='Survival plot by gender',
                   xlab='Time (d)')+
  guides(colour=guide_legend(nrow=2,byrow = T))#byrow = T))
pi1s
#pi1s$plot+facet_wrap(~strata)
pi1s1_s <- pi1s$plot+facet_wrap(~Sex)+
  theme(strip.text.x = element_text(size = 18,face = 'bold'))
pi1s1_s

table(combined_sur5$Analgesic)
table(combined_sur5$Analgesic,combined_sur5$Sex)
pi1s_a <- ggsurvplot(kmfit2_1,risk.table = T,
                     xlim=c(0,7),xlab="Time (d)",
                     palette =c(c("gray28","cyan","magenta",
                                  "gray56","cyan3","magenta3")),
                     #risk.table.col="strata",table.col="strata"
                     risk.table.y.text=F,
                     tables.theme = theme_cleantable(),
                     risk.table.y.text.color=T,
                     tables.y.axis="")
pi1s_a
pi1sT_s <- pi1s_a$table+facet_grid(.~Sex,scales = 'free')+
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 18,face = 'bold'))
pi1sT_s

pi1d_s <- ggarrange(pi1s1_s,pi1sT_s,ncol = 1,
                    heights = c(.6,.4))
pi1d_s

#http://sape.inf.usi.ch/quick-reference/ggplot2/colour
pi1s <- ggsurvplot(kmfit2_1,
                   palette = c("orange","orange","orange",
                               "blue","blue","blue"),
                   #legend.labs=c('Female, None',
                   #              'Female, Metapyrin',
                    #             'Female, Melosus',
                     #            'Male, None',
                      #           'Male, Metapyrin',
                       #          'Male, Melosus'),
                   size=1.8,xlim=c(0,7),
                   font.legend = c(10, "bold"),
                   font.x=c(18,'bold'),
                   font.y=c(18,'bold'),
                   font.caption=c(14,'bold'),
                   font.tickslab=c(16,'bold'),
                   legend.title='',
                  legend="none",
                   pval = F,pval.method = T,
                   pval.size=3,
                   #risk.table = T,
                   ncensor.plot=F,
                   #risk.table = 'abs_pct',
                   #risk.table.title='Number at risk by gender: n',
                   surv.scale='percent',
                   #ggtheme = theme_bw(),
                   #linetype = 'strata',
                   # title='Survival plot by gender',
                   xlab='Time (d)')+
  guides(colour=guide_legend(nrow=3,
                             byrow = T))#byrow = T))
pi1s
pi1s1_a <- pi1s$plot+facet_wrap(~Analgesic)+
  theme(strip.text.x = element_text(size = 18,face = 'bold'))
pi1s1_a

pi1s_a <- ggsurvplot(kmfit2_1,risk.table = T,
                     xlim=c(0,7),xlab="Time (d)",
                     palette =c(c("orange","blue","orange2",
                                  "blue2","orange3","blue3")),
                     #risk.table.col="strata",table.col="strata"
                     risk.table.y.text=F,
                     tables.theme = theme_cleantable(),
                     risk.table.y.text.color=T,
                     tables.y.axis="")
pi1s_a

pi1sT_a <- pi1s_a$table+facet_grid(.~Analgesic,scales = 'free')+
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 18,face = 'bold'))
pi1sT_a

pi1d_a <- ggarrange(pi1s1_a,pi1sT_a,ncol = 1,
                    heights = c(.6,.4))
pi1d_a
#################################################
pi1a1 <- ggarrange(pi1a$plot,pi1a$table,
                  ncol = 1,
                  heights = c(.6,.4))
pi1a1
pi1b1 <- ggarrange(pi1b$plot,pi1b$table,
                  ncol = 1,
                  heights = c(.6,.4))
pi1b1

pi1c <- ggarrange(pi1a1,NULL,pi1d_a,nrow = 1,
                  widths = c(1,0.05,1),
                  common.legend = T,
                  labels = c("A","B"))
pi1c

pi1d <- ggarrange(pi1b1,NULL,pi1d_s,nrow = 1,
                  widths = c(1,0.05,1),
                  common.legend = T,
                  labels = c("C","D"))
pi1d

tiff(filename = 'pl1for5_0902.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")
pl1 <- ggarrange(pi1c,NULL,pi1d,ncol = 1,
                 heights = c(1,0.05,1))
pl1
dev.off()

tiff(filename = 'pl1for5_0829.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl1 <- ggarrange(pi1a1,pi1b1,pi1c,pi1d,
                 labels = c("A","B","C","D"),
                 font.label = list(size=20))
pl1
dev.off()

tiff(filename = 'pl1for5_0901.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl1 <- ggarrange(pi1a1,pi1d_s,pi1b1,pi1d_a,
                 labels = c("A","B","C","D"),
                 font.label = list(size=20))
pl1
dev.off()

##############################
#black "#000000"
```
The first two digits are the level of red, the next two green, 
and the last two blue. The value for each ranges from 00 to FF in 
hexadecimal (base-16) notation, which is equivalent to 0 and 255 
in base-10. For example, in the table below, 
“#FFFFFF” is white and “#990000” is a deep red.
```
#"#DE1A1A","#29BF12","#00A5CF", #maximum_red, kelly_green,rich_blue
#"red","green","blue"
#"#CC0000", ,"#000099"

#######################################################
#220914 add significance table
library(survival)
library(survminer)
pairwise_survdiff(Surv(Time, Status)~Analgesic,
                  data = combined_sur5)
pairwise_survdiff(Surv(Time, Status)~Sex,
                  data = combined_sur5)
res1 <- pairwise_survdiff(Surv(Time, Status)~Sex+Analgesic,
                  data = combined_sur5)
res1p <- data.frame(res1$p.value)
library(writexl)
write_xlsx(res1p,"220914res1.xlsx")
