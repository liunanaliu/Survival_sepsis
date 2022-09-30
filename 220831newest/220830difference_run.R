#compare the difference between run 1-3 and run 4
load("/Users/nanaliu/Desktop/combined_sur5.rda")
setwd("~/Desktop")
combined_sur5$run <- ifelse(combined_sur5$round==4,"Severe","Mild")
summary(combined_sur5)

library(survminer)
surv_a <-  survival::Surv(combined_sur5$Time,combined_sur5$Status)
surv_a
#grouped by gender
kmfit1 <- survival::survfit(surv_a~combined_sur5$Sex+run,
                            data=combined_sur5)
pi1e <- ggsurvplot(kmfit1,
                   data = combined_sur5,
                   #fun = 'pct',
                   #legend='bottom',
                   size=1.8,
                   font.legend = c(16, "bold"),
                   font.x=c(18,'bold'),
                   font.y=c(18,'bold'),
                   font.caption=c(14,'bold'),
                   font.tickslab=c(16,'bold'),#for x,and y axis
                   legend.title='',
                   legend.labs=c('Female, Mild','Female, Severe',
                                 'Male Mild','Male Severe'),
                   break.time.by=1,
                   xlim=c(0,7),
                   #conf.int = T,
                   pval = T,pval.method = T,
                   pval.size=5,
                   tables.height = .4,
                   #risk.table = T,ncensor.plot=F,
                   #risk.table.pos='in', https://github.com/kassambara/survminer/issues/245
                   risk.table = 'abs_pct',
                   risk.table.title='Number at risk by analgesic : n (%)',
                   surv.scale='percent',
                   #ggtheme = theme_bw(),
                   #linetype = 'strata',
                   #title='Survival plot by analgesic',
                   xlab='Time (d)')
pi1e

kmfit1_2 <- survival::survfit(surv_a~combined_sur5$Analgesic+run,
                            data=combined_sur5)
kmfit1_2
pi1e2 <- ggsurvplot(kmfit1_2,
                   data = combined_sur5,
                   #fun = 'pct',
                   #legend='bottom',
                   size=1.8,
                   font.legend = c(16, "bold"),
                   font.x=c(18,'bold'),
                   font.y=c(18,'bold'),
                   font.caption=c(14,'bold'),
                   font.tickslab=c(16,'bold'),#for x,and y axis
                   legend.title='',
                   legend.labs=c('Melosus, Mild','Melosus, Severe',
                                 'Metapyrin, Mild','Metapyrin, Severe',
                                 'None, Mild','None, Severe'),
                   break.time.by=1,
                   xlim=c(0,7),
                   #conf.int = T,
                   pval = T,pval.method = T,
                   pval.size=5,
                   tables.height = .4,
                   #risk.table = T,ncensor.plot=F,
                   #risk.table.pos='in', https://github.com/kassambara/survminer/issues/245
                   risk.table = 'abs_pct',
                   risk.table.title='Number at risk by analgesic : n (%)',
                   surv.scale='percent',
                   #ggtheme = theme_bw(),
                   #linetype = 'strata',
                   #title='Survival plot by analgesic',
                   xlab='Time (d)')
pi1e2


#################################################
library(forestmodel)
library(survival)
set.seed(1234)
pi1f <- forest_model(coxph(surv_a~Sex+Analgesic+run,combined_sur5))
pi1f

#################################################
library(survival)
kmfit2_2 <- survfit(surv_a ~ Sex+Analgesic+run,combined_sur5)
kmfit2_2
pi1s2 <- ggsurvplot(kmfit2_2,
                   palette = c("#DE1A1A","red","#29BF12","green","#00A5CF", "blue",
                              "#DE1A1A","red","#29BF12","green","#00A5CF", "blue"),
                   legend.labs=c('Female,Melosus,Mild','Female,Melosus,Severe',
                                 'Female,Metapyrin,Mild','Female,Metapyrin,Severe',
                                 'Female,None,Mild','Female,None,Severe',
                                 'Male,Melosus,Mild','Male,Melosus,Severe',
                                 'Male,Metapyrin,Mild','Male,Metapyrin,Severe',
                                 'Male,None,Mild','Male,None,Severe'),
                   size=1.8,xlim=c(0,7),
                   font.legend = c(10, "bold"),
                   font.x=c(18,'bold'),
                   font.y=c(18,'bold'),
                   font.caption=c(14,'bold'),
                   font.tickslab=c(16,'bold'),
                   legend.title='',
                   #legend="bottom",
                   pval = T,pval.method = T,
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
pi1s2
#pi1s$plot+facet_wrap(~strata)
pi1s1_2 <- pi1s2$plot+facet_wrap(~Sex)+
  theme(strip.text.x = element_text(size = 12,face = 'bold'))
pi1s1_2
library(ggsci)
pi1s_a2 <- ggsurvplot(kmfit2_2,risk.table = T,
                     xlim=c(0,7),xlab="Time (d)",
                     #palette = "jco",
                     #palette =c("#DE1A1A","#29BF12","#00A5CF", #maximum_red, kelly_green,rich_blue
                      #          "red","green","blue"),
                     #risk.table.col="strata",table.col="strata"
                     risk.table.y.text=F,
                     tables.theme = theme_cleantable(),
                     risk.table.y.text.color=T,
                     tables.y.axis="")
pi1s_a2

pi1sT <- pi1s_a2$table+facet_grid(.~Sex,scales = 'free')+
  theme(legend.position = 'none')
pi1sT
pi1d <- ggarrange(pi1s1_2,pi1sT,ncol = 1,
                  heights = c(.6,.4))
pi1d

#################################################
#################################################
pi1a1 <- ggarrange(pi1e$plot,pi1e$table,
                   ncol = 1,
                   heights = c(.6,.4))
pi1a1
pi1b1 <- ggarrange(pi1e2$plot,pi1e2$table,
                   ncol = 1,
                   heights = c(.6,.4))
pi1b1
tiff(filename = 'pl1for_run_0830.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl1 <- ggarrange(pi1a1,pi1b1,pi1f,pi1d,
                 labels = c("A","B","C","D"),
                 font.label = list(size=20))
pl1
dev.off()


