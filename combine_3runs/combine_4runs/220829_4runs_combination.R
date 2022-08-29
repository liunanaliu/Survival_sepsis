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

surv_a <-  survival::Surv(combined_sur5$Time,combined_sur5$Status)
surv_a
kmfit1 <- survival::survfit(surv_a~1,
                            data=combined_sur5)


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
                     font.legend = c(16, "bold"),
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
                     risk.table.title='Number at risk by analgesic : n (%)',
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
                    palette = c("#00A5CF","#29BF12","#DE1A1A"),
                    size=1.8,
                    font.legend = c(16, "bold"),
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
                    risk.table.title='Number at risk by analgesic : n (%)',
                    surv.scale='percent',
                    #ggtheme = theme_bw(),
                    #linetype = 'strata',
                    #title='Survival plot by analgesic',
                    xlab='Time (d)')
pi1b


#################################################
set.seed(1234)
pi1c <- forest_model(coxph(surv_a~Sex+Analgesic,combined_sur5))
pi1c

#################################################
library(survival)
kmfit2_1 <- survfit(surv_a ~ Sex+Analgesic,combined_sur5)
kmfit2_1
pi1s <- ggsurvplot(kmfit2_1,
                   palette = c("#DE1A1A","#29BF12","#00A5CF", 
                               "red","green","blue"),
                   legend.labs=c('Female, None',
                                 'Female, Metapyrin',
                                 'Female, Melosus',
                                 'Male, None',
                                 'Male, Metapyrin',
                                 'Male, Melosus'),
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
  guides(colour=guide_legend(nrow=1))#byrow = T))
pi1s
#pi1s$plot+facet_wrap(~strata)
pi1s1 <- pi1s$plot+facet_wrap(~Sex)+
  theme(strip.text.x = element_text(size = 12,face = 'bold'))
pi1s1

pi1s_a <- ggsurvplot(kmfit2_1,risk.table = T,
                     xlim=c(0,7),xlab="Time (d)",
                     palette =c("#DE1A1A","#29BF12","#00A5CF", #maximum_red, kelly_green,rich_blue
                                "red","green","blue"),
                     #risk.table.col="strata",table.col="strata"
                     risk.table.y.text=F,
                     tables.theme = theme_cleantable(),
                     risk.table.y.text.color=T,
                     tables.y.axis="")
pi1s_a

pi1sT <- pi1s_a$table+facet_grid(.~Sex,scales = 'free')+
  theme(legend.position = 'none')
pi1sT
pi1d <- ggarrange(pi1s1,pi1sT,ncol = 1,
                  heights = c(.6,.4))
pi1d

#################################################
pi1a1 <- ggarrange(pi1a$plot,pi1a$table,
                  ncol = 1,
                  heights = c(.6,.4))
pi1a1
pi1b1 <- ggarrange(pi1b$plot,pi1b$table,
                  ncol = 1,
                  heights = c(.6,.4))
pi1b1
tiff(filename = 'pl1for5_0829.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl1 <- ggarrange(pi1a1,pi1b1,pi1c,pi1d,
                 labels = c("A","B","C","D"),
                 font.label = list(size=20))
pl1
dev.off()




