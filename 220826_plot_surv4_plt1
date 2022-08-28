#220827 survival 4th analysis
setwd("~/Downloads")
X20220819survival_run4 <- read_excel("Downloads/20220819survival_run4.xlsx")

surv4 <- X20220819survival_run4
setwd("B:/ORG/Forschung/AN3/Liu_Na_LN/20220419_cxcr2_sepsis/220819_4th_run")

surv4  <-  surv4[-1,]
#exclude 2 mice without sepsis, at hte location of 32 - 85178 and 48 -85251
surv4 <- surv4[-c(32,48),]
names(surv4)[c(7,8)] <- c('Time','Status')

surv4$Analgesic
surv4$Analgesic <- factor(surv4$Analgesic,
                          levels = c('No','Metapyrin','Melosus'))
surv4$Sex <- factor(surv4$Sex,
                    levels = c("F","M"))
save(surv4,file = "surv4.rda")
surv_a <-  survival::Surv(surv4$Time,surv4$Status)
surv_a
kmfit1 <- survival::survfit(surv_a~1,
                            data=surv4)

install.packages('nloptr')
#lme4 pbkrtest car rstatix ggpubr survminer
remotes::install_github("jyypma/nloptr")
library(survminer)
ggsurvplot(kmfit1, data=surv4)


p_kmfit1 <- ggsurvplot(kmfit1,
                       #data = combined_sur,
                       legend.title='',
                       legend.labs='Sepsis',
                       break.time.by=1,
                       xlim=c(0,7),
                       pval = T,pval.method = T,
                       risk.table = T,ncensor.plot=F,
                       risk.table.title='Number at risk by time',
                       surv.scale='percent',
                       #ggtheme = theme_bw(),
                       #linetype = 'strata',
                       title='4th Survival plot of PCI',
                       xlab='Time(d)')
p_kmfit1
'''
survplot$plot+
geom_hline(yintercept=1,size=1)
'''
###############################################
#grouped by gender
kmfit2 <- survival::survfit(surv_a~surv4$Sex,
                            data=surv4)

p_kmfit2 <-  ggsurvplot(kmfit2,
                        #data = combined_sur,
                        legend.title='Gender',
                        legend.labs=c('Female','Male'),
                        break.time.by=1,
                        xlim=c(0,7),
                        pval = T,pval.method = T,
                        risk.table = T,ncensor.plot=F,
                        risk.table.title='Number at risk by time',
                        surv.scale='percent',
                        #ggtheme = theme_bw(),
                        #linetype = 'strata',
                        title='Survival plot by gender',
                        xlab='Time(d)')

tiff(filename = 'pi1sex1_0827.tiff', #pi1a, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")
#https://stackoverflow.com/questions/59041065/how-do-i-change-the-font-for-all-elements-in-ggsurvplot
pi1sex <- ggsurvplot(kmfit2,
                     data = surv4,
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
pi1sex
dev.off()

#########################################
#grouped by analgesia
kmfit3 <-  survival::survfit(surv_a~surv4$Analgesic)
kmfit3

table(surv4$Analgesic)
#20, 19, 19
survdiff(surv_a~surv4$Analgesic)
pairwise_survdiff(Surv(Time,Status)~Analgesic,
                  data = surv4,
                  p.adjust.method = "BH")
#melosus is better than the rest of none and metapyrin
#add 3 p-values in plot
tiff(filename = 'pi1a.tiff', #pi1a, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")
#https://stackoverflow.com/questions/59041065/how-do-i-change-the-font-for-all-elements-in-ggsurvplot
ggsurvplot(kmfit3,surv4)
#orde rwas wrong, the first was melosus
pi1a2 <- ggsurvplot(kmfit3,
                   data = as_tibble(surv4),
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
pi1a2
dev.off()
#big issue about the color conflicts with plot and table!!

tiff(filename = 'pi1sex3_0828.tiff', #pi1s1, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")
pi1sex3 <- ggsurvplot_facet(kmfit3,surv4,facet.by = 'Sex',
                            color = "Analgesic",
                            palette = c("#DE1A1A","#29BF12","#00A5CF"),
                            size=1.8, 
                            xlim= c(1,7),
                            pval = T,pval.method = T,
                            pval.size=5,
                            risk.table = T,
                            ncensor.plot=F,
                            font.legend = c(16, "bold"),
                            font.x=c(18,"bold"),
                            font.y=c(18,'bold'),
                            font.caption=c(14,'bold'),
                            font.tickslab=c(16,'bold'),
                            legend.title='',
                            axis.ticks=element_text('Female','Male'),
                            #labeller=labeller(Sex=sex),
                            xlab='Time (d)',
                            ylab='Overall survival')+
  theme()
pi1sex3
dev.off()

table <- ggrisktable(kmfit2_1,data = surv4,
                     #color='Sex',
                     risk.table.type = 'percentage',
                     risk.table.title='Number at risk by gender')
table$mapping
#####################################################
table <- ggrisktable(kmfit2,data = surv4,
                     color='Sex',
                     risk.table.type = 'percentage',
                     risk.table.title='Number at risk by gender')
table
library(survival)
kmfit2_1 <- survfit(surv_a ~ Sex+Analgesic,surv4)
kmfit2_1
#pairwise significance for sex and analgeisc group
pairwise_survdiff(Surv(Time,Status)~ Sex+Analgesic,
                  data = surv4,
                  p.adjust.method = "BH")
##############
tiff(filename = 'pi1sT_220828.tiff',
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")
ggsurvplot(kmfit2_1,surv4)
#order was correct, with none as the first
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

pi1s_a$plot + facet_grid(.~Analgesic)
pi1s_a$plot + facet_grid(.~Sex)
pi1s_a$table+facet_grid(.~Analgesic,scales = 'free')+
  theme(legend.position = 'none')
pi1sT <- pi1s_a$table+facet_grid(.~Sex,scales = 'free')+
  theme(legend.position = 'none')
pi1sT
pi1d <- ggarrange(pi1s1,pi1sT,ncol = 1,
          heights = c(.6,.4))
pi1d
dev.off()
###########################################################
#forest plot
library(survminer)
library(survival)
colnames(surv4)
model <- coxph(surv_a ~ Sex+Analgesic),
               data = surv4)
model
m <- summary(model)
rownames(m$coefficients)
names(model$coefficients) <- c("Male","Metapyrin","Melosus")
m$conf.int

ggforest(model,data = surv4,
         main = "Hazard ratio",
         fontsize = 0.8)
#error error error 
package_version("survminer")
head(colon)
colon$rx
colon$sex
library(dplyr)
surv4$Sex <- recode(surv4$Sex,"F"="Female","M"="Male")
surv4$Analgesic <- recode(surv4$Analgesic,"No"="None")
save(surv4,file = "surv4.rda")
library(forestmodel)
tiff(filename = 'pi1b_0828.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=20, height=14, 
     res = 600, compression= "lzw")

set.seed(1234)
forestmodel::forest_model(glm(Status ~ Sex + Analgesic,
                              data = surv4,
                              family = "binomial"))
pi1f <- forest_model(coxph(surv_a~Sex+Analgesic,surv4))
pi1f
#beter!!!
dev.off()

########################################################################
pi1a <- ggarrange(pi1sex$plot,pi1sex$table,
                  ncol = 1,
                  heights = c(.6,.4))
pi1a
pi1b <- ggarrange(pi1a2$plot,pi1a2$table,
                  ncol = 1,
                  heights = c(.6,.4))
pi1b
pi1f
pi1d

tiff(filename = 'pl1_0828.tiff', #pi1b, 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl1 <- ggarrange(pi1a,pi1b,pi1f,pi1d,
          labels = c("A","B","C","D"),
          font.label = list(size=20))
pl1
dev.off()


