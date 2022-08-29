#20220530 analysis for second run 
setwd("B:/ORG/Forschung/AN3/Liu_Na_LN/20220419_cxcr2_sepsis/20220520second_run")
library(readxl)
X220525survival_run2 <- read_excel("220525survival_run2.xlsx", 
                                     sheet = "survival", col_types = c("text", 
                                      "text", "text", "text", "date", "text", 
                                      "numeric", "text"))
View(X220525survival_run2)

#step1 for the survival analysis
second_sur <- X220525survival_run2
library(survival)
table(second_sur$`Status             (0=live, 1 = dead)`)

surv <- Surv(second_sur$`Duration (days)`,second_sur$`Status             (0=live, 1 = dead)`==1)
kmfit1 <- survfit(surv~1)

library(survminer)
survplot <- ggsurvplot(kmfit1,data = second_sur,
                       title='survival plot of PCI')
survplot$plot+
  geom_hline(yintercept=1,size=1)

kmfit2 <- survfit(surv~second_sur$Sex)
ggsurvplot(kmfit2,data = second_sur,pval = T,
           risk.table = T,ncensor.plot=F,
           title='survival plot by gender')

kmfit3 <- survfit(surv~second_sur$Analgesic)
ggsurvplot(kmfit3,data = second_sur,pval = T,
           risk.table = T,ncensor.plot=F,
           title='survival plot by analgesic')

attach(second_sur)
my.surv <- Surv(`Duration (days)`,`Status             (0=live, 1 = dead)`==1)
m <- coxph(my.surv~Sex+Analgesic,data = second_sur)

fit <- survfit(m,data=second_sur)
fit$call$formula <- m$call$formula
ggsurvplot(fit,palette = '#2E9FDF',ggtheme = theme_minimal())
detach(dat)

#step2 is for bw analysis
X220525survival_run2 <- read_excel("220525survival_run2.xlsx", 
                                 sheet = "bw", col_types = c("numeric", 
                                 "numeric", "text", "text", "date", 
                                 "text", "numeric", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
View(X220525survival_run2)

second_bw <- X220525survival_run2

library(tidyverse)
library(ggpubr)
library(rstatix)

names(second_bw)[2] <- 'SEK00'
second_bw_wide <- second_bw %>% gather(key = 'time',
                                     value = 'weight',
                                     "t0","t0,75", "t1,75",   
                                     "t2,75","t3,75","t4,75",
                                     "t5,75","t6,75", "t7")  %>%
  convert_as_factor(No,SEK00,time)
head(second_bw_wide)

sum(is.na(second_bw))
#67
sum(is.na(second_bw_wide))
#67

library(ggplot2)
ggplot(second_bw_wide,
       aes(x=time,y=weight,color=Analgesic,group=Analgesic))+
  geom_line(size=1)
#not ideal
#Removed 3 row(s) containing missing values (geom_path).

lp <- ggline(second_bw_wide,'time','weight',
             linetype = 'Analgesic',
             color = 'Analgesic',add = c('mean_se','jitter'),
             facet.by = 'Sex')
lp
#1: Removed 67 rows containing non-finite values (stat_summary)
#2: Removed 67 rows containing missing values (geom_point). 

ggline(second_bw_wide,'time','weight',
       linetype = 'Analgesic',
       color = 'Analgesic',add = c('mean_se','jitter'))+
  facet_grid(rows = vars(Sex))
#not display the real data

ggline(second_bw_wide,'time','weight',
       linetype = 'Analgesic',
       color = 'Analgesic',add = 'mean_se')+
  facet_grid(Sex~.)
#also not for the real data displayed

stat.test <- second_bw_wide %>%
  group_by(time) %>%
  t_test(weight ~ Analgesic) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test

stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "time")

ggline(second_bw_wide,'time','weight',
       linetype = 'Analgesic',
       color = 'Analgesic',add = 'mean_se')
lp <- ggline(second_bw_wide,'time','weight',
             linetype = 'Analgesic',
             color = 'Analgesic',add = 'mean_se',
             facet.by = 'Sex')
lp
lp + stat_pvalue_manual(
  stat.test,  label = "{p.adj}{p.adj.signif}", 
  tip.length = 0, linetype  = "blank"
)
#Error in `check_aesthetics()`:
#! Aesthetics must be either length 1 or the same as the data (54): group, step.increase, bracket.nudge.y and bracket.shorten
#Run `rlang::last_error()` to see where the error occurred.
