X20220519_survival_run1 <- read_excel("B:/ORG/Forschung/AN3/Liu_Na_LN/
20220419_cxcr2_sepsis/20220519_survival_run1.xlsx")
first_sur <- X20220519_survival_run1[1:30,]
library(survival)
table(first_sur$`Status             (0=live, 1 = dead)`)

surv <- Surv(first_sur$`Duration (days)`,first_sur$`Status             (0=live, 1 = dead)`==1)
kmfit1 <- survfit(surv~1)

library(survminer)
survplot <- ggsurvplot(kmfit1,data = first_sur,
                       title='survival plot of PCI')
survplot$plot+
  geom_hline(yintercept=1,size=1)

kmfit2 <- survfit(surv~first_sur$Sex)
ggsurvplot(kmfit2,data = first_sur,pval = T,
           risk.table = T,ncensor.plot=F,
           title='survival plot by gender')

kmfit3 <- survfit(surv~first_sur$Analgesic)
ggsurvplot(kmfit3,data = first_sur,pval = T,
           risk.table = T,ncensor.plot=F,
           title='survival plot by analgesic')

attach(first_sur)
my.surv <- Surv(`Duration (days)`,`Status             (0=live, 1 = dead)`==1)
m <- coxph(my.surv~Sex+Analgesic,data = first_sur)

fit <- survfit(m,data=first_sur)
fit$call$formula <- m$call$formula
ggsurvplot(fit,palette = '#2E9FDF',ggtheme = theme_minimal())
detach(dat)

