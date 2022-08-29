#20220523 for css and pain relief
#how to extract one cell based on ,
X20220519_survival_run1 <- read_excel("B:/ORG/Forschung/AN3/Liu_Na_LN/
                                      20220419_cxcr2_sepsis/20220519_survival_run1.xlsx", 
                                     sheet = "bw", col_types = c("numeric", 
                                       "numeric", "text", "text", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))
first_bw <- X20220519_survival_run1
table(first_bw$Status)
first_bw <- first_bw[first_bw$Status==0,]
table(first_bw$Analgesic)

#two-way repeated measures ANOVA
#1 betwen-group (treatment), 1 within-group (time)
#https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/ 
library(tidyverse)
library(ggpubr)
library(rstatix)
#rename(first_bw,c('SEK00-'='SEK00'))
# Column `SEK00` doesn't exist.
names(first_bw)[2] <- 'SEK00'
first_bw_wide <- first_bw %>% gather(key = 'time',
                                     value = 'weight',
                                     "t0","t0,75", "t1,75",   
                                     "t2,75","t3,75","t4,75",
                                     "t5,75","t6,75" )  %>%
  convert_as_factor(No,SEK00,time)
head(first_bw_wide)
#https://rkabacoff.github.io/datavis/Time.html
library(ggplot2)
ggplot(first_bw_wide,
       aes(x=time,y=weight,color=Analgesic,group=Analgesic))+
  geom_line(size=1)


lp <- ggline(first_bw_wide,'time','weight',
               linetype = 'Analgesic',
               color = 'Analgesic',add = c('mean_se','jitter'),
             facet.by = 'Sex')
lp


#try to facet in the same column
ggline(first_bw_wide,'time','weight',
       linetype = 'Analgesic',
       color = 'Analgesic',add = c('mean_se','jitter'))+
  facet_grid(rows = vars(Sex))

ggline(first_bw_wide,'time','weight',
       linetype = 'Analgesic',
       color = 'Analgesic',add = 'mean_se')+
  facet_grid(Sex~.)

#the data is not real,why did it happen???
#https://ggplot2.tidyverse.org/reference/facet_grid.html 
stat.test <- first_bw_wide %>%
  group_by(time) %>%
  t_test(weight ~ Analgesic) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "time") 

#modify the statistics to the pairwise 3 groups
stat.test <- first_bw_wide %>%
  group_by(time) %>%
  t_test(weight ~ Analgesic)
stat.test
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "time") 

lp <- ggline(first_bw_wide,'time','weight',
             linetype = 'Analgesic',
             color = 'Analgesic',add = 'mean_se',
             facet.by = 'Sex')
lp

lp + stat_pvalue_manual(
  stat.test,  label = "{p.adj}{p.adj.signif}", 
  tip.length = 0, linetype  = "blank"
)

lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  linetype  = "blank", vjust = 2,
  hide.ns = T
)

first_bw_wide_f <- first_bw_wide[first_bw_wide$Sex=='f',]
first_bw_wide_m <- first_bw_wide[first_bw_wide$Sex=='m',]
lp_f <- ggline(first_bw_wide_f,'time','weight',
       linetype = 'Analgesic',
       color = 'Analgesic',add = c('mean_se','jitter'))
ggline(first_bw_wide_m,'time','weight',
       linetype = 'Analgesic',
       color = 'Analgesic',add = 'mean_se')

lp_m <- ggline(first_bw_wide_m,'time','weight',
       linetype = 'Analgesic',
       color = 'Analgesic',add = c('mean_se','jitter'))

#https://www.datanovia.com/en/blog/how-to-add-p-values-onto-a-grouped-ggplot-using-the-ggpubr-r-package/ 
#very important on how to add p-values
stat.test <- first_bw_wide_m %>%
  group_by(time) %>%
  t_test(weight ~ Analgesic) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "time") 
lp_m + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
)


#for the female mice
stat.test <- first_bw_wide_f %>%
  group_by(time) %>%
  t_test(weight ~ Analgesic) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "time") 
lp_f + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
)

lp_f + stat_pvalue_manual(
  stat.test,  label = "{p.adj}{p.adj.signif}", 
  linetype  = "blank", vjust = 2,
  hide.ns = T
)

#grouped box plots
bxp <- ggboxplot(first_bw_wide,
                 x='time',y='weight',
                 color = 'Analgesic',fill = 'Sex',
                 facet.by = 'Sex')
bxp
