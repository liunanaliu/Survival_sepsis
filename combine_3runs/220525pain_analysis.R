#220530 pain
pain_overall <- X20220519_survival_run1
#change the format of table from long to wide
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
names(pain_overall)[2] <- 'SEK00'

pain_overall_wide <- pain_overall %>% gather(key = 'time',
                                             value = 'pain',
                                             "t0o","t0,25o", "t0,75o",   
                                             "t1,25o","t1,75o","t2,25o",
                                             "t2,75o","t3,25o","t3,75o", 
                                             "t4,25o","t4,75o","t5,25o","t5,75o",
                                             "t6,25o","t6,75o","t7o",
                                             "t0n","t0,25n", "t0,75n",   
                                             "t1,25n","t1,75n","t2,25n",
                                             "t2,75n","t3,25n","t3,75n", 
                                             "t4,25n","t4,75n","t5,25n","t5,75n",
                                             "t6,25n","t6,75n","t7n",
                                             "t0c","t0,25c", "t0,75c",   
                                             "t1,25c","t1,75c","t2,25c",
                                             "t2,75c","t3,25c","t3,75c", 
                                             "t4,25c","t4,75c","t5,25c","t5,75c",
                                             "t6,25c","t6,75c","t7c",
                                             "t0e","t0,25e", "t0,75e",   
                                             "t1,25e","t1,75e","t2,25e",
                                             "t2,75e","t3,25e","t3,75e", 
                                             "t4,25e","t4,75e","t5,25e","t5,75e",
                                             "t6,25e","t6,75e","t7e",
                                             "t0w","t0,25w", "t0,75w",   
                                             "t1,25w","t1,75w","t2,25w",
                                             "t2,75w","t3,25w","t3,75w", 
                                             "t4,25w","t4,75w","t5,25w","t5,75w",
                                             "t6,25w","t6,75w","t7w")  %>%
  convert_as_factor(No,SEK00,time)
head(pain_overall_wide)

library(stringr)
library(stringi)
pain_overall_wide$pain_type <- stri_sub(pain_overall_wide$time,-1)
table(pain_overall_wide$pain_type)
#extract pain type from 5 parts, including c,e,n,o,w

pain_overall_wide$time <- stri_sub(pain_overall_wide$time,
                                   1,
                                   str_length(pain_overall_wide$time)-1)
pain_overall_wide$time <- stri_sub(pain_overall_wide$time,2,
                                   str_length(pain_overall_wide$time))

table(pain_overall_wide$time)

save(pain_overall_wide,file = 'pain_overall_wide.rda')

library(ggplot2)
lp <- ggline(pain_overall_wide,'time','pain',
             linetype = 'Analgesic',
             color = 'Analgesic',add =c('mean_se','jitter'))
lp

lp <- ggline(pain_overall_wide,'time','pain',
             linetype = 'Analgesic',
             color = 'Analgesic',add ='mean_se',
             facet.by = 'Sex')
lp


lp <- ggline(css_overall_wide_wo4,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add ='mean_se',
             facet.by = 'Sex')
lp


ggplot(pain_overall_wide,aes(x=time,y=pain,fill=pain_type,group=Analgesic))+
  geom_bar(position = position_dodge2(0,75),width=0,5)+
  facet_wrap(~Analgesic)
#Error in match.arg(preserve) : 'arg' must be NULL or a character vector

ggplot(pain_overall_wide,aes(x=time,y=pain,fill=pain_type))+
  geom_bar(position = 'fill',stat = 'identity')+
  facet_grid(~Analgesic)

sum(is.na(pain_overall$`t1,75o`))
sum(is.na(pain_overall$`t2,25o`))
sum(is.na(pain_overall$`t2,75o`))
sum(is.na(pain_overall$`t3,25o`))
sum(is.na(pain_overall$`t3,75o`))
sum(is.na(pain_overall$`t4,25o`))
sum(is.na(pain_overall$`t4,75o`))
sum(is.na(pain_overall$`t5,25o`))
sum(is.na(pain_overall$`t5,75o`))

ggplot(pain_overall_wide,aes(x=time,y=pain,fill=pain_type))+
  geom_bar(stat = 'identity')+
  annotate('text',
           x=1:length(table(pain_overall_wide$time)),
           y=aggregate(pain~time,pain_overall_wide,max)[,2],
           label=c(30,30,30,30,29,24,24,24,21,19,19,19,19,19,19,19),
           col='black',
           vjust=-1)
