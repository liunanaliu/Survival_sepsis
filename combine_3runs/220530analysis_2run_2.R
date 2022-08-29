#second run for css and pain analysis

#pain analysis for second run
pain_overall <- X220525survival_run2
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
#4
sum(is.na(pain_overall$`t2,25o`))
#4
sum(is.na(pain_overall$`t2,75o`))
#7
sum(is.na(pain_overall$`t3,25o`))
#8
sum(is.na(pain_overall$`t3,75o`))
#8
sum(is.na(pain_overall$`t4,25o`))
#8
sum(is.na(pain_overall$`t4,75o`))
#8
sum(is.na(pain_overall$`t5,25o`))
#13
sum(is.na(pain_overall$`t5,75o`))
#13
sum(is.na(pain_overall$`t6,25o`))
#13
sum(is.na(pain_overall$`t6,75o`))
#14
sum(is.na(pain_overall$`t7o`))
#14

ggplot(pain_overall_wide,aes(x=time,y=pain,fill=pain_type))+
  geom_bar(stat = 'identity')+
  annotate('text',
           x=1:length(table(pain_overall_wide$time)),
           y=aggregate(pain~time,pain_overall_wide,max)[,2],
           label=c(31,31,31,31,27,27,24,23,23,23,23,18,18,18,17,17),
           col='black',
           vjust=-1)


#css analysis for second run
css_overall <- X220525survival_run2
#change the format of table from long to wide
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
names(css_overall)[2] <- 'SEK00'

#get the overall css for each timepoint per mouse
css_overall$`t0o` <- rowSums(css_overall[,c(9:13)])
css_overall$`t0,25o` <- rowSums(css_overall[,c(14:18)])
css_overall$`t0,75o` <- rowSums(css_overall[,c(19:23)])
css_overall$`t1,25o` <- rowSums(css_overall[,c(24:28)])
css_overall$`t1,75o` <- rowSums(css_overall[,c(29:33)])
css_overall$`t2,25o` <- rowSums(css_overall[,c(34:38)])
css_overall$`t2,75o` <- rowSums(css_overall[,c(39:43)])
css_overall$`t3,25o` <- rowSums(css_overall[,c(44:48)])
css_overall$`t3,75o` <- rowSums(css_overall[,c(49:53)])
css_overall$`t4,25o` <- rowSums(css_overall[,c(54:58)])
css_overall$`t4,75o` <- rowSums(css_overall[,c(59:63)])
css_overall$`t5,25o` <- rowSums(css_overall[,c(64:68)])
css_overall$`t5,75o` <- rowSums(css_overall[,c(69:73)])
css_overall$`t6,25o` <- rowSums(css_overall[,c(74:78)])
css_overall$`t6,75o` <- rowSums(css_overall[,c(79:83)])
css_overall$`t7o` <- rowSums(css_overall[,c(84:88)])

names(css_overall)


css_overall_wide <- css_overall %>% gather(key = 'time',
                                             value = 'css',
                                           "t0o","t0,25o", "t0,75o",   
                                           "t1,25o","t1,75o","t2,25o",
                                           "t2,75o","t3,25o","t3,75o", 
                                           "t4,25o","t4,75o","t5,25o","t5,75o",
                                           "t6,25o","t6,75o","t7o",
                                           "t0f","t0,25f", "t0,75f",   
                                           "t1,25f","t1,75f","t2,25f",
                                           "t2,75f","t3,25f","t3,75f", 
                                           "t4,25f","t4,75f","t5,25f","t5,75f",
                                           "t6,25f","t6,75f","t7f",
                                           "t0d","t0,25d", "t0,75d",   
                                           "t1,25d","t1,75d","t2,25d",
                                           "t2,75d","t3,25d","t3,75d", 
                                           "t4,25d","t4,75d","t5,25d","t5,75d",
                                           "t6,25d","t6,75d","t7d",
                                           "t0p","t0,25p", "t0,75p",   
                                           "t1,25p","t1,75p","t2,25p",
                                           "t2,75p","t3,25p","t3,75p", 
                                           "t4,25p","t4,75p","t5,25p","t5,75p",
                                           "t6,25p","t6,75p","t7p",
                                           "t0r","t0,25r", "t0,75r",   
                                           "t1,25r","t1,75r","t2,25r",
                                           "t2,75r","t3,25r","t3,75r", 
                                           "t4,25r","t4,75r","t5,25r","t5,75r",
                                           "t6,25r","t6,75r","t7r",
                                           "t0a","t0,25a", "t0,75a",   
                                           "t1,25a","t1,75a","t2,25a",
                                           "t2,75a","t3,25a","t3,75a", 
                                           "t4,25a","t4,75a","t5,25a","t5,75a",
                                           "t6,25a","t6,75a","t7a")  %>%
  convert_as_factor(No,SEK00,time)
head(css_overall_wide)

library(stringr)
library(stringi)
css_overall_wide$css_type <- stri_sub(css_overall_wide$time,-1)
table(css_overall_wide$css_type)
#extract css type from 5 parts, including a,d,f,p,r but also include o,overall



css_overall_wide$time <- stri_sub(css_overall_wide$time,
                                   2,
                                   str_length(css_overall_wide$time)-1)


table(css_overall_wide$time)

save(css_overall_wide,file = 'css_overall_wide.rda')



css_overall_wide_wo <- css_overall_wide[css_overall_wide$css_type!='o',]
css_overall_wide_o <- css_overall_wide[css_overall_wide$css_type=='o',]

library(ggplot2)
lp <- ggline(css_overall_wide,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add =c('mean_se','jitter'))
lp

lp <- ggline(css_overall_wide,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add ='mean_se',
             facet.by = 'Sex')
lp


lp <- ggline(css_overall_wide_wo,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add ='mean_se',
             facet.by = 'Sex')
lp


ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type,group=Analgesic))+
  geom_bar(position = position_dodge2(0,75),width=0,5)+
  facet_wrap(~Analgesic)
#Error in match.arg(preserve) : 'arg' must be NULL or a character vector

ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type))+
  geom_bar(position = 'fill',stat = 'identity')+
  facet_grid(~Analgesic)

sum(is.na(pain_overall$`t1,75o`))
#4
sum(is.na(pain_overall$`t2,25o`))
#4
sum(is.na(pain_overall$`t2,75o`))
#7
sum(is.na(pain_overall$`t3,25o`))
#8
sum(is.na(pain_overall$`t3,75o`))
#8
sum(is.na(pain_overall$`t4,25o`))
#8
sum(is.na(pain_overall$`t4,75o`))
#8
sum(is.na(pain_overall$`t5,25o`))
#13
sum(is.na(pain_overall$`t5,75o`))
#13
sum(is.na(pain_overall$`t6,25o`))
#13
sum(is.na(pain_overall$`t6,75o`))
#14
sum(is.na(pain_overall$`t7o`))
#14

ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type))+
  geom_bar(stat = 'identity')+
  annotate('text',
           x=1:length(table(css_overall_wide_wo$time)),
           y=aggregate(css~time,css_overall_wide_wo,max)[,2],
           label=c(31,31,31,31,27,27,24,23,23,23,23,18,18,18,17,17),
           col='black',
           vjust=-1)
