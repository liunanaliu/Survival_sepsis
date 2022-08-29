#20220523 css analysis
css <- X20220519_survival_run1


library(stringr)
#https://www.geeksforgeeks.org/strsplit-function-in-r/ 
str_split(css$t0,'-') %>% .[[c(30,1)]]
str_split(css$t0,'-')[,1]
str_split(css$t0,',')
c <- matrix(as.numeric(unlist(str_split(css$t0,','))),ncol = 6,byrow = T)
c[,1]
#how to write loop
#in order to extract overall css from each timepoint
css_overall <- na.omit(css)
#remove na

css_overall$t0o <- matrix(as.numeric(unlist(str_split(css_overall$t0,','))),
                          ncol = 6,byrow = T)[,1]
css_overall$`t0,25o` <- matrix(unlist(str_split(css_overall$`t0,25`,',')),
                          ncol = 6,byrow = T)[,1]
css_overall$`t0,75o` <- matrix(unlist(str_split(css_overall$`t0,75`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall$`t1,25o` <- matrix(unlist(str_split(css_overall$`t1,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall$`t1,75o` <- matrix(unlist(str_split(css_overall$`t1,75`,',')),
                               ncol = 6,byrow = T)[,1]

css_overall$`t2,25o` <- matrix(unlist(str_split(css_overall$`t2,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall$`t2,75o` <- matrix(unlist(str_split(css_overall$`t2,75`,',')),
                               ncol = 6,byrow = T)[,1]

css_overall$`t3,25o` <- matrix(unlist(str_split(css_overall$`t3,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall$`t3,75o` <- matrix(unlist(str_split(css_overall$`t3,75`,',')),
                               ncol = 6,byrow = T)[,1]

css_overall$`t4,25o` <- matrix(unlist(str_split(css_overall$`t4,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall$`t4,75o` <- matrix(unlist(str_split(css_overall$`t4,75`,',')),
                               ncol = 6,byrow = T)[,1]

css_overall$`t5,25o` <- matrix(unlist(str_split(css_overall$`t5,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall$`t5,75o` <- matrix(unlist(str_split(css_overall$`t5,75`,',')),
                               ncol = 6,byrow = T)[,1]

css_overall$`t6,25o` <- matrix(unlist(str_split(css_overall$`t6,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall$`t6,75o` <- matrix(unlist(str_split(css_overall$`t6,75`,',')),
                               ncol = 6,byrow = T)[,1]

#for categorical variables, try generalized linear model, glm it is
prop.table(table(css_overall$`t4,75o`))*100

xtabs(~Analgesic+`t6,75o`,css_overall)

#change the factor into numeric variables
c <- matrix(as.numeric(unlist(str_split(css$t0,','))),
            ncol = 6,byrow = T)
c[,1]
#or 
as.numeric(css_overall$`t6,75o`)

#divide single score for css, including f(fur), d(dia), p(posture),
#r(react),a(activate),css=add them up, and have 1,2,3,4 grades for severe

attach(css_overall)
css_overall$t0f <- matrix(as.numeric(unlist(str_split(css_overall$t0,','))),
                          ncol = 6,byrow = T)[,2]
css_overall$t0f<- ifelse(css_overall$t0f==1,0,1)
css_overall$t0d <- matrix(as.numeric(unlist(str_split(css_overall$t0,','))),
                          ncol = 6,byrow = T)[,3]
css_overall$t0p <- matrix(as.numeric(unlist(str_split(css_overall$t0,','))),
                          ncol = 6,byrow = T)[,4]
css_overall$t0r <- matrix(as.numeric(unlist(str_split(css_overall$t0,','))),
                          ncol = 6,byrow = T)[,5]
css_overall$t0a <- matrix(as.numeric(unlist(str_split(css_overall$t0,','))),
                          ncol = 6,byrow = T)[,6]

css_overall$t0o <- rowSums(css_overall[,c(35:39)])

#run for the rest of timepoints
#for t0,25
css_overall$`t0,25f` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,25`,','))),
                          ncol = 6,byrow = T)[,2]
css_overall$`t0,25f`<- ifelse(css_overall$`t0,25f`==1,0,1)
css_overall$`t0,25d` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,25`,','))),
                          ncol = 6,byrow = T)[,3]
css_overall$`t0,25p` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,25`,','))),
                          ncol = 6,byrow = T)[,4]
css_overall$`t0,25r` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,25`,','))),
                          ncol = 6,byrow = T)[,5]
css_overall$`t0,25a` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,25`,','))),
                          ncol = 6,byrow = T)[,6]

css_overall$`t0,25o` <- rowSums(css_overall[,c(40:44)])

#for t0,75
css_overall$`t0,75f` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,75`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t0,75f`<- ifelse(css_overall$`t0,75f`==1,0,1)
css_overall$`t0,75d` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,75`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t0,75p` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,75`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t0,75r` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,75`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t0,75a` <- matrix(as.numeric(unlist(str_split(css_overall$`t0,75`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t0,75o` <- rowSums(css_overall[,c(45:49)])

#for t1,25
css_overall$`t1,25f` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,25`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t1,25f`<- ifelse(css_overall$`t1,25f`==1,0,1)
css_overall$`t1,25d` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,25`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t1,25p` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,25`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t1,25r` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,25`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t1,25a` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,25`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t1,25o` <- rowSums(css_overall[,c(50:54)])

#for t 1,75
css_overall$`t1,75f` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,75`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t1,75f`<- ifelse(css_overall$`t1,75f`==1,0,1)
css_overall$`t1,75d` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,75`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t1,75p` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,75`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t1,75r` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,75`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t1,75a` <- matrix(as.numeric(unlist(str_split(css_overall$`t1,75`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t1,75o` <- rowSums(css_overall[,c(55:59)])

#for t2,25
css_overall$`t2,25f` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,25`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t2,25f`<- ifelse(css_overall$`t2,25f`==1,0,1)
css_overall$`t2,25d` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,25`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t2,25p` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,25`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t2,25r` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,25`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t2,25a` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,25`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t2,25o` <- rowSums(css_overall[,c(60:64)])

#for t2,75
css_overall$`t2,75f` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,75`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t2,75f`<- ifelse(css_overall$`t2,75f`==1,0,1)
css_overall$`t2,75d` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,75`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t2,75p` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,75`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t2,75r` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,75`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t2,75a` <- matrix(as.numeric(unlist(str_split(css_overall$`t2,75`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t2,75o` <- rowSums(css_overall[,c(65:69)])

#for t3,25
css_overall$`t3,25f` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,25`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t3,25f`<- ifelse(css_overall$`t3,25f`==1,0,1)
css_overall$`t3,25d` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,25`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t3,25p` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,25`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t3,25r` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,25`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t3,25a` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,25`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t3,25o` <- rowSums(css_overall[,c(70:74)])

#for t3,75
css_overall$`t3,75f` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,75`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t3,75f`<- ifelse(css_overall$`t3,75f`==1,0,1)
css_overall$`t3,75d` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,75`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t3,75p` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,75`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t3,75r` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,75`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t3,75a` <- matrix(as.numeric(unlist(str_split(css_overall$`t3,75`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t3,75o` <- rowSums(css_overall[,c(75:79)])

#for t4,25
css_overall$`t4,25f` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,25`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t4,25f`<- ifelse(css_overall$`t4,25f`==1,0,1)
css_overall$`t4,25d` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,25`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t4,25p` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,25`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t4,25r` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,25`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t4,25a` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,25`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t4,25o` <- rowSums(css_overall[,c(80:84)])

#for t4,75
css_overall$`t4,75f` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,75`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t4,75f`<- ifelse(css_overall$`t4,75f`==1,0,1)
css_overall$`t4,75d` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,75`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t4,75p` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,75`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t4,75r` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,75`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t4,75a` <- matrix(as.numeric(unlist(str_split(css_overall$`t4,75`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t4,75o` <- rowSums(css_overall[,c(85:89)])


#for 5,25
css_overall$`t5,25f` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,25`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t5,25f`<- ifelse(css_overall$`t5,25f`==1,0,1)
css_overall$`t5,25d` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,25`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t5,25p` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,25`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t5,25r` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,25`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t5,25a` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,25`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t5,25o` <- rowSums(css_overall[,c(90:94)])

#for t5,75
css_overall$`t5,75f` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,75`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t5,75f`<- ifelse(css_overall$`t5,75f`==1,0,1)
css_overall$`t5,75d` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,75`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t5,75p` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,75`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t5,75r` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,75`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t5,75a` <- matrix(as.numeric(unlist(str_split(css_overall$`t5,75`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t5,75o` <- rowSums(css_overall[,c(95:99)])


#for t6,25
css_overall$`t6,25f` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,25`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t6,25f`<- ifelse(css_overall$`t6,25f`==1,0,1)
css_overall$`t6,25d` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,25`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t6,25p` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,25`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t6,25r` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,25`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t6,25a` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,25`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t6,25o` <- rowSums(css_overall[,c(100:104)])


#for t6,75
css_overall$`t6,75f` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,75`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t6,75f`<- ifelse(css_overall$`t6,75f`==1,0,1)
css_overall$`t6,75d` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,75`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t6,75p` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,75`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t6,75r` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,75`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t6,75a` <- matrix(as.numeric(unlist(str_split(css_overall$`t6,75`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t6,75o` <- rowSums(css_overall[,c(105:109)])

save(css_overall,file = 'css_overall.rda')


names(css_overall)[2] <- 'SEK00'
css_overall_wide <- css_overall %>% gather(key = 'time',
                                     value = 'css',
                                     "t0o","t0,25o", "t0,75o",   
                                     "t1,25o","t1,75o","t2,25o",
                                     "t2,75o","t3,25o","t3,75o", 
                                     "t4,25o","t4,75o","t5,25o","t5,75o",
                                     "t6,25o","t6,75o" )  %>%
  convert_as_factor(No,SEK00,time)
head(css_overall_wide)

css_overall_wide2 <- css_overall %>% gather(key = 'time',
                                           value = 'css',
                                           "t0o","t0,25o", "t0,75o",   
                                           "t1,25o","t1,75o","t2,25o",
                                           "t2,75o","t3,25o","t3,75o", 
                                           "t4,25o","t4,75o","t5,25o","t5,75o",
                                           "t6,25o","t6,75o",
                                           "t0f","t0,25f", "t0,75f",   
                                           "t1,25f","t1,75f","t2,25f",
                                           "t2,75f","t3,25f","t3,75f", 
                                           "t4,25f","t4,75f","t5,25f","t5,75f",
                                           "t6,25f","t6,75f",
                                           "t0d","t0,25d", "t0,75d",   
                                           "t1,25d","t1,75d","t2,25d",
                                           "t2,75d","t3,25d","t3,75d", 
                                           "t4,25d","t4,75d","t5,25d","t5,75d",
                                           "t6,25d","t6,75d",
                                           "t0p","t0,25p", "t0,75p",   
                                           "t1,25p","t1,75p","t2,25p",
                                           "t2,75p","t3,25p","t3,75p", 
                                           "t4,25p","t4,75p","t5,25p","t5,75p",
                                           "t6,25p","t6,75p",
                                           "t0r","t0,25r", "t0,75r",   
                                           "t1,25r","t1,75r","t2,25r",
                                           "t2,75r","t3,25r","t3,75r", 
                                           "t4,25r","t4,75r","t5,25r","t5,75r",
                                           "t6,25r","t6,75r",
                                           "t0a","t0,25a", "t0,75a",   
                                           "t1,25a","t1,75a","t2,25a",
                                           "t2,75a","t3,25a","t3,75a", 
                                           "t4,25a","t4,75a","t5,25a","t5,75a",
                                           "t6,25a","t6,75a")  %>%
  convert_as_factor(No,SEK00,time)
head(css_overall_wide2)

library(stringr)
library(stringi)
css_overall_wide2$css_type <- stri_sub(css_overall_wide2$time,-1)
#extract css type from 5 parts, including f,d,p,r,a
css_overall_wide2$time <- stri_sub(css_overall_wide2$time,
                                       1,
                                       str_length(css_overall_wide2$time)-1)
save(css_overall_wide2,file = 'css_overall_wide2.rda')


#change the level of time, t0o is in the third order, that is a error,
#t0o should be the first level
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
css_overall_wide$time <- factor(css_overall_wide$time,
                                levels = c("t0o","t0,25o", "t0,75o",   
                                           "t1,25o","t1,75o","t2,25o",
                                           "t2,75o","t3,25o","t3,75o", 
                                           "t4,25o","t4,75o","t5,25o","t5,75o",
                                           "t6,25o","t6,75o"),
                                labels = c( "t0","t0,25", "t0,75",   
                                            "t1,25","t1,75","t2,25",
                                            "t2,75","t3,25","t3,75", "t4,25",   
                                            "t4,75","t5,25","t5,75",
                                            "t6,25","t6,75"))

library(ggplot2)
lp <- ggline(css_overall_wide2,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add =c('mean_se','jitter'))
lp

lp <- ggline(css_overall_wide2,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add ='mean_se',
             facet.by = 'Sex')
lp



#css_overall_wide <- css_overall_wide[,c(1,2,3,4,90,91)]
#work well for the bw analysis
css_overall_wide$css <- as.numeric(css_overall_wide$css)
#no effects on that error??
#i guess the reason is too much replicate or constant data, 
#the css did not vary so much
stat.test <- css_overall_wide %>%
  group_by(time) %>%
  t_test(css ~ Analgesic)

stat.test <- css_overall_wide %>%
  dplyr::group_by(time) %>%
  rstatix::t_test(css ~ Analgesic) 
stat.test
#error, did not know why??? signle run works fun, but not for 
#t_test
compare_means(css~Analgesic,data = css_overall_wide,
              group.by = time)
#http://sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
#also error,! Can't convert a function to a symbol.
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "time") 


lp + stat_pvalue_manual(
  stat.test,  label = "{p.adj}{p.adj.signif}", 
  tip.length = 0, linetype  = "blank"
)

lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  linetype  = "blank", vjust = 2,
  hide.ns = T
)



library(dplyr)
css_overall %>% 
  rowwise() %>%
  mutate(
    t0o = sum(c(t0f,t0d))
    )
#https://www.datasciencemadesimple.com/sum-function-in-r/#:~:text=Sum%20function%20in%20R%20%E2%80%93%20sum%20%28%29%2C%20is,wise%20sum%20lets%20see%20an%20example%20of%20each.
#error, why

# css_overall$t0o <- sum(c(53:39)) error

#stacked bar plot
#fill=1
css_overall_wide_wo <- css_overall_wide2[css_overall_wide2$css_type!='o',]
ggbarplot(css_overall_wide_wo,x='time',y='css',
          color='Analgesic')
#standard/ scale data
#https://albert-rapp.de/post/2021-09-11-position-adjustment/

ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type))+
  geom_col(position = 'fill',
           stat='identity')
+
  #geom_bar(position = 'fill')
#next is to add subgroups
#20220525
ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type,group=Analgesic))+
  geom_bar(position = position_dodge2(0,75),width=0,5)+
  facet_wrap(~Analgesic)
  
ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type))+
  geom_bar(position = 'fill',stat = 'identity')+
  facet_grid(~Analgesic)
#work well

#not to scale the css
ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type))+
  geom_bar(stat = 'identity')

ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type))+
  geom_bar(stat = 'identity')+
  facet_grid(~Analgesic)

#change the lables in x-axis from t to without t
ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type))+
  geom_bar(stat = 'identity')
 # scale_x_discrete(labels=c("t0"=,"t0,25", "t0,75",   
                            "t1,25","t1,75","t2,25",
                            "t2,75","t3,25","t3,75", "t4,25",   
                            "t4,75","t5,25","t5,75",
                            "t6,25","t6,75")
#https://www.delftstack.com/howto/r/ggplot-axis-tick-labels-in-r/
#or more easier to directly change the time in the data frame
css_overall_wide_wo$time <- matrix((unlist(str_sub(css_overall_wide_wo$time,2))))
matrix((unlist(str_sub(css_overall_wide_wo$time,2))))
save(css_overall_wide_wo,file ='css_overall_wide_wo.rda' )
#set the working directory
#http://www.sthda.com/english/wiki/running-rstudio-and-setting-up-your-working-directory-easy-r-programming#:~:text=For%20the%20first%20time%20you%20use%20R%2C%20the,MAC%20OSX%3A%20Tools%20%E2%80%93%3E%20Change%20the%20working%20directory

#add the number of mice to be analysis
css_overall_wide_wo$time <- as.factor(css_overall_wide_wo$time)
ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type))+
  geom_bar(stat = 'identity')+
  annotate('text',
           x=1:length(table(css_overall_wide_wo$time)),
           y=aggregate(css~time,css_overall_wide_wo,max)[,2],
          label=table(css_overall_wide_wo$time)/5,
           col='black',
           vjust=-1)

#failed ggplot(css_overall_wide_wo,aes(x=time,y=css,fill=css_type))+
  geom_bar(stat = 'identity')+
  facet_grid(~Analgesic)+
  annotate('text',
           x=1:length(table(css_overall_wide_wo$time)),
           y=aggregate(css~time,css_overall_wide_wo,max)[,2],
           label=table(css_overall_wide_wo$time)/5,
           col='black',
           vjust=-1)
#also work well
#https://statisticsglobe.com/add-number-observations-group-ggplot2-boxplot-r

#next is to include dead mice with NAs
#https://stackoverflow.com/questions/47933752/ignoring-missing-na-values-in-ggplot
#ggplot2 ignores NA values, 