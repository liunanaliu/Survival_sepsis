#220607 add significance
install.packages('ggsignif')
install.packages("processx")
library(ggsignif)
library(ggplot2)
library(ggpubr)
library(processx)
library(ggprism)
my_comparison <- list(c('No','Melosus'),c('No','Metapyrin'),c('Metapyrin','Melosus'))
lp1 <- ggline(combined_css_wide6_ona,'time','css_points',
              linetype = 'Analgesic',
              color = 'Analgesic',add ='mean_se')+
  geom_signif(comparisons = my_comparison,
              map_signif_level = T)
lp1
# https://blog.csdn.net/chengdehe/article/details/105681638
#no help
combined_css_wide6_o$css_points <- as.double(combined_css_wide6_o$css_points)
str(combined_css_wide6_o$time)
#factor
combined_css_wide6_ona <- na.omit(combined_css_wide6_o)
#1280-1030=250

my_comparison <- list(c('No','Melosus'),c('No','Metapyrin'),c('Metapyrin','Melosus'))
lp1 <- ggline(combined_css_wide6_ona,'time','css_points',
              linetype = 'Analgesic',
              color = 'Analgesic',add ='mean_se')+
  stat_compare_means(comparisons = my_comparison,
                     label = 'p.signif',
                     hide.ns = T,
                     label.y = c(4.3,6.8,8.5,8.5,9,7.5,6.8,
                                 6,6,6,6,5,4.3,4.3,4.3,4.3))
lp1
#Computation failed in `stat_signif()`:
#missing value where TRUE/FALSE needed
sum(is.na(combined_css_wide6_o))
#250
sum(is.nan(combined_css_wide6_o))
#default method not implemented for type 'list'
sum(is.infinite(combined_css_wide6_o))
#default method not implemented for type 'list'
#https://www.programmingr.com/r-error-messages/error-in-lm-fitx-y-offset-offset-singular-ok-singular-ok-na-nan-inf-in-y/
#no help
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
devtools::install_github("const-ae/ggsignif")

Sys.setenv(LANG = "en")
res <- compare_means(css_points ~ Analgesic,
                     group.by = 'time',
                     data = combined_css_wide6_o)
#45
res2 <- res %>% filter(p.signif !='ns')
#11

#error in xmin & xmax
lp1 <- ggline(combined_css_wide6_ona,'time','css_points',
              linetype = 'Analgesic',
              color = 'Analgesic',add ='mean_se')+
  stat_pvalue_manual(res2,x='time',label = 'p.signif')
lp1

#In Ops.factor(time, 0.1) : ‘+’ not meaningful for factors
#no signif between group pf melosus & metapyrin
#* is a symbol of melosus vs no, # is a symbol of metapyrin vs no
fix(res2)
#change # for metapyrin vs no
res2$y.position <- 5
fix(res2)
res2$x.position <- res2$time

lp2 <- ggline(combined_css_wide6_ona,'time','css_points',
              linetype = 'Analgesic',
              color = 'Analgesic',add ='mean_se')+
  geom_bracket(data = res2,aes(xmin=time,
                               xmax=time,
                               label=p.signif),
               y.position = res2$y.position,
               linetype  = "blank")
lp2




lp3 <- ggline(combined_css_wide6_o,'time','css_points',
              linetype = 'Analgesic',
              color = 'Analgesic',add ='mean_se',
              facet.by = 'Sex')+
                 geom_bracket(data = res3,aes(xmin=time,
                               xmax=time,
                               label=p.signif),
               y.position = res3$y.position,
               linetype  = "blank")
lp3
res <- compare_means(css_points ~ Analgesic,
                     group.by = c('time','Sex'),
                     data = combined_css_wide6_o)
#85
res3 <- res %>% filter(p.signif !='ns')
#15
#melosus just functions in females from 0,75 timepoint to 1,75 
res3$y.position <- 5
fix(res3)
save(res3,file = 'res3.rda')

css_overall_wide_wo <- combined_css_wide6_wo


n1 <- c(30,30,30,30,29,24,24,24,21,19,19,19,19,19,19,19)
n2 <- c(31,31,31,31,27,27,24,23,23,23,23,18,18,18,17,17)
n3 <- c(19,19,19,19,18,18,17,16,16,16,16,16,16,16,16,16)
n <- n1+n2+n3


lp4<- ggplot(css_overall_wide_wo,aes(x=time,y=css_points,
                                     fill=criteria))+
  geom_bar(position = 'fill',stat = 'identity')+
  theme(axis.text.x = element_text(size=6))+
  annotate('text',
           x=1:length(table(css_overall_wide_wo$time)),
           y=0.05,
           label=n,
           col='black',
           vjust=-1)
lp4

lp5<- ggplot(css_overall_wide_wo,aes(x=time,y=css_points,
                                     fill=criteria))+
  geom_bar(position = 'fill',stat = 'identity')+
  theme(axis.text.x = element_text(size=6))+
  facet_grid(~Analgesic)
lp5
#conside add the sub-groups, namely analgesic, number by time, 

#no need for the further plot due to distinct runs

lp5 <- ggline(css_overall_wide_wo,'time','css',
              linetype = 'round',
              color = 'round',add = 'mean_se',
              facet.by = 'Sex')

lp5

lp6 <- ggline(combine_css_overall_wide,'time','css',
              linetype = 'Analgesic',
              color = 'Analgesic',add = 'mean_se',
              facet.by = 'round')+
  theme(axis.text.x = element_text(size=6))

lp6
