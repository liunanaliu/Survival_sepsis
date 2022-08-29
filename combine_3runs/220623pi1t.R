#220623
#https://aosmith.rbind.io/2019/05/13/small-multiples-plot/
#for share axis in one ggarrange combined plot

#but how to combine plots from non-ggplot2
#https://stackoverflow.com/questions/62496887/plot-multiple-plots-that-were-not-created-using-ggplot-in-a-row-grid

#https://stackoverflow.com/questions/30794273/plot-a-data-frame-as-a-table
#make a table for human feces
library(gridExtra)
library(grid)
chargeV <- data.frame(row.names = paste('name',1:9))
fix(chargeV)#bacterial strain
chargeV$`Mean ` <- c(8.00E+11,
                     6.98E+11,
                     1.03E+10,
                     3.36E+09,
                     1.33E+08,
                     1.18E+08,
                     3.85E+06,
                     1.40E+06,
                     1.02E+04)
chargeV$SD <- c(1.48E+11,
                1.26E+11,
                2.18E+10,
                3.18E+09,
                4.38E+07,
                8.16E+07,
                8.10E+05,
                4.30E+05,
                6.91E+03)
row.names(chargeV)
rownames(chargeV)
colnames(chargeV)
chargeV2 <- data.frame(row.names = paste('name',1:5))
fix(chargeV2)#virus strain
chargeV2$Concentration <- c('n.d.',
                           'n.d.',
                            'n.d.',
                            'n.d.',
                            'n.d.')
#n.d. = not detected (below clinical detection limit)
chargeV2$Method <- 'PCR'
chargeV2$N <- 3


chargeV$Bacterial <- rownames(chargeV)
colnames(chargeV)[1] <- 'Mean'
chargeV <- chargeV %>% select(Bacterial,everything())
chargeV <- chargeV[,-5]

chargeV2$Virus <- rownames(chargeV2)
chargeV2 <- chargeV2 %>% select(Virus,everything())

fix(chargeV)
fix(chargeV2)
#add strain in the first column

save(chargeV,file = 'chargeV.rda')
save(chargeV2,file = 'chargeV2.rda')

library(ggplot2)
ggplot(chargeV,aes(bacteria,Mean))+
  geom_errorbar(aes(ymin = Mean-SD,ymax = Mean+SD))

load("B:/ORG/Forschung/AN3/Liu_Na_LN/20220419_cxcr2_sepsis/20220601combined_3runs/220603plot_modification/chargeV2.rda")
load("B:/ORG/Forschung/AN3/Liu_Na_LN/20220419_cxcr2_sepsis/20220601combined_3runs/220603plot_modification/chargeV.rda")


tiff(filename = 'pi1t.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

v <- tableGrob(chargeV,
               rows = NULL,
               theme=ttheme_minimal(base_size = 12,
                                    core=list(fg_params=list(fontface='bold.italic',
                                                             c(rep('plain',3)))),
                                    colhead=list(bg_params=list(fill="#F8766D")),
                                    base_colour = 'black',
                                    base_family = ''))
#v$widths <- unit(rep(1/ncol(v),ncol(v),'npc'))
v2 <- tableGrob(chargeV2,
                rows = NULL,
                theme = ttheme_minimal(base_size = 12,
                                       core=list(fg_params=list(fontface='bold.italic',
                                                                c(rep('plain',3)))),
                                       colhead=list(bg_params=list(fill="#F8766D")),
                                       base_colour = 'black',
                                       base_family = ''))
#https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
#https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
options(scipen = -1)
options(digits = 1)
getOption('scipen')
getOption('digits')
#https://www.jianshu.com/p/ba7d4caae8bd
#https://blog.csdn.net/datanewlook/article/details/108947031
pi1t <- grid.arrange(gtable_combine(v,v2,along = 2))
save(pi1t,file = '220629pi1t.rda')
pi1t
pi1d1 <- grid.draw(pi1t)
dev.off()
