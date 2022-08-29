#error for pi1b
#https://stackoverflow.com/questions/59230825/r-grid-arrange-tiff-microscopy-rgb
library(tiff)
library(grid)
library(gridExtra)
library(jpeg)
library(ggpubr)
library(raster)

load("B:/ORG/Forschung/AN3/Liu_Na_LN/20220419_cxcr2_sepsis/20220601combined_3runs/220603plot_modification/pi1c.rda")
pi1c <- pi1a
pi1c1 <- ggarrange(pi1c$plot,pi1c$table,
                   ncol = 1,
                   heights = c(.6,.4))
pi1c1
#pi1c for overall survival
# pi1a <- grid::rasterGrob(jpeg::readJPEG('pi1a.jpeg'))
#pi1a for flow chart
pi1b <- grid::rasterGrob(tiff::readTIFF('pi1b.tiff'))
pi1b2 <- tiff::readTIFF('pi1b.tiff',native = T)
pi1d <- grid::rasterGrob(tiff::readTIFF('pi1t.tiff'))

grid.arrange(pi1b,pi1d)
#pi1d for forest risk ratio
#pi1b <- grid::rasterGrob(tiff::readTIFF('pi1t.tiff'))
#pi1b for charge V, directly use pi1t fror combination
#no need to convert the format
pi1e 
#pi1e for survival_sex & analgesic
pi1 <- grid.arrange(pi1a,arrangeGrob(pi1t,pi1c1,ncol = 2,
                                     widths = c(1,.8)),
                    arrangeGrob(pi1d,pi1e,ncol = 2,
                                widths = c(1,.8)),
                    labels = c('A','B','C','D','E'),
                    heights = c(1,.8,.8),
                    font.label = list(size=26))
pi1
#Error in grob$wrapvp <- vp : 
#(converted from warning) Coercing LHS to a list

ggarrange(pi1t,pi1c1,pi1d,pi1e,
          ncol = 2)
ggarrange(pi1c1,pi1e,
          ncol = 2)


ggarrange(pi1b,pi1c1,pi1d,pi1e,
          labels = c('B','C','D','E'),
          ncol = 2,
        widths = c(.6,1),
          #heights = c(1,.8,.8),
          font.label = list(size=26))

#220630
load("B:/ORG/Forschung/AN3/Liu_Na_LN/20220419_cxcr2_sepsis/20220601combined_3runs/220603plot_modification/pi1c.rda")
tiff(filename = 'pi1a-new.tiff', 
     units="cm",#scale = 1, 
     width=8.5, height=5.5, 
     res = 600, compression= "lzw")
pi1a
dev.off()
