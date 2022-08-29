#
pi1a1 <- ggarrange(pi1a$plot,pi1a$table,
                   ncol = 1,
                   heights = c(.6,.4))

save(pi1a1,file = 'pi1a1220701.rda')
pi1c1 <- ggarrange(pi1s1,pi1sT,
                   ncol = 1,
                   heights = c(.6,.4))

tiff(filename = 'pi1_0701A.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pi1 <-  ggarrange(pi1t,pi1a1,pi1b,pi1e,
                  labels = c('A','B','C','D'),
                  widths = c(.6,1),
                  heights = c(1,1.2),
                  font.label = list(size=26))  
pi1
save(pi1,file = 'pi1220701.rda')
dev.off()

#220711 for the summerschool of polytarget
tiff(filename = 'pi1234_overall.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pi1234 <- ggarrange(pi1a$plot,pi2a,pi3a,pi4a,
          labels = c('A','B','C','D'),
          common.legend = T,
          font.label = list(size=26))
pi1234
dev.off()
