#for another method of handle na
css <- X20220519_survival_run1
css_overall <- css

#css_overall <- na.omit(css)
#remove na, previous way

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

css_overall2 <- css_overall
css_overall3 <- na.omit(css_overall)

css_overall3$`t2,25o` <- matrix(unlist(str_split(css_overall3$`t2,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall3$`t2,75o` <- matrix(unlist(str_split(css_overall3$`t2,75`,',')),
                               ncol = 6,byrow = T)[,1]

css_overall3$`t3,25o` <- matrix(unlist(str_split(css_overall3$`t3,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall3$`t3,75o` <- matrix(unlist(str_split(css_overall3$`t3,75`,',')),
                               ncol = 6,byrow = T)[,1]

css_overall3$`t4,25o` <- matrix(unlist(str_split(css_overall3$`t4,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall3$`t4,75o` <- matrix(unlist(str_split(css_overall3$`t4,75`,',')),
                               ncol = 6,byrow = T)[,1]

css_overall3$`t5,25o` <- matrix(unlist(str_split(css_overall3$`t5,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall3$`t5,75o` <- matrix(unlist(str_split(css_overall3$`t5,75`,',')),
                               ncol = 6,byrow = T)[,1]

css_overall3$`t6,25o` <- matrix(unlist(str_split(css_overall3$`t6,25`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall3$`t6,75o` <- matrix(unlist(str_split(css_overall3$`t6,75`,',')),
                               ncol = 6,byrow = T)[,1]
css_overall3$`t7o` <- matrix(unlist(str_split(css_overall3$`t7`,',')),
                                ncol = 6,byrow = T)[,1]

css_overall <- css_overall3


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

css_overall$t0o <- rowSums(css_overall[,c(37:41)])

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

css_overall$`t0,25o` <- rowSums(css_overall[,c(42:46)])

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

css_overall$`t0,75o` <- rowSums(css_overall[,c(47:51)])

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

css_overall$`t1,25o` <- rowSums(css_overall[,c(52:56)])

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

css_overall$`t1,75o` <- rowSums(css_overall[,c(57:61)])

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

css_overall$`t2,25o` <- rowSums(css_overall[,c(62:66)])

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

css_overall$`t2,75o` <- rowSums(css_overall[,c(67:71)])

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

css_overall$`t3,25o` <- rowSums(css_overall[,c(72:76)])

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

css_overall$`t3,75o` <- rowSums(css_overall[,c(77:81)])

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

css_overall$`t4,25o` <- rowSums(css_overall[,c(82:86)])

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

css_overall$`t4,75o` <- rowSums(css_overall[,c(87:91)])


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

css_overall$`t5,25o` <- rowSums(css_overall[,c(92:96)])

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

css_overall$`t5,75o` <- rowSums(css_overall[,c(97:101)])


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

css_overall$`t6,25o` <- rowSums(css_overall[,c(102:106)])


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

css_overall$`t6,75o` <- rowSums(css_overall[,c(107:111)])

css_overall$`t7f` <- matrix(as.numeric(unlist(str_split(css_overall$`t7`,','))),
                               ncol = 6,byrow = T)[,2]
css_overall$`t7f`<- ifelse(css_overall$`t7f`==1,0,1)
css_overall$`t7d` <- matrix(as.numeric(unlist(str_split(css_overall$`t7`,','))),
                               ncol = 6,byrow = T)[,3]
css_overall$`t7p` <- matrix(as.numeric(unlist(str_split(css_overall$`t7`,','))),
                               ncol = 6,byrow = T)[,4]
css_overall$`t7r` <- matrix(as.numeric(unlist(str_split(css_overall$`t7`,','))),
                               ncol = 6,byrow = T)[,5]
css_overall$`t7a` <- matrix(as.numeric(unlist(str_split(css_overall$`t7`,','))),
                               ncol = 6,byrow = T)[,6]

css_overall$`t7o` <- rowSums(css_overall[,c(112:116)])

css_overall3 <- css_overall
#very important to project the right object!!!

#now processing css_overall2 with 30 mice
#11 dead mice all happened before t4,25
#as long as there is na in the row, you could not extract values any more! very important!!!
#the first dead mouse is t1,75, so i could just use data before t1,75 nut not include t1,75

css_overall2 <- css_overall2[is.na(css_overall2$t7),]
#rrmove alive mice
css_overall2 <- css_overall2 [-5,]
#temporary remove the frist dead mouse 84588
css_overall <- css_overall2

css_overall2 <- insertRows()
css_overall2[,c(25,46:50)] <- NULL
css_84588 <- css_overall2[5,]
css_overall2 <- css_overall2[-5,]


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

css_overall$t0o <- rowSums(css_overall[,c(26:30)])

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

css_overall$`t0,25o` <- rowSums(css_overall[,c(31:35)])

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

css_overall$`t0,75o` <- rowSums(css_overall[,c(36:40)])

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

css_overall$`t1,25o` <- rowSums(css_overall[,c(41:45)])

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

css_overall$`t1,75o` <- rowSums(css_overall[,c(46:50)])

save(css_overall3,file = 'css_overall3.rda')

#now want to combine dead and alive mice
css_overall2 <- css_overall

#rbind(css_overall2,css_84588)
library(tibble)
css_84588 <- add_column(css_84588,'t1,75o'=NA,.after = 't1,25o')
css_84588$'t1,75f' <- css_84588$'t1,75d' <- css_84588$'t1,75p' <- css_84588$'t1,75r' <- css_84588$'t1,75a' <- NA
css_overall2[nrow(css_overall2)+1,] <- css_84588
#https://www.geeksforgeeks.org/how-to-insert-blank-row-into-dataframe-in-r/#:~:text=The%20method%20insertRows%20%28%29%20in%20R%20language%20can,is%20declared%20in%20the%20form%20of%20a%20vector.
install.packages('berryFunctions') 
save(css_overall2,file = 'css_overall2.rda')

#failed
if(!requireNamespace("remotes", quitly=TRUE)) install.packages("remotes")
remotes::install_github("brry/berryFunctions")

library(berryFunctions)
#https://statisticsglobe.com/add-new-row-at-specific-index-to-data-frame-in-r
#or flexible usage of cbind
add_column(css_84588,'t1,75o'=NA,.after = 't1,25o')
add_column(css_overall2,'status'='1',.after = 'Analgesic')
css_overall2 <- add_column(css_overall2,'status'='1',.after = 'Analgesic')
css_overall3 <- add_column(css_overall3,'status'='0',.after = 'Analgesic')
library(dplyr)
full_join(css_overall2,css_overall3,by='SEK00-')
#https://stackoverflow.com/questions/70397285/r-dplyr-full-join-no-common-key-need-common-columns-to-blend-together

name_col <- colnames(css_overall2)
css_overall4 <- full_join(css_overall2,css_overall3,by=name_col)
#still lack the dead mice after t1,75!!!!
save(css_overall4,file = 'css_overall4.rda')
write.table(css_overall4,file = 'css_overall4.csv')
write.table(css_overall4,file = 'css_overall4.excel')
#220530
fix(css_overall4)
#add all na except for overall score of css via adding 5 scores
#also remember to change fur from 1 to 0,
save(css_overall4,file = 'css_overall4.rda')


names(css_overall4)[2] <- 'SEK00'

library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
css_overall_wide4 <- css_overall4 %>% gather(key = 'time',
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
head(css_overall_wide4)

library(stringr)
library(stringi)
css_overall_wide4$css_type <- stri_sub(css_overall_wide4$time,-1)
#extract css type from 5 parts, including f,d,p,r,a
css_overall_wide4$time <- stri_sub(css_overall_wide4$time,
                                   1,
                                   str_length(css_overall_wide4$time)-1)
css_overall_wide4$time <- stri_sub(css_overall_wide4$time,2,
                                   str_length(css_overall_wide4$time))
save(css_overall_wide4,file = 'css_overall_wide4.rda')

library(ggplot2)
lp <- ggline(css_overall_wide4,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add =c('mean_se','jitter'))
lp

lp <- ggline(css_overall_wide4,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add ='mean_se',
             facet.by = 'Sex')
lp


lp <- ggline(css_overall_wide_wo4,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add ='mean_se',
             facet.by = 'Sex')
lp
#not true, just usecss_type='o'
lp <- ggline(css_overall_wide_o4,'time','css',
             linetype = 'Analgesic',
             color = 'Analgesic',add ='mean_se',
             facet.by = 'Sex')
lp
css_overall_wide_wo4 <- css_overall_wide4[css_overall_wide4$css_type!='o',]
css_overall_wide_o4 <- css_overall_wide4[css_overall_wide4$css_type=='o',]

ggplot(css_overall_wide4,aes(x=time,y=css,fill=css_type,group=Analgesic))+
  geom_bar(position = position_dodge2(0,75),width=0,5)+
  facet_wrap(~Analgesic)

ggplot(css_overall_wide_wo4,aes(x=time,y=css,fill=css_type))+
  geom_bar(position = 'fill',stat = 'identity')+
  facet_grid(~Analgesic)

#does not work
ggplot(css_overall_wide_wo4,aes(x=time,y=css,fill=css_type))+
  geom_bar(stat = 'identity')+
  annotate('text',
           x=1:length(table(css_overall_wide_wo4$time)),
           y=aggregate(css~time,css_overall_wide_wo4,max)[,2],
          # label=table(css_overall_wide_wo4$time)/5,
           col='black',
           vjust=-1)

ggplot(css_overall_wide_wo4,aes(x=time,y=css,fill=css_type))+
  geom_bar(stat = 'identity')+
  annotate('text',
           x=1:length(table(css_overall_wide_wo4$time)),
           y=aggregate(css~time,css_overall_wide_wo4,max)[,2],
           label=c(30,30,30,30,29,24,24,24,21,19,19,19,19,19,19,19),
           col='black',
           vjust=-1)
