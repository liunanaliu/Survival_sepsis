#for css of 4th survival
library(readxl)
X20220819survival_run4 <- read_excel("20220819survival_run4.xlsx", 
                                      sheet = "css")

css_4th <- X20220819survival_run4
css_4th <- css_4th[-c(32,48),]
#exclude the 2 non-sepsis outliers
css_4th$Analgesic <- ifelse(css_4th$Analgesic=="No","None",css_4th$Analgesic)
css_4th$Analgesic <- factor(css_4th$Analgesic,
                           levels = c('None','Metapyrin','Melosus'))
css_4th$Sex <- factor(css_4th$Sex,
                     levels = c("F","M"))
css_4th$Sex <- ifelse(css_4th$Sex=="F","Female","Male")

(colnames(css_4th))

#get the real css subtype based on body weight loss
#if fur=1, 
#if fur=2, posture=2
table(css_4th$t0f)
table(css_4th$t0p)
css_4th$`t0p` <- ifelse(css_4th$t0p > css_4th$t0f,css_4th$t0p,css_4th$t0f)

table(css_4th$`t0,25f`)
table(css_4th$`t0,25p`)
css_4th$`t0,25p` <- ifelse(css_4th$`t0,25p` > css_4th$`t0,25f`,
                           css_4th$`t0,25p`,css_4th$`t0,25f`)

table(css_4th$`t0,75f`)
table(css_4th$`t0,75p`)
css_4th$`t0,75p` <- ifelse(css_4th$`t0,75p` > css_4th$`t0,75f`,
                           css_4th$`t0,75p`,css_4th$`t0,75f`)

table(css_4th$`t1,25f`)
table(css_4th$`t1,25p`)
css_4th$`t1,25p` <- ifelse(css_4th$`t1,25p` > css_4th$`t1,25f`,
                           css_4th$`t1,25p`,css_4th$`t1,25f`)

table(css_4th$`t1,75f`)
table(css_4th$`t1,75p`)
css_4th$`t1,75p` <- ifelse(css_4th$`t1,75p` > css_4th$`t1,75f`,
                           css_4th$`t1,75p`,css_4th$`t1,75f`)

table(css_4th$`t2,25p`)
css_4th$`t2,25p` <- ifelse(css_4th$`t2,25p` > css_4th$`t2,25f`,
                           css_4th$`t2,25p`,css_4th$`t2,25f`)

table(css_4th$`t2,75p`)
css_4th$`t2,75p` <- ifelse(css_4th$`t2,75p` > css_4th$`t2,75f`,
                           css_4th$`t2,75p`,css_4th$`t2,75f`)

table(css_4th$`t3,25p`)
css_4th$`t3,25p` <- ifelse(css_4th$`t3,25p` > css_4th$`t3,25f`,
                           css_4th$`t3,25p`,css_4th$`t3,25f`)

table(css_4th$`t3,75p`)
css_4th$`t3,75p` <- ifelse(css_4th$`t3,75p` > css_4th$`t3,75f`,
                           css_4th$`t3,75p`,css_4th$`t3,75f`)

table(css_4th$`t4,25p`)
css_4th$`t4,25p` <- ifelse(css_4th$`t4,25p` > css_4th$`t4,25f`,
                           css_4th$`t4,25p`,css_4th$`t4,25f`)

table(css_4th$`t4,75p`)
css_4th$`t4,75p` <- ifelse(css_4th$`t4,75p` > css_4th$`t4,75f`,
                           css_4th$`t4,75p`,css_4th$`t4,75f`)

table(css_4th$`t5,25p`)
css_4th$`t5,25p` <- ifelse(css_4th$`t5,25p` > css_4th$`t5,25f`,
                           css_4th$`t5,25p`,css_4th$`t5,25f`)

table(css_4th$`t5,75p`)
css_4th$`t5,75p` <- ifelse(css_4th$`t5,75p` > css_4th$`t5,75f`,
                           css_4th$`t5,75p`,css_4th$`t5,75f`)

table(css_4th$`t6,25p`)
css_4th$`t6,25p` <- ifelse(css_4th$`t6,25p` > css_4th$`t6,25f`,
                           css_4th$`t6,25p`,css_4th$`t6,25f`)

table(css_4th$`t6,75p`)
css_4th$`t6,75p` <- ifelse(css_4th$`t6,75p` > css_4th$`t6,75f`,
                           css_4th$`t6,75p`,css_4th$`t6,75f`)

table(css_4th$t7f)
table(css_4th$t7p)
css_4th$`t7p` <- ifelse(css_4th$t7p > css_4th$t7f,
                        css_4th$t7p,css_4th$t7f)
#posture --- fur, 2 save into 1 parameter
#so first for the dia+1, then for the loss of weight to decide on symptomatic
bw_4th_re <- bw_4th
bw_4th_re$`t0l` <- 1
bw_4th_re$`t0,75l` <- (bw_4th_re$t0-bw_4th_re$`t0,75`)
bw_4th_re$`t1,75l` <- (bw_4th_re$`t0,75`-bw_4th_re$`t1,75`)
bw_4th_re$`t2,75l` <- (bw_4th_re$`t1,75`-bw_4th_re$`t2,75`)
bw_4th_re$`t3,75l` <- (bw_4th_re$`t2,75`-bw_4th_re$`t3,75`)
bw_4th_re$`t4,75l` <- (bw_4th_re$`t3,75`-bw_4th_re$`t4,75`)
bw_4th_re$`t5,75l` <- (bw_4th_re$`t4,75`-bw_4th_re$`t5,75`)
bw_4th_re$`t6,75l` <- (bw_4th_re$`t5,75`-bw_4th_re$`t6,75`)
bw_4th_re$`t7l` <- (bw_4th_re$`t6,75`-bw_4th_re$`t7`)
save(bw_4th_re,file = "bw_4th_re.rda")
#firstly give the real s parameter for f to avoid more additional values,
#due to 1 time in one day to weight, css within 1 day shared the same values for loss of bw
table(css_4th$t0f)
css_4th$t0f <- ifelse(bw_4th_re$`t0,75l`<0.1,css_4th$t0d+1,
       ifelse(bw_4th_re$`t0,75l`<=0.18,3,4))
table(css_4th$`t0,25f`)
css_4th$`t0,25f` <- ifelse(bw_4th_re$`t0,75l`<0.1,css_4th$`t0,25d`+1,
                      ifelse(bw_4th_re$`t0,75l`<=0.18,3,4))
table(css_4th$`t0,75f`)
css_4th$`t0,75f` <- ifelse(bw_4th_re$`t0,75l`<0.1,css_4th$`t0,75d`+1,
                           ifelse(bw_4th_re$`t0,75l`<=0.18,3,4))
table(css_4th$`t1,25f`)
css_4th$`t1,25f` <- ifelse(bw_4th_re$`t1,75l`<0.1,css_4th$`t1,25d`+1,
                           ifelse(bw_4th_re$`t1,75l`<=0.18,3,4))
table(css_4th$`t1,75f`)
css_4th$`t1,75f` <- ifelse(bw_4th_re$`t1,75l`<0.1,css_4th$`t1,75d`+1,
                           ifelse(bw_4th_re$`t1,75l`<=0.18,3,4))
table(css_4th$`t2,25f`)
css_4th$`t2,25f` <- ifelse(bw_4th_re$`t2,75l`<0.1,css_4th$`t2,25d`+1,
                           ifelse(bw_4th_re$`t2,75l`<=0.18,3,4))
table(css_4th$`t2,75f`)
css_4th$`t2,75f` <- ifelse(bw_4th_re$`t2,75l`<0.1,css_4th$`t2,75d`+1,
                           ifelse(bw_4th_re$`t2,75l`<=0.18,3,4))
table(css_4th$`t3,25f`)
css_4th$`t3,25f` <- ifelse(bw_4th_re$`t3,75l`<0.1,css_4th$`t3,25d`+1,
                           ifelse(bw_4th_re$`t3,75l`<=0.18,3,4))
table(css_4th$`t3,75f`)
css_4th$`t3,75f` <- ifelse(bw_4th_re$`t3,75l`<0.1,css_4th$`t3,75d`+1,
                           ifelse(bw_4th_re$`t3,75l`<=0.18,3,4))

table(css_4th$`t4,25f`)
css_4th$`t4,25f` <- ifelse(bw_4th_re$`t4,75l`<0.1,css_4th$`t4,25d`+1,
                           ifelse(bw_4th_re$`t4,75l`<=0.18,3,4))
table(css_4th$`t4,75f`)
css_4th$`t4,75f` <- ifelse(bw_4th_re$`t4,75l`<0.1,css_4th$`t4,75d`+1,
                           ifelse(bw_4th_re$`t4,75l`<=0.18,3,4))

table(css_4th$`t5,25f`)
css_4th$`t5,25f` <- ifelse(bw_4th_re$`t5,75l`<0.1,css_4th$`t5,25d`+1,
                           ifelse(bw_4th_re$`t5,75l`<=0.18,3,4))
table(css_4th$`t5,75f`)
css_4th$`t5,75f` <- ifelse(bw_4th_re$`t5,75l`<0.1,css_4th$`t5,75d`+1,
                           ifelse(bw_4th_re$`t5,75l`<=0.18,3,4))

table(css_4th$`t6,25f`)
css_4th$`t6,25f` <- ifelse(bw_4th_re$`t6,75l`<0.1,css_4th$`t6,25d`+1,
                           ifelse(bw_4th_re$`t6,75l`<=0.18,3,4))
table(css_4th$`t6,75f`)
css_4th$`t6,75f` <- ifelse(bw_4th_re$`t6,75l`<0.1,css_4th$`t6,75d`+1,
                           ifelse(bw_4th_re$`t6,75l`<=0.18,3,4))
 
table(css_4th$`t7f`)
css_4th$`t7f` <- ifelse(bw_4th_re$`t7l`<0.1,css_4th$`t7d`+1,
                           ifelse(bw_4th_re$`t7l`<=0.18,3,4)) 
````
ifelse(bw$loss>0.1,0,
      ifelse(bw$loss>0.18,3,4))
for (i in colnames(bw_4th)[9:17]){
  if(bw_4th[,i+1] - bw_4th[,i]<0.1){
    bw_4th[[paste("wl",i)]] <- 0
  }else if (ifelse(bw_4th[,i+1] - bw_4th[,i]>0.18){
    bw_4th[[paste("wl",i)]] <- 3  
    }else{bw_4th[[paste("wl",i)]] <- 4}
````
colnames(css_4th_2)
css_4th_2 <- css_4th[,-c(10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)]
#then changed the type of f to s
#delect type of d
#get the sum of css for each time point
#fur effect???
css_4th_2$`t0o` <- rowSums(css_4th_2[,c(9:12)])
css_4th_2$`t0,25o` <- rowSums(css_4th_2[,c(13:16)])
css_4th_2$`t0,75o` <- rowSums(css_4th_2[,c(17:20)])
css_4th_2$`t1,25o` <- rowSums(css_4th_2[,c(21:24)])
css_4th_2$`t1,75o` <- rowSums(css_4th_2[,c(25:28)])
css_4th_2$`t2,25o` <- rowSums(css_4th_2[,c(29:32)])
css_4th_2$`t2,75o` <- rowSums(css_4th_2[,c(33:36)])
css_4th_2$`t3,25o` <- rowSums(css_4th_2[,c(37:40)])
css_4th_2$`t3,75o` <- rowSums(css_4th_2[,c(41:44)])
css_4th_2$`t4,25o` <- rowSums(css_4th_2[,c(45:48)])
css_4th_2$`t4,75o` <- rowSums(css_4th_2[,c(49:52)])
css_4th_2$`t5,25o` <- rowSums(css_4th_2[,c(53:56)])
css_4th_2$`t5,75o` <- rowSums(css_4th_2[,c(57:60)])
css_4th_2$`t6,25o` <- rowSums(css_4th_2[,c(61:64)])
css_4th_2$`t6,75o` <- rowSums(css_4th_2[,c(65:68)])
css_4th_2$`t7o` <- rowSums(css_4th_2[,c(69:72)])
#d changed into s after transformed into long format
#get the overall score for css
#f,d,p,r,a will be 4 parameters, a,p,r,s
summary(css_4th_2)
colnames(css_4th_2)
css_4th_1_2 <- gather(css_4th_2,key = "Time",value = "CSS",
                       't0o','t0,25o','t0,75o','t1,25o','t1,75o',
                       't2,25o','t2,75o','t3,25o','t3,75o',
                       't4,25o','t4,75o','t5,25o','t5,75o',
                       't6,25o','t6,75o','t7o',
                       't0f','t0,25f','t0,75f','t1,25f','t1,75f',
                       't2,25f','t2,75f','t3,25f','t3,75f',
                       't4,25f','t4,75f','t5,25f','t5,75f',
                       't6,25f','t6,75f','t7f',
                       't0p','t0,25p','t0,75p','t1,25p','t1,75p',
                       't2,25p','t2,75p','t3,25p','t3,75p',
                       't4,25p','t4,75p','t5,25p','t5,75p',
                       't6,25p','t6,75p','t7p',
                       't0r','t0,25r','t0,75r','t1,25r','t1,75r',
                       't2,25r','t2,75r','t3,25r','t3,75r',
                       't4,25r','t4,75r','t5,25r','t5,75r',
                       't6,25r','t6,75r','t7r',
                       't0a','t0,25a','t0,75a','t1,25a','t1,75a',
                       't2,25a','t2,75a','t3,25a','t3,75a',
                       't4,25a','t4,75a','t5,25a','t5,75a',
                       't6,25a','t6,75a','t7a')
css_4th_1_2

css_4th_1_2$type <- stringi::stri_sub(css_4th_1_2$Time,-1)
table(css_4th_1_2$type)
css_4th_1_2$type <- ifelse(css_4th_1_2$type=="f","s",
                           css_4th_1_2$type)

css_4th_1_2$day <- stringi::stri_sub(css_4th_1_2$Time,2,
                                      stringr::str_length(css_4th_1_2$Time)-1)
table(css_4th_1_2$day)

save(css_4th_1_2,file = "css_4th_1_2.rda")

css_4th_1_2o <- css_4th_1_2[css_4th_1_2$type=="o",]

pi4a <- ggline(css_4th_1_2o,'day','CSS',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar')+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi4a


pi4b <- ggline(css_4th_1_2o,'day','CSS',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "Sex")+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time (d)',y='Cumulative CSS')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=18,face = 'bold'))

pi4b
#why there is a peak for day7 compare to day6.75
#in theory, there is no difference

css_4th_1_2no <- css_4th_1_2[css_4th_1_2$type !="o",]
#3712
table(css_4th_1_2no$type)
css_4th_1_2no$type <- recode(css_4th_1_2no$type, 
                             'a' ='Activity','p' ='Posture',
                             'r'  ='Reaction','s' ='Symptomatic')

n=c(58,58,58,52,28,22,18,16,15,14,14,14,13,13,13,12)
#the number of mice to be observed
pi4c <- ggplot(css_4th_1_2no,aes(x=day,y=CSS,fill=type))+
  theme_classic()+
  geom_bar(position = 'fill',stat = 'identity')+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time(d)',y='CSS distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=16,face='bold'),
        legend.title = element_blank(),
        legend.position="top",
        legend.text=element_text(size=12,face = 'bold'))+
  annotate('text',
           x=1:length(table(css_4th_1_2no$day)),
           y=0.05,
           label=n,
           col='black',
           vjust=-1,
           size=4,
           fontface='bold')
pi4c

pi4d <- ggline(css_4th_1_2no,'day','CSS',
               linetype = 'Analgesic',size = 1.5,
               color = 'Analgesic',
               palette = c("#DE1A1A","#29BF12","#00A5CF"),
               add ='mean_se',error.plot = 'upper_errorbar',
               facet.by = "type")+
  scale_x_discrete(labels = c('0','','0.75','','1.75',
                              '','2.75','','3.75',
                              '','4.75','','5.75',
                              '','6.75',''))+
  labs(x='Time (d)',y='CSS distribution')+
  theme(axis.title.y = element_text(size=18,face='bold'),
        axis.text.y = element_text(size = 14,face = 'bold'),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text.x = element_text(size=12,face='bold'),
        legend.title = element_blank(),
        legend.text=element_text(size=14,face = 'bold'))

pi4d

############################################
tiff(filename = 'pl4_0829.tiff', 
     units="cm",#scale = 1, 
     width=45, height=38, 
     res = 600, compression= "lzw")

pl4 <- ggarrange(pi4a,pi4b,pi4c,pi4d,#ncol = 2,
                 font.label = list(size=20),
                 #widths = c(.6,1,.6,1),
                 labels = c('A','B','C','D'))

pl4
dev.off()


