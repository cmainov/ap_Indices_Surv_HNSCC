library(ggplot2)
library(ggalluvial)
library(ggpubr)

# load long formatted data 
setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/Final set of R files/Data Created')

longdat<-readRDS('data_long_list.rds')

# create function
alluv.plot<-function(ind.name,lng.data,ind.cut){
  
ahlng<-lng.data
ahlng$aheiq <- factor(ahlng$aheiq , levels = c(5,4,3,2,1))
ahlng$timeInt <- factor(ahlng$timeInt)

ahlngb<-ahlng[ahlng$timeInt!=3,]
ahlng1<-ahlng[ahlng$timeInt!=1,]


ahlng$change<-factor(ifelse(eval(parse(text=paste0('ahlng$',ind.cut)))<eval(parse(text=paste0('ahlng$',ind.cut,'_001')))& ahlng$timeInt==1,'Increase',
                            ifelse(eval(parse(text=paste0('ahlng$',ind.cut)))==eval(parse(text=paste0('ahlng$',ind.cut,'_001')))& ahlng$timeInt==1,'No Change',
                                   ifelse(eval(parse(text=paste0('ahlng$',ind.cut)))>eval(parse(text=paste0('ahlng$',ind.cut,'_001')))& ahlng$timeInt==1,'Decrease',
                                          ifelse(eval(parse(text=paste0('ahlng$',ind.cut,'_001')))<eval(parse(text=paste0('ahlng$',ind.cut,'_002'))) & ahlng$timeInt==2,'Increase',
                                                 ifelse(eval(parse(text=paste0('ahlng$',ind.cut,'_001')))==eval(parse(text=paste0('ahlng$',ind.cut,'_002')))& ahlng$timeInt==2,'No Change',
                                                        ifelse(eval(parse(text=paste0('ahlng$',ind.cut,'_001')))>eval(parse(text=paste0('ahlng$',ind.cut,'_002')))& ahlng$timeInt==2,'Decrease',NA)))))))




ahlng$timeInt<-factor(ahlng$timeInt,levels = levels(ahlng$timeInt),
                      labels=c('PT','Year 1','Year 2'))

ara1<-ggplot(ahlng,
             aes(x = timeInt, stratum = aheiq, alluvium = idnum,
                 label = aheiq)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            mapping=aes(color = change),width=0.1) +
  scale_color_manual(values=c('Decrease'='darkorange','Increase'='black','No Change'='grey50'))+
  geom_stratum(width = 0.1) + 
  theme(legend.position = "backfront") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),family='Helvetica Light')+
  ylim(0, 468)+theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=14,color='grey50'),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(family='Helvetica Bold',size=12),

        text=element_text(family='Helvetica Light'),
        legend.position = 'none')+ggtitle(label=ind.name)+
  coord_cartesian(expand=F)





return(ara1)
}

# single example 
alluv.plot(ind.name = 'AHEI-2010',lng.data = longdat[[1]],ind.cut='ahei_index_q')

# loop and index on each diet quality index
indices.names<-c('AHEI-2010','aMED','DASH','Low Carbohydrate','Animal-Based Low Carbohydrate','Plant-Based Low Carbohydrate')
cut.names_b<-c('ahei_index_q','amed_index_q','dash_index_q','keto_score_q','animal_keto_score_q','veg_keto_score_q')


# intialize list to store plots
a.plots<-list()

for(i in 1:length(indices.names)){
  ag<-alluv.plot(ind.name = indices.names[i],lng.data = longdat[[i]],
                      ind.cut =cut.names_b[i])
  a.plots[[i]]<-ag
}

# arrange plots
do.call('ggarrange',list(a.plots[[1]],a.plots[[4]],a.plots[[5]],a.plots[[6]]))

ara2<-ggarrange(a.plots[[1]]+theme(axis.text.x = element_blank())+
            theme(plot.margin = margin(t = 2,  # Top margin
                                               r = 10,  # Right margin
                                               b = 10,  # Bottom margin
                                               l = 1)),
          a.plots[[2]]+theme(axis.text.x = element_blank())+
            theme(plot.margin = margin(t = 2,  # Top margin
                                       r = 10,  # Right margin
                                       b = 10,  # Bottom margin
                                       l = 1)),
          a.plots[[4]]+
            theme(plot.margin = margin(t = 1,  # Top margin
                                       r = 10,  # Right margin
                                       b = 2,  # Bottom margin
                                       l = 1)),
          a.plots[[6]]+
            theme(plot.margin = margin(t = 1,  # Top margin
                                       r = 10,  # Right margin
                                       b = 2,  # Bottom margin
                                       l =1)),ncol=2,nrow=2)


annotate_figure(ara2,left=text_grob("Index     \nQuintile",family = "Helvetica Light",
               hjust=0.4,rot=0,vjust=0,face='bold'),
               bottom =text_grob("Visit",family = "Helvetica Light",
                                 face='bold',vjust=0.3),fig.lab.face = 'bold')


ara2+annotate("text", x = 1, y = 1, label = "Group 1")


