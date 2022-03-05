setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/Final set of R files/Data Created')

nspore<-readRDS('diet_scores_data_820_imputed.rds')
cont.names_b<-c('AHEI_INDEX','aMED_INDEX','DASH_INDEX','keto_score','animal_keto_score','veg_keto_score')
index.cut.names<-c('ahei_index_q','amed_index_q','dash_index_q','keto_score_q','animal_keto_score_q','veg_keto_score_q')
trend.names<-paste0(index.cut.names,'_trend')

# now redo trend variable
trend.func<-function(rank.var,cont.var,df,trend.var,id.var,x){
  medians<-vector()
  for (i in 1:x){
    newdf<-df[df[[rank.var]]==i,]
    medians[i]<-median(newdf[[cont.var]],na.rm=T)
  }
  
  newdf1<-list()
  for (i in 1:length(medians)){
    newdf1[[i]]<-within(df,assign(trend.var,ifelse(df[[rank.var]]==i,medians[i],NA)))
  }
  
  newssf <- na.omit(do.call("rbind", newdf1)[,c(id.var,trend.var)])
  newssf<-left_join(df,newssf,by=id.var)
  return(newssf)
  
}

for (i in 1:length(index.cut.names)){
  nspore<-trend.func(rank.var=index.cut.names[i],
                    cont.var=cont.names_b[i],df=nspore, trend.var=trend.names[i],id.var='idnum',x=5)
}


# fit models and generate table

base.tab<-list()
indices.names<-c('AHEI-2010','aMED','DASH','Low Carbohydrate','Animal-Based Low Carbohydrate','Plant-Based Low Carbohydrate')

# 3 year censor indicator and time
nspore$stime2<-ifelse(nspore$Stime>36,36,nspore$Stime)
nspore$DeathStat2<-ifelse(nspore$Stime>36 & nspore$DeathStat==1,0,nspore$DeathStat)

# recode censor for cause specific mortality
nspore<-nspore%>%
  mutate(cadeathstat=if_else(Cause_of_Death==4,1,0,missing=0))


# Fit all models and generate table of results with single loop

# All-cause mortality
for (i in 1: length(cont.names_b)){
  
main.mod<-coxph(Surv(stime2, DeathStat2) ~ eval(parse(text=paste0('factor(',index.cut.names[i],')')))+CALOR+bmi+factor(tumsite)+
                    factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                    factor(SEX)+factor(smoker)+Age_at_Diagnosis+factor(ace_overall_score)+factor(modality_cat), data = nspore)

trend.mod<-coxph(Surv(stime2, DeathStat2) ~ eval(parse(text=trend.names[i]))+CALOR+bmi+factor(tumsite)+
                   factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                   factor(SEX)+factor(smoker)+Age_at_Diagnosis+factor(ace_overall_score)+factor(modality_cat), data = nspore)

mod.tab<-data.frame(Index=indices.names[i],Q1='1.00',Q2=paste0(round(exp(main.mod$coefficients)[1],digits=2),' (',
                                        paste0(round(exp(confint(main.mod))[1,],digits=2),collapse='-'),')'),
                    Q3=paste0(round(exp(main.mod$coefficients)[2],digits=2),' (',
                              paste0(round(exp(confint(main.mod))[2,],digits=2),collapse='-'),')'),
                    Q4=paste0(round(exp(main.mod$coefficients)[3],digits=2),' (',
                              paste0(round(exp(confint(main.mod))[3,],digits=2),collapse='-'),')'),
                    Q5=paste0(round(exp(main.mod$coefficients)[4],digits=2),' (',
                              paste0(round(exp(confint(main.mod))[4,],digits=2),collapse='-'),')'),
                    ptrend=paste0(round(summary(trend.mod)$coefficients[1,5],digits=2)),
                    pq5=paste0(round(summary(main.mod)$coefficients[4,5],digits=2)))





base.tab[[i]]<-mod.tab%>%
  mutate(Q2=ifelse(summary(main.mod)$coefficients[1,5]<0.05 & summary(main.mod)$coefficients[1,5]>=0.01,str_replace(Q2,'$','*'),
                   ifelse(summary(main.mod)$coefficients[1,5]<0.01,str_replace(Q2,'$','**'),Q2)))%>%
  mutate(Q3=ifelse(summary(main.mod)$coefficients[2,5]<0.05 & summary(main.mod)$coefficients[2,5]>=0.01,str_replace(Q3,'$','*'),
                   ifelse(summary(main.mod)$coefficients[2,5]<0.01,str_replace(Q3,'$','**'),Q3)))%>%
  mutate(Q4=ifelse(summary(main.mod)$coefficients[3,5]<0.05 & summary(main.mod)$coefficients[3,5]>=0.01,str_replace(Q4,'$','*'),
                   ifelse(summary(main.mod)$coefficients[3,5]<0.01,str_replace(Q4,'$','**'),Q4)))%>%
  mutate(Q5=ifelse(summary(main.mod)$coefficients[4,5]<0.05 & summary(main.mod)$coefficients[4,5]>=0.01,str_replace(Q5,'$','*'),
                   ifelse(summary(main.mod)$coefficients[4,5]<0.01,str_replace(Q5,'$','**'),Q5)))%>%
  mutate(pq5=ifelse(summary(main.mod)$coefficients[4,5]>=0.05,pq5,
                    ifelse(summary(main.mod)$coefficients[4,5]<0.05 & summary(main.mod)$coefficients[4,5]>=0.01,str_replace(pq5,'$','*'),
                           ifelse(summary(main.mod)$coefficients[4,5]<0.01,str_replace(pq5,'$','**'),`pq5`))))%>%
  mutate(ptrend=(ifelse(summary(trend.mod)$coefficients[1,5]>=0.05,ptrend,
                        ifelse(summary(trend.mod)$coefficients[1,5]<0.05 & summary(trend.mod)$coefficients[1,5]>=0.01,str_replace(ptrend,'$','*'),
                               ifelse(summary(trend.mod)$coefficients[1,5]<0.01,str_replace(ptrend,'$','**'),ptrend)))))%>%
  mutate(ptrend=str_replace(ptrend,'0(?=\\*)','< 0.01'))%>%
  mutate(pq5=str_replace(pq5,'0(?=\\*)','< 0.01'))
  
}

# combine results into single table
base.table<-do.call('rbind',base.tab)

# text processing for significant digits
for(i in c(3:6)){
  base.table[,i]<-str_replace(base.table[,i],'(?<=\\.\\d)(\\))','0)')
  base.table[,i]<-str_replace(base.table[,i],'(?<=\\.\\d)(\\s)','0 ')
  base.table[,i]<-str_replace(base.table[,i],'(?<=\\.\\d)(\\-)','0-')
  base.table[,i]<-str_replace(base.table[,i],'(?<=\\-\\d)(\\))','.00)')
  base.table[,i]<-str_replace(base.table[,i],'(?<=^\\d)(\\s\\()','.00 (')
  
}

for(i in c(7:8)){
  base.table[,i]<-str_replace(base.table[,i],'(?<=\\.\\d)($)','0')
}


# Cancer-specific mortality
for (i in 1: length(cont.names_b)){
  
  main.mod<-coxph(Surv(stime2, cadeathstat) ~ eval(parse(text=paste0('factor(',index.cut.names[i],')')))+CALOR+bmi+factor(tumsite)+
                    factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                    factor(SEX)+factor(smoker)+Age_at_Diagnosis+factor(ace_overall_score)+factor(modality_cat), data = nspore)
  
  trend.mod<-coxph(Surv(stime2, cadeathstat) ~ eval(parse(text=trend.names[i]))+CALOR+bmi+factor(tumsite)+
                     factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                     factor(SEX)+factor(smoker)+Age_at_Diagnosis+factor(ace_overall_score)+factor(modality_cat), data = nspore)
  
  mod.tab<-data.frame(Index=indices.names[i],Q1='1.00',Q2=paste0(round(exp(main.mod$coefficients)[1],digits=2),' (',
                                                                 paste0(round(exp(confint(main.mod))[1,],digits=2),collapse='-'),')'),
                      Q3=paste0(round(exp(main.mod$coefficients)[2],digits=2),' (',
                                paste0(round(exp(confint(main.mod))[2,],digits=2),collapse='-'),')'),
                      Q4=paste0(round(exp(main.mod$coefficients)[3],digits=2),' (',
                                paste0(round(exp(confint(main.mod))[3,],digits=2),collapse='-'),')'),
                      Q5=paste0(round(exp(main.mod$coefficients)[4],digits=2),' (',
                                paste0(round(exp(confint(main.mod))[4,],digits=2),collapse='-'),')'),
                      ptrend=paste0(round(summary(trend.mod)$coefficients[1,5],digits=2)),
                      pq5=paste0(round(summary(main.mod)$coefficients[4,5],digits=2)))
  
  
  
  
  
  base.tab[[i]]<-mod.tab%>%
    mutate(Q2=ifelse(summary(main.mod)$coefficients[1,5]<0.05 & summary(main.mod)$coefficients[1,5]>=0.01,str_replace(Q2,'$','*'),
                     ifelse(summary(main.mod)$coefficients[1,5]<0.01,str_replace(Q2,'$','**'),Q2)))%>%
    mutate(Q3=ifelse(summary(main.mod)$coefficients[2,5]<0.05 & summary(main.mod)$coefficients[2,5]>=0.01,str_replace(Q3,'$','*'),
                     ifelse(summary(main.mod)$coefficients[2,5]<0.01,str_replace(Q3,'$','**'),Q3)))%>%
    mutate(Q4=ifelse(summary(main.mod)$coefficients[3,5]<0.05 & summary(main.mod)$coefficients[3,5]>=0.01,str_replace(Q4,'$','*'),
                     ifelse(summary(main.mod)$coefficients[3,5]<0.01,str_replace(Q4,'$','**'),Q4)))%>%
    mutate(Q5=ifelse(summary(main.mod)$coefficients[4,5]<0.05 & summary(main.mod)$coefficients[4,5]>=0.01,str_replace(Q5,'$','*'),
                     ifelse(summary(main.mod)$coefficients[4,5]<0.01,str_replace(Q5,'$','**'),Q5)))%>%
    mutate(pq5=ifelse(summary(main.mod)$coefficients[4,5]>=0.05,pq5,
                      ifelse(summary(main.mod)$coefficients[4,5]<0.05 & summary(main.mod)$coefficients[4,5]>=0.01,str_replace(pq5,'$','*'),
                             ifelse(summary(main.mod)$coefficients[4,5]<0.01,str_replace(pq5,'$','**'),`pq5`))))%>%
    mutate(ptrend=(ifelse(summary(trend.mod)$coefficients[1,5]>=0.05,ptrend,
                          ifelse(summary(trend.mod)$coefficients[1,5]<0.05 & summary(trend.mod)$coefficients[1,5]>=0.01,str_replace(ptrend,'$','*'),
                                 ifelse(summary(trend.mod)$coefficients[1,5]<0.01,str_replace(ptrend,'$','**'),ptrend)))))%>%
    mutate(ptrend=str_replace(ptrend,'0(?=\\*)','< 0.01'))%>%
    mutate(pq5=str_replace(pq5,'0(?=\\*)','< 0.01'))
  
}

# combine results into single table
base.table.ca<-do.call('rbind',base.tab)

# text processing for significant digits
for(i in c(3:6)){
  base.table.ca[,i]<-str_replace(base.table.ca[,i],'(?<=\\.\\d)(\\))','0)')
  base.table.ca[,i]<-str_replace(base.table.ca[,i],'(?<=\\.\\d)(\\s)','0 ')
  base.table.ca[,i]<-str_replace(base.table.ca[,i],'(?<=\\.\\d)(\\-)','0-')
  base.table.ca[,i]<-str_replace(base.table.ca[,i],'(?<=\\-\\d)(\\))','.00)')
  base.table.ca[,i]<-str_replace(base.table.ca[,i],'(?<=^\\d)(\\s\\()','.00 (')
  
}

for(i in c(7:8)){
  base.table.ca[,i]<-str_replace(base.table.ca[,i],'(?<=\\.\\d)($)','0')
}


#save table
setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/Final set of R files/Tables')
write.csv(rbind(base.table,base.table.ca)
,'baselineindex_results_table_527.csv')
write.table(rbind(base.table,base.table.ca)
,"baselineindex_results_table_527.txt",sep=",",row.names=FALSE)

