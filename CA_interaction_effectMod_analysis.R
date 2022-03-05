library(readxl)
library(tidyverse)
library(rms)
library(nnet)
library(survey)
library(discsurv)
library(forestmangr)

setwd('/Volumes/My Passport for Mac/Arthur Lab/Dietary Inflammatory Index/Analyses/Final set of R files/Data Created')

nspore <-readRDS('diet_scores_data_820_imputed.rds')

nspore$ace_overall_score<-as.factor(nspore$ace_overall_score)
nspore$modality_cat<-as.factor(nspore$modality_cat)


# redo death/censor variable
nspore<-nspore%>%
  mutate(cadeathstat=if_else(Cause_of_Death==4,1,0,missing=0))

msm.func<-function(diet.cut.var,diet.cut.var_001,diet.cut.var_002,
                   diet.cont.var,diet.cont.var_001,diet.cont.var_002,
                   full.index.name,df,inter.var){
  
  diet.cont.var.x<-paste0(diet.cont.var,'.x')
  diet.cont.var_001.x<-paste0(diet.cont.var_001,'.x')
  diet.cont.var_002.x<-paste0(diet.cont.var_002,'.x')
  
  # put data into long format required for discrete time survival analysis
  
  require(discSurv)
  require(forestmangr)
  spore_long <- dataLong(dataSet = df, 
                         timeColumn = "distime", 
                         censColumn = "cadeathstat",
                         timeAsFactor = F)
  
  
  
  
  # create time varying variable for indices
  library(tidyverse)
  spore_long_diet<- spore_long %>%
    group_by(idnum) %>%
    mutate(bmindex= ifelse(timeInt <= s1_mon,bmi,
                           ifelse(timeInt > s1_mon & timeInt <= s2_mon,BMI_001,
                                  ifelse(timeInt >s2_mon & timeInt <=s3_mon, BMI_002,21)))) %>%
    mutate(calories= ifelse(timeInt <= s1_mon,CALOR,
                            ifelse(timeInt > s1_mon & timeInt <= s2_mon,CALOR_001,
                                   ifelse(timeInt >s2_mon & timeInt <=s3_mon, CALOR_002,21))))
  
  
  spore_long_diet$aheiq<- ifelse(spore_long_diet$timeInt <= spore_long_diet$s1_mon,spore_long_diet[[diet.cut.var]],
                                 ifelse(spore_long_diet$timeInt > spore_long_diet$s1_mon & spore_long_diet$timeInt <= spore_long_diet$s2_mon,spore_long_diet[[diet.cut.var_001]],
                                        ifelse(spore_long_diet$timeInt >spore_long_diet$s2_mon & spore_long_diet$timeInt <=spore_long_diet$s3_mon, spore_long_diet[[diet.cut.var_002]],NA)))
  
  # continuous index score
  spore_long_diet$ahei<- ifelse(spore_long_diet$timeInt <= spore_long_diet$s1_mon,spore_long_diet[[diet.cont.var]],
                                ifelse(spore_long_diet$timeInt > spore_long_diet$s1_mon & spore_long_diet$timeInt <= spore_long_diet$s2_mon,spore_long_diet[[diet.cont.var_001]],
                                       ifelse(spore_long_diet$timeInt >spore_long_diet$s2_mon & spore_long_diet$timeInt <=spore_long_diet$s3_mon, spore_long_diet[[diet.cont.var_002]],NA)))
  
  # scaled continuous index score
  spore_long_diet$ahei.x<- ifelse(spore_long_diet$timeInt <= spore_long_diet$s1_mon,spore_long_diet[[diet.cont.var.x]],
                                  ifelse(spore_long_diet$timeInt > spore_long_diet$s1_mon & spore_long_diet$timeInt <= spore_long_diet$s2_mon,spore_long_diet[[diet.cont.var_001.x]],
                                         ifelse(spore_long_diet$timeInt >spore_long_diet$s2_mon & spore_long_diet$timeInt <=spore_long_diet$s3_mon, spore_long_diet[[diet.cont.var_002.x]],NA)))
  
  # truncate time at 36 mos
  
  short_time<-spore_long_diet[!spore_long_diet$timeInt > 3,]
  
  length(unique(short_time$idnum))
  length(short_time$idnum)
  
  
  
  
  # create weights
  
  library(nnet)
  short_time$lastahei<-ifelse(short_time$timeInt <= short_time$s1_mon,short_time[[diet.cut.var]],
                              ifelse(short_time$timeInt > short_time$s1_mon & short_time$timeInt <= short_time$s2_mon,short_time[[diet.cut.var]],
                                     ifelse(short_time$timeInt > short_time$s2_mon & short_time$timeInt <= short_time$s3_mon, short_time[[diet.cut.var_001]],21)))
  
  short_time$lastahei.x<-ifelse(short_time$timeInt <= short_time$s1_mon,short_time[[diet.cont.var.x]],
                                ifelse(short_time$timeInt > short_time$s1_mon & short_time$timeInt <= short_time$s2_mon,short_time[[diet.cont.var.x]],
                                       ifelse(short_time$timeInt > short_time$s2_mon & short_time$timeInt <= short_time$s3_mon, short_time[[diet.cont.var_001.x]],21)))
  
  
  model1<- multinom(aheiq~factor(lastahei)+factor(SEX)+factor(hpv)+factor(tumsite)+factor(educ_3cat)+
                      factor(smoker)+factor(stagebinary)+Age_at_Diagnosis+modality_cat+
                      bmi+CALOR+ace_overall_score,
                    data=short_time)
  txnum<-as.data.frame(predict(model1,newdata=short_time,type='prob'))
  model2<- multinom(aheiq~factor(lastahei)+factor(SEX)+factor(hpv)+factor(tumsite)+rcs(calories,3)+rcs(bmindex,3)+factor(educ_3cat)+
                      factor(smoker)+factor(stagebinary)+Age_at_Diagnosis+modality_cat+ace_overall_score,
                    data=short_time,Hess = TRUE)
  txden<-as.data.frame(predict(model2,newdata=short_time,type='prob'))
  colnames(txnum)<- c('txnum0','txnum1','txnum2','txnum3','txnum4')
  colnames(txden)<- c('txden0','txden1','txden2','txden3','txden4')
  
  txden$id=as.integer(rownames(txden))
  txnum$id=as.integer(rownames(txnum))
  short_time$id=as.integer(rownames(short_time))
  
  short_time2<-inner_join(txden,short_time,by='id')
  
  
  
  # prob of observed value
  short_time2$txdens= ifelse(short_time2$aheiq==1,short_time2$txden0,
                             ifelse(short_time2$aheiq==2,short_time2$txden1,
                                    ifelse(short_time2$aheiq==3,short_time2$txden2,
                                           ifelse(short_time2$aheiq==4,short_time2$txden3,
                                                  ifelse(short_time2$aheiq==5,short_time2$txden4,27)))))
  
  short_time3<-left_join(txnum,short_time2,by='id')
  short_time3$txnums= ifelse(short_time3$aheiq==1,short_time3$txnum0,
                             ifelse(short_time3$aheiq==2,short_time3$txnum1,
                                    ifelse(short_time3$aheiq==3,short_time3$txnum2,
                                           ifelse(short_time3$aheiq==4,short_time3$txnum3,
                                                  ifelse(short_time3$aheiq==5,short_time3$txnum4,27)))))
  
  # stabilized iptw
  short_time3$stabiptw=short_time3$txnums/short_time3$txdens
  sum(is.na(short_time3$txdens))
  
  
  short_time3<-short_time3%>%
    group_by(idnum)%>%
    mutate(stabiptw_b=ifelse(timeInt==1,stabiptw,NA))%>%
    mutate(stabiptw_1=ifelse(timeInt==2,stabiptw,NA))%>%
    mutate(stabiptw_2=ifelse(timeInt==3,stabiptw,NA))
  
  # subset to make a wider dataframe with just weights
  subsgh<-short_time3[,c('idnum','stabiptw_b','stabiptw_1','stabiptw_2')]
  
  
  # manipulate that data frame to get all weight values in a single row for each participant 
  suppressWarnings((subwide<-data.frame(pivot_wider(subsgh,id_cols=idnum,values_from = c(stabiptw_b,stabiptw_1,stabiptw_2)))))
  colnames(subwide)<-str_remove_all(colnames(subwide),'(?<=\\d)(\\_)')
  colnames(subwide)<-str_remove_all(colnames(subwide),'(?<=b)(\\_)')
  for (i in 2:ncol(subwide)){
    subwide[,i]<-as.character(subwide[,i])
  }
  
  
  suppressWarnings(for (i in 2:ncol(subwide)){
    subwide[,i]<-str_remove_all(subwide[,i],'c\\(|\\( |\\)|\\,|NA') 
    subwide[,i]<-as.numeric(subwide[,i])
    
  }
  )
  # remove the original columns
  short_time3<-short_time3[,-which(colnames(short_time3) %in% c('stabiptw_b','stabiptw_1','stabiptw_2'))]
  
  # now merge so that each individual has all weights for each time point at any row
  short_time3<-left_join(short_time3,subwide,by='idnum')
  
  # now compute final stabilized IPTW
  short_time3<-short_time3%>%
    mutate(stabiptw=ifelse(timeInt==1,stabiptw_b,
                           ifelse(timeInt==2,stabiptw_1*stabiptw_b,
                                  ifelse(timeInt==3,stabiptw_2*stabiptw_b*stabiptw_1,NA))))
  
  
  miss<- short_time3[is.na(short_time3$stabiptw) ==TRUE,]
  length(unique(miss$idnum))
  
  # censor weights
  
  # censor variable and lagged censor variable
  short_time3<-short_time3%>%
    group_by('idnum')%>%
    mutate(c1=ifelse(short_time3$cadeathstat==1,0,
                     ifelse(short_time3$cadeathstat==0 & short_time3$timeInt==short_time3$s1_mon |short_time3$timeInt==max(timeInt),1,0)))%>%
    mutate(c2=ifelse(short_time3$cadeathstat==1,0,
                     ifelse(short_time3$cadeathstat==0 & short_time3$timeInt==short_time3$s2_mon |short_time3$timeInt==max(timeInt),1,0)))%>%
    mutate(c3=ifelse(short_time3$cadeathstat==1,0,
                     ifelse(short_time3$cadeathstat==0 & short_time3$timeInt==short_time3$s3_mon |short_time3$timeInt==max(timeInt),1,0)))%>%
    mutate(censorvar=ifelse(short_time3$timeInt <= short_time3$s1_mon,c1,
                            ifelse(short_time3$timeInt> short_time3$s1_mon & short_time3$timeInt<= short_time3$s2_mon,c2,
                                   ifelse(short_time3$timeInt> short_time3$s2_mon & short_time3$timeInt<= short_time3$s3_mon,c3,97))))
  
  
  # lag censor variable
  short_time3<-short_time3%>%
    group_by(idnum)%>%
    mutate(lastcens=ifelse(timeInt !=1,lag(censorvar,k=1),0))
  
  
  
  # IPTC models
  
  model3<- glm(censorvar~factor(lastahei)+factor(lastcens)+factor(SEX)+factor(hpv)+factor(tumsite)+CALOR+bmi+factor(educ_3cat)+
                 factor(smoker)+factor(stagebinary)+Age_at_Diagnosis+modality_cat+ace_overall_score,
               data=short_time3,family=binomial(link='logit'))
  csnum<-as.data.frame(1-predict(model3,newdata=short_time3,type='response'))
  model4<- glm(censorvar~factor(lastahei)+factor(lastcens)+factor(SEX)+factor(hpv)+factor(tumsite)+rcs(calories,3)+rcs(bmindex,3)+factor(educ_3cat)+
                 factor(smoker)+factor(stagebinary)+Age_at_Diagnosis+modality_cat+ace_overall_score,
               data=short_time3,family=binomial(link='logit'))
  csden<-as.data.frame(1-predict(model4,newdata=short_time3,type='response'))
  colnames(csnum)<- c('csnum')
  colnames(csden)<- c('csden')
  csden$id=as.integer(rownames(csden))
  csnum$id=as.integer(rownames(csnum))
  
  short_time4<-inner_join(csden,csnum)
  short_time4$stabipcw<- short_time4$csnum/short_time4$csden
  short_time4<-inner_join(short_time3,short_time4)
  
  #final censoring weight
  short_time4$stabipcw<- short_time4$csnum/short_time4$csden
  
  
  short_time4<-short_time4%>%
    group_by(idnum)%>%
    mutate(stabipcw_b=ifelse(timeInt==1,stabipcw,NA))%>%
    mutate(stabipcw_1=ifelse(timeInt==2,stabipcw,NA))%>%
    mutate(stabipcw_2=ifelse(timeInt==3,stabipcw,NA))
  # subset to make a wider dataframe with just weights
  subslk<-short_time4[,c('idnum','stabipcw_b','stabipcw_1','stabipcw_2')]
  
  
  # manipulate that data frame to get all weight values in a single row for each participant 
  suppressWarnings(subwideb<-data.frame(pivot_wider(subslk,id_cols=idnum,values_from = c(stabipcw_b,stabipcw_1,stabipcw_2))))
  colnames(subwideb)<-str_remove_all(colnames(subwideb),'(?<=\\d)(\\_)')
  colnames(subwideb)<-str_remove_all(colnames(subwideb),'(?<=b)(\\_)')
  for (i in 2:ncol(subwideb)){
    subwide[,i]<-as.character(subwideb[,i])
  }
  
  
  suppressWarnings(for (i in 2:ncol(subwideb)){
    subwideb[,i]<-str_remove_all(subwideb[,i],'c\\(|\\( |\\)|\\,|NA') 
    subwideb[,i]<-as.numeric(subwideb[,i])
    
  })
  
  # remove the original columns
  short_time4<-short_time4[,-which(colnames(short_time4) %in% c('stabipcw_b','stabipcw_1','stabipcw_2'))]
  
  # now merge so that each individual has all weights for each time point at any row
  short_time4<-left_join(short_time4,subwideb,by='idnum')
  
  # now compute final stabilized ipcw
  short_time4<-short_time4%>%
    mutate(stabipcw=ifelse(timeInt==1,stabipcw_b,
                           ifelse(timeInt==2,stabipcw_1*stabipcw_b,
                                  ifelse(timeInt==3,stabipcw_2*stabipcw_b*stabipcw_1,NA))))
  
  
  
  # final stabilized weight for model(swit)
  short_time4$swit<-short_time4$stabipcw*short_time4$stabiptw
  sum(is.na(short_time4$swit))
  premodeldata<-short_time4[!is.na(short_time4$swit),]
  
  length(unique(short_time4$idnum))
  
  # truncating subjects with weight>99th percentile
  premodeldatatrunc<-unique(premodeldata[premodeldata$swit> quantile(premodeldata$swit,0.99),c('idnum','swit')])
  nrow(premodeldatatrunc)
  
  premodeldata<-premodeldata%>%mutate(swit=ifelse(swit>quantile(premodeldata$swit,0.98),quantile(premodeldata$swit,0.98),swit))
  
  
  # Fit models
  
  library(survey)
  library(rms)
  
  weighted.designahei<-svydesign(id=premodeldata$idnum, data=premodeldata,
                                 weight=premodeldata$swit)
  # check for confounding
  chisq.test(xtabs(premodeldata$swit ~ premodeldata$stagebinary + premodeldata$aheiq))
  xtabs(premodeldata$swit ~ premodeldata$stagebinary + premodeldata$aheiq)
  
  
  # now do it with Cox PH model accounting for clusters to show that the 
  # two methods produce nearly equivalent results
  premodeldata$start<-premodeldata$timeInt-1
  premodeldata$end<-premodeldata$timeInt
  premodeldata$y
  weighted.designahei<-svydesign(id=premodeldata$idnum, data=premodeldata,
                                 weight=premodeldata$swit)
  
  aheimodel1<-eval(parse(text="svycoxph(Surv(start, end, y) ~ ahei.x +lastahei.x+CALOR+bmi+factor(tumsite)+
                                factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                                factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                                modality_cat,
                               design=weighted.designahei,data=premodeldata)"))
  
  
  aheimodel2<-eval(parse(text=paste0('svycoxph(Surv(start, end, y) ~ ahei.x +lastahei.x+CALOR+bmi+factor(tumsite)+
                                factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                                factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                                modality_cat',paste0('+factor(',inter.var,")*ahei.x"),
                      ',design=weighted.designahei,data=premodeldata)')))
  
  # likelihood ratio test
pval<-round(pchisq(-2*aheimodel1$ll[2]-(-2*aheimodel2$ll[2]),df=aheimodel1$degf.resid-aheimodel2$degf.resid,lower.tail = F),digits=2) #model c vs b

lrt.tab<-data.frame(index=full.index.name,strat.variable=inter.var,p=pval) 

lrt.tab<-lrt.tab%>%
  mutate(p=(ifelse(p>=0.05,p,
                      ifelse(p<0.05 & p>=0.01,str_replace(p,'$','*'),
                             ifelse(p<0.01,'<0.01**',p)))))

modelobj<-aheimodel2

  
 
  return(list(tab=lrt.tab,mod.obj=aheimodel2))
  
  
  
}

strat.vars<-c('stagebinary','hpv','tumsite')
pvalahei<-list()
for(i in 1:length(strat.vars)){
  pvalahei[[i]]<-msm.func(diet.cut.var=cut.names_b[1],diet.cut.var_001=cut.names_001[1],
                          diet.cut.var_002=cut.names_002[1],
                          diet.cont.var=cont.names_b[1],diet.cont.var_001=cont.names_001[1],
                          diet.cont.var_002=cont.names_002[1],df=nspore,
                          full.index.name=indices.names[1],inter.var=strat.vars[i])$tab
}

do.call('rbind',pvalahei)

strat.vars<-c('stagebinary','hpv','tumsite')
pvalahei<-list()
pvalamed<-list()
pvaldash<-list()
pvalketo<-list()
pvalaketo<-list()
pvalvketo<-list()
for(j in 1:length(strat.vars)){
  pvalahei[[j]]<-msm.func(diet.cut.var=cut.names_b[1],diet.cut.var_001=cut.names_001[1],
           diet.cut.var_002=cut.names_002[1],
           diet.cont.var=cont.names_b[1],diet.cont.var_001=cont.names_001[1],
           diet.cont.var_002=cont.names_002[1],df=nspore,
           full.index.name=indices.names[1],inter.var=strat.vars[j])$tab
  
  pvalamed[[j]]<-msm.func(diet.cut.var=cut.names_b[2],diet.cut.var_001=cut.names_001[2],
                          diet.cut.var_002=cut.names_002[2],
                          diet.cont.var=cont.names_b[2],diet.cont.var_001=cont.names_001[2],
                          diet.cont.var_002=cont.names_002[2],df=nspore,
                          full.index.name=indices.names[2],inter.var=strat.vars[j])$tab
  
  
  
  pvaldash[[j]]<-msm.func(diet.cut.var=cut.names_b[3],diet.cut.var_001=cut.names_001[3],
                        diet.cut.var_002=cut.names_002[3],
                        diet.cont.var=cont.names_b[3],diet.cont.var_001=cont.names_001[3],
                        diet.cont.var_002=cont.names_002[3],df=nspore,
                        full.index.name=indices.names[3],inter.var=strat.vars[j])$tab
  
  pvalketo[[j]]<-msm.func(diet.cut.var=cut.names_b[4],diet.cut.var_001=cut.names_001[4],
           diet.cut.var_002=cut.names_002[4],
           diet.cont.var=cont.names_b[4],diet.cont.var_001=cont.names_001[4],
           diet.cont.var_002=cont.names_002[4],df=nspore,
           full.index.name=indices.names[4],inter.var=strat.vars[j])$tab
  
  pvalaketo[[j]]<-msm.func(diet.cut.var=cut.names_b[5],diet.cut.var_001=cut.names_001[5],
           diet.cut.var_002=cut.names_002[5],
           diet.cont.var=cont.names_b[5],diet.cont.var_001=cont.names_001[5],
           diet.cont.var_002=cont.names_002[5],df=nspore,
           full.index.name=indices.names[5],inter.var=strat.vars[j])$tab
  
  pvalvketo[[j]]<-msm.func(diet.cut.var=cut.names_b[6],diet.cut.var_001=cut.names_001[6],
           diet.cut.var_002=cut.names_002[6],
           diet.cont.var=cont.names_b[6],diet.cont.var_001=cont.names_001[6],
           diet.cont.var_002=cont.names_002[6],df=nspore,
           full.index.name=indices.names[6],inter.var=strat.vars[j])$tab
  }

tot.strat<-rbind(do.call('rbind',pvalahei),
              do.call('rbind',pvalamed),
              do.call('rbind',pvaldash),
              do.call('rbind',pvalketo),
              do.call('rbind',pvalaketo),
              do.call('rbind',pvalvketo))




setwd('/Volumes/My Passport for Mac/Arthur Lab/Dietary Inflammatory Index/Analyses/Final set of R files/Tables')
write.csv(tot.strat,'lrt_strat_824_CA.csv')
