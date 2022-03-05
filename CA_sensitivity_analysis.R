library(readxl)
library(tidyverse)
library(rms)
library(nnet)
library(survey)
library(discsurv)
library(forestmangr)

setwd('/Volumes/My Passport for Mac/Arthur Lab/Dietary Inflammatory Index/Analyses/Final set of R files/Data Created')

nspore <-data.frame(readRDS('diet_scores_data_820_notimputed.rds')) # import non-imputed data

# change key variables to factor type
nspore$ace_overall_score<-as.factor(nspore$ace_overall_score)
nspore$modality_cat<-as.factor(nspore$modality_cat)

# redo death/censor variable
nspore<-nspore%>%
  mutate(cadeathstat=if_else(Cause_of_Death==4,1,0,missing=0))

# create function to generate all results for all diet indices used
msm.func<-function(diet.cut.var,diet.cut.var_001,diet.cut.var_002,
                   diet.cont.var,diet.cont.var_001,diet.cont.var_002,
                   full.index.name,df){
  
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
  require(tidyverse)
  spore_long_diet<- spore_long %>%
    group_by(idnum) %>%
    mutate(bmindex= ifelse(timeInt <= s1_mon,bmi,
                           ifelse(timeInt > s1_mon & timeInt <= s2_mon,BMI_001,
                                  ifelse(timeInt >s2_mon & timeInt <=s3_mon, BMI_002,21)))) %>%
    mutate(calories= ifelse(timeInt <= s1_mon,CALOR,
                            ifelse(timeInt > s1_mon & timeInt <= s2_mon,CALOR_001,
                                   ifelse(timeInt >s2_mon & timeInt <=s3_mon, CALOR_002,21))))
  
  
  spore_long_diet$indexq<- ifelse(spore_long_diet$timeInt <= spore_long_diet$s1_mon,spore_long_diet[[diet.cut.var]],
                                  ifelse(spore_long_diet$timeInt > spore_long_diet$s1_mon & spore_long_diet$timeInt <= spore_long_diet$s2_mon,spore_long_diet[[diet.cut.var_001]],
                                         ifelse(spore_long_diet$timeInt >spore_long_diet$s2_mon & spore_long_diet$timeInt <=spore_long_diet$s3_mon, spore_long_diet[[diet.cut.var_002]],NA)))
  
  # continuous index score
  spore_long_diet$index<- ifelse(spore_long_diet$timeInt <= spore_long_diet$s1_mon,spore_long_diet[[diet.cont.var]],
                                 ifelse(spore_long_diet$timeInt > spore_long_diet$s1_mon & spore_long_diet$timeInt <= spore_long_diet$s2_mon,spore_long_diet[[diet.cont.var_001]],
                                        ifelse(spore_long_diet$timeInt >spore_long_diet$s2_mon & spore_long_diet$timeInt <=spore_long_diet$s3_mon, spore_long_diet[[diet.cont.var_002]],NA)))
  
  # scaled continuous index score
  spore_long_diet$index.x<- ifelse(spore_long_diet$timeInt <= spore_long_diet$s1_mon,spore_long_diet[[diet.cont.var.x]],
                                   ifelse(spore_long_diet$timeInt > spore_long_diet$s1_mon & spore_long_diet$timeInt <= spore_long_diet$s2_mon,spore_long_diet[[diet.cont.var_001.x]],
                                          ifelse(spore_long_diet$timeInt >spore_long_diet$s2_mon & spore_long_diet$timeInt <=spore_long_diet$s3_mon, spore_long_diet[[diet.cont.var_002.x]],NA)))
  
  # truncate time at 36 mos
  
  short_time<-spore_long_diet[!spore_long_diet$timeInt > 3,]
  
  length(unique(short_time$idnum))
  length(short_time$idnum)
  
  
  
  
  # create weights
  
  require(nnet)
  short_time$lastindex<-ifelse(short_time$timeInt <= short_time$s1_mon,short_time[[diet.cut.var]],
                               ifelse(short_time$timeInt > short_time$s1_mon & short_time$timeInt <= short_time$s2_mon,short_time[[diet.cut.var]],
                                      ifelse(short_time$timeInt > short_time$s2_mon & short_time$timeInt <= short_time$s3_mon, short_time[[diet.cut.var_001]],21)))
  
  short_time$lastindex.x<-ifelse(short_time$timeInt <= short_time$s1_mon,short_time[[diet.cont.var.x]],
                                 ifelse(short_time$timeInt > short_time$s1_mon & short_time$timeInt <= short_time$s2_mon,short_time[[diet.cont.var.x]],
                                        ifelse(short_time$timeInt > short_time$s2_mon & short_time$timeInt <= short_time$s3_mon, short_time[[diet.cont.var_001.x]],21)))
  
  
  model1<- multinom(indexq~factor(lastindex)+factor(SEX)+factor(hpv)+factor(tumsite)+factor(educ_3cat)+
                      factor(smoker)+factor(stagebinary)+Age_at_Diagnosis+modality_cat+
                      bmi+CALOR+ace_overall_score,
                    data=short_time)
  txnum<-as.data.frame(predict(model1,newdata=short_time,type='prob'))
  model2<- multinom(indexq~factor(lastindex)+factor(SEX)+factor(hpv)+factor(tumsite)+rcs(calories,3)+rcs(bmindex,3)+factor(educ_3cat)+
                      factor(smoker)+factor(stagebinary)+Age_at_Diagnosis+modality_cat+ace_overall_score,
                    data=short_time,Hess = TRUE)
  txden<-as.data.frame(predict(model2,newdata=short_time,type='prob'))
  colnames(txnum)<- c('txnum0','txnum1','txnum2','txnum3','txnum4')
  colnames(txden)<- c('txden0','txden1','txden2','txden3','txden4')
  
  txden$id=as.integer(rownames(txden))
  txnum$id=as.integer(rownames(txnum))
  short_time$id=as.integer(rownames(short_time))
  
  short_time2<-inner_join(txden,short_time,by='id')
  
  
  
  # generate probability of observed value
  short_time2$txdens= ifelse(short_time2$indexq==1,short_time2$txden0,
                             ifelse(short_time2$indexq==2,short_time2$txden1,
                                    ifelse(short_time2$indexq==3,short_time2$txden2,
                                           ifelse(short_time2$indexq==4,short_time2$txden3,
                                                  ifelse(short_time2$indexq==5,short_time2$txden4,27)))))
  
  short_time3<-left_join(txnum,short_time2,by='id')
  short_time3$txnums= ifelse(short_time3$indexq==1,short_time3$txnum0,
                             ifelse(short_time3$indexq==2,short_time3$txnum1,
                                    ifelse(short_time3$indexq==3,short_time3$txnum2,
                                           ifelse(short_time3$indexq==4,short_time3$txnum3,
                                                  ifelse(short_time3$indexq==5,short_time3$txnum4,27)))))
  
  # compute stabilized iptw
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
  
  
  
  ### censor weights
  
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
  
  model3<- glm(censorvar~factor(lastindex)+factor(lastcens)+factor(SEX)+factor(hpv)+factor(tumsite)+CALOR+bmi+factor(educ_3cat)+
                 factor(smoker)+factor(stagebinary)+Age_at_Diagnosis+modality_cat+ace_overall_score,
               data=short_time3,family=binomial(link='logit'))
  csnum<-as.data.frame(1-predict(model3,newdata=short_time3,type='response'))
  model4<- glm(censorvar~factor(lastindex)+factor(lastcens)+factor(SEX)+factor(hpv)+factor(tumsite)+rcs(calories,3)+rcs(bmindex,3)+factor(educ_3cat)+
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
  
  # final censoring weight
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
  
  
  
  
  
  
  
  # final stabilized weight for model (variable is called swit)
  short_time4$swit<-short_time4$stabipcw*short_time4$stabiptw
  sum(is.na(short_time4$swit))
  premodeldata<-short_time4[!is.na(short_time4$swit),]
  
  length(unique(short_time4$idnum))
  
  # truncating subjects with weight>98th percentile
  premodeldata<-premodeldata%>%mutate(swit=ifelse(swit>quantile(premodeldata$swit,0.98),quantile(premodeldata$swit,0.98),swit))
  
  
  
  # Fit models
  
  library(survey)
  library(rms)
  
  weighted.designindex<-svydesign(id=premodeldata$idnum, data=premodeldata,
                                  weight=premodeldata$swit)
  # check for confounding
  chisq.test(xtabs(premodeldata$swit ~ premodeldata$stagebinary + premodeldata$indexq))
  xtabs(premodeldata$swit ~ premodeldata$stagebinary + premodeldata$indexq)
  
  # discrete start time variable
  premodeldata$start<-premodeldata$timeInt-1
  
  # discrete end time variable
  premodeldata$end<-premodeldata$timeInt
  
  # censor status variable
  premodeldata$y
  
  # design object
  weighted.designindex<-svydesign(id=premodeldata$idnum, data=premodeldata,
                                  weight=premodeldata$swit)
  
  
  indexmodel<-svycoxph(Surv(start, end, y) ~ factor(indexq) +factor(lastindex)+CALOR+bmi+
                         factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                         factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                         modality_cat,
                       design=weighted.designindex,data=premodeldata)
  
  indexmodel.cont.lin<-svycoxph(Surv(start, end, y) ~ index.x +lastindex.x+CALOR+bmi+
                                  factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                                  factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                                  modality_cat,
                                design=weighted.designindex,data=premodeldata)
  
  indexmodel.cont.quad<-svycoxph(Surv(start, end, y) ~ index+ I(index^2) + lastindex+CALOR+bmi+
                                   factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                                   factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                                   modality_cat,
                                 design=weighted.designindex,data=premodeldata)
  
  # Unweighted model
  unweighted.designindex<-svydesign(id=premodeldata$idnum, data=premodeldata,
                                    weight=NULL)
  
  indexmodel.unw<-svycoxph(Surv(start, end, y) ~ factor(indexq) +factor(lastindex)+CALOR+bmi+
                             factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                             factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                             modality_cat,
                           design=unweighted.designindex,data=premodeldata)
  
  indexmodel.cont.lin.unw<-svycoxph(Surv(start, end, y) ~ index.x + lastindex.x+CALOR+bmi+
                                      factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                                      factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                                      modality_cat,
                                    design=unweighted.designindex,data=premodeldata)
  
  
  indexmodel.cont.quad.unw<-svycoxph(Surv(start, end, y) ~ index+ I(index^2) + lastindex.x+ CALOR+bmi+
                                       factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                                       factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                                       modality_cat,
                                     design=unweighted.designindex,data=premodeldata)
  
  
  
  # TREND
  
  spore2<-df
  group1_b<-spore2[spore2[[diet.cut.var]]==1,]
  median1_b<-median(group1_b[[diet.cont.var]])
  group2_b<-spore2[spore2[[diet.cut.var]]==2,]
  median2_b<-median(group2_b[[diet.cont.var]])
  group3_b<-spore2[spore2[[diet.cut.var]]==3,]
  median3_b<-median(group3_b[[diet.cont.var]])
  group4_b<-spore2[spore2[[diet.cut.var]]==4,]
  median4_b<-median(group4_b[[diet.cont.var]])
  group5_b<-spore2[spore2[[diet.cut.var]]==5,]
  median5_b<-median(group5_b[[diet.cont.var]])
  
  group1_001<-spore2[spore2[[diet.cut.var_001]]==1 & is.na(spore2[[diet.cut.var_001]])==FALSE,]
  median1_001<-median(group1_001[[diet.cont.var_001]])
  group2_001<-spore2[spore2[[diet.cut.var_001]]==2 & is.na(spore2[[diet.cut.var_001]])==FALSE,]
  median2_001<-median(group2_001[[diet.cont.var_001]])
  group3_001<-spore2[spore2[[diet.cut.var_001]]==3 & is.na(spore2[[diet.cut.var_001]])==FALSE,]
  median3_001<-median(group3_001[[diet.cont.var_001]])
  group4_001<-spore2[spore2[[diet.cut.var_001]]==4 & is.na(spore2[[diet.cut.var_001]])==FALSE,]
  median4_001<-median(group4_001[[diet.cont.var_001]])
  group5_001<-spore2[spore2[[diet.cut.var_001]]==5 & is.na(spore2[[diet.cut.var_001]])==FALSE,]
  median5_001<-median(group5_001[[diet.cont.var_001]])
  
  
  group1_002<-spore2[spore2[[diet.cut.var_002]]==1 & is.na(spore2[[diet.cut.var_002]])==FALSE,]
  median1_002<-median(group1_002[[diet.cont.var_002]])
  group2_002<-spore2[spore2[[diet.cut.var_002]]==2 & is.na(spore2[[diet.cut.var_002]])==FALSE,]
  median2_002<-median(group2_002[[diet.cont.var_002]])
  group3_002<-spore2[spore2[[diet.cut.var_002]]==3 & is.na(spore2[[diet.cut.var_002]])==FALSE,]
  median3_002<-median(group3_002[[diet.cont.var_002]])
  group4_002<-spore2[spore2[[diet.cut.var_002]]==4 & is.na(spore2[[diet.cut.var_002]])==FALSE,]
  median4_002<-median(group4_002[[diet.cont.var_002]])
  group5_002<-spore2[spore2[[diet.cut.var_002]]==5 & is.na(spore2[[diet.cut.var_002]])==FALSE,]
  median5_002<-median(group5_002[[diet.cont.var_002]])
  
  # making trend variable a timevarying covariate
  premodeldata$trendindex= ifelse(premodeldata$indexq==1 & premodeldata$timeInt <= premodeldata$s1_mon,median1_b,
                                  ifelse(premodeldata$indexq==2 & premodeldata$timeInt <= premodeldata$s1_mon,median2_b,
                                         ifelse(premodeldata$indexq==3 & premodeldata$timeInt <= premodeldata$s1_mon,median3_b,
                                                ifelse(premodeldata$indexq==4 & premodeldata$timeInt <= premodeldata$s1_mon,median4_b,
                                                       ifelse(premodeldata$indexq==5 & premodeldata$timeInt <= premodeldata$s1_mon,median5_b,
                                                              ifelse(premodeldata$indexq==1 & premodeldata$timeInt > premodeldata$s1_mon & premodeldata$timeInt <= premodeldata$s2_mon,median1_001,
                                                                     ifelse(premodeldata$indexq==2 & premodeldata$timeInt > premodeldata$s1_mon & premodeldata$timeInt <= premodeldata$s2_mon,median2_001,
                                                                            ifelse(premodeldata$indexq==3 & premodeldata$timeInt > premodeldata$s1_mon & premodeldata$timeInt <= premodeldata$s2_mon,median3_001,
                                                                                   ifelse(premodeldata$indexq==4 & premodeldata$timeInt > premodeldata$s1_mon & premodeldata$timeInt <= premodeldata$s2_mon,median4_001,
                                                                                          ifelse(premodeldata$indexq==5 & premodeldata$timeInt > premodeldata$s1_mon & premodeldata$timeInt <= premodeldata$s2_mon,median5_001,
                                                                                                 ifelse(premodeldata$indexq==1 & premodeldata$timeInt > premodeldata$s2_mon & premodeldata$timeInt <= premodeldata$s3_mon,median1_002,
                                                                                                        ifelse(premodeldata$indexq==2 & premodeldata$timeInt > premodeldata$s2_mon & premodeldata$timeInt <= premodeldata$s3_mon,median2_002,
                                                                                                               ifelse(premodeldata$indexq==3 & premodeldata$timeInt > premodeldata$s2_mon & premodeldata$timeInt <= premodeldata$s3_mon,median3_002,
                                                                                                                      ifelse(premodeldata$indexq==4 & premodeldata$timeInt > premodeldata$s2_mon & premodeldata$timeInt <= premodeldata$s3_mon,median4_002,
                                                                                                                             ifelse(premodeldata$indexq==5 & premodeldata$timeInt > premodeldata$s2_mon & premodeldata$timeInt <= premodeldata$s3_mon,median5_002,97)))))))))))))))
  
  # trend model
  weighted.designindextrend<-svydesign(id=premodeldata$idnum, data=premodeldata,
                                       weight=premodeldata$swit)
  trendmodelindex<-svycoxph(Surv(start, end, y) ~ trendindex +CALOR+bmi+factor(tumsite)+
                              factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                              factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                              modality_cat,  design=weighted.designindextrend)
  
  unweighted.designindextrend<-svydesign(id=premodeldata$idnum, data=premodeldata,
                                         weight=NULL)
  unw.trendmodelindex<-svycoxph(Surv(start, end, y) ~ trendindex +CALOR+bmi+factor(tumsite)+
                                  factor(hpv)+factor(tumsite)+factor(stagebinary)+factor(educ_3cat)+
                                  factor(SEX)+factor(smoker)+Age_at_Diagnosis+ace_overall_score+
                                  modality_cat,  design=unweighted.designindextrend)
  
  
  
  ######## MAKE TABLE OF RESULTS ########  
  
  ### Weighted results
  mod.tab<-data.frame(Q1='1.00',Q2=paste0(round(exp(indexmodel$coefficients)[1],digits=2),' (',
                                          paste0(round(exp(confint(indexmodel))[1,],digits=2),collapse='-'),')'),
                      Q3=paste0(round(exp(indexmodel$coefficients)[2],digits=2),' (',
                                paste0(round(exp(confint(indexmodel))[2,],digits=2),collapse='-'),')'),
                      Q4=paste0(round(exp(indexmodel$coefficients)[3],digits=2),' (',
                                paste0(round(exp(confint(indexmodel))[3,],digits=2),collapse='-'),')'),
                      Q5=paste0(round(exp(indexmodel$coefficients)[4],digits=2),' (',
                                paste0(round(exp(confint(indexmodel))[4,],digits=2),collapse='-'),')'),
                      ptrend=paste0(round(summary(trendmodelindex)$coefficients[1,6],digits=2)),
                      pq5=paste0(round(summary(indexmodel)$coefficients[4,6],digits=2)),
                      ORCont=paste0(round(exp(indexmodel.cont.lin$coefficients[1]),digits =2),' (',
                                    paste0(round(exp(confint(indexmodel.cont.lin))[1,],digits=2),collapse='-'),')'),
                      QuadP=paste0(round(summary(indexmodel.cont.quad)$coefficients[2,6],digits=2)))
  
  
  
  
  
  
  mod.tab<-mod.tab%>%
    mutate(Q2=ifelse(summary(indexmodel)$coefficients[1,6]<0.05 & summary(indexmodel)$coefficients[1,6]>=0.01,str_replace(Q2,'$','*'),
                     ifelse(summary(indexmodel)$coefficients[1,6]<0.01,str_replace(Q2,'$','**'),Q2)))%>%
    mutate(Q3=ifelse(summary(indexmodel)$coefficients[2,6]<0.05 & summary(indexmodel)$coefficients[2,6]>=0.01,str_replace(Q3,'$','*'),
                     ifelse(summary(indexmodel)$coefficients[2,6]<0.01,str_replace(Q3,'$','**'),Q3)))%>%
    mutate(Q4=ifelse(summary(indexmodel)$coefficients[3,6]<0.05 & summary(indexmodel)$coefficients[3,6]>=0.01,str_replace(Q4,'$','*'),
                     ifelse(summary(indexmodel)$coefficients[3,6]<0.01,str_replace(Q4,'$','**'),Q4)))%>%
    mutate(Q5=ifelse(summary(indexmodel)$coefficients[4,6]<0.05 & summary(indexmodel)$coefficients[4,6]>=0.01,str_replace(Q5,'$','*'),
                     ifelse(summary(indexmodel)$coefficients[4,6]<0.01,str_replace(Q5,'$','**'),Q5)))%>%
    mutate(pq5=ifelse(summary(indexmodel)$coefficients[4,6]>=0.05,pq5,
                      ifelse(summary(indexmodel)$coefficients[4,6]<0.05 & summary(indexmodel)$coefficients[4,6]>=0.01,str_replace(pq5,'$','*'),
                             ifelse(summary(indexmodel)$coefficients[4,6]<0.01,'<0.01**',`pq5`))))%>%
    mutate(ORCont=ifelse(summary(indexmodel.cont.lin)$coefficients[1,6]<0.05 & summary(indexmodel.cont.lin)$coefficients[1,6]>=0.01,str_replace(ORCont,'$','*'),
                         ifelse(summary(indexmodel.cont.lin)$coefficients[1,6]<0.01,str_replace(ORCont,'$','**'),ORCont)))%>%
    mutate(QuadP=ifelse(summary(indexmodel.cont.quad)$coefficients[2,6]<0.05 & summary(indexmodel.cont.quad)$coefficients[2,6]>=0.01,str_replace(QuadP,'$','*'),
                        ifelse(summary(indexmodel.cont.quad)$coefficients[2,6]<0.01,str_replace(QuadP,'$','**'),QuadP)))%>%
    mutate(ptrend=(ifelse(summary(trendmodelindex)$coefficients[1,6]>=0.05,ptrend,
                          ifelse(summary(trendmodelindex)$coefficients[1,6]<0.05 & summary(trendmodelindex)$coefficients[1,6]>=0.01,str_replace(ptrend,'$','*'),
                                 ifelse(summary(trendmodelindex)$coefficients[1,6]<0.01,'<0.01**',ptrend)))))
  
  
  ### Unweighted results
  mod.tab.unw<-data.frame(Q1='1.00',Q2=paste0(round(exp(indexmodel.unw$coefficients)[1],digits=2),' (',
                                              paste0(round(exp(confint(indexmodel.unw))[1,],digits=2),collapse='-'),')'),
                          Q3=paste0(round(exp(indexmodel.unw$coefficients)[2],digits=2),' (',
                                    paste0(round(exp(confint(indexmodel.unw))[2,],digits=2),collapse='-'),')'),
                          Q4=paste0(round(exp(indexmodel.unw$coefficients)[3],digits=2),' (',
                                    paste0(round(exp(confint(indexmodel.unw))[3,],digits=2),collapse='-'),')'),
                          Q5=paste0(round(exp(indexmodel.unw$coefficients)[4],digits=2),' (',
                                    paste0(round(exp(confint(indexmodel.unw))[4,],digits=2),collapse='-'),')'),
                          ptrend=paste0(round(summary(unw.trendmodelindex)$coefficients[1,5],digits=2)),
                          pq5=paste0(round(summary(indexmodel.unw)$coefficients[4,5],digits=2)),
                          ORCont=paste0(round(exp(indexmodel.cont.lin.unw$coefficients[1]),digits =2),' (',
                                        paste0(round(exp(confint(indexmodel.cont.lin.unw))[1,],digits=2),collapse='-'),')'),
                          QuadP=paste0(round(summary(indexmodel.cont.quad.unw)$coefficients[2,5],digits=2)))
  
  
  
  mod.tab.unw<-mod.tab.unw%>%
    mutate(Q2=ifelse(summary(indexmodel.unw)$coefficients[1,5]<0.05 & summary(indexmodel.unw)$coefficients[1,5]>=0.01,str_replace(Q2,'$','*'),
                     ifelse(summary(indexmodel.unw)$coefficients[1,5]<0.01,str_replace(Q2,'$','**'),Q2)))%>%
    mutate(Q3=ifelse(summary(indexmodel.unw)$coefficients[2,5]<0.05 & summary(indexmodel.unw)$coefficients[2,5]>=0.01,str_replace(Q3,'$','*'),
                     ifelse(summary(indexmodel.unw)$coefficients[2,5]<0.01,str_replace(Q3,'$','**'),Q3)))%>%
    mutate(Q4=ifelse(summary(indexmodel.unw)$coefficients[3,5]<0.05 & summary(indexmodel.unw)$coefficients[3,5]>=0.01,str_replace(Q4,'$','*'),
                     ifelse(summary(indexmodel.unw)$coefficients[3,5]<0.01,str_replace(Q4,'$','**'),Q4)))%>%
    mutate(Q5=ifelse(summary(indexmodel.unw)$coefficients[4,5]<0.05 & summary(indexmodel.unw)$coefficients[4,5]>=0.01,str_replace(Q5,'$','*'),
                     ifelse(summary(indexmodel.unw)$coefficients[4,5]<0.01,str_replace(Q5,'$','**'),Q5)))%>%
    mutate(pq5=ifelse(summary(indexmodel.unw)$coefficients[4,5]>=0.05,pq5,
                      ifelse(summary(indexmodel.unw)$coefficients[4,5]<0.05 & summary(indexmodel.unw)$coefficients[4,5]>=0.01,str_replace(pq5,'$','*'),
                             ifelse(summary(indexmodel.unw)$coefficients[4,5]<0.01,'<0.01**',`pq5`))))%>%
    mutate(ORCont=ifelse(summary(indexmodel.cont.lin.unw)$coefficients[1,5]<0.05 & summary(indexmodel.cont.lin.unw)$coefficients[1,5]>=0.01,str_replace(ORCont,'$','*'),
                         ifelse(summary(indexmodel.cont.lin.unw)$coefficients[1,5]<0.01,str_replace(ORCont,'$','**'),ORCont)))%>%
    mutate(QuadP=ifelse(summary(indexmodel.cont.quad.unw)$coefficients[2,5]<0.05 & summary(indexmodel.cont.quad.unw)$coefficients[2,5]>=0.01,str_replace(QuadP,'$','*'),
                        ifelse(summary(indexmodel.cont.quad.unw)$coefficients[2,5]<0.01,str_replace(QuadP,'$','**'),QuadP)))%>%
    mutate(ptrend=(ifelse(summary(unw.trendmodelindex)$coefficients[1,5]>=0.05,ptrend,
                          ifelse(summary(unw.trendmodelindex)$coefficients[1,5]<0.05 & summary(unw.trendmodelindex)$coefficients[1,5]>=0.01,str_replace(ptrend,'$','*'),
                                 ifelse(summary(unw.trendmodelindex)$coefficients[1,5]<0.01,'<0.01**',ptrend)))))
  
  
  mod.tab.comb<-rbind(mod.tab,mod.tab.unw)
  model.type<-c('MSM','Unweighted')
  diet.index<-c(full.index.name,full.index.name)
  mod.tab.comb<-cbind(diet.index,model.type,mod.tab.comb)
  
  # weight descriptive stats
  sweights_stats<-list()
  for(i in 1:3){
    premodeldata1<-subset(premodeldata,timeInt==i)
    sweights_stats[[i]]<-round_df(data.frame(index=full.index.name,visit=i,
                                             minimum=min(premodeldata1$swit),maximum=max(premodeldata1$swit),average=mean(premodeldata1$swit),medians=median(premodeldata1$swit)),digits=2)
  }
  
  sweights_stats<-do.call('rbind',sweights_stats)
  
  # weights boxplots
  weight.box<-ggplot(data = premodeldata,
                     aes(x = factor(timeInt), y = swit)) +
    geom_boxplot() +
    labs(x='Visit',y='Stabilized Weight')+labs(title=full.index.name)+theme_minimal()+theme(text = element_text(family="Helvetica Light"))
  
  
  ## Adjusted survival curves
  setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/Final set of R files')
  library(survminer)
  source('surv_miner_bugfix_826.R')
  require(ggsci)
  
  adj.curve<-ggadjustedcurves(fit=indexmodel,variable='indexq',data=premodeldata,method='conditional',
                              title= full.index.name,
                              font.title=c(16, "bold"),
                              legend.title = "Quintile",
                              font.legend = c(10, "bold"),
                              legend = c(0.2,0.4),
                              ylab = "Adjusted Survival Rate",
                              xlab ="Follow-up (Years)",
                              size=0.6)+theme(text=element_text(family="Helvetica Light"),
                                              plot.title = element_text(color='grey45',size=13))+scale_color_npg()
  
  
  
  results<-list(table=mod.tab.comb,rarwcoeff=exp(indexmodel$coefficients),summ=summary(indexmodel),
                conf=confint(indexmodel),trend=summary(trendmodelindex),trend.unw=summary(unw.trendmodelindex),obs.used=length(fitted.values(indexmodel)),
                miss.diet=length(unique(premodeldata$idnum)),weights_stats=sweights_stats,
                weight_boxplots=weight.box,surv.curve=adj.curve,long.data=premodeldata,quad.mod=indexmodel.cont.quad,
                lin.mod=indexmodel.cont.lin)
  return(results)
  
  
  
}


# HOW TO USE THE FUNCTION (not run)
try79<-msm.func(diet.cut.var='ahei_index_q',diet.cut.var_001='ahei_index_q_001',
                diet.cut.var_002='ahei_index_q_002',
                diet.cont.var='AHEI_INDEX',diet.cont.var_001='AHEI_INDEX_001',
                diet.cont.var_002='AHEI_INDEX_002',full.index.name='AHEI-2010',df=nspore)

try78<-msm.func(diet.cut.var='veg_keto_score_q',diet.cut.var_001='veg_keto_score_q_001',
                diet.cut.var_002='veg_keto_score_q_002',
                diet.cont.var='veg_keto_score',diet.cont.var_001='veg_keto_score_001',
                diet.cont.var_002='veg_keto_score_002',full.index.name='AHEI-2010',df=nspore)



#######USE AN INTERATIVE LOOP TO GENERATE TABLE OF RESULTS

cont.names_b<-c('AHEI_INDEX','aMED_INDEX','DASH_INDEX','keto_score','animal_keto_score','veg_keto_score')
cont.names_001<-vector()
cont.names_002<-vector()
for (i in 1: length(cont.names_b)){
  cont.names_001[i]<-paste0(cont.names_b[i],'_001')
  cont.names_002[i]<-paste0(cont.names_b[i],'_002')
}

cut.names_b<-c('ahei_index_q','amed_index_q','dash_index_q','keto_score_q','animal_keto_score_q','veg_keto_score_q')
cut.names_001<-vector()
cut.names_002<-vector()
for (i in 1: length(cut.names_b)){
  cut.names_001[i]<-paste0(cut.names_b[i],'_001')
  cut.names_002[i]<-paste0(cut.names_b[i],'_002')
}

indices.names<-c('AHEI-2010','aMED','DASH','Low Carbohydrate','Animal-Based Low Carbohydrate','Plant-Based Low Carbohydrate')
comb.results<-list()
comb.obs<-list()
comb.weight.stats<-list()
comb.weight.boxp<-list()
comb.surv.data<-list()
for (i in (1:length(cut.names_b))){
  comb.results[[i]]<-msm.func(diet.cut.var=cut.names_b[i],diet.cut.var_001=cut.names_001[i],
                              diet.cut.var_002=cut.names_002[i],
                              diet.cont.var=cont.names_b[i],diet.cont.var_001=cont.names_001[i],
                              diet.cont.var_002=cont.names_002[i],
                              full.index.name=indices.names[i],df=nspore)$table
  
  comb.obs[[i]]<-msm.func(diet.cut.var=cut.names_b[i],diet.cut.var_001=cut.names_001[i],
                          diet.cut.var_002=cut.names_002[i],
                          diet.cont.var=cont.names_b[i],diet.cont.var_001=cont.names_001[i],
                          diet.cont.var_002=cont.names_002[i],
                          full.index.name=indices.names[i],df=nspore)$obs.used
  
  comb.weight.stats[[i]]<-msm.func(diet.cut.var=cut.names_b[i],diet.cut.var_001=cut.names_001[i],
                                   diet.cut.var_002=cut.names_002[i],
                                   diet.cont.var=cont.names_b[i],diet.cont.var_001=cont.names_001[i],
                                   diet.cont.var_002=cont.names_002[i],
                                   full.index.name=indices.names[i],df=nspore)$weights_stats
  comb.weight.boxp[[i]]<-msm.func(diet.cut.var=cut.names_b[i],diet.cut.var_001=cut.names_001[i],
                                  diet.cut.var_002=cut.names_002[i],
                                  diet.cont.var=cont.names_b[i],diet.cont.var_001=cont.names_001[i],
                                  diet.cont.var_002=cont.names_002[i],
                                  full.index.name=indices.names[i],df=nspore)$weight_boxplots
  comb.surv.data[[i]]<-msm.func(diet.cut.var=cut.names_b[i],diet.cut.var_001=cut.names_001[i],
                                diet.cut.var_002=cut.names_002[i],
                                diet.cont.var=cont.names_b[i],diet.cont.var_001=cont.names_001[i],
                                diet.cont.var_002=cont.names_002[i],
                                full.index.name=indices.names[i],df=nspore)$long.data
  
  
}

#check number of observations used in each model
comb.obs
# table of final results
fin.results<-do.call('rbind',comb.results)

# text process to get correct number of significant digits
str_extract('0.7 (0.4-1.5)','\\.\\d(?=\\s)')
str_replace('0.7 (0.4-1.5)','(?<=\\.\\d)(\\s)','0 ')
str_replace('0.7 (0.4-1.5)','(?<=\\.\\d)(\\-)','0-')
str_replace('0.7 (0.4-1.5)','(?<=\\.\\d)(\\))','0)')
str_replace('1.00 (0.4-1.5)','(?<=^\\d)(\\s\\()','.00 (')




for(i in c(4:7,10)){
  fin.results[,i]<-str_replace(fin.results[,i],'(?<=\\.\\d)(\\))','0)')
  fin.results[,i]<-str_replace(fin.results[,i],'(?<=\\.\\d)(\\s)','0 ')
  fin.results[,i]<-str_replace(fin.results[,i],'(?<=\\.\\d)(\\-)','0-')
  fin.results[,i]<-str_replace(fin.results[,i],'(?<=\\-\\d)(\\))','.00)')
  fin.results[,i]<-str_replace(fin.results[,i],'(?<=^\\d)(\\s\\()','.00 (')
  
}

for(i in c(8:9,11)){
  fin.results[,i]<-str_replace(fin.results[,i],'(?<=\\.\\d)($)','0')
}


View(fin.results)


# save table
setwd('/Volumes/My Passport for Mac/Arthur Lab/Dietary Inflammatory Index/Analyses/Final set of R files/Tables')
write.csv(fin.results,'sensitivity_results_tableCA_905.csv')
write.table(fin.results,"sensitivity_results_tableCA_905.txt",sep=",",row.names=FALSE)


#### weights table

fin.weights<-do.call('rbind',comb.weight.stats)

write.csv(fin.weights,'weight_stats_table_820.csv')

# weights plots
library(ggpubr)
do.call('ggarrange',comb.weight.boxp)

