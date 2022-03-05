library(tidyverse)

setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/Final set of R files/Data Created')

nspore<-readRDS('diet_scores_data_314_notimputed.rds')


nspore$distime<- as.integer(round(nspore$Stime,digits=0))/12
nspore<- nspore%>%
  mutate(hpv=ifelse(HPV_STATUS==0,0,
                    ifelse(HPV_STATUS==1,1,
                           ifelse(HPV_STATUS==2,2,
                                  ifelse(HPV_STATUS==99,2,99)))))%>%
  
  dplyr::select(idnum,distime,SEX,Age_at_Diagnosis,dissite,STAGE_B,CALOR,CALOR_001,CALOR_002,bmi,BMI_001,BMI_002,HIGHEST_ED,
                RACE,modality_cat,smoker,drinker,HPV_STATUS,keto_score_q,keto_score_q_001,keto_score_q_002,
                dash_index_q,dash_index_q_001,dash_index_q_002,amed_index_q,amed_index_q_001,amed_index_q_002,
                ahei_index_q,ahei_index_q_001,ahei_index_q_002,AHEI_INDEX,AHEI_INDEX_001,AHEI_INDEX_002,
                aMED_INDEX,aMED_INDEX_001,aMED_INDEX_002,keto_score,keto_score_001,keto_score_002,
                animal_keto_score,animal_keto_score_001,animal_keto_score_002,
                veg_keto_score,veg_keto_score_001,veg_keto_score_002,
                animal_keto_score_q,animal_keto_score_q_001,animal_keto_score_q_002,
                veg_keto_score_q,veg_keto_score_q_001,veg_keto_score_q_002,
                DASH_INDEX,
                DASH_INDEX_001,DASH_INDEX_002,Stime,distime,DeathStat,s_1,s_2,s_3,survivalcensor,hpv,HPV_STATUS,
                ace_overall_score,modality_cat,Cause_of_Death)%>%
  mutate(RACE=ifelse(RACE==99,NA,RACE))%>%
  mutate(HIGHEST_ED=ifelse(HIGHEST_ED==99,NA,HIGHEST_ED))%>%
  mutate(ace_overall_score=ifelse(ace_overall_score==99,NA,ace_overall_score))

# remove those with missing covariates from analysis (n)

sapply(nspore[c('RACE','HIGHEST_ED','ace_overall_score','bmi')],function(x) sum(is.na(x)))

remov<-na.omit(nspore[c('RACE','HIGHEST_ED','ace_overall_score','bmi','modality_cat',
                               'STAGE_B','smoker','HPV_STATUS','idnum')])

nspore<-nspore[nspore$idnum %in% remov$idnum,]


nspore$stagebinary<-ifelse(nspore$STAGE_B %in% c(0,1,2),0,
                           ifelse(nspore$STAGE_B %in% c(3,4),1,NA))

nspore$educ_3cat<-ifelse(nspore$HIGHEST_ED %in% c(0,1,2),0,
                         ifelse(nspore$HIGHEST_ED %in% c(3,4,5),1,NA))

nspore$tumsite<-ifelse(nspore$dissite %in% c(1,4),1,
                       ifelse(nspore$dissite ==3,3,
                              ifelse(nspore$dissite ==2,2,NA)))

# interval times
nspore$s1_mon<- round(nspore$s_1/365,digits=0)
nspore$s2_mon<- round(nspore$s_2/365,digits=0)
nspore$s3_mon<- round(nspore$s_3/365,digits=0)

nspore$distime=round(nspore$distime)
nspore$s1_mon=round(nspore$s1_mon)
nspore$s2_mon=round(nspore$s2_mon)
nspore$s3_mon=round(nspore$s3_mon)

nspore$distime<-ifelse(nspore$distime==0,1,nspore$distime)
nspore$s1_mon<-ifelse(nspore$s1_mon==0,1,nspore$s1_mon)
nspore$s2_mon<-ifelse(nspore$s2_mon==1 & nspore$distime==2,2,
                  ifelse(nspore$idnum==3535,2,nspore$s2_mon)) # fixing id 3535


sapply(nspore[,c('ahei_index_q','ahei_index_q_001','ahei_index_q_002')], function(x) sum(is.na(x)==F))

nrow(nspore[is.na(nspore$ahei_index_q_001)==T & is.na(nspore$ahei_index_q_002)==F,])
# 20 individuals with a gap

nrow(nspore[is.na(nspore$ahei_index_q_001)==T & is.na(nspore$ahei_index_q_002)==T & nspore$distime==2,])
# 26 with missing year1 and year 2 but with 2 years of survival time

nrow(nspore[is.na(nspore$ahei_index_q_001)==T & is.na(nspore$ahei_index_q_002)==T & nspore$distime>=3,])
# 52 with missing year1 and year 2 but with >=3 years of survival time

nrow(nspore[is.na(nspore$ahei_index_q_001)==F & is.na(nspore$ahei_index_q_002)==T & nspore$distime>=3,])
# 45 with missing index_002 and not index_001 and with distime>=3


nrow(nspore[(is.na(nspore$ahei_index_q_001)==T & is.na(nspore$ahei_index_q_002)==T & nspore$distime>=3)|
              (is.na(nspore$ahei_index_q_001)==F & is.na(nspore$ahei_index_q_002)==T & nspore$distime>=3)|
              (is.na(nspore$ahei_index_q_001)==T & is.na(nspore$ahei_index_q_002)==T & nspore$distime==2)|
              (is.na(nspore$ahei_index_q_001)==T & is.na(nspore$ahei_index_q_002)==F),])

## 143 missing values out of 1200
# 12% missing

# create indicators for these subjects
nspore<-nspore%>%
  mutate(miss_total=ifelse((is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime>=3)|
  (is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime==2)|
    (is.na(ahei_index_q_001)==F & is.na(ahei_index_q_002)==T & distime>=3)|
  (is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==F),1,0))

nspore<-nspore%>%

  mutate(miss_3=ifelse(miss_total==1 & (is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime>=3)
                       & !(is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime==2) &
                         !(is.na(ahei_index_q_001)==F & is.na(ahei_index_q_002)==T & distime>=3) &
                         !(is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==F),1,0))%>%
  mutate(miss_2=ifelse(miss_total==1 & !(is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime>=3)
                       & (is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime==2) &
                         !(is.na(ahei_index_q_001)==F & is.na(ahei_index_q_002)==T & distime>=3) &
                         !(is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==F),1,0))%>%
  mutate(miss_1=ifelse(miss_total==1 & !(is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime>=3)
                       & !(is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime==2) &
                         !(is.na(ahei_index_q_001)==F & is.na(ahei_index_q_002)==T & distime>=3) &
                         (is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==F),1,0))%>%
  mutate(miss_4=ifelse(miss_total==1 & !(is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime>=3)
                       & !(is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==T & distime==2) &
                         !(is.na(ahei_index_q_001)==T & is.na(ahei_index_q_002)==F)&
                       (is.na(ahei_index_q_001)==F & is.na(ahei_index_q_002)==T & distime>=3),1,0))%>%
  mutate(total_imputed=ifelse(miss_1==1 | miss_4==1,1,0))


# copy as a separate dataset so that we can create a dataset that will be used in the sensitivity analysis
# at the end of this program
notimputed<-nspore


table(nspore$miss_1)
table(nspore$miss_2)
table(nspore$miss_3)
table(nspore$miss_4)
table(nspore$miss_total)



# imputation
nspore<-nspore%>%
  mutate(DeathStat=ifelse(miss_2==1 | miss_3==1, 0, DeathStat))%>%
  mutate(distime=ifelse(miss_2==1| miss_3==1, 1, distime))%>%
  mutate(AHEI_INDEX_001=ifelse(miss_1==1,(AHEI_INDEX+AHEI_INDEX_002)/2,AHEI_INDEX_001))%>%
  mutate(AHEI_INDEX_002=ifelse(miss_4==1,(AHEI_INDEX+AHEI_INDEX_001)/2,AHEI_INDEX_002))%>%
  mutate(aMED_INDEX_001=ifelse(miss_1==1,(aMED_INDEX+aMED_INDEX_002)/2,aMED_INDEX_001))%>%
  mutate(aMED_INDEX_002=ifelse(miss_4==1,(aMED_INDEX+aMED_INDEX_001)/2,aMED_INDEX_002))%>%
  mutate(DASH_INDEX_001=ifelse(miss_1==1,(DASH_INDEX+DASH_INDEX_002)/2,DASH_INDEX_001))%>%
  mutate(DASH_INDEX_002=ifelse(miss_4==1,(DASH_INDEX+DASH_INDEX_001)/2,DASH_INDEX_002))%>%
  mutate(keto_score_001=ifelse(miss_1==1,(keto_score+keto_score_002)/2,keto_score_001))%>%
  mutate(keto_score_002=ifelse(miss_4==1,(keto_score+keto_score_001)/2,keto_score_002))%>%
  mutate(animal_keto_score_001=ifelse(miss_1==1,(animal_keto_score+animal_keto_score_002)/2,animal_keto_score_001))%>%
  mutate(animal_keto_score_002=ifelse(miss_4==1,(animal_keto_score+animal_keto_score_001)/2,animal_keto_score_002))%>%
  mutate(veg_keto_score_001=ifelse(miss_1==1,(veg_keto_score+veg_keto_score_002)/2,veg_keto_score_001))%>%
  mutate(veg_keto_score_002=ifelse(miss_4==1,(veg_keto_score+veg_keto_score_001)/2,veg_keto_score_002))%>%
  mutate(CALOR_001=ifelse(miss_1==1,(CALOR+CALOR_002)/2,CALOR_001))%>%
  mutate(CALOR_002=ifelse(miss_4==1,(CALOR+CALOR_001)/2,CALOR_002))


  
sapply(nspore[c('AHEI_INDEX_001','AHEI_INDEX_002',
                'aMED_INDEX_001','aMED_INDEX_002',
                'DASH_INDEX_001','DASH_INDEX_002',
                'keto_score_001','keto_score_002',
                'animal_keto_score_001','animal_keto_score_002',
                'veg_keto_score_001','veg_keto_score_002')],function(x) sum(is.na(x)))



# redo censor variable for 3 years
nspore$deathstat2<-ifelse(nspore$DeathStat==1 & nspore$distime>3, 0,nspore$DeathStat)
nspore<-as.data.frame(nspore)


############# Now look at BMI

# create indicator variables indicating what riskset a subject belongs to
nspore<-nspore%>%
  mutate(riskset1=ifelse(is.na(AHEI_INDEX)==F,1,0))%>%
  mutate(riskset2=ifelse(is.na(AHEI_INDEX_001)==F,1,0))%>%
  mutate(riskset3=ifelse(is.na(AHEI_INDEX_002)==F,1,0))

nrow(nspore[nspore$riskset2==1 & is.na(nspore$BMI_001)==T,])
# no subjects missing bmi from risk set 2

nrow(nspore[nspore$riskset3==1 & is.na(nspore$BMI_002)==T,])
# 12 subjects missing bmi from risk set 3

## all individuals with missing BMI are also individual who had diet imputed

# create indicator variable for those with bmiimputed and also impute their 
# bmi_002 as the trajectory mean

nspore<-nspore%>%
  mutate(bmi_imputed=ifelse((riskset2==1 & is.na(BMI_001)==T)|
                              (riskset3==1 & is.na(BMI_002)==T),1,0))%>%
  mutate(bmi_imputed1=ifelse((riskset2==1 & is.na(BMI_001)==T),1,0))%>%
  mutate(bmi_imputed2=ifelse( (riskset3==1 & is.na(BMI_002)==T),1,0))%>%
  mutate(BMI_002=ifelse(bmi_imputed2==1,(bmi+BMI_001)/2,BMI_002))%>%
  mutate(BMI_001=ifelse(bmi_imputed1==1,(bmi+BMI_002)/2,BMI_001))
                  

table(nspore$bmi_imputed)
table(nspore$bmi_imputed2)
table(nspore$bmi_imputed1)
# 12 individuals had their BMI_002 imputed (as noted above)

sum(is.na(nspore$BMI_001==F & is.na(nspore$AHEI_INDEX_001)==F))
sum(is.na(nspore$BMI_002==F & is.na(nspore$AHEI_INDEX_002)==F))



index.names<-c('DASH_INDEX','AHEI_INDEX','aMED_INDEX','keto_score','veg_keto_score','animal_keto_score',
               'DASH_INDEX_001','AHEI_INDEX_001','aMED_INDEX_001','keto_score_001','veg_keto_score_001','animal_keto_score_001',
               'DASH_INDEX_002','AHEI_INDEX_002','aMED_INDEX_002','keto_score_002','veg_keto_score_002','animal_keto_score_002')

index.cut.names<-c('dash_index_q','ahei_index_q','amed_index_q',
                   'keto_score_q','veg_keto_score_q','animal_keto_score_q',
                   'dash_index_q_001','ahei_index_q_001','amed_index_q_001',
                   'keto_score_q_001','veg_keto_score_q_001','animal_keto_score_q_001',
                   'dash_index_q_002','ahei_index_q_002','amed_index_q_002',
                   'keto_score_q_002','veg_keto_score_q_002','animal_keto_score_q_002')

index.cut.names

# get quant cut function I wrote
source('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/quantfunction.R')
nspore<-nspore[,!colnames(nspore) %in% index.cut.names]
for (i in 1:length(index.names)){
  nspore[,index.cut.names[i]]<-quant.cut(var=index.names[i],x=5,df=nspore)
  
}





###### NOTE:

# there will be 1018 person years in the analysis although there are 1058 rows in the data
# due to rounding of the survival times; i.e., 40 individuals have their survival times rounded down after
# completing one of the follow up FFQs
nrow(nspore2[nspore2$riskset2==1 & nspore2$riskset3==0 &nspore2$distime!=2,])
nrow(nspore2[nspore2$riskset3==1 & nspore2$distime!=3,])



###### Now create scaled variables for the diet indices (scaled by SD)

index.names<-c('DASH_INDEX','AHEI_INDEX','aMED_INDEX','keto_score','veg_keto_score','animal_keto_score',
               'DASH_INDEX_001','AHEI_INDEX_001','aMED_INDEX_001','keto_score_001','veg_keto_score_001','animal_keto_score_001',
               'DASH_INDEX_002','AHEI_INDEX_002','aMED_INDEX_002','keto_score_002','veg_keto_score_002','animal_keto_score_002')

index.names.x<-paste0(index.names,'.x')

for(i in 1:length(index.names.x)){
  nspore[,index.names.x[i]]<-nspore[[index.names[i]]]/sd(nspore[[index.names[i]]],na.rm=T)
}

# save imputed dataset
setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/Final set of R files/Data Created')


saveRDS(nspore,'diet_scores_data_820_imputed.rds')


# now create dataset for sensitivity analysis
notimputed<-notimputed%>%
  mutate(deathstat2=ifelse(DeathStat==1 & distime>3, 0,DeathStat))%>%
  mutate(deathstat2=ifelse(miss_2==1 | miss_3==1 | miss_1==1 | miss_4==1, 0, deathstat2))%>% # censor those with missing diet data
  mutate(distime=ifelse(miss_2==1 | miss_3==1 | miss_1, 1, 
                        ifelse(miss_4==1 ,2,distime)))%>% # set their follow up time to the second visit

  mutate(riskset1=ifelse(is.na(AHEI_INDEX)==F,1,0))%>%
  mutate(riskset2=ifelse(is.na(AHEI_INDEX_001)==F,1,0))%>%
  mutate(riskset3=ifelse(is.na(AHEI_INDEX_002)==F,1,0))%>%
  mutate(bmi_imputed2=ifelse((riskset3==1 & is.na(BMI_002)==T),1,0))%>%
  mutate(distime=ifelse(bmi_imputed2==1,2,distime))%>%
  mutate(deathstat2=(ifelse(bmi_imputed2==1,0,deathstat2)))
  


table(notimputed$riskset1)# 468
table(notimputed$riskset2)# 309
table(notimputed$riskset3)# 216

# redo quantiles
notimputed<-notimputed[,!colnames(notimputed) %in% index.cut.names]
for (i in 1:length(index.names)){
  notimputed[,index.cut.names[i]]<-quant.cut(var=index.names[i],x=5,df=notimputed)
  
}

# create scaled continuus variable
###### Now create scaled variables for the diet indices (scaled by SD)

index.names<-c('DASH_INDEX','AHEI_INDEX','aMED_INDEX','keto_score','veg_keto_score','animal_keto_score',
               'DASH_INDEX_001','AHEI_INDEX_001','aMED_INDEX_001','keto_score_001','veg_keto_score_001','animal_keto_score_001',
               'DASH_INDEX_002','AHEI_INDEX_002','aMED_INDEX_002','keto_score_002','veg_keto_score_002','animal_keto_score_002')

index.names.x<-paste0(index.names,'.x')

for(i in 1:length(index.names.x)){
  notimputed[,index.names.x[i]]<-notimputed[[index.names[i]]]/sd(notimputed[[index.names[i]]],na.rm=T)
}

saveRDS(notimputed,'diet_scores_data_820_notimputed.rds')

