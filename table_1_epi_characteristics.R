setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/Final set of R files/Data Created')



nspore<-readRDS('diet_scores_data_820_imputed.rds')


########### Create Table 1



### Write function for table 1 categorical variables
tab1.var.freq<-function(var.name,data,table.var.name,strata.var=NULL,strata.level=NULL){ #var.name is quoted string of how
  # variable is stored in dataset, data=is the dataset stored in R environment
  # and table.var.name is a character string of how that section of table 1 should be titled
  # strata.var is the variable, quoted, to stratify on, and strata.level is a quoted string
  # containing the level of strata.var that is to be examined
  
  if(is.null(strata.var)==T){
    data2<-data
  }
  else {
    data2<-data[data[[strata.var]]==strata.level,]
  }
  
  rowvar.name<-vector()
  levelvec<-levels(factor(data[[var.name]]))
  for (i in 1:length(levelvec)){
    rowvar.name[i]<-paste0(levelvec[i])
  }
  rowvar.name<-c(table.var.name,rowvar.name)
  
  rowvar.freq<-vector()
  for (i in 1:length(levelvec)){
    rowvar.freq[i]<-paste0(table(factor(data2[[var.name]]))[i],' (',round(100*table(factor(data2[[var.name]]))[i]/sum(table(factor(data2[[var.name]]))),digits=1),')')
  }
  
  rowvar.freq<-c('',rowvar.freq)
  rowvar.freq<-ifelse(rowvar.freq=='NA (NA)',paste0('0 (0.0)'),rowvar.freq)
  
  partial.table<-data.frame(cbind(rowvar.name,rowvar.freq))
  colnames(partial.table)<-c('Characteristic','Frequency (%) or Mean (SD)')
  return(partial.table)
  
  
}




#### Write table 1 function for continuous variables getting mean and sd
tab1.var.mean<-function(var.name,data,table.var.name,strata.var=NULL,strata.level=NULL){
  
  if(is.null(strata.var)==T){
    data<-data
  }
  else {
    data<-data[data[[strata.var]]==strata.level,]
  }
  
  
  rowvar.name<-c(table.var.name)
  rowvar.mean<-c(paste0(round(mean(data[[var.name]],na.rm=T),digits=1),' (',round(sd(data[[var.name]]),digits=1),')'))
  
  partial.table<-data.frame(cbind(rowvar.name,rowvar.mean))
  colnames(partial.table)<-c('Characteristic','Frequency (%) or Mean (SD)')
  return(partial.table)
  
}

tab1.var.mean(var.name='bmi',data=nspore,table.var.name = 'BMI')
tab1.var.mean(var.name='bmi',data=nspore,table.var.name = 'BMI',
              strata.var = 'ahei_index_q',
              strata.level='1')



# dichotomous treatment variable and race
nspore$mod_catbin<-ifelse(nspore$modality_cat %in% c(11,20,25),'rad/adj rad',
                          ifelse(nspore$modality_cat %in% c(10,30,45),'no rad',NA))

nspore$race_bin<-factor(ifelse(nspore$RACE ==1,'nhwhite','other'))


# global
age<-tab1.var.mean(var.name='Age_at_Diagnosis',data=nspore,table.var.name = 'Age')
gender<-tab1.var.freq(var.name='SEX',data=nspore,table.var.name='Gender')
bmi<-tab1.var.mean(var.name='bmi',data=nspore,table.var.name = 'Body Mass Index')
smokers<-tab1.var.freq(var.name='smoker',data=nspore,table.var.name='Smoking Status')
hpv<-tab1.var.freq(var.name='hpv',data=nspore,table.var.name='HPV Status')
educ<-tab1.var.freq(var.name='educ_3cat',data=nspore,table.var.name='Education Status')
tx<-tab1.var.freq(var.name='mod_catbin',data=nspore,table.var.name='Treatment')
aces<-tab1.var.freq(var.name='ace_overall_score',data=nspore,table.var.name='ACE-27')
site<tab1.var.freq(var.name='tumsite',data=nspore,table.var.name='site')
stages<-tab1.var.freq(var.name='stagebinary',data=nspore,table.var.name='stage')
race<-tab1.var.freq(var.name='race_bin',data=nspore,table.var.name='Race/ethnicity')


table1<-rbind(age,gender,educ,race,bmi,site,stages,hpv,tx,aces,smokers)


#### Stratified on upper and lower Qs


index.names<-c('AHEI_INDEX','aMED_INDEX','DASH_INDEX','keto_score','animal_keto_score','veg_keto_score')

index.cut.names<-c('ahei_index_med','amed_index_med','dash_index_med',
                   'keto_score_med','animal_keto_score_med','veg_keto_score_med')

index.cut.names

# get quant cut function I wrote
source('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/quantfunction.R')
nspore<-nspore[,!colnames(nspore) %in% index.cut.names]
for (i in 1:length(index.names)){
  nspore[,index.cut.names[i]]<-quant.cut(var=index.names[i],x=2,df=nspore)
  
}

table1.strat<-function(indexq){

age1<-tab1.var.mean(var.name='Age_at_Diagnosis',data=nspore,table.var.name = 'Age',
                   strata.var=indexq,strata.level = 1)
age2<-tab1.var.mean(var.name='Age_at_Diagnosis',data=nspore,table.var.name = 'Age',
                   strata.var=indexq,strata.level = 2)
gender1<-tab1.var.freq(var.name='SEX',data=nspore,table.var.name='Gender',
                      strata.var=indexq,strata.level = 1)
gender2<-tab1.var.freq(var.name='SEX',data=nspore,table.var.name='Gender',
                      strata.var=indexq,strata.level = 2)
bmi1<-tab1.var.mean(var.name='bmi',data=nspore,table.var.name = 'Body Mass Index',
                   strata.var=indexq,strata.level = 1)
bmi2<-tab1.var.mean(var.name='bmi',data=nspore,table.var.name = 'Body Mass Index',
                   strata.var=indexq,strata.level = 2)
                   
smokers1<-tab1.var.freq(var.name='smoker',data=nspore,table.var.name='Smoking Status',
                       strata.var=indexq,strata.level = 1)
smokers2<-tab1.var.freq(var.name='smoker',data=nspore,table.var.name='Smoking Status',
                       strata.var=indexq,strata.level = 2)
hpv1<-tab1.var.freq(var.name='hpv',data=nspore,table.var.name='HPV Status',
                   strata.var=indexq,strata.level = 1)
hpv2<-tab1.var.freq(var.name='hpv',data=nspore,table.var.name='HPV Status',
                   strata.var=indexq,strata.level = 2)
educ1<-tab1.var.freq(var.name='educ_3cat',data=nspore,table.var.name='Education Status',
                    strata.var=indexq,strata.level = 1)
educ2<-tab1.var.freq(var.name='educ_3cat',data=nspore,table.var.name='Education Status',
                    strata.var=indexq,strata.level = 2)
tx1<-tab1.var.freq(var.name='mod_catbin',data=nspore,table.var.name='Treatment',
                  strata.var=indexq,strata.level = 1)
tx2<-tab1.var.freq(var.name='mod_catbin',data=nspore,table.var.name='Treatment',
                  strata.var=indexq,strata.level = 2)
aces1<-tab1.var.freq(var.name='ace_overall_score',data=nspore,table.var.name='ACE-27',
                    strata.var=indexq,strata.level = 1)
aces2<-tab1.var.freq(var.name='ace_overall_score',data=nspore,table.var.name='ACE-27',
                    strata.var=indexq,strata.level = 2)
site1<-tab1.var.freq(var.name='tumsite',data=nspore,table.var.name='site',
                   strata.var=indexq,strata.level = 1)
site2<-tab1.var.freq(var.name='tumsite',data=nspore,table.var.name='site',
                   strata.var=indexq,strata.level = 2)
stages1<-tab1.var.freq(var.name='stagebinary',data=nspore,table.var.name='stage',
                      strata.var=indexq,strata.level = 1)
stages2<-tab1.var.freq(var.name='stagebinary',data=nspore,table.var.name='stage',
                      strata.var=indexq,strata.level = 2)
race1<-tab1.var.freq(var.name='race_bin',data=nspore,table.var.name='Race/ethnicity',
                    strata.var=indexq,strata.level = 1)
race2<-tab1.var.freq(var.name='race_bin',data=nspore,table.var.name='Race/ethnicity',
                    strata.var=indexq,strata.level = 2)

table1a<-rbind(age1,gender1,educ1,race1,bmi1,site1,stages1,hpv1,tx1,aces1,smokers1)
table1b<-rbind(age2,gender2,educ2,race2,bmi2,site2,stages2,hpv2,tx2,aces2,smokers2)
table1ab<-cbind(table1a,table1b)[,-3]
return(table1ab)
}

tables1f<-list()
for (i in 1: length(index.cut.names)){
  tables1f[[i]]<-table1.strat(indexq=index.cut.names[i])
}

prefin<-do.call('cbind',tables1f)


prefin<-prefin[-which(str_detect(colnames(prefin),'Characteristic')==T)]

fin<-cbind(table1,prefin)

# clean up trailing zeros
for(i in 2:14){
  fin[,i]<-str_replace_all(fin[,i],'(?<=\\d\\d)(\\))','.0)')
  fin[,i]<-str_replace_all(fin[,i],'(?<=\\(\\d)(\\))','.0)')

  
}


# save 
setwd('/Volumes/My Passport for Mac/Arthur Lab/Dietary Inflammatory Index/Analyses/Final set of R files/Tables')
write.table(fin,"table1_9221.txt",sep=",",row.names=FALSE)


# age min max computations
mins1<-vector()
maxs1<-vector()
mins2<-vector()
maxs2<-vector()
for( i in 1:length(index.cut.names)){
  # lower half (median)
  mins1[i]<-min(subset(nspore,eval(parse(text=paste0(index.cut.names[i],'==1'))))[['Age_at_Diagnosis']])
  maxs1[i]<-max(subset(nspore,eval(parse(text=paste0(index.cut.names[i],'==1'))))[['Age_at_Diagnosis']])
  
  # upper half (median)
  mins2[i]<-min(subset(nspore,eval(parse(text=paste0(index.cut.names[i],'==2'))))[['Age_at_Diagnosis']])
  maxs2[i]<-max(subset(nspore,eval(parse(text=paste0(index.cut.names[i],'==2'))))[['Age_at_Diagnosis']])
  
}
mins1
maxs1
mins2
maxs2

# number of observations in each median quantile
table(nspore$ahei_index_med)
table(nspore$amed_index_med)
table(nspore$dash_index_med)
table(nspore$keto_score_med)
table(nspore$animal_keto_score_med)
table(nspore$veg_keto_score_med)

