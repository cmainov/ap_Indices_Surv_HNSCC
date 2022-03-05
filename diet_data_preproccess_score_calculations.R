#####RECALCULATION OF DIET INDEX SCORES

#BEGIN WITH DATA AFTER EXCLUSIONS AND DELETE ALL PREVIOUSLY COMPUTED DIET SCORE COLUMNS
library(readxl)
library(tidyverse)
library(haven)
library(tidyverse)
library(Hmisc)

setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/R_wd')
spore<- read_xlsx('exclusions_825.xlsx')



setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/R_wd')
spore<- read_xlsx('exclusions_825.xlsx')

# fix punch variable which is already converted already
which( colnames(spore)%in% c('PUNCH','PUNCH_002','PUNCH_001'))
spore<-spore[,-c(1271,1388,2028)]
setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/Datasets')

spore2<-read_sas('final_merge.sas7bdat')
nrow(spore)

# rename idnum column
colnames(spore2)[colnames(spore2)=='Idnum']<-'idnum'

spore<-left_join(spore,spore2[,c('idnum','PUNCH','PUNCH_002','PUNCH_001')], by='idnum')

############### CONVERSION OF RAW INTAKE VALUES TO SERVINGS #####
# Convesion Table Document from Harvard:
# https://regepi.bwh.harvard.edu/health/FFQ/files/Food%20group%20servings%20for%20grid07%20and%20bklt07.doc

## FIRST CONVERSION
vars1<-'AVOCADO RAISGRP PRUN_J BAN A_J GRFRT_J O_J O_J_CALC OTH_F_J ICE_LET ROM_LET CELERY TOM_J TOFU SALSA CARROT_R CARROT_C YOG_FR_NONFAT YOG_LOFAT YOG_NONFAT YOG_PLAIN WALNUTS NUTS OTH_NUTS HOTDOG PROC_MTS CHIX_DOG'
vars1<-unlist(str_split(vars1,' '))

spore[,vars1] #this shows the above diet variables are not missing from the FFQ in the combined dataset


##Including one year and two year variables
vars1_001<-vector()
for (i in 1: length(vars1)){
  vars1_001[i]<-paste0(vars1[i],'_001')
}

vars1_002<-vector()
for (i in 1: length(vars1)){
  vars1_002[i]<-paste0(vars1[i],'_002')
}

vars1_all<-c(vars1,vars1_001,vars1_002)

#get column indices
(col.index.1<-which( colnames(spore)%in% vars1_all))

#####FOR LOOP CONVERSION

for (i in col.index.1){
  spore[i]<-ifelse(spore[i]==1,0,
                     ifelse(spore[i]==2,0.02,
                            ifelse(spore[i]==3,0.08,
                                   ifelse(spore[i]==4,0.14,
                                          ifelse(spore[i]==5,0.43,
                                                 ifelse(spore[i]==6,0.8,
                                                        ifelse(spore[i]==7,1,
                                                               ifelse(spore[i]==8,2,
                                                                      ifelse(spore[i]==9,0,NA)))))))))
}
spore['AVOCADO']       
spore['AVOCADO']


## SECOND CONVERSION

vars2<-'PRUN A_SCE PEACHES STRAW TOM BROC CABB_COLE CAUL BRUSL CORN PEAS MIX_VEG BEANS ZUKE SWT_POT SPIN_CKD SPIN_RAW KALE YEL_SQS ONIONS ONIONS1 PEPPERS BACON HAMB XTRLEAN_HAMBURG SAND_BF_HAM PORK BEEF02 tuna dk_fish oth_fish fr_fish_kids shrimp_ckd'
vars2<-toupper(vars2)
vars2<-unlist(str_split(vars2,' '))
spore[,vars2] #this shows the above diet variables are not missing from the FFQ in the combined dataset


##Including one year and two year variables

vars2_001<-vector()
for (i in 1: length(vars2)){
  vars2_001[i]<-paste0(vars2[i],'_001')
}

vars2_002<-vector()
for (i in 1: length(vars2)){
  vars2_002[i]<-paste0(vars2[i],'_002')
}

vars2_all<-c(vars2,vars2_001,vars2_002)

#get column indices
(col.index.2<-which( colnames(spore)%in% vars2_all))

#####FOR LOOP CONVERSION

for (i in col.index.2){
  spore[i]<-ifelse(spore[i]==1,0,
                     ifelse(spore[i]==2,0.02,
                            ifelse(spore[i]==3,0.08,
                                   ifelse(spore[i]==4,0.14,
                                          ifelse(spore[i]==5,0.43,
                                                 ifelse(spore[i]==6,0.8,
                                                        ifelse(spore[i]==7,1,
                                                               ifelse(spore[i]==8,0,NA))))))))
}


spore[,vars2_all]


## THIRD CONVERSION

vars3<-'CANT PUNCH APPLE ORANG GRFRT SOYMILK_FORT SKIM_KIDS MILK2 MILK P_BU COKE OTH_CARB LOCALNO LOCALCAF'
vars3<-unlist(str_split(vars3,' '))
spore[,vars3] #this shows the above diet variables are not missing from the FFQ in the combined dataset


##Including one year and two year variables
vars3_001<-vector()
for (i in 1: length(vars3)){
  vars3_001[i]<-paste0(vars3[i],'_001')
}

vars3_002<-vector()
for (i in 1: length(vars3)){
  vars3_002[i]<-paste0(vars3[i],'_002')
}

vars3_all<-c(vars3,vars3_001,vars3_002)

# get column indices
(col.index.3<-which( colnames(spore)%in% vars3_all))

##### FOR LOOP CONVERSION

for (i in col.index.3){
  spore[i]<-ifelse(spore[i]==1,0,
                     ifelse(spore[i]==2,0.02,
                            ifelse(spore[i]==3,0.08,
                                   ifelse(spore[i]==4,0.14,
                                          ifelse(spore[i]==5,0.43,
                                                 ifelse(spore[i]==6,0.8,
                                                        ifelse(spore[i]==7,1,
                                                               ifelse(spore[i]==8,2.5,
                                                                      ifelse(spore[i]==9,4,
                                                                             ifelse(spore[i]==10,0,NA))))))))))
}



spore[,vars3_all]

## FOURTH CONVERSION

vars4<-'blue apricot tom_s st_beans BOLOGNA'
vars4<-toupper(vars4)
vars4<-unlist(str_split(vars4,' '))
spore[,vars4] #this shows the above diet variables are not missing from the FFQ in the combined dataset


## Including one year and two year variables
vars4_001<-vector()
for (i in 1: length(vars4)){
  vars4_001[i]<-paste0(vars4[i],'_001')
}

vars4_002<-vector()
for (i in 1: length(vars4)){
  vars4_002[i]<-paste0(vars4[i],'_002')
}

vars4_all<-c(vars4,vars4_001,vars4_002)




# get column indices
(col.index.4<-which( colnames(spore)%in% vars4_all))


##### FOR LOOP CONVERSION

for (i in col.index.4){
  spore[i]<-ifelse(spore[i]==1,0,
                     ifelse(spore[i]==2,0.02,
                            ifelse(spore[i]==3,0.08,
                                   ifelse(spore[i]==4,0.14,
                                          ifelse(spore[i]==5,0.43,
                                                 ifelse(spore[i]==6,0.8,
                                                        ifelse(spore[i]==7,0,NA)))))))
}


spore[,vars4_all]

## FIFTH (ALCOHOLIC DRINKS)

vars5<-'R_WINE W_WINE BEER BEER_LITE LIQ GARLIC2'
vars5<-toupper(vars5)
vars5<-unlist(str_split(vars5,' '))
spore[,vars5] #this shows the above diet variables are not missing from the FFQ in the combined dataset

## Including one year and two year variables
vars5_001<-vector()
for (i in 1: length(vars5)){
  vars5_001[i]<-paste0(vars5[i],'_001')
}

vars5_002<-vector()
for (i in 1: length(vars5)){
  vars5_002[i]<-paste0(vars5[i],'_002')
}

vars5_all<-c(vars5,vars5_001,vars5_002)


# get column indices
(col.index.5<-which( colnames(spore)%in% vars5_all))

##### FOR LOOP CONVERSION

# serving for alcohol is 1 cup per day
for (i in col.index.5){
  spore[i]<-ifelse(spore[i]==1,0,
                     ifelse(spore[i]==2,0.02,
                            ifelse(spore[i]==3,0.08,
                                   ifelse(spore[i]==4,0.14,
                                          ifelse(spore[i]==5,0.43,
                                                 ifelse(spore[i]==6,0.8,
                                                        ifelse(spore[i]==7,1,
                                                               ifelse(spore[i]==8,2.5,
                                                                      ifelse(spore[i]==9,4.5,
                                                                             ifelse(spore[i]==10,6,
                                                                                    ifelse(spore[i]==11,0,NA)))))))))))
}


spore[,vars5_all]


######## CALCULATE DIET SCORES

## Create food groups FOR ALL 3 TIMEPOINTS


spore<-spore%>%
  mutate(T_FRUIT=BLUE+APRICOT+CANT+APPLE+ORANG+GRFRT+PRUN+A_SCE+PEACHES+
           STRAW+AVOCADO+RAISGRP+BAN)%>%
  mutate(T_VEG=TOM_S+TOM+BROC+CABB_COLE+CAUL+BRUSL+CORN+MIX_VEG+ZUKE+SWT_POT+
           SPIN_CKD+SPIN_RAW+KALE+YEL_SQS+ONIONS+ONIONS1+PEPPERS+ICE_LET+
           ROM_LET+CELERY+TOM_J+SALSA+CARROT_R+CARROT_C+GARLIC2)%>%
  mutate(T_LEGUME=ST_BEANS+BEANS+SOYMILK_FORT+PEAS+P_BU+TOFU+WALNUTS+NUTS)%>%
  # making a total legume category as well one that collapses nuts and beans separately
  mutate(LEGUME_BEAN=ST_BEANS+BEANS+SOYMILK_FORT+PEAS+TOFU)%>%
  mutate(LEGUME_NUTS=P_BU+WALNUTS+NUTS+OTH_NUTS)%>%
  mutate(T_RED_PROC_MTS=BOLOGNA+BACON+HOTDOG+PROC_MTS+CHIX_DOG+HAMB+XTRLEAN_HAMBURG+
           SAND_BF_HAM+PORK+BEEF02)%>%
  mutate(LF_YOG=ifelse(YOG_LOFAT==0,YOG_PLAIN,0))%>%### Fixing yogurt variables; need to be constructed from two FFQ items
  mutate(NONFAT_YOG=ifelse(YOG_NONFAT==0,YOG_PLAIN,
                           ifelse(is.na(YOG_NONFAT)==T,0,0)))%>%
  mutate(REG_YOG=ifelse(YOG_REG==0,YOG_PLAIN,0))%>%
  mutate(T_FRUIT_001=BLUE_001+APRICOT_001+CANT_001+APPLE_001+ORANG_001+GRFRT_001+PRUN_001+A_SCE_001+PEACHES_001+
           STRAW_001+AVOCADO_001+RAISGRP_001+BAN_001)%>%
  mutate(T_VEG_001=TOM_S_001+TOM_001+BROC_001+CABB_COLE_001+CAUL_001+BRUSL_001+CORN_001+MIX_VEG_001+ZUKE_001+SWT_POT_001+
           SPIN_CKD_001+SPIN_RAW_001+KALE_001+YEL_SQS_001+ONIONS_001+ONIONS1_001+PEPPERS_001+ICE_LET_001+
           ROM_LET_001+CELERY_001+TOM_J_001+SALSA_001+CARROT_R_001+CARROT_C_001+GARLIC2_001)%>%
  mutate(T_LEGUME_001=ST_BEANS_001+BEANS_001+SOYMILK_FORT_001+PEAS_001+P_BU_001+TOFU_001+WALNUTS_001+NUTS_001)%>%
  # making a total legume category as well one that collapses nuts and beans separately
  mutate(LEGUME_BEAN_001=ST_BEANS_001+BEANS_001+SOYMILK_FORT_001+PEAS_001+TOFU_001)%>%
  mutate(LEGUME_NUTS_001=P_BU_001+WALNUTS_001+NUTS_001+OTH_NUTS_001)%>%
  mutate(T_RED_PROC_MTS_001=BOLOGNA_001+BACON_001+HOTDOG_001+PROC_MTS_001+CHIX_DOG_001+HAMB_001+XTRLEAN_HAMBURG_001+
           SAND_BF_HAM_001+PORK_001+BEEF02_001)%>%
  mutate(LF_YOG_001=ifelse(YOG_LOFAT==0,YOG_PLAIN_001,0))%>%### Fixing yogurt variables; need to be constructed from two FFQ items
  mutate(NONFAT_YOG_001=ifelse(YOG_NONFAT_001==0,YOG_PLAIN_001,
                               ifelse(is.na(YOG_NONFAT_001)==T,0,0)))%>%
  mutate(REG_YOG_001=ifelse(YOG_REG_001==0,YOG_PLAIN_001,0))%>%## there should be 3 distinct yogurt variables
  
  ######## YEAR 2 FOOD GROUP VARIABLES
  mutate(T_FRUIT_002=BLUE_002+APRICOT_002+CANT_002+APPLE_002+ORANG_002+GRFRT_002+PRUN_002+A_SCE_002+PEACHES_002+
           STRAW_002+AVOCADO_002+RAISGRP_002+BAN_002)%>%
  mutate(T_VEG_002=TOM_S_002+TOM_002+BROC_002+CABB_COLE_002+CAUL_002+BRUSL_002+CORN_002+MIX_VEG_002+ZUKE_002+SWT_POT_002+
           SPIN_CKD_002+SPIN_RAW_002+KALE_002+YEL_SQS_002+ONIONS_002+ONIONS1_002+PEPPERS_002+ICE_LET_002+
           ROM_LET_002+CELERY_002+TOM_J_002+SALSA_002+CARROT_R_002+CARROT_C_002+GARLIC2_002)%>%
  mutate(T_LEGUME_002=ST_BEANS_002+BEANS_002+SOYMILK_FORT_002+PEAS_002+P_BU_002+TOFU_002+WALNUTS_002+NUTS_002)%>%
  # making a total legume category as well one that collapses nuts and beans separately
  mutate(LEGUME_BEAN_002=ST_BEANS_002+BEANS_002+SOYMILK_FORT_002+PEAS_002+TOFU_002)%>%
  mutate(LEGUME_NUTS_002=P_BU_002+WALNUTS_002+NUTS_002+OTH_NUTS_002)%>%
  mutate(T_RED_PROC_MTS_002=BOLOGNA_002+BACON_002+HOTDOG_002+PROC_MTS_002+CHIX_DOG_002+HAMB_002+XTRLEAN_HAMBURG_002+
           SAND_BF_HAM_002+PORK_002+BEEF02_002)%>%
  mutate(LF_YOG_002=ifelse(YOG_LOFAT==0,YOG_PLAIN_002,0))%>%### Fixing yogurt variables; need to be constructed from two FFQ items
  mutate(NONFAT_YOG_002=ifelse(YOG_NONFAT_002==0,YOG_PLAIN_002,
                               ifelse(is.na(YOG_NONFAT_002)==T,0,0)))%>%
  mutate(REG_YOG_002=ifelse(YOG_REG_002==0,YOG_PLAIN_002,0))## there should be 3 distinct yogurt variables##there should be 3 distinct yogurt variables


## Fixing NAs that should be 0 in yogurt variables
spore[is.na(spore$NONFAT_YOG)==T,'NONFAT_YOG']<-0
spore[is.na(spore$LF_YOG)==T,'LF_YOG']<-0
spore[is.na(spore$REG_YOG)==T,'REG_YOG']<-0
spore[is.na(spore$TUNA)==T,'TUNA']<-0 

spore[is.na(spore$NONFAT_YOG_001)==T,'NONFAT_YOG_001']<-0
spore[is.na(spore$LF_YOG_001)==T,'LF_YOG_001']<-0
spore[is.na(spore$REG_YOG_001)==T,'REG_YOG_001']<-0
spore[is.na(spore$TUNA_001)==T,'TUNA_001']<-0

spore[is.na(spore$NONFAT_YOG_002)==T,'NONFAT_YOG_002']<-0
spore[is.na(spore$LF_YOG_002)==T,'LF_YOG_002']<-0
spore[is.na(spore$REG_YOG_002)==T,'REG_YOG_002']<-0
spore[is.na(spore$TUNA_002)==T,'TUNA_002']<-0

# computing final dairy variables

## Punch is missing for 002 and baseline
spore<-spore%>%
  mutate(T_LF_DAIRY=MILK2+SKIM_KIDS+YOG_FR_NONFAT+LF_YOG+NONFAT_YOG)%>%
  mutate(T_DAIRY=MILK2+SKIM_KIDS+YOG_FR_NONFAT+LF_YOG+NONFAT_YOG+REG_YOG+MILK)%>%
  mutate(SUG_SWT_BEV=PUNCH+OTH_CARB+COKE+OTH_F_J)%>%
  mutate(T_FRUITJUICE_SWT_BEV=A_J+GRFRT_J+O_J+O_J_CALC+PRUN_J+PUNCH+COKE+OTH_CARB)%>%
  mutate(TOTAL_FISH=TUNA+DK_FISH+OTH_FISH+FR_FISH_KIDS+SHRIMP_CKD)%>%

mutate(T_LF_DAIRY_001=MILK2_001+SKIM_KIDS_001+YOG_FR_NONFAT_001+LF_YOG_001+NONFAT_YOG_001)%>%
  mutate(T_DAIRY_001=MILK2_001+SKIM_KIDS_001+YOG_FR_NONFAT_001+LF_YOG_001+NONFAT_YOG_001+REG_YOG_001+MILK_001)%>%
  mutate(SUG_SWT_BEV_001=PUNCH_001+OTH_CARB_001+COKE_001+OTH_F_J_001)%>%
  mutate(T_FRUITJUICE_SWT_BEV_001=A_J_001+GRFRT_J_001+O_J_001+O_J_CALC_001+PRUN_J_001+PUNCH_001+COKE_001+OTH_CARB_001)%>%
  mutate(TOTAL_FISH_001=TUNA_001+DK_FISH_001+OTH_FISH_001+FR_FISH_KIDS_001+SHRIMP_CKD_001)%>%

mutate(T_LF_DAIRY_002=MILK2_002+SKIM_KIDS_002+YOG_FR_NONFAT_002+LF_YOG_002+NONFAT_YOG_002)%>%
  mutate(T_DAIRY_002=MILK2_002+SKIM_KIDS_002+YOG_FR_NONFAT_002+LF_YOG_002+NONFAT_YOG_002+REG_YOG_002+MILK_002)%>%
  mutate(SUG_SWT_BEV_002=PUNCH_002+OTH_CARB_002+COKE_002+OTH_F_J_002)%>%
  mutate(T_FRUITJUICE_SWT_BEV_002=A_J_002+GRFRT_J_002+O_J_002+O_J_CALC_002+PRUN_J_002+PUNCH_002+COKE_002+OTH_CARB_002)%>%
  mutate(TOTAL_FISH_002=TUNA_002+DK_FISH_002+OTH_FISH_002+FR_FISH_KIDS_002+SHRIMP_CKD_002)


#############################DASH SCORES (BASELINE, 1 YEAR AND 2 YEAR)########

##### Create quantiles of the food categories for DASH score

# function to make things faster
quant.fast<-function(cut.var,cont.var,x,df){
  df[[cut.var]]<-factor(cut2(df[[cont.var]],g=x),ordered = T)
  df[[cut.var]]<-factor(df[[cut.var]],levels=levels(df[[cut.var]]),labels=c(1:x))
  return(df[[cut.var]])
}


# collecting names so that a do loop can be used


c.vars<-c('SODIUM','WHGRN','T_VEG','T_LEGUME','T_FRUIT','T_LF_DAIRY',
          'SUG_SWT_BEV','T_RED_PROC_MTS','T_DAIRY','LEGUME_BEAN','LEGUME_NUTS')

c.vars_001<-vector()
for (i in 1: length(c.vars)){
  c.vars_001[i]<-paste0(c.vars[i],'_001')
}

c.vars_002<-vector()
for (i in 1: length(c.vars)){
  c.vars_002[i]<-paste0(c.vars[i],'_002')
}

c.vars_all<-c(c.vars,c.vars_001,c.vars_002)

# get column locations
(cont.loc<-which( colnames(spore) %in% c.vars_all))

cont.var.names<-names(spore)[cont.loc]
cut.var.names<-vector()
for (i in 1:length(cont.var.names)){
  cut.var.names[i]<-paste0(cont.var.names[i],'_Q')
}

# create quintile variables
for (i in 1:length(cont.var.names)){
  spore[,cut.var.names[i]]<-quant.fast(cut.var=cut.var.names[i],
                                         cont.var=cont.var.names[i],df=spore,x=5)
  
}

# BAD food groups
(badgrps<-which( colnames(spore) %in% c('T_RED_PROC_MTS_Q','SODIUM_Q','SUG_SWT_BEV_Q',
                                        'T_RED_PROC_MTS_001_Q','SODIUM_001_Q','SUG_SWT_BEV_001_Q',
                                        'T_RED_PROC_MTS_002_Q','SODIUM_002_Q','SUG_SWT_BEV_002_Q')))
flag.names<-names(spore[,badgrps])
flag.cut.names<-vector()

for (i in 1:length(flag.names)){
  flag.cut.names[i]<-paste0(flag.names[i],'_FLAG')
}

for (i in 1:length(flag.names)){
  spore[,flag.cut.names[i]]<-
    ifelse(spore[,flag.names[i]]==1,5,
           ifelse(spore[,flag.names[i]]==2,4,
                  ifelse(spore[,flag.names[i]]==3,3,
                         ifelse(spore[,flag.names[i]]==4,2,
                                ifelse(spore[,flag.names[i]]==5,1,NA)))))
  
  
  
}

# checking before adding up final scores


spore<-spore%>%
  mutate(DASH_INDEX=as.numeric(WHGRN_Q)+as.numeric(T_FRUIT_Q)+as.numeric(T_VEG_Q)+
           as.numeric(T_LEGUME_Q)+
           as.numeric(T_LF_DAIRY_Q)+as.numeric(SODIUM_Q_FLAG)+
           as.numeric(T_RED_PROC_MTS_Q_FLAG)+as.numeric(SUG_SWT_BEV_Q_FLAG))%>%
  mutate(DASH_INDEX_001=as.numeric(WHGRN_001_Q)+as.numeric(T_FRUIT_001_Q)+as.numeric(T_VEG_001_Q)+
           as.numeric(T_LEGUME_001_Q)+
           as.numeric(T_LF_DAIRY_001_Q)+as.numeric(SODIUM_001_Q_FLAG)+
           as.numeric(T_RED_PROC_MTS_001_Q_FLAG)+as.numeric(SUG_SWT_BEV_001_Q_FLAG))%>%
  mutate(DASH_INDEX_002=as.numeric(WHGRN_002_Q)+as.numeric(T_FRUIT_002_Q)+as.numeric(T_VEG_002_Q)+
           as.numeric(T_LEGUME_002_Q)+
           as.numeric(T_LF_DAIRY_002_Q)+as.numeric(SODIUM_002_Q_FLAG)+
           as.numeric(T_RED_PROC_MTS_002_Q_FLAG)+as.numeric(SUG_SWT_BEV_002_Q_FLAG))%>%
  select(-c(T_RED_PROC_MTS_Q_FLAG,SUG_SWT_BEV_Q_FLAG,T_FRUIT_Q,T_VEG_Q,T_LF_DAIRY_Q,
            SODIUM_Q_FLAG,SODIUM_Q,T_RED_PROC_MTS_Q,SUG_SWT_BEV_Q,T_LEGUME_Q,
            WHGRN_Q,
            T_RED_PROC_MTS_001_Q_FLAG,SUG_SWT_BEV_001_Q_FLAG,T_FRUIT_001_Q,T_VEG_001_Q,T_LF_DAIRY_001_Q,
            SODIUM_001_Q_FLAG,SODIUM_001_Q,T_RED_PROC_MTS_001_Q,SUG_SWT_BEV_001_Q,T_LEGUME_001_Q,
            WHGRN_001_Q,
            T_RED_PROC_MTS_Q_FLAG,SUG_SWT_BEV_Q_FLAG,T_FRUIT_Q,T_VEG_Q,T_LF_DAIRY_Q,
            SODIUM_Q_FLAG,SODIUM_Q,T_RED_PROC_MTS_Q,SUG_SWT_BEV_Q,T_LEGUME_Q,
            WHGRN_Q))

sapply(spore[,c('DASH_INDEX','DASH_INDEX_001','DASH_INDEX_002')],function(x) sum(is.na(x)==F))
sapply(spore[,c('DASH_INDEX','DASH_INDEX_001','DASH_INDEX_002')],function(x) min(x,na.rm = T))
sapply(spore[,c('DASH_INDEX','DASH_INDEX_001','DASH_INDEX_002')],function(x) max(x,na.rm = T))
sapply(spore[,c('DASH_INDEX','DASH_INDEX_001','DASH_INDEX_002')],function(x) hist(x))



###### AHEI-2010 score

spore<-spore%>%
  
  ### BASELINE/PRETREATMENT
  mutate(VEG_AHEI=ifelse(T_VEG==0,0,
                         ifelse(T_VEG>0 & T_VEG<5,2*T_VEG,
                                ifelse(T_VEG>=5, 10,NA))))%>%
  mutate(FRUIT_AHEI=ifelse(T_FRUIT==0,0,
                           ifelse(T_FRUIT<4 & T_FRUIT>0,2.5*T_FRUIT,
                                  ifelse(T_FRUIT>=4,10,NA))))%>%
  mutate(WHGRN_AHEI=ifelse(WHGRN==0,0,
                           ifelse(SEX==1 & WHGRN<90 & WHGRN>0,(10/90)*WHGRN,
                                  ifelse(SEX==2 & WHGRN<75 &WHGRN>0,(10/75)*WHGRN,
                                         ifelse(SEX==2 & WHGRN>=75,10,
                                                ifelse(SEX==1 & WHGRN>=90,10,NA))))))%>%
  mutate(SWT_BEV_AHEI=ifelse(T_FRUITJUICE_SWT_BEV==0,10,
                             ifelse(T_FRUITJUICE_SWT_BEV>0 & T_FRUITJUICE_SWT_BEV<1,10-10*(T_FRUITJUICE_SWT_BEV),
                                    ifelse(T_FRUITJUICE_SWT_BEV>=1,0,NA))))%>%
  mutate(LEGUME_AHEI=ifelse(T_LEGUME==0,0,
                            ifelse(T_LEGUME<1 &T_LEGUME>0,10*T_LEGUME,
                                   ifelse(T_LEGUME>=1,10,NA))))%>%
  mutate(RED_PROCMT_AHEI=ifelse(T_RED_PROC_MTS==0,10,
                                ifelse(T_RED_PROC_MTS>0 &T_RED_PROC_MTS<1.5,(10/1.5)*T_RED_PROC_MTS,
                                       ifelse(T_RED_PROC_MTS>=1.5,0,NA))))%>%
  mutate(TTRANSF=T161+T181+T201+(CLA/1000))%>%
  mutate(TRANS_FAT_EN=((TTRANSF*9)/CALOR))%>%
  mutate(TRANS_FAT_AHEI=ifelse(TRANS_FAT_EN>=0.04,0,
                               ifelse(TRANS_FAT_EN<0.04 & TRANS_FAT_EN>0.005,10-(TRANS_FAT_EN-0.005)/(0.035*10),
                                      ifelse(TRANS_FAT_EN<=0.005,10,NA))))%>%
  mutate(OMEGA_AHEI=ifelse(OMEGA==0,0,
                           ifelse(OMEGA>=0.25,10,
                                  ifelse(OMEGA>0 & OMEGA <0.25,OMEGA*40,NA))))%>%
  mutate(POLY_ENERGY=POLY*9/CALOR)%>%
  mutate(PUFA_AHEI=ifelse(POLY_ENERGY<=0.02,0,
                          ifelse(POLY_ENERGY>0.02 &POLY_ENERGY<0.10,((POLY_ENERGY-0.02)/0.08*10),
                                 ifelse(POLY_ENERGY>=0.10,10,NA))))%>%
  mutate(ALC_SERVINGS=R_WINE+W_WINE+BEER+BEER_LITE+LIQ)%>%
  mutate(ALC_AHEI=ifelse(SEX==1 & ALC_SERVINGS>=3.5,0,
                         ifelse(SEX==2 & ALC_SERVINGS>=2.5,0,
                                ifelse(SEX==1 & ALC_SERVINGS<=2 & ALC_SERVINGS>=0.5,10,
                                       ifelse(SEX==2 & ALC_SERVINGS<=1.5 & ALC_SERVINGS>=0.5,10,
                                              ifelse(SEX==1 & ALC_SERVINGS<0.5 & ALC_SERVINGS>=0,5,
                                                     ifelse(SEX==2 & ALC_SERVINGS<0.5 & ALC_SERVINGS>=0,5,
                                                            ifelse(SEX==1 & ALC_SERVINGS>2 & ALC_SERVINGS<3.5,5,
                                                                   ifelse(SEX==2 & ALC_SERVINGS>1.5 & ALC_SERVINGS<2.5,5,NA)))))))))%>%
  mutate(SOD_DEC=as.numeric(quant.fast(cut.var='SODIUM_DEC',
                                       cont.var='SODIUM',df=spore,x=11))-1)%>%
  mutate(SODIUM_AHEI=10-SOD_DEC)%>%
  mutate(AHEI_INDEX=SODIUM_AHEI+ALC_AHEI+PUFA_AHEI+OMEGA_AHEI+TRANS_FAT_AHEI+RED_PROCMT_AHEI+
           LEGUME_AHEI+SWT_BEV_AHEI+WHGRN_AHEI+FRUIT_AHEI+VEG_AHEI)%>%
  
  ##### YEAR 1
  
  mutate(VEG_AHEI_001=ifelse(T_VEG_001==0,0,
                         ifelse(T_VEG_001>0 & T_VEG_001<5,2*T_VEG_001,
                                ifelse(T_VEG_001>=5, 10,NA))))%>%
  mutate(FRUIT_AHEI_001=ifelse(T_FRUIT_001==0,0,
                           ifelse(T_FRUIT_001<4 & T_FRUIT_001>0,2.4*T_FRUIT_001,
                                  ifelse(T_FRUIT_001>=4,10,NA))))%>%
  mutate(WHGRN_AHEI_001=ifelse(WHGRN_001==0,0,
                           ifelse(SEX==1 & WHGRN_001<90 & WHGRN_001>0,(10/90)*WHGRN_001,
                                  ifelse(SEX==2 & WHGRN_001<75 &WHGRN_001>0,(10/75)*WHGRN_001,
                                         ifelse(SEX==2 & WHGRN_001>=75,10,
                                                ifelse(SEX==1 & WHGRN_001>=90,10,NA))))))%>%
  mutate(SWT_BEV_AHEI_001=ifelse(T_FRUITJUICE_SWT_BEV_001==0,10,
                             ifelse(T_FRUITJUICE_SWT_BEV_001>0 & T_FRUITJUICE_SWT_BEV_001<1,10-10*(T_FRUITJUICE_SWT_BEV_001),
                                    ifelse(T_FRUITJUICE_SWT_BEV_001>=1,0,NA))))%>%
  mutate(LEGUME_AHEI_001=ifelse(T_LEGUME_001==0,0,
                            ifelse(T_LEGUME_001<1 &T_LEGUME_001>0,10*T_LEGUME_001,
                                   ifelse(T_LEGUME_001>=1,10,NA))))%>%
  mutate(RED_PROCMT_AHEI_001=ifelse(T_RED_PROC_MTS_001==0,10,
                                ifelse(T_RED_PROC_MTS_001>0 &T_RED_PROC_MTS_001<1.5,(10/1.5)*T_RED_PROC_MTS_001,
                                       ifelse(T_RED_PROC_MTS_001>=1.5,0,NA))))%>%
  mutate(TTRANSF_001=T161_001+T181_001+T201_001+(CLA_001/1000))%>%
  mutate(TRANS_FAT_EN_001=((TTRANSF_001*9)/CALOR_001))%>%
  mutate(TRANS_FAT_AHEI_001=ifelse(TRANS_FAT_EN_001>=0.04,0,
                               ifelse(TRANS_FAT_EN_001<0.04 & TRANS_FAT_EN_001>0.005,10-(TRANS_FAT_EN_001-0.005)/0.035*10,
                                      ifelse(TRANS_FAT_EN_001<=0.005,10,NA))))%>%
  mutate(OMEGA_AHEI_001=ifelse(OMEGA_001==0,0,
                           ifelse(OMEGA_001>=0.25,10,
                                  ifelse(OMEGA_001>0 & OMEGA_001 <0.25,OMEGA_001*40,NA))))%>%
  mutate(POLY_ENERGY_001=POLY_001*9/CALOR_001)%>%
  mutate(PUFA_AHEI_001=ifelse(POLY_ENERGY_001<=0.02,0,
                          ifelse(POLY_ENERGY_001>0.02 &POLY_ENERGY_001<0.10,((POLY_ENERGY_001-0.02)/0.08*10),
                                 ifelse(POLY_ENERGY_001>=0.10,10,NA))))%>%
  mutate(ALC_SERVINGS_001=R_WINE_001+W_WINE_001+BEER_001+BEER_LITE_001+LIQ_001)%>%
  mutate(ALC_AHEI_001=ifelse(SEX==1 & ALC_SERVINGS_001>=3.5,0,
                         ifelse(SEX==2 & ALC_SERVINGS_001>=2.5,0,
                                ifelse(SEX==1 & ALC_SERVINGS_001<=2 & ALC_SERVINGS_001>=0.5,10,
                                       ifelse(SEX==2 & ALC_SERVINGS_001<=1.5 & ALC_SERVINGS_001>=0.5,10,
                                              ifelse(SEX==1 & ALC_SERVINGS_001<0.5 & ALC_SERVINGS_001>=0,5,
                                                     ifelse(SEX==2 & ALC_SERVINGS_001<0.5 & ALC_SERVINGS_001>=0,5,
                                                            ifelse(SEX==1 & ALC_SERVINGS_001>2 & ALC_SERVINGS_001<3.5,5,
                                                                   ifelse(SEX==2 & ALC_SERVINGS_001>1.5 & ALC_SERVINGS_001<2.5,5,NA)))))))))%>%
  mutate(SOD_DEC_001=as.numeric(quant.fast(cut.var='SOD_DEC_001',
                                       cont.var='SODIUM_001',df=spore,x=11))-1)%>%
  mutate(SODIUM_AHEI_001=10-SOD_DEC_001)%>%
  mutate(AHEI_INDEX_001=SODIUM_AHEI_001+ALC_AHEI_001+PUFA_AHEI_001+OMEGA_AHEI_001+TRANS_FAT_AHEI_001+RED_PROCMT_AHEI_001+
           LEGUME_AHEI_001+SWT_BEV_AHEI_001+WHGRN_AHEI_001+FRUIT_AHEI_001+VEG_AHEI_001)%>%
  
  
  ##### YEAR 2
  
  mutate(VEG_AHEI_002=ifelse(T_VEG_002==0,0,
                             ifelse(T_VEG_002>0 & T_VEG_002<5,2*T_VEG_002,
                                    ifelse(T_VEG_002>=5, 10,NA))))%>%
  mutate(FRUIT_AHEI_002=ifelse(T_FRUIT_002==0,0,
                               ifelse(T_FRUIT_002<4 & T_FRUIT_002>0,2.4*T_FRUIT_002,
                                      ifelse(T_FRUIT_002>=4,10,NA))))%>%
  mutate(WHGRN_AHEI_002=ifelse(WHGRN_002==0,0,
                               ifelse(SEX==1 & WHGRN_002<90 & WHGRN_002>0,(10/90)*WHGRN_002,
                                      ifelse(SEX==2 & WHGRN_002<75 &WHGRN_002>0,(10/75)*WHGRN_002,
                                             ifelse(SEX==2 & WHGRN_002>=75,10,
                                                    ifelse(SEX==1 & WHGRN_002>=90,10,NA))))))%>%
  mutate(SWT_BEV_AHEI_002=ifelse(T_FRUITJUICE_SWT_BEV_002==0,10,
                                 ifelse(T_FRUITJUICE_SWT_BEV_002>0 & T_FRUITJUICE_SWT_BEV_002<1,10-10*(T_FRUITJUICE_SWT_BEV_002),
                                        ifelse(T_FRUITJUICE_SWT_BEV_002>=1,0,NA))))%>%
  mutate(LEGUME_AHEI_002=ifelse(T_LEGUME_002==0,0,
                                ifelse(T_LEGUME_002<1 &T_LEGUME_002>0,10*T_LEGUME_002,
                                       ifelse(T_LEGUME_002>=1,10,NA))))%>%
  mutate(RED_PROCMT_AHEI_002=ifelse(T_RED_PROC_MTS_002==0,10,
                                    ifelse(T_RED_PROC_MTS_002>0 &T_RED_PROC_MTS_002<1.5,(10/1.5)*T_RED_PROC_MTS_002,
                                           ifelse(T_RED_PROC_MTS_002>=1.5,0,NA))))%>%
  mutate(TTRANSF_002=T161_002+T181_002+T201_002+(CLA_002/1000))%>%
  mutate(TRANS_FAT_EN_002=((TTRANSF_002*9)/CALOR_002))%>%
  mutate(TRANS_FAT_AHEI_002=ifelse(TRANS_FAT_EN_002>=0.04,0,
                                   ifelse(TRANS_FAT_EN_002<0.04 & TRANS_FAT_EN_002>0.005,10-(TRANS_FAT_EN_002-0.005)/0.035*10,
                                          ifelse(TRANS_FAT_EN_002<=0.005,10,NA))))%>%
  mutate(OMEGA_AHEI_002=ifelse(OMEGA_002==0,0,
                               ifelse(OMEGA_002>=0.25,10,
                                      ifelse(OMEGA_002>0 & OMEGA_002 <0.25,OMEGA_002*40,NA))))%>%
  mutate(POLY_ENERGY_002=POLY_002*9/CALOR_002)%>%
  mutate(PUFA_AHEI_002=ifelse(POLY_ENERGY_002<=0.02,0,
                              ifelse(POLY_ENERGY_002>0.02 &POLY_ENERGY_002<0.10,((POLY_ENERGY_002-0.02)/0.08*10),
                                     ifelse(POLY_ENERGY_002>=0.10,10,NA))))%>%
  mutate(ALC_SERVINGS_002=R_WINE_002+W_WINE_002+BEER_002+BEER_LITE_002+LIQ_002)%>%
  mutate(ALC_AHEI_002=ifelse(SEX==1 & ALC_SERVINGS_002>=3.5,0,
                             ifelse(SEX==2 & ALC_SERVINGS_002>=2.5,0,
                                    ifelse(SEX==1 & ALC_SERVINGS_002<=2 & ALC_SERVINGS_002>=0.5,10,
                                           ifelse(SEX==2 & ALC_SERVINGS_002<=1.5 & ALC_SERVINGS_002>=0.5,10,
                                                  ifelse(SEX==1 & ALC_SERVINGS_002<0.5 & ALC_SERVINGS_002>=0,5,
                                                         ifelse(SEX==2 & ALC_SERVINGS_002<0.5 & ALC_SERVINGS_002>=0,5,
                                                                ifelse(SEX==1 & ALC_SERVINGS_002>2 & ALC_SERVINGS_002<3.5,5,
                                                                       ifelse(SEX==2 & ALC_SERVINGS_002>1.5 & ALC_SERVINGS_002<2.5,5,NA)))))))))%>%
  mutate(SOD_DEC_002=as.numeric(quant.fast(cut.var='SOD_DEC_002',
                                           cont.var='SODIUM_002',df=spore,x=11))-1)%>%
  mutate(SODIUM_AHEI_002=10-SOD_DEC_002)%>%
  mutate(AHEI_INDEX_002=SODIUM_AHEI_002+ALC_AHEI_002+PUFA_AHEI_002+OMEGA_AHEI_002+TRANS_FAT_AHEI_002+RED_PROCMT_AHEI_002+
           LEGUME_AHEI_002+SWT_BEV_AHEI_002+WHGRN_AHEI_002+FRUIT_AHEI_002+VEG_AHEI_002)


### ENSURE CALCULATIONS WERE DONE APPROPRIATELY
sapply(spore[,c('AHEI_INDEX','AHEI_INDEX_001','AHEI_INDEX_002')],function(x) min(x,na.rm = T))
sapply(spore[,c('AHEI_INDEX','AHEI_INDEX_001','AHEI_INDEX_002')],function(x) max(x,na.rm = T))
sapply(spore[,c('AHEI_INDEX','AHEI_INDEX_001','AHEI_INDEX_002')],function(x) hist(x))
sapply(spore[,c('AHEI_INDEX','AHEI_INDEX_001','AHEI_INDEX_002')],function(x) mean(x,na.rm=T))

# Descriptive statistics AHEI-2010
sapply(spore[,c('SODIUM_AHEI','ALC_AHEI','PUFA_AHEI','OMEGA_AHEI','TRANS_FAT_AHEI','RED_PROCMT_AHEI',
                  'LEGUME_AHEI','SWT_BEV_AHEI','WHGRN_AHEI','FRUIT_AHEI','VEG_AHEI','AHEI_INDEX')],function(x) sum(is.na(x)))


sapply(spore[,c('SODIUM_AHEI','ALC_AHEI','PUFA_AHEI','OMEGA_AHEI','TRANS_FAT_AHEI','RED_PROCMT_AHEI',
                  'LEGUME_AHEI','SWT_BEV_AHEI','WHGRN_AHEI','FRUIT_AHEI','VEG_AHEI','AHEI_INDEX')],function(x) max(x,na.rm=T))

sapply(spore[,c('SODIUM_AHEI','ALC_AHEI','PUFA_AHEI','OMEGA_AHEI','TRANS_FAT_AHEI','RED_PROCMT_AHEI',
                  'LEGUME_AHEI','SWT_BEV_AHEI','WHGRN_AHEI','FRUIT_AHEI','VEG_AHEI','AHEI_INDEX')],function(x) min(x,na.rm=T))


sapply(spore[,c('SODIUM_AHEI_001','ALC_AHEI_001','PUFA_AHEI_001','OMEGA_AHEI_001','TRANS_FAT_AHEI_001','RED_PROCMT_AHEI_001',
                  'LEGUME_AHEI_001','SWT_BEV_AHEI_001','WHGRN_AHEI_001','FRUIT_AHEI_001','VEG_AHEI_001','AHEI_INDEX_001')],function(x) sum(is.na(x)==F))


sapply(spore[,c('SODIUM_AHEI_001','ALC_AHEI_001','PUFA_AHEI_001','OMEGA_AHEI_001','TRANS_FAT_AHEI_001','RED_PROCMT_AHEI_001',
                  'LEGUME_AHEI_001','SWT_BEV_AHEI_001','WHGRN_AHEI_001','FRUIT_AHEI_001','VEG_AHEI_001','AHEI_INDEX_001')],function(x) max(x,na.rm=T))

sapply(spore[,c('SODIUM_AHEI_001','ALC_AHEI_001','PUFA_AHEI_001','OMEGA_AHEI_001','TRANS_FAT_AHEI_001','RED_PROCMT_AHEI_001',
                  'LEGUME_AHEI_001','SWT_BEV_AHEI_001','WHGRN_AHEI_001','FRUIT_AHEI_001','VEG_AHEI_001','AHEI_INDEX_001')],function(x) min(x,na.rm=T))

sapply(spore[,c('SODIUM_AHEI_002','ALC_AHEI_002','PUFA_AHEI_002','OMEGA_AHEI_002','TRANS_FAT_AHEI_002','RED_PROCMT_AHEI_002',
                'LEGUME_AHEI_002','SWT_BEV_AHEI_002','WHGRN_AHEI_002','FRUIT_AHEI_002','VEG_AHEI_002','AHEI_INDEX_002')],function(x) sum(is.na(x)==F))


sapply(spore[,c('SODIUM_AHEI_002','ALC_AHEI_002','PUFA_AHEI_002','OMEGA_AHEI_002','TRANS_FAT_AHEI_002','RED_PROCMT_AHEI_002',
                'LEGUME_AHEI_002','SWT_BEV_AHEI_002','WHGRN_AHEI_002','FRUIT_AHEI_002','VEG_AHEI_002','AHEI_INDEX_002')],function(x) max(x,na.rm=T))

sapply(spore[,c('SODIUM_AHEI_002','ALC_AHEI_002','PUFA_AHEI_002','OMEGA_AHEI_002','TRANS_FAT_AHEI_002','RED_PROCMT_AHEI_002',
                'LEGUME_AHEI_002','SWT_BEV_AHEI_002','WHGRN_AHEI_002','FRUIT_AHEI_002','VEG_AHEI_002','AHEI_INDEX_002')],function(x) min(x,na.rm=T))


# REMOVE UNNECESSARY VARIABLES
spore<-spore%>%
  select(-c(SODIUM_AHEI,ALC_AHEI,PUFA_AHEI,OMEGA_AHEI,TRANS_FAT_AHEI,RED_PROCMT_AHEI,
            LEGUME_AHEI,SWT_BEV_AHEI,WHGRN_AHEI,FRUIT_AHEI,VEG_AHEI,POLY_ENERGY,TRANS_FAT_EN,
            SOD_DEC,
            SODIUM_AHEI_001,ALC_AHEI_001,PUFA_AHEI_001,OMEGA_AHEI_001,TRANS_FAT_AHEI_001,RED_PROCMT_AHEI_001,
            LEGUME_AHEI_001,SWT_BEV_AHEI_001,WHGRN_AHEI_001,FRUIT_AHEI_001,VEG_AHEI_001,POLY_ENERGY_001,TRANS_FAT_EN_001,
            SOD_DEC_001,
            SODIUM_AHEI_002,ALC_AHEI_002,PUFA_AHEI_002,OMEGA_AHEI_002,TRANS_FAT_AHEI_002,RED_PROCMT_AHEI_002,
            LEGUME_AHEI_002,SWT_BEV_AHEI_002,WHGRN_AHEI_002,FRUIT_AHEI_002,VEG_AHEI_002,POLY_ENERGY_002,TRANS_FAT_EN_002,
            SOD_DEC_002))



########## aMED Score ################


spore<-spore%>%
  mutate(MFAT_SFAT_RATIO=MONFAT/SATFAT)%>%
  mutate(MFAT_SFAT_RATIO_001=MONFAT_001/SATFAT_001)%>%
  mutate(MFAT_SFAT_RATIO_002=MONFAT_002/SATFAT_002)

# using my quant function and for loop to create median split-variables for the 9 food groups
# collecting names so that a do loop can be used
(amedgrps<-which( colnames(spore) %in% c("WHGRN","T_RED_PROC_MTS","LEGUME_BEAN","LEGUME_NUTS","T_FRUIT","T_VEG","TOTAL_FISH","MFAT_SFAT_RATIO",
                                         "WHGRN_001","T_RED_PROC_MTS_001","LEGUME_BEAN_001","LEGUME_NUTS_001","T_FRUIT_001","T_VEG_001","TOTAL_FISH_001","MFAT_SFAT_RATIO_001",
                                         "WHGRN_002","T_RED_PROC_MTS_002","LEGUME_BEAN_002","LEGUME_NUTS_002","T_FRUIT_002","T_VEG_002","TOTAL_FISH_002","MFAT_SFAT_RATIO_002")))


cont.var.names.amed<-names(spore[,amedgrps])
cut.var.names.amed<-vector()
for (i in 1:length(cont.var.names.amed)){
  cut.var.names.amed[i]<-paste0(cont.var.names.amed[i],'_Q')
}



# create median variables and subtract 1 to scale to 0 or 1

for (i in 1:length(cont.var.names.amed)){
  spore[,cut.var.names.amed[i]]<-as.numeric(quant.fast(cut.var=cut.var.names.amed[i],
                                                         cont.var=cont.var.names.amed[i],df=spore,x=2))-1
  
}


spore<-spore%>%
  # PRETREATMENT
  mutate(ALC_AMED=ifelse(ALCO>=5 & ALCO<=15,1,0))%>%
  mutate(aMED_INDEX=ALC_AMED+WHGRN_Q+abs(T_RED_PROC_MTS_Q-1)+LEGUME_BEAN_Q+LEGUME_NUTS_Q+
           T_FRUIT_Q+T_VEG_Q+TOTAL_FISH_Q+MFAT_SFAT_RATIO_Q)%>%
  # 1 YEAR
  mutate(ALC_AMED_001=ifelse(ALCO_001>=5 & ALCO_001<=15,1,0))%>%
  mutate(aMED_INDEX_001=ALC_AMED_001+WHGRN_001_Q+abs(T_RED_PROC_MTS_001_Q-1)+LEGUME_BEAN_001_Q+LEGUME_NUTS_001_Q+
           T_FRUIT_001_Q+T_VEG_001_Q+TOTAL_FISH_001_Q+MFAT_SFAT_RATIO_001_Q)%>%
  #2 YEAR
  mutate(ALC_AMED_002=ifelse(ALCO_002>=5 & ALCO_002<=15,1,0))%>%
  mutate(aMED_INDEX_002=ALC_AMED_002+WHGRN_002_Q+abs(T_RED_PROC_MTS_002_Q-1)+LEGUME_BEAN_002_Q+LEGUME_NUTS_002_Q+
           T_FRUIT_002_Q+T_VEG_002_Q+TOTAL_FISH_002_Q+MFAT_SFAT_RATIO_002_Q)%>%
  # DELETE VARIABLES NOT KEPT
  select(-c(ALC_AMED,WHGRN_Q,T_RED_PROC_MTS_Q,LEGUME_BEAN_Q,LEGUME_NUTS_Q,
            T_FRUIT_Q,T_VEG_Q,TOTAL_FISH_Q,MFAT_SFAT_RATIO_Q,
            ALC_AMED_001,WHGRN_001_Q,T_RED_PROC_MTS_001_Q,LEGUME_BEAN_001_Q,LEGUME_NUTS_001_Q,
            T_FRUIT_001_Q,T_VEG_001_Q,TOTAL_FISH_001_Q,MFAT_SFAT_RATIO_001_Q,
            ALC_AMED_002,WHGRN_002_Q,T_RED_PROC_MTS_002_Q,LEGUME_BEAN_002_Q,LEGUME_NUTS_002_Q,
            T_FRUIT_002_Q,T_VEG_002_Q,TOTAL_FISH_002_Q,MFAT_SFAT_RATIO_002_Q))


# Descriptive stats aMED
sapply(spore[,c('aMED_INDEX','aMED_INDEX_001','aMED_INDEX_002')],function(x) min(x,na.rm = T))
sapply(spore[,c('aMED_INDEX','aMED_INDEX_001','aMED_INDEX_002')],function(x) max(x,na.rm = T))
sapply(spore[,c('aMED_INDEX','aMED_INDEX_001','aMED_INDEX_002')],function(x) hist(x))
sapply(spore[,c('aMED_INDEX','aMED_INDEX_001','aMED_INDEX_002')],function(x) mean(x,na.rm=T))
sapply(spore[,c('aMED_INDEX','aMED_INDEX_001','aMED_INDEX_002')],function(x) sum(is.na(x)==F))


###### LOW CARBOHYDRATE SCORE

spore<-spore%>%
  mutate(PERC_CARB=(CARBO*4)/CALOR)%>%
  mutate(PERC_FAT=(TFAT*9)/CALOR)%>%
  mutate(PERC_PROT=(PROT*4)/CALOR)%>%
  mutate(PERC_CARB_001=(CARBO_001*4)/CALOR_001)%>%
  mutate(PERC_FAT_001=(TFAT_001*9)/CALOR_001)%>%
  mutate(PERC_PROT_001=(PROT_001*4)/CALOR_001)%>%
  mutate(PERC_CARB_002=(CARBO_002*4)/CALOR_002)%>%
  mutate(PERC_FAT_002=(TFAT_002*9)/CALOR_002)%>%
  mutate(PERC_PROT_002=(PROT_002*4)/CALOR_002)



# categorize into undeciles and then compute final score
(ketogrps<-which( colnames(spore) %in% c('PERC_PROT','PERC_FAT','PERC_CARB',
                                         'PERC_PROT_001','PERC_FAT_001','PERC_CARB_001',
                                         'PERC_PROT_002','PERC_FAT_002','PERC_CARB_002')))


cont.var.names.keto<-names(spore[,ketogrps])
cut.var.names.keto<-vector()
for (i in 1:length(cont.var.names.keto)){
  cut.var.names.keto[i]<-paste0(cont.var.names.keto[i],'_Q')
}



# create decile variables and subtract 1 to scale to 0 (minimum) or 10 (max)

for (i in 1:length(cont.var.names.keto)){
  spore[,cut.var.names.keto[i]]<-as.numeric(quant.fast(cut.var=cut.var.names.keto[i],
                                                         cont.var=cont.var.names.keto[i],df=spore,x=11))-1
  
}

spore<-spore%>%
  ## PRETREATMENT
  mutate(PERC_CARB_Q=10-PERC_CARB_Q)%>%
  mutate(keto_score=PERC_FAT_Q+PERC_PROT_Q+PERC_CARB_Q)%>%
  # 1 YEAR
  mutate(PERC_CARB_001_Q=10-PERC_CARB_001_Q)%>%
  mutate(keto_score_001=PERC_FAT_001_Q+PERC_PROT_001_Q+PERC_CARB_001_Q)%>%
  #2 YEAR
  mutate(PERC_CARB_002_Q=10-PERC_CARB_002_Q)%>%
  mutate(keto_score_002=PERC_FAT_002_Q+PERC_PROT_002_Q+PERC_CARB_002_Q)%>%
  select(-c(PERC_FAT_Q,PERC_PROT_Q,PERC_CARB_Q,
            PERC_FAT,PERC_PROT,PERC_CARB,
            PERC_FAT_001_Q,PERC_PROT_001_Q,PERC_CARB_001_Q,
            PERC_FAT_001,PERC_PROT_001,PERC_CARB_001,
            PERC_FAT_002_Q,PERC_PROT_002_Q,PERC_CARB_002_Q,
            PERC_FAT_002,PERC_PROT_002,PERC_CARB_002))

# descriptive stats low-carb
sapply(spore[,c('keto_score','keto_score_001','keto_score_002')],function(x) min(x,na.rm = T))
sapply(spore[,c('keto_score','keto_score_001','keto_score_002')],function(x) max(x,na.rm = T))
sapply(spore[,c('keto_score','keto_score_001','keto_score_002')],function(x) hist(x))
sapply(spore[,c('keto_score','keto_score_001','keto_score_002')],function(x) mean(x,na.rm=T))
sapply(spore[,c('keto_score','keto_score_001','keto_score_002')],function(x) sum(is.na(x)==F))




#########  ANIMAL KETO SCORE ########



spore<-spore%>%
  mutate(PERC_CARB=(CARBO*4)/CALOR)%>%
  mutate(PERC_AFAT=(AFAT*9)/CALOR)%>%
  mutate(PERC_APROT=(APROT*4)/CALOR)%>%
  mutate(PERC_CARB_001=(CARBO_001*4)/CALOR_001)%>%
  mutate(PERC_AFAT_001=(AFAT_001*9)/CALOR_001)%>%
  mutate(PERC_APROT_001=(APROT_001*4)/CALOR_001)%>%
  mutate(PERC_CARB_002=(CARBO_002*4)/CALOR_002)%>%
  mutate(PERC_AFAT_002=(AFAT_002*9)/CALOR_002)%>%
  mutate(PERC_APROT_002=(APROT_002*4)/CALOR_002)



# categorize into undeciles and then compute final score
(ketogrps<-which( colnames(spore) %in% c('PERC_APROT','PERC_AFAT','PERC_CARB',
                                         'PERC_APROT_001','PERC_AFAT_001','PERC_CARB_001',
                                         'PERC_APROT_002','PERC_AFAT_002','PERC_CARB_002')))


cont.var.names.keto<-names(spore[,ketogrps])
cut.var.names.keto<-vector()
for (i in 1:length(cont.var.names.keto)){
  cut.var.names.keto[i]<-paste0(cont.var.names.keto[i],'_Q')
}



# create decile variables and subtract 1 to scale to 0 (minimum) or 10 (max)

for (i in 1:length(cont.var.names.keto)){
  spore[,cut.var.names.keto[i]]<-as.numeric(quant.fast(cut.var=cut.var.names.keto[i],
                                                       cont.var=cont.var.names.keto[i],df=spore,x=11))-1
  
}

spore<-spore%>%
  ## PRETREATMENT
  mutate(PERC_CARB_Q=10-PERC_CARB_Q)%>%
  mutate(animal_keto_score=PERC_AFAT_Q+PERC_APROT_Q+PERC_CARB_Q)%>%
  # 1 YEAR
  mutate(PERC_CARB_001_Q=10-PERC_CARB_001_Q)%>%
  mutate(animal_keto_score_001=PERC_AFAT_001_Q+PERC_APROT_001_Q+PERC_CARB_001_Q)%>%
  #2 YEAR
  mutate(PERC_CARB_002_Q=10-PERC_CARB_002_Q)%>%
  mutate(animal_keto_score_002=PERC_AFAT_002_Q+PERC_APROT_002_Q+PERC_CARB_002_Q)%>%
  select(-c(PERC_AFAT_Q,PERC_APROT_Q,PERC_CARB_Q,
            PERC_AFAT,PERC_APROT,PERC_CARB,
            PERC_AFAT_001_Q,PERC_APROT_001_Q,PERC_CARB_001_Q,
            PERC_AFAT_001,PERC_APROT_001,PERC_CARB_001,
            PERC_AFAT_002_Q,PERC_APROT_002_Q,PERC_CARB_002_Q,
            PERC_AFAT_002,PERC_APROT_002,PERC_CARB_002))

# descriptive stats animal low-carb
sapply(spore[,c('animal_keto_score','animal_keto_score_001','animal_keto_score_002')],function(x) min(x,na.rm = T))
sapply(spore[,c('animal_keto_score','animal_keto_score_001','animal_keto_score_002')],function(x) max(x,na.rm = T))
sapply(spore[,c('animal_keto_score','animal_keto_score_001','animal_keto_score_002')],function(x) hist(x))
sapply(spore[,c('animal_keto_score','animal_keto_score_001','animal_keto_score_002')],function(x) mean(x,na.rm=T))
sapply(spore[,c('animal_keto_score','animal_keto_score_001','animal_keto_score_002')],function(x) sum(is.na(x)==F))



#########  Plant-Based KETO SCORE ########
(col.index.1<-which( colnames(spore)%in% c('VPROT_002')))


spore<-spore%>%
  mutate(PERC_CARB=(CARBO*4)/CALOR)%>%
  mutate(PERC_VFAT=(VFAT*9)/CALOR)%>%
  mutate(PERC_VPROT=(VPROT*4)/CALOR)%>%
  mutate(PERC_CARB_001=(CARBO_001*4)/CALOR_001)%>%
  mutate(PERC_VFAT_001=(VFAT_001*9)/CALOR_001)%>%
  mutate(PERC_VPROT_001=(VPROT_001*4)/CALOR_001)%>%
  mutate(PERC_CARB_002=(CARBO_002*4)/CALOR_002)%>%
  mutate(PERC_VFAT_002=(VFAT_002*9)/CALOR_002)%>%
  mutate(PERC_VPROT_002=(VPROT_002*4)/CALOR_002)



# categorize into undeciles and then compute final score
(ketogrps<-which( colnames(spore) %in% c('PERC_VPROT','PERC_VFAT','PERC_CARB',
                                         'PERC_VPROT_001','PERC_VFAT_001','PERC_CARB_001',
                                         'PERC_VPROT_002','PERC_VFAT_002','PERC_CARB_002')))


cont.var.names.keto<-names(spore[,ketogrps])
cut.var.names.keto<-vector()
for (i in 1:length(cont.var.names.keto)){
  cut.var.names.keto[i]<-paste0(cont.var.names.keto[i],'_Q')
}



# create decile variables and subtract 1 to scale to 0 (minimum) or 10 (max)

for (i in 1:length(cont.var.names.keto)){
  spore[,cut.var.names.keto[i]]<-as.numeric(quant.fast(cut.var=cut.var.names.keto[i],
                                                       cont.var=cont.var.names.keto[i],df=spore,x=11))-1
  
}

spore<-spore%>%
  ## PRETREATMENT
  mutate(PERC_CARB_Q=10-PERC_CARB_Q)%>%
  mutate(veg_keto_score=PERC_VFAT_Q+PERC_VPROT_Q+PERC_CARB_Q)%>%
  # 1 YEAR
  mutate(PERC_CARB_001_Q=10-PERC_CARB_001_Q)%>%
  mutate(veg_keto_score_001=PERC_VFAT_001_Q+PERC_VPROT_001_Q+PERC_CARB_001_Q)%>%
  #2 YEAR
  mutate(PERC_CARB_002_Q=10-PERC_CARB_002_Q)%>%
  mutate(veg_keto_score_002=PERC_VFAT_002_Q+PERC_VPROT_002_Q+PERC_CARB_002_Q)%>%
  select(-c(PERC_VFAT_Q,PERC_VPROT_Q,PERC_CARB_Q,
            PERC_VFAT,PERC_VPROT,PERC_CARB,
            PERC_VFAT_001_Q,PERC_VPROT_001_Q,PERC_CARB_001_Q,
            PERC_VFAT_001,PERC_VPROT_001,PERC_CARB_001,
            PERC_VFAT_002_Q,PERC_VPROT_002_Q,PERC_CARB_002_Q,
            PERC_VFAT_002,PERC_VPROT_002,PERC_CARB_002))

# descriptive stats PB low-carb
sapply(spore[,c('veg_keto_score','veg_keto_score_001','veg_keto_score_002')],function(x) min(x,na.rm = T))
sapply(spore[,c('veg_keto_score','veg_keto_score_001','veg_keto_score_002')],function(x) max(x,na.rm = T))
sapply(spore[,c('veg_keto_score','veg_keto_score_001','veg_keto_score_002')],function(x) hist(x))
sapply(spore[,c('veg_keto_score','veg_keto_score_001','veg_keto_score_002')],function(x) mean(x,na.rm=T))
sapply(spore[,c('veg_keto_score','veg_keto_score_001','veg_keto_score_002')],function(x) sum(is.na(x)==F))



##### MAKE QUINTILE VARIABLES FOR ALL DIET SCORES AT ALL TIME POINTS #########
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



for (i in 1:length(index.names)){
  spore[,index.cut.names[i]]<-as.numeric(quant.fast(cut.var=index.cut.names[i],
                                                      cont.var=index.names[i],df=spore,x=5))
  
}

## Trend variable is made inside of the main analysis function used to generate final results
# So no need to generate it here

setwd('/Users/Chris/Documents/OneDrive - University of Illinois - Urbana/Arthur lab stuff/Dietary Inflammatory Index/Analyses/Final set of R files/Data Created/')
saveRDS(spore,'diet_scores_data_314_notimputed.rds')

