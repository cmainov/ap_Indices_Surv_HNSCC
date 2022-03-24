library(ggpubr)
library(rms)
library(tidyverse)
library(GenKern)

setwd('/Volumes/My Passport for Mac/Arthur Lab/Dietary Inflammatory Index/Analyses/Final set of R files/Data Created')

# read in long form datasets compiled from all-cause mortality analysis (from AC_mortality_analysis.R)
comb.surv.data_ac<-readRDS('data_long_list.rds')

# read in long form datasets compiled from cancer-specific mortality analysis (from CA_mortality_analysis.R)
comb.surv.data_ca<-readRDS('data_long_list_ca.rds')

# combine all datasets into single list
comb.surv.data_all<-c(comb.surv.data_ac,comb.surv.data_ca)


####### Use loop to generate spline plots #########

out.plots<-list() # intialize list to store plots

# indices to examine and index on
lab.index<-rep(c('AHEI-2010','aMED','DASH',
                 'Low-Carbohydrate (LC)','Animal-Based LC','Plant-Based LC'),2)

for(i in 1:12){
  df<-data.frame(comb.surv.data_all[[i]])
  df$x<-as.numeric(df$index)
  df$x_trend<-as.numeric(df$trendindex)
  
  dd <- datadist(df)
  dd$limits$x[2] <-   unique(df[nearest(df$x,median(df$x)),'x'])
  options(datadist = "dd")
  
  
  df[,c('hpv','tumsite','stagebinary','educ_3cat','SEX','smoker')]<-sapply(df[,c('hpv','tumsite','stagebinary','educ_3cat','SEX','smoker')],function(x) factor(x))
  
  modelspline<-cph(Surv(start, end, y)~rcs(x,5)+lastindex.x+CALOR+bmi+
                     hpv+tumsite+stagebinary+educ_3cat+
                     SEX+smoker+Age_at_Diagnosis+ace_overall_score+
                     modality_cat+cluster(id),data=df)
  
  
  pdata1 <- Predict(modelspline, x,ref.zero = TRUE, fun = exp)
  
  
  newdf<-data.frame(pdata1)
  newdf$relative<-1
  newdf$all<-'Reference (HR=1)'
  newdf$ci<-'95% Confidence Bounds'
  
  if (i==1){
    spp<-ggplot(data=newdf,mapping=aes(x=x,y=yhat))+geom_line(size=0.8)+
      geom_ribbon(aes(ymin=lower, ymax=upper,col=ci,fill=ci), alpha=0.2)+theme_classic()+
      geom_line(aes(y=relative, x=x,linetype=all))+
      scale_linetype_manual(values=c('dashed'))+
      theme(legend.position=c(0.5,0.8),
            text=element_text(family='Helvetica Light'),
            legend.title=element_blank(),
            legend.spacing.y =unit(0.01,'cm'),
            legend.text = element_text(size=8))+
      labs(x='AHEI-2010', y='Hazard Ratio')
  } else if (i %in% c(4,7,10)){
    spp<-ggplot(data=newdf,mapping=aes(x=x,y=yhat))+geom_line(size=0.8)+
      geom_ribbon(aes(ymin=lower, ymax=upper,col=ci,fill=ci), alpha=0.2)+theme_classic()+
      geom_line(aes(y=relative, x=x,linetype=all))+
      scale_linetype_manual(values=c('dashed'))+
      theme(legend.position='none',
            text=element_text(family='Helvetica Light'),
            legend.title=element_blank(),
            legend.spacing.y =unit(0.01,'cm'),
            legend.text = element_text(size=8))+
      labs(x=lab.index[i], y='Hazard Ratio')
    
  } else if (i %in% c(2,3,5,6,8,9,11,12)){
    spp<-ggplot(data=newdf,mapping=aes(x=x,y=yhat))+geom_line(size=0.8)+
      geom_ribbon(aes(ymin=lower, ymax=upper,col=ci,fill=ci), alpha=0.2)+theme_classic()+
      geom_line(aes(y=relative, x=x,linetype=all))+
      scale_linetype_manual(values=c('dashed'))+
      theme(legend.position='none',
            text=element_text(family='Helvetica Light'),
            legend.title=element_blank(),
            legend.spacing.y =unit(0.01,'cm'),
            legend.text = element_text(size=8))+
      labs(x=lab.index[i], y='')
    
  }
  
  out.plots[[i]]<-spp
}


splots_ca<-ggarrange(out.plots[[7]],out.plots[[8]],out.plots[[9]],out.plots[[10]],out.plots[[11]],
                     out.plots[[12]])
splots_ac<-ggarrange(out.plots[[1]],out.plots[[2]],out.plots[[3]],out.plots[[4]],out.plots[[5]],
                     out.plots[[6]])

splots2_ac<-annotate_figure(splots_ac,
                            top = text_grob("All-Cause Mortality", 
                                            color = "black", size = 18,hjust=2.50,family = "Helvetica Light"))


splots2_ca<-annotate_figure(splots_ca,
                            top = text_grob("Cancer-Specific Mortality", 
                                            color = "black", size = 18,hjust=1.86,family = "Helvetica Light"))


## Arrange into final figure

ggarrange(splots2_ac,splots2_ca,nrow=2,ncol=1)

# Save
setwd('/Volumes/My Passport for Mac/Arthur Lab/Dietary Inflammatory Index/Analyses/Manuscript Write-ups/Figures')
ggsave("spline_plots_finalv2.jpeg",width = 33, height = 20, units = "cm")

