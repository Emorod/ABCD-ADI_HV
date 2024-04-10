# ADI HV Sensitivity Analysis

# Emerald Yuan

# load libraries


rm(list=ls())
library(data.table)
library(lme4)
library(lmerTest)
library(dplyr)
library(parameters)
library(jtools)
library(sjPlot)
library(car)
library(tableone)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(interactionR)
library(interactions)
library(readr)
library(writexl)


#setting wd
setwd("C:/Users/qyuan25/OneDrive - Emory University/Shared Documents - SOM Ku Lab/General/ABCD Data/ADI, HV, and Positive Family and School Environment")


#import dataset
data_2_14 <- read_csv("data_2_14.csv")

data<-data_2_14 


#creating subset with participants living in residence for more than 1 year
#sub<-subset(data, reshist_addr1_years>1)

#MISSING DATA: 1. get rid of those with missing quality data for neighborhoods
sub<-subset(data, reshist_addr1_status=="OK")

#MISSING DATA: 2. get rid of those with missing data for other variables
datas = sub%>%dplyr::select(
                           age,sex,src_subject_id,
                           rel_family_id,
                           White_non_Hispanic,
                           Hispanic_ethnicity,
                           p_edu_highest,
                           demo_comb_income_v2,
                           reshist_addr1_adi_edu_h,
                           reshist_addr1_adi_income,
                           reshist_addr1_adi_in_dis,
                           
                           reshist_addr1_adi_home_o,
                           reshist_addr1_adi_unemp,
                           
                           reshist_addr1_adi_pov,
                           reshist_addr1_adi_b138,
                           
                           reshist_addr1_adi_sp,
                           reshist_addr1_adi_ncar,
                           
                           reshist_addr1_popdensity,
                           site,crpbi_y_ss_parent,
                           rh,lh,icv,srpf_y_ss_ses,
)%>%na.omit() # 10114 sample size
(writexl)
write_xlsx(datas,"data_11_20.xlsx")

# import dataset from abcd for the quality control

QC<- read_csv("C:/Users/qyuan25/OneDrive - Emory University/Shared Documents - SOM Ku Lab/General/ABCD Data/abcd-data-release-5.0/core/imaging/mri_y_qc_incl.csv")

colnames(QC) 

#only subset imgincl_t1w_include
QC_B<-subset(QC, QC$eventname=="baseline_year_1_arm_1")
colnames(QC) 
QC1<-QC_B%>%dplyr::select(src_subject_id,imgincl_t1w_include)

#put qc and data 214 together (left join)
colnames(sub1)

set<-left_join(sub1,QC1,by="src_subject_id") #10114

#save the dataset#######################################################################
setwd("C:/Users/qyuan25/OneDrive - Emory University/Shared Documents - SOM Ku Lab/General/ABCD Data/ADI, HV, and Positive Family and School Environment")
write.csv(set,"SensAna_ADI_HV_data_10_24")


data<-read.csv("SensAna_ADI_HV_data_10_24")#########----------------------read csv for sensitivity analysis#########

#subset the sample for imgincl_t1w_include = 1

datas<-subset(data,data$imgincl_t1w_include==1) #9801 participants


#normalize variables in datas
datas$agez<-scale(datas$age,center=T,scale=T)
datas$sexz<-scale(datas$sex,center=T,scale=T)
datas$FH_vspz<-scale(datas$FH_vsp,center=T,scale=T)
datas$rhz<-scale(datas$rh,center=T,scale=T)
datas$lhz<-scale(datas$lh,center=T,scale=T)
datas$icvz<-scale(datas$icv,center=T,scale=T)

datas$srpf_y_ss_sesz<-scale(datas$srpf_y_ss_ses,center=T,scale=T)

datas$reshist_addr1_adi_unempz1<-scale(datas$reshist_addr1_adi_unemp,center=T,scale=T)
datas$reshist_addr1_adi_povz1<-scale(datas$reshist_addr1_adi_pov,center=T,scale=T)
datas$reshist_addr1_adi_spz1<-scale(datas$reshist_addr1_adi_sp,center=T,scale=T)
datas$reshist_addr1_adi_home_oz1<-scale(datas$reshist_addr1_adi_home_o,center=T,scale=T)

datas$reshist_addr1_adi_edu_h1<-scale(datas$reshist_addr1_adi_edu_h,center=T,scale=T)
datas$reshist_addr1_adi_income1<-scale(datas$reshist_addr1_adi_income,center=T,scale=T)
datas$reshist_addr1_adi_in_dis1<-scale(datas$reshist_addr1_adi_in_dis,center=T,scale=T)
datas$reshist_addr1_adi_b1381<-scale(datas$reshist_addr1_adi_b138,center=T,scale=T)
datas$reshist_addr1_adi_ncar1<-scale(datas$reshist_addr1_adi_ncar,center=T,scale=T)

datas$reshist_addr1_popdensityz1<-scale(datas$reshist_addr1_popdensity,center=T,scale=T)

#add parental education (high school graduated)
datas$p_edu_hs<-ifelse(is.na(datas$p_edu_highest),NA,
                       ifelse(datas$p_edu_highest>=13,1,0))



# add parental education (high school graduated)
datas$p_edu_hs<-ifelse(is.na(datas$p_edu_highest),NA,
                      ifelse(datas$p_edu_highest>=13,1,0))
datas$crpbi_y_ss_parentz<-scale(datas$crpbi_y_ss_parent,center =T, scale = T)
# save datas
library(writexl)
write_xlsx(datas,"ADI_HV_SPSS_10_24.xlsx")

#generate multirace and education level for table one

#generate multirace and education level for table 1???


#create a subset of data just for univariate model
sub1a = datas%>%dplyr::select(
  reshist_addr1_adi_edu_h1,
  reshist_addr1_adi_income1,
  reshist_addr1_adi_in_dis1,
  
  reshist_addr1_adi_home_oz1,
  reshist_addr1_adi_unempz1,
  
  reshist_addr1_adi_povz1,
  reshist_addr1_adi_b1381,
  
  reshist_addr1_adi_spz1,
  reshist_addr1_adi_ncar1,
  
  reshist_addr1_popdensityz1,
  rhz,lhz,icvz,srpf_y_ss_sesz,crpbi_y_ss_parentz,
  agez,sex,src_subject_id,
  rel_family_id,
  White_non_Hispanic,
  Hispanic_ethnicity,
  p_edu_hs,
  FH_vspz,
  site,demo_comb_income_v2
)%>%na.omit()

## The loop used to run the univariate results ------ right HV
results <- NULL
for(i in sub1a[,1:9]){
  m <- lmer(rhz ~ i +agez+
              sex+
              White_non_Hispanic+
              p_edu_hs +demo_comb_income_v2+
              FH_vspz +icvz+(1 | rel_family_id)+(1 | site),data=sub1a)
  coefs <- data.frame(coef(summary(m)))
  beta <- coefs$Estimate[2]
  se <- coefs$Std..Error[2]
  p <- coefs$Pr...t..[2]
  conf <- data.frame(confint(m))
  upper_ci <- conf$X2.5..[4]
  lower_ci <- conf$X97.5..[4]
  values<-data.frame(beta,se,upper_ci,lower_ci,p)
  results <- rbind(results,paste(values))
  results <- data.frame(results)
}
results<-as.data.frame(sapply(results,as.numeric))
results


#####################################-----------Univariate Results for Left HV

results <- NULL
for(i in sub1a[,1:9]){
  m <- lmer(lhz ~ i +agez+
              sex+
              White_non_Hispanic+
              p_edu_hs +demo_comb_income_v2+
              FH_vspz +icvz+(1 | rel_family_id)+(1 | site),data=sub1a)
  coefs <- data.frame(coef(summary(m)))
  beta <- coefs$Estimate[2]
  se <- coefs$Std..Error[2]
  p <- coefs$Pr...t..[2]
  conf <- data.frame(confint(m))
  upper_ci <- conf$X2.5..[4]
  lower_ci <- conf$X97.5..[4]
  values<-data.frame(beta,se,upper_ci,lower_ci,p)
  results <- rbind(results,paste(values))
  results <- data.frame(results)
}
results<-as.data.frame(sapply(results,as.numeric))
results

data<-datas
plot(data$srpf_y_ss_ses, data$rhv, pch=16)

# Full model LH                                   ----------------Non-Adjusted
m<-lmer(lhz ~ 
          # agez+
          # sex+
          # White_non_Hispanic+
          #  p_edu_hs + 
          #  demo_comb_income_v2+
          #   icvz+
          reshist_addr1_adi_edu_h1+
          reshist_addr1_adi_income1+
         reshist_addr1_adi_in_dis1+
         reshist_addr1_adi_home_oz1+
          reshist_addr1_adi_unempz1+
         reshist_addr1_adi_povz1+
         reshist_addr1_adi_b1381+
          reshist_addr1_adi_spz1+
         reshist_addr1_adi_ncar1+
          (1 | rel_family_id)+(1 | site), data=sub1a)
summary(m)
summ(m, confint = TRUE, level= 0.95, digits = 3)
conf_intervals <- confint(m, level = 0.95)
print(conf_intervals)

#after step-wise elimination
m<-lmer(lhz ~ 
          #age+
          #sex+
          #White_non_Hispanic+
          # p_edu_hs + 
          # demo_comb_income_v2+
          #  icv+
          reshist_addr1_adi_edu_h1+
          reshist_addr1_adi_income1+
          # reshist_addr1_adi_in_dis+
          #reshist_addr1_adi_home_o+
          reshist_addr1_adi_unempz1+
          # reshist_addr1_adi_pov+
          # reshist_addr1_adi_b138+
          reshist_addr1_adi_spz1+
          #reshist_addr1_adi_ncar+
          (1 | rel_family_id)+(1 | site), data=sub1a)
summary(m)
summ(m)
conf_intervals <- confint(m, level = 0.95)
print(conf_intervals)


#overall adjusted model LH
m<-lmer(lhz ~ 
          agez+
          sex+
          White_non_Hispanic+
          p_edu_hs + 
          demo_comb_income_v2+
          icvz+
          reshist_addr1_adi_edu_h1+
          reshist_addr1_adi_income1+
          #reshist_addr1_adi_in_dis+
          #reshist_addr1_adi_home_o+
          reshist_addr1_adi_unempz1+
          #reshist_addr1_adi_pov+
          #reshist_addr1_adi_b138+
          reshist_addr1_adi_spz1+
          #reshist_addr1_adi_ncar+
          (1 | rel_family_id)+(1 | site), data=sub1a)
summary(m)
summ(m)
conf_intervals <- confint(m, level = 0.95)
print(conf_intervals)

#################-               RH      ------------
# Full model LH                                   ----------------Non-Adjusted
m<-lmer(rhz ~ 
          # agez+
          # sex+
          # White_non_Hispanic+
          #  p_edu_hs + 
          #  demo_comb_income_v2+
          #   icvz+
          reshist_addr1_adi_edu_h1+
          reshist_addr1_adi_income1+
          reshist_addr1_adi_in_dis1+
          reshist_addr1_adi_home_oz1+
          reshist_addr1_adi_unempz1+
          reshist_addr1_adi_povz1+
          reshist_addr1_adi_b1381+
          reshist_addr1_adi_spz1+
          reshist_addr1_adi_ncar1+
          (1 | rel_family_id)+(1 | site), data=sub1a)
summary(m)
summ(m, confint = TRUE, level= 0.95, digits = 3)
conf_intervals <- confint(m, level = 0.95)
print(conf_intervals)


#after step-wise elimination
m<-lmer(rhz ~ 
          #age+
          #sex+
          #White_non_Hispanic+
          # p_edu_hs + 
          # demo_comb_income_v2+
          #  icv+
          #reshist_addr1_adi_edu_h1+
          reshist_addr1_adi_income1+
          # reshist_addr1_adi_in_dis+
          #reshist_addr1_adi_home_o+
          reshist_addr1_adi_unempz1+
          # reshist_addr1_adi_pov+
          # reshist_addr1_adi_b138+
          reshist_addr1_adi_spz1+
          #reshist_addr1_adi_ncar+
          (1 | rel_family_id)+(1 | site), data=sub1a)
summary(m)
summ(m)
conf_intervals <- confint(m, level = 0.95)
print(conf_intervals)

#overall adjusted model LH
m<-lmer(rhz ~ 
          agez+
          sex+
          White_non_Hispanic+
          p_edu_hs + 
          demo_comb_income_v2+
          icvz+
         # reshist_addr1_adi_edu_h1+
          reshist_addr1_adi_income1+
          #reshist_addr1_adi_in_dis+
          #reshist_addr1_adi_home_o+
          reshist_addr1_adi_unempz1+
          #reshist_addr1_adi_pov+
          #reshist_addr1_adi_b138+
          reshist_addr1_adi_spz1+
          #reshist_addr1_adi_ncar+
          (1 | rel_family_id)+(1 | site), data=sub1a)
summary(m)
summ(m)
conf_intervals <- confint(m, level = 0.95)
print(conf_intervals)

#interaction adjusted model with school environment
m<-lmer(rhz ~ 
          agez+
          sex+
          White_non_Hispanic+
          p_edu_hs + 
          #FH_vsp +
          demo_comb_income_v2+
          icvz+
          #reshist_addr1_adi_edu_h+
          reshist_addr1_adi_income1+ #standardized
          #reshist_addr1_adi_in_dis+
          #reshist_addr1_adi_home_o+
          reshist_addr1_adi_unempz1+ #standardized
          #reshist_addr1_adi_pov+
          #reshist_addr1_adi_b138+
          reshist_addr1_adi_spz1*srpf_y_ss_sesz+ #standardized
          #reshist_addr1_adi_ncar+
          icvz+
          (1 | rel_family_id)+(1 | site), data=sub1a)
summary(m)

summ(m)
# interaction adjusted for school environment and single parent
m<-lmer(reshist_addr1_adi_spz1 ~ 
          srpf_y_ss_sesz+
          #reshist_addr1_adi_ncar+
          #icv+
          (1 | rel_family_id)+(1 | site), data=sub1a)
summary(m)

interact_plot(m, pred = reshist_addr1_adi_spz1, modx = srpf_y_ss_sesz, interval = TRUE, 
              x.label = 'Neighborhood Single Parents', y.label = 'Right HV',
              legend.main = 'Positive School Environment')

sim_slopes(m, pred = reshist_addr1_adi_spz1, modx = srpf_y_ss_sesz, johnson_neyman = FALSE)

#interaction adjusted model with family environment
m<-lmer(rhz ~ 
          agez+
          sex+
          White_non_Hispanic+
          p_edu_hs + 
          #FH_vsp +
          demo_comb_income_v2+
          icvz+
          #reshist_addr1_adi_edu_h+
          reshist_addr1_adi_income1+ #standardized
          #reshist_addr1_adi_in_dis+
          #reshist_addr1_adi_home_o+
          reshist_addr1_adi_unempz1+ #standardized
          #reshist_addr1_adi_pov+
          #reshist_addr1_adi_b138+
          reshist_addr1_adi_spz1*crpbi_y_ss_parentz+ #standardized
          #reshist_addr1_adi_ncar+
          icvz+
          (1 | rel_family_id)+(1 | site), data=sub1a)
summary(m)
summ(m)
