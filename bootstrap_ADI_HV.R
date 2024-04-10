## Run Bootstrapping 500


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
library(broom.mixed)
library(boot)



# Load dataset

setwd("C:/Users/qyuan25/OneDrive - Emory University/Shared Documents - SOM Ku Lab/General/ABCD Data/ADI, HV, and Positive Family and School Environment")
ADI_HV <- read.csv("C:/Users/qyuan25/OneDrive - Emory University/Shared Documents - SOM Ku Lab/General/ABCD Data/ADI, HV, and Positive Family and School Environment/data_11_20.csv") # N = 10114

data_2_14<- read.csv("C:/Users/qyuan25/OneDrive - Emory University/Shared Documents - SOM Ku Lab/General/ABCD Data/ADI, HV, and Positive Family and School Environment/data_2_14.csv") 
# full dataset including missing N = 11876

data<-data_2_14
#MISSING DATA: * check the missing value for each variables of interest
# get the missing data for each variable
sub1 = data%>%dplyr::select(
  age,sex,src_subject_id,
  rel_family_id,
  White_non_Hispanic,  Hispanic_ethnicity,
  p_edu_highest,
  demo_comb_income_v2,reshist_addr1_status,
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
  site,crpbi_y_ss_parent,rh,lh,icv,
  srpf_y_ss_ses,         
)


apply(is.na(sub1),2,sum)
# Total 11876
summary(data$reshist_addr1_adi_b138) #654 NA???

summary(sub1$reshist_addr1_status!="OK") #703 not "OK"
table(sub1$reshist_addr1_status)



#MISSING DATA: 1. get rid of those with missing quality data for neighborhoods
sub<-subset(sub1, reshist_addr1_status=="OK")


apply(is.na(sub),2,sum)

#MISSING DATA: 2. get rid of those with missing data for other variables
sub1 = sub%>%dplyr::select( age,sex,src_subject_id,
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
)%>%na.omit()


# normalize variables 

#normalizing variables
sub1$agez<-scale(sub1$age,center=T,scale=T)
sub1$sexz<-scale(sub1$sex,center=T,scale=T)
sub1$rhz<-scale(sub1$rh,center=T,scale=T)
sub1$lhz<-scale(sub1$lh,center=T,scale=T)
sub1$icvz<-scale(sub1$icv,center=T,scale=T)

sub1$srpf_y_ss_sesz<-scale(sub1$srpf_y_ss_ses,center=T,scale=T)
sub1$demo_comb_income_v2z<-scale(sub1$demo_comb_income_v2,center=T, scale=T)

sub1$reshist_addr1_adi_unempz1<-scale(sub1$reshist_addr1_adi_unemp,center=T,scale=T)
sub1$reshist_addr1_adi_povz1<-scale(sub1$reshist_addr1_adi_pov,center=T,scale=T)
sub1$reshist_addr1_adi_spz1<-scale(sub1$reshist_addr1_adi_sp,center=T,scale=T)
sub1$reshist_addr1_adi_home_oz1<-scale(sub1$reshist_addr1_adi_home_o,center=T,scale=T)
sub1$reshist_addr1_adi_edu_h1<-scale(sub1$reshist_addr1_adi_edu_h,center=T,scale=T)
sub1$reshist_addr1_adi_income1<-scale(sub1$reshist_addr1_adi_income,center=T,scale=T)
sub1$reshist_addr1_adi_in_dis1<-scale(sub1$reshist_addr1_adi_in_dis,center=T,scale=T)
sub1$reshist_addr1_adi_b1381<-scale(sub1$reshist_addr1_adi_b138,center=T,scale=T)
sub1$reshist_addr1_adi_ncar1<-scale(sub1$reshist_addr1_adi_ncar,center=T,scale=T)

sub1$reshist_addr1_popdensityz1<-scale(sub1$reshist_addr1_popdensity,center=T,scale=T)

sub1$sf<-(sub1$reshist_addr1_adi_spz1-sub1$reshist_addr1_adi_home_oz1)/2
sub1$crpbi_y_ss_parentz<-scale(sub1$crpbi_y_ss_parent,center =T, scale = T)


# add parental education (high school graduated)
sub1$p_edu_hs<-ifelse(is.na(sub1$p_edu_highest),NA,
                      ifelse(sub1$p_edu_highest>=13,1,0))


# after list-wise deletion
m<-lmer(rhz ~ 
          agez+
          sex+
          White_non_Hispanic+
          p_edu_hs + 
          demo_comb_income_v2z+
          icvz+
          #  reshist_addr1_adi_edu_h1+
          reshist_addr1_adi_income1+
          #  reshist_addr1_adi_in_dis1+
          #  reshist_addr1_adi_home_oz1+
          reshist_addr1_adi_unempz1+
          #  reshist_addr1_adi_povz1+
          #  reshist_addr1_adi_b1381+
          reshist_addr1_adi_spz1+
          #  reshist_addr1_adi_ncar1+
          (1 | rel_family_id)+(1 | site), data=sub1)

summ(m)

set.seed(123)

# Bootstrapping 5000
boot_m <- bootMer(m, FUN = fixef, nsim = 500)

# summary of boostrap results
summary(boot_m)

# Calculate 95% bootstrap confidence intervals
boot_ci <- t(sapply(1:ncol(boot_m$t), function(i) {
  quantile(boot_m$t[, i], c(0.025, 0.975))
}))


# Attach column names
colnames(boot_ci) <- c("Lower CI", "Upper CI")

# Combine original estimates and confidence intervals

coefficients_and_ci <- cbind(original = fixef(m), boot_ci)

# Display coefficients and confidence intervals
coefficients_and_ci 

original_coef <- fixef(m)

# Calculate p-values for each coefficient
p_values <- sapply(seq_along(original_coef), function(i) {
  original <- original_coef[i]
  boot_dist <- boot_m$t[, i]
  
  # Calculate the proportion of bootstrap samples greater or less than zero
  if (original > 0) {
    p_value <- mean(boot_dist <= 0)
  } else {
    p_value <- mean(boot_dist >= 0)
  }
  
  # Return the p-value
  p_value
})

# Display p-values
p_values

summary_m <- summary(m)

original_p_values <- format(summary_m$coefficients[, "Pr(>|t|)"],scientific=FALSE)

boot_means <- colMeans(boot_m$t)
# Combine coefficients, confidence intervals, and p-values into a data frame
boot_results <- data.frame(
  Original = original_coef,
  Original_P_value = original_p_values,
  Bootstrapped_Mean = boot_means,
  Lower_CI = boot_ci[, 1],
  Upper_CI = boot_ci[, 2],
  P_Value = p_values
)

# Print the table
print(boot_results)

# other ways of calculating p value
library(lmeresampler)
p_values_boot <- sapply(seq_along(original_coef), function(i) {
  original <- original_coef[i]
  boot_dist <- boot_m$t[, i]
  
  # Calculate the proportion of bootstrap samples greater or less than zero
  if (original > 0) {
    p_value <- mean(boot_dist <= 0)
  } else {
    p_value <- mean(boot_dist >= 0)
  }
  
  # Return the p-value
  p_value
})

# Display p-values
print(p_values_boot)

# Calculate p-values using lmeresampler
p_values_lmeresampler <- bootstrap_pvals(m, type = "parametric", B = 5000)

# Print the p-values
print(p_values_lmeresampler)

# 500 parametric bootstraped
# term                       Estimate `Std. Error`     df `t value` `Pr(>|t|)` p.value
# <chr>                         <dbl>        <dbl>  <dbl>     <dbl>      <dbl>   <dbl>
#   1 (Intercept)               -0.0456        0.0515    271.   -0.884   0.377     0.375  
# 2 agez                       0.00980       0.00748  9257.    1.31    0.190     0.204  
# 3 sex                        0.0739        0.0174  10063.    4.25    0.0000217 0.00200
# 4 White_non_Hispanic         0.0536        0.0204   8473.    2.63    0.00865   0.00798
# 5 p_edu_hs                  -0.0331        0.0435   8882.   -0.761   0.446     0.469  
# 6 demo_comb_income_v2z       0.0427        0.0106   8724.    4.02    0.0000584 0.00200
# 7 icvz                       0.630         0.00965  9063.   65.3     0         0.00200
# 8 reshist_addr1_adi_income1  0.000672      0.0128   6438.    0.0524  0.958     0.946  
# 9 reshist_addr1_adi_unempz1 -0.0159        0.0118   8585.   -1.35    0.177     0.166  
# 10 reshist_addr1_adi_spz1    -0.0309        0.0135   8565.   -2.29    0.0219    0.0259 
