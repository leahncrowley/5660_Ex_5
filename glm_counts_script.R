# GLMs with Count Data - Advanced Data Analytics 
# Leah N. Crowley - Fall 2023 

# Call requried packages to workspace: 
  library(ggplot2)
  library(arm)
  library(ggfortify)
  library(performance)
  library(AICcmodavg)
  library(grid)
  library(dplyr)
  library(easystats)
  library(MASS)

# Read in data you are using for this assignment: 
  diseases <- read.csv("diseases.csv")
  
# Separate by gender:
  men.diseases <- filter(diseases, male_gender == "1")
  women.diseases <- filter(diseases, male_gender == "0") 

# TOTAL DISEASES
 
# Does the number of chronic diseases a man gets in his lifetime increase with age?
  ggplot(men.diseases, aes(age, total_diseases)) +
    geom_smooth(method="glm", method.args=list(family="poisson"(link="log")), color="darkblue") +
    ylab("Number of Chronic Diseases He Has") +
    xlab("Man's Age") +
    ggtitle("Mens Age and Chronic Diseases")+
    theme_light()
  
# Histogram:
  ggplot(data=men.diseases, aes(total_diseases)) + 
    geom_histogram(color="darkblue", fill="lightblue")+
    theme_light()+
    ylab("Count (Men)")+
    xlab("Total Number of Chronic Diseases an Individual Has")+
    geom_vline(aes(xintercept=mean(total_diseases)),color="black", linetype="dashed", size=1)
  
# Poisson model:
  model_mendis_poisson <- glm(total_diseases ~ age, family = poisson, data=men.diseases)
  summary(model_mendis_poisson) 
  
# Check model: 
  autoplot(model_mendis_poisson)
  
# Compensate for overdispersion by using quasipoisson model: 
  model_mendis_quasi <- glm(total_diseases ~ age, family = quasipoisson, data = men.diseases)
  summary(model_mendis_quasi)

# Check model: 
  autoplot(model_mendis_quasi) 

  
# What about with women?
  ggplot(women.diseases, aes(age, total_diseases)) +
    geom_smooth(method="glm", method.args=list(family="poisson"(link="log")), color="deeppink") +
    ylab("Number of Chronic Diseases She Has") +
    xlab("Woman's Age") +
    ggtitle("Womens Age and Chronic Diseases")+
    theme_light()
  
# Histogram:
  ggplot(data=women.diseases, aes(total_diseases)) + 
    geom_histogram(color="deeppink", fill="lightpink") +
    theme_light()+
    ylab("Count (Women)")+
    xlab("Total Number of Chronic Diseases an Individual Has")+
    geom_vline(aes(xintercept=mean(total_diseases)),color="black", linetype="dashed", size=1)

# Poisson model:
  model_womdis_poisson <- glm(total_diseases ~ age, family = poisson, data=women.diseases)
  summary(model_womdis_poisson)  
  
# Check model: 
  autoplot(model_womdis_poisson)
  
# Compensate for overdispersion by using quasipoisson model: 
  model_womdis_quasi <- glm(total_diseases ~ age, family = quasipoisson, data = women.diseases)
  summary(model_womdis_quasi)
  
# Check model: 
  autoplot(model_womdis_quasi)  
  
# CARDIOVASCULAR DISEASES 
  
# Does the number of chronic cardiovascular disease a man gets in his lifetime increase with age?
  ggplot(men.diseases, aes(age, cardio_disease)) +
    geom_smooth(method="glm", method.args=list(family="poisson"(link="log")), color="darkblue") +
    ylab("Number of Chronic Cardiovascular Diseases He Has") +
    xlab("Man's Age") +
    ggtitle("Mens Age and Chronic Cardiovascular Diseases")+
    theme_light()
  
# Histogram:
  ggplot(data=men.diseases, aes(cardio_disease)) + 
    geom_histogram(color="darkblue", fill="lightblue")+
    theme_light()+
    ylab("Count (Men)")+
    xlab("Total Number of Chronic Cardiovascular Diseases an Individual Has")+
    geom_vline(aes(xintercept=mean(total_diseases)),color="black", linetype="dashed", size=1)
  
# Poisson model:
  model_mencar_poisson <- glm(cardio_disease ~ age, family = poisson, data=men.diseases)
  summary(model_mencar_poisson) 
  # Much better-dispersed than total diseases! 
  
# Check model: 
  autoplot(model_mencar_poisson)
  
# What about with women?
  ggplot(women.diseases, aes(age, cardio_disease)) +
    geom_smooth(method="glm", method.args=list(family="poisson"(link="log")), color="deeppink") +
    ylab("Number of Chronic Cardiovascular Diseases She Has") +
    xlab("Woman's Age") +
    ggtitle("Womens Age and Chronic Cardiovascular Diseases")+
    theme_light()
  
# Histogram:
  ggplot(data=women.diseases, aes(cardio_disease)) + 
    geom_histogram(color="deeppink", fill="lightpink")+
    theme_light()+
    ylab("Count (Women)")+
    xlab("Total Number of Chronic Cardiovascular Diseases an Individual Has")+
    geom_vline(aes(xintercept=mean(total_diseases)),color="black", linetype="dashed", size=1)
  
# Poisson model:
  model_womcar_poisson <- glm(cardio_disease ~ age, family = poisson, data=women.diseases)
  summary(model_womcar_poisson) 
  # Better dispersion than total diseases, but still rather overdispersed.
  
# Check model: 
  autoplot(model_womcar_poisson)
  
# Compensate for overdispersion by using quasipoisson model: 
  model_womcar_quasi <- glm(cardio_disease ~ age, family = quasipoisson, data = women.diseases)
  summary(model_womcar_quasi)
  
# Check model: 
  autoplot(model_womcar_quasi)
  