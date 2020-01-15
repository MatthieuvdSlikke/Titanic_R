options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(dplyr)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) 

#density plot age grouped by sex
t_age_sex <- titanic %>% filter(!is.na(Age) &!is.na(Sex)) %>% group_by(Sex) %>% 
  ggplot(aes(Age,y= ..count..,fill=Sex))+ geom_density(alpha=0.2)
t_age_sex 

#creating a data frame to store the mean age and standard deviation
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age)) 

# Q-Q plot with mean and standard deviation 
t_age <- titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(sample=Age))+ geom_qq(dparams = params) +geom_abline()
t_age

#survivals by sex, 0 being dead and 1 alive
t_survival_sex <- titanic %>% filter(!is.na(Survived)&!is.na(Sex)) %>% group_by(Sex) %>%
  ggplot(aes(Survived)) + geom_bar() + facet_grid(.~Sex)
t_survival_sex

#classify by age with a bar plot
t_survival_age <- titanic %>% filter(!is.na(Survived)&!is.na(Age)) %>% 
  mutate(classAge= case_when(.$Age>=70~"70-80",
                             .$Age>=50~"50-70",
                             .$Age>=30~"30-50",
                             .$Age>=18~"18-30",
                             .$Age>=10~"10-18",
                             .$Age>=0~"0-8"))%>%
  group_by(classAge) %>%
  ggplot(aes(Survived)) + geom_bar()+ facet_grid(.~classAge)
t_survival_age

#boxplot + point  to see how many people survived based on how much they paid
t_survival_fare <- titanic %>% filter(!is.na(Survived)&!is.na(Fare)&Fare!=0) %>%
  ggplot(aes(Survived,Fare)) + geom_boxplot() +geom_point(alpha= 1/20) +geom_jitter() 
t_survival_fare

#bar plot to see who survived based on their class
t_survival_pclass <- titanic %>% filter(!is.na(Survived)&!is.na(Pclass)) %>%
  ggplot(aes(Survived)) + geom_bar() + facet_grid(.~Pclass) 
t_survival_pclass


titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar() +
  ylab("Proportion")
# barplot of passenger class filled by survival with position_fill
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")
# Barplot of survival filled by passenger class with position_fill
titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")


#density of people who survived and died based on class, age and sex
t_surv <- titanic %>% filter(!is.na(Survived)&!is.na(Age)&!is.na(Sex)&!is.na(Pclass)) %>%
  ggplot(aes(Age,y=..count..,color=Survived))+ geom_density() +facet_grid(Sex~Pclass)
t_surv

