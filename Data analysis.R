library(emmeans)
library(lmerTest)

library(lme4)
library(plyr)
library(ggplot2)
options(scipen=999)


m1 = glmer(accuracy ~ pronoun_form * stimuli_type + 
             (1|subjects) + (1|item),
           family = binomial(),
           data = Pronouns_data_all)

summary(m1)

full_data = subset(Pronouns_data_all, pronoun_form=="full") #subset of only full form pronouns

reduced_data = subset(Pronouns_data_all, pronoun_form=="reduced") #subset of only reduced form pronouns


t.test(full_data$accuracy, mu=0.5, alt="g") 


t.test(reduced_data$accuracy, mu=0.5, alt="g") 


m2 = glmer(accuracy ~ pronoun_type + 
             (1|subjects) + (1|item),
           family = binomial(),
           data = My_data_all)


summary(m2)