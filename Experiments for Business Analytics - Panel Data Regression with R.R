#**************************************************************
#BUS160 - Experiments for Business Analytics
#Igor Jose Moreira Marques - 190580124
#Assessed Coursework - Prof. Georg Von Graevenitz
#**************************************************************

rm(list=ls())
setwd('/Users/igorm/OneDrive/Queen Mary University of London/BUS160 - Experiments for Business Analytics/Coursework/Working From Home (Bloom et. al)/Datasets')

library(haven)
library(stargazer)
library(ggpubr)
library(lattice)
library(stats)
library(lfe)
library(plm)
library(Hmisc)
library(magrittr)

#Loading datasets
exhaustion <- read_dta("Exhaustion.dta")

#Converting dataset to a data frame
exhaustion <- data.frame(exhaustion)

#Initial data exploration
stargazer(exhaustion, type="text", title="Exhaustion Dataset - Summary Statistics", out = "Summary.txt")

#creating varible to identify the experiment timeline (before and after experiment started)
exhaustion <- within(exhaustion, {period <- ave(experiment_expgroup,year_week,FUN=max)})

#Creating label for gender
exhaustion <- within(exhaustion, {gender <- ifelse(men==1, "male", "female")})

#Creating label for experiment groups
exhaustion <- within(exhaustion, {expgroup_label <- ifelse(expgroup==1, "treatment", "control")})

#Creating a variable that contains the week number for each observation
exhaustion <- within(exhaustion, {week <- as.numeric(substr(year_week,5,6))})

#Converting variables to object (dummy variables)
exhaustion$personid <- factor(exhaustion$personid)
exhaustion$week <- factor(exhaustion$week)
exhaustion$year_week <- factor(exhaustion$year_week)

#Plotting the distribution of some variables

#Verifying if distribution of exhaustion levels of treatment and control group before the experiment are similar
histogram(~ exhaustion | expgroup_label, data = subset(exhaustion, period==0), nint = 30)

densityplot(~ exhaustion | expgroup_label, data = subset(exhaustion, period==0), layout=c(1,2),
            panel=function(x,...){
              panel.densityplot(x,...)
              panel.abline(v=quantile(x,.5), col.line="red")
              panel.abline(v=mean(x), col.line="green")
            })

#Exhaustion Levels Distribution
g1 <- ggplot(data = exhaustion, aes(x=exhaustion)) + 
  geom_histogram(aes(y=..density..), bins = 20, color="blue", fill="white") +
  geom_density(alpha=.1, fill="blue") + 
  ggtitle("Exhaustion Levels Distribution") +
  labs(x="Exhaustion")
print(g1)

#Exhaustion Levels Distribution - Natural Logarithm Transformed
g2 <- ggplot(data = exhaustion, aes(x=lnexhaustion)) + 
  geom_histogram(aes(y=..density..), bins = 20, color="blue", fill="white") +
  geom_density(alpha=.1, fill="blue") + 
  ggtitle("Exhaustion Levels Distribution - Natural Logarithm Transformed") +
  labs(x="Exhaustion (ln-transformed)")
print(g2)

#Weekly Average of Exhaustion Levels
g3 <- ggplot(data=exhaustion, aes(group=expgroup_label)) +
  geom_point(data=subset(exhaustion, period==0),
             aes(x=year_week, y=exhaustion), color="orange", alpha=0.4) +
  geom_point(data=subset(exhaustion, period==1),
             aes(x=year_week, y=exhaustion), color="lightblue", alpha=0.5) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  stat_summary(data=subset(exhaustion, expgroup_label=="control" & gender=="male"), 
               aes(x=year_week, y=exhaustion, colour="Men - Control"), fun.y=mean, geom="line", lwd=1.5) +
  stat_summary(data=subset(exhaustion, expgroup_label=="control" & gender=="female"), 
               aes(x=year_week, y=exhaustion, colour="Women - Control"), fun.y=mean, geom="line", lwd=1.5) +
  stat_summary(data=subset(exhaustion, expgroup_label=="treatment" & gender=="male"), 
               aes(x=year_week, y=exhaustion, colour="Men - Treatment"), fun.y=mean, geom="line", lwd=1.5) +
  stat_summary(data=subset(exhaustion, expgroup_label=="treatment" & gender=="female"), 
               aes(x=year_week, y=exhaustion, colour="Women - Treatment"), fun.y=mean, geom="line", lwd=1.5) +
  ggtitle("Weekly Average of Exhaustion Levels") + 
  labs(x="Week", y="Exhaustion")
print(g3)

#Age Distribution - Exhaustion Dataset
g4 <- ggplot(data = exhaustion, aes(x=age)) + 
  geom_histogram(aes(y=..density..), bins = 15, color="red", fill="white") +
  geom_density(alpha=.1, fill="red") + 
  ggtitle("Age Distribution - Exhaustion Dataset") +
  labs(x="Age (years)")
print(g4)

#Tenure Distribution - Exhaustion Dataset
g5 <- ggplot(data = exhaustion, aes(x=tenure)) + 
  geom_histogram(aes(y=..density..), bins = 20, color="green", fill="white") +
  geom_density(alpha=.1, fill="green") + 
  ggtitle("Tenure Distribution - Exhaustion Dataset") +
  labs(x="Tenure (months)")
print(g5)

#Gross Wage Distribution - Exhaustion Dataset
g6 <- ggplot(data = exhaustion, aes(x=grosswage)) + 
  geom_histogram(aes(y=..density..), bins = 20, color="blue", fill="white") +
  geom_density(alpha=.1, fill="blue") + 
  ggtitle("Gross Wage Distribution - Exhaustion Dataset") +
  labs(x="Gross Wage (1000 yaun)")
print(g6)

#Commute Distance Distribution - Exhaustion Dataset
g7 <- ggplot(data = exhaustion, aes(x=commute)) + 
  geom_histogram(aes(y=..density..), bins = 20, color="orange", fill="white") +
  geom_density(alpha=.1, fill="orange") + 
  ggtitle("Commute Distance Distribution - Exhaustion Dataset") +
  labs(x="Commute (min)")
print(g7)

#Correlation matrix
exhaustion_num <- exhaustion[c(2,5,8,9,10,13)]
cor(exhaustion_num)

#Models

#Hypothesis 1 (original model)

#OLS Modelling
m_ols <- lm(exhaustion ~ expgroup + period + experiment_expgroup, data=exhaustion)
m_ln_ols <- lm(lnexhaustion ~ expgroup + period + experiment_expgroup, data=exhaustion)

#Panel Data Modelling - Fixed and Time Effects
m_felm <- felm(exhaustion ~ expgroup + period + experiment_expgroup | personid + week, data=exhaustion)
m_ln_felm <- felm(lnexhaustion ~ expgroup + period + experiment_expgroup | personid + week, data=exhaustion)

stargazer(m_ols, m_ln_ols, m_felm, m_ln_felm, type="text", align = TRUE, out = "h1_original.txt")

#Hypothesis 2 (introducing gender variable)

#OLS Modelling
m1_ols <- lm(exhaustion ~ expgroup + period + experiment_expgroup + men, data=exhaustion)
m2_ols <- lm(exhaustion ~ expgroup + period + experiment_expgroup + men + experiment_expgroup*men, data=exhaustion)

stargazer(m1_ols, m2_ols, type="text", align = TRUE)

#Panel Data Modelling - Fixed and Time Effects

m1_felm <- felm(exhaustion ~ expgroup + period + experiment_expgroup + men + experiment_expgroup*men | personid, data = exhaustion)
m2_felm <- felm(exhaustion ~ expgroup + period + experiment_expgroup + men + experiment_expgroup*men | week, data = exhaustion)
m3_felm <- felm(exhaustion ~ expgroup + period + experiment_expgroup + men + experiment_expgroup*men | week + personid, data = exhaustion)

stargazer(m1_felm, m2_felm, m3_felm, type="text", align = TRUE)

#Model Comparisons

stargazer(m2_ols, m3_felm, m1_felm, m2_felm,  type="text", align = TRUE, out="final_com.txt")
stargazer(m2_ols, m3_felm,  type="text", align = TRUE, out="final_com.txt") #table for the report

