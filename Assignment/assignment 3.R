# Assignment 3 

rm(list = ls())

library(tidyverse)
library(stargazer)
library(lubridate)
library(AER)
library(stargazer)


#read in data 
data <- read_csv("Finlit_new.csv")


# manipulate data:

unique(data$age)
unique(data$f10)
unique(data$f15)






# data <- 
#   data %>% 
#   filter(!(b1 == "Ik wil het niet zeggen" | b1 == "Ik weet het niet")) %>% 
#   mutate(male = as.numeric(ifelse(male == "male",1,0)),
#          partner = as.numeric(ifelse(partner =="Ja",1,0)),
#          age = ifelse(age=="<30",0,
#                       ifelse(age=="30s",1,
#                              ifelse(age=="40s",2,
#                                     ifelse(age=="50s",3,4)))),
#          f10 = ifelse(f10 == "worse",0,
#                       ifelse(f10=="no sibling",1,2)),
#          f15 = ifelse(f15 == "intermediate or high",0,
#                       ifelse(f15 =="low",1,2)),
#          b1 = ifelse(b1 == "1 zeer slecht",0,
#                      ifelse(b1 =="2",1,
#                             ifelse(b1=="3",2,
#                                    ifelse(b1=="4",3,
#                                           ifelse(b1=="5",4,
#                                                  ifelse(b1=="6",5,6))))))) %>% 
#  drop_na()



  
  
summary(data)  

# boxplot(data$b1)
# # boxplots for numerical variables
# boxplot(data$numkids) # might drop two observations with 7 and 6 children 
# boxplot(data$lincome)
# boxplot(data$indexlit1_new1)
# boxplot(data$indexlit2_new1)





########################### models #################################

# "naive" regression model

naive_reg <- lm(dstocks_mut~indexlit1_new1+age+edu2+edu3+edu4+edu5+
                            edu6+male+partner+numkids+
                            retired+dum_selfempl+lincome+
                            tot_non_equity_wealth_cat+indexlit2_new1,data = data)

summary(naive_reg)




# 1.stage regressions 
fsreg1 <- lm(indexlit1_new1~f10+f15+age+edu2+edu3+edu4+edu5+edu6+male+partner+numkids+
               retired+dum_selfempl+lincome+tot_non_equity_wealth_cat+indexlit2_new1
               ,data = data)



# 2.stage regressions 

iv_fit_11I <- ivreg(dstocks_mut~indexlit1_new1+age+edu2+edu3+edu4+edu5+
                      edu6+male+partner+numkids+
                      retired+dum_selfempl+lincome+
                      tot_non_equity_wealth_cat+indexlit2_new1|f10+f15+age+edu2+edu3+edu4+edu5+
                      edu6+male+partner+numkids+
                      retired+dum_selfempl+lincome+
                      tot_non_equity_wealth_cat+indexlit2_new1,data = data)


summary(iv_fit_11I)

# literacy index increasing by 1 increases the probability of 
# participation by 18,36% 
# --> iv regression gives us correct slope of 0.1836 , different to the ols one.

#  statistically significantly different from 0 on 5% niveau
# economically obv. very relevant and significant/important 
 
# there may also
# be learning and improvement in knowledge (and familiarity with the questions asked in the module) via
# participation in the stock market. This alternative argument leads to OLS estimates that are biased upward

# task 3 
# What reasons do the authors name for using an Instrumental Variable approach?

# endogoneity , because measurement error in literacy index 
# responses are imprecise and may result from guessing
#  there may also
# be learning and improvement in knowledge --> upward bias 
 

# How do the authors abstract the estimates obtained using OLS and IV?

# 461 page --> explain different categorics of variables 


# How do the authors discuss relevancy? (F-statistic)
# The first-stage regressions reported in Table 8A show
# that not only are our instruments statistically significant
# but the F-statistics are high and above or in range with the
# value recommended to avoid the weak instruments problem (Staiger and Stock, 1997; Bound, Jaeger, and Baker,
# 1995).



# The estimates in the second stage reported in Table 8B
# show that the relationship between literacy and stock
# market participation remains positive and statistically significant in the Generalized Method of Moments (GMM)
# regression. Moreover, the exogeneity test is not rejected.
# Thus, the OLS estimates do not differ significantly from the
# GMM estimates. The results of the Hansen J-test show that
# the over-identifying restrictions are also not rejected



stargazer(naive_reg,fsreg1,iv_fit_11I,title="Results", align=TRUE)
stargazer(naive_reg,fsreg2,iv_fit_12I,title="Results",align=TRUE)












