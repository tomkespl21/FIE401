########### FIE 401 exam ######################

rm(list=ls())
# library(ggplot2)
library(tidyverse)
library(stargazer)  # nice regression tables
# library(corrplot)
library(DescTools)  # winsorizing 
library(purrr)
library(lubridate)  # for ymd function
library(readr)
library(rvest)
library(xtable)     # descritpive stats table
library(psych)      
library(plm)    # panel regression
library(lmtest) # coeftest()
library(sandwich) 



# load data 
emissions <- read_rds("emissions.rds")

location <- read_rds("emissions_location.rds")

house <- read_delim('1976-2020-house.tab') %>%
  # Only select variables explained in the assignment
  select(year, state, state_po, state_fips, state_cen, state_ic, office, district,
         stage, runoff, special, candidate, party, writein, candidatevotes,
         totalvotes)

################# data work 

#change house colnames
#colnames(house) <- house[1,]
#house <- house[-1, ] 

# only report from 107th congress to 116th 
emissions <-
  emissions %>%
  # Filter chemicals that are regulated under the Clean Air Act
  filter(cleanAirActIndicator == 'YES') %>%
  rename(stateAbb = facilityState) %>%
  # Exclude Virgin Islands
  filter(stateAbb %in% house$state_po)

# # Check if facilityID is unique
# emissions %>%
#   mutate(facilityIDandZIP = paste(facilityID, facilityZIP) %>%
#            as.factor(),
#          facilityID = as.factor(facilityID)) -> x
# # They are 1:1

house <-
  house %>%
  # Exclude special elections
  filter(special == FALSE & 
           # Exclude write in candidates
           writein == FALSE) %>%
  group_by(year, district, state) %>%
  # Calculate percentage of votes for the candidate in the district
  mutate(percentageFor = candidatevotes/totalvotes,
         # Determine the winner
         winner = ifelse(percentageFor == max(percentageFor), 1, 0),
         # only a dffierence of 5% between winning versus runner-up
         difference = max(percentageFor) - percentageFor,
         marginal.x = ifelse(difference < 0.05 & difference != 0, 1, 0)) %>% 
  mutate(marginal = sum(marginal.x),
         state = str_to_title(state)) %>%
  # change grams to pounds
  #mutate(emissionAir = ifelse(unitOfMeasurement=="Grams",
  #                                  emissionAir*0.002205),
  #                                  emissionAir) %>% 
  
  ungroup() %>%
  filter(winner == 1) %>%
  rename(statename = state,
         stateAbb = state_po) %>%
  # Add year variable 
  mutate(y1 = year + 1,
         y2 = y1 + 1) %>%
  pivot_longer(cols = y1:y2, names_to = 'x', values_to = 'reportingYear') %>%
  select(-x) %>%
  select(statename, stateAbb, district, reportingYear, marginal, party)

## LOCATION --------------------------------------------------------------------

# Add year variable to location
# Representatives that serve from 3rd of January 2021 to 2023, referred
# to as the 117th congress
# 2021 - 117*2 = 1787
# year1 = congress * 2 + 1787 ; year2 = year1 + 1

location <-
  location %>%
  mutate(y1 = congress*2 + 1787,
         y2 = y1 + 1) %>%
  pivot_longer(cols = y1:y2, names_to = 'x', values_to = 'reportingYear') %>%
  select(-x)

## MERGE -----------------------------------------------------------------------

data <- left_join(emissions, location) %>%
  left_join(., house)



## clean up data --------------------

data <- 
  data %>% 
  drop_na()




#------------------------Winsorize
data$emissionAir <- Winsorize(data$emissionAir, probs = c(0.005, 0.995), na.rm=T)
data$prodIndexChange <- Winsorize(data$prodIndexChange, probs = c(0.005, 0.995), na.rm=T)
data$prodIndexNormLevel <- Winsorize(data$prodIndexNormLevel, probs = c(0.005, 0.995), na.rm=T)


sum <- str(data)
sum2 <- summary(data)
sum3 <- describe(data)

#sum2=subset(sum3, select=c("n", "mean", "median", "sd", "min", "max"))
sum3=subset(sum3, select=c("n", "mean", "median", "sd", "min", "max"))

xtable(sum2)
xtable(sum3)

stargazer(sum3)



### task 2 regressions -----------------


# plain regression 
reg1 <- lm(emissionAir~party+prodIndexChange,data = data)

data$id <- paste(data$facilityID, data$chemicalID, sep="_")
#data$id <- group_indices(my.data, st_name, race)    

# declare panel data set
panel <- pdata.frame(data,index = c("id","reportingYear"))
# check
pdim(panel)


reg1 <- plm(emissionAir~party+prodIndexChange ,data = panel,
                               effect = "individual",
                               model = "within")

robust_se1 <- coeftest(reg1, vcov=vcovHC(reg1,cluster="group", type="sss"))[,2]

fixef(reg1,effect="individual")
# Are detected changes because firms change output and hence pollution,
# or do firms change pollution for a given output level?

# --> thats why we control for that 


# model 2 : individual effect
reg2 <- plm(emissionAir~party+prodIndexChange+statename ,data = panel,
            effect = "individual",
            model = "within")
robust_se2 <- coeftest(reg2, vcov=vcovHC(reg2,cluster="group", type="sss"))[,2]
fixef(reg2,effect="individual")



# model 3 : individual and time effect

reg3 <- plm(emissionAir~party+prodIndexChange+statename ,data = panel,
            effect = "twoway",
            model = "within")
robust_se3 <- coeftest(reg3, vcov=vcovHC(reg3,cluster="group", type="sss"))[,2]
fixef(reg4,effect="individual")
fixef(reg4,effect="time")


# Note that robust_se3 might be not computed as you are likely to run out of memory

# running a panel regression with twoway fixed effects
# is basically implicit difference-in-difference approach

