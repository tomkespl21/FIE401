# Finacial Econometrics Assignment 1 

rm(list=ls())

# libraries
library(tidyverse)
library(lubridate) # for ymd function
library(gridExtra) # for graphs
library(DescTools) # for winsorize() function 
library(stargazer) # nice tables
library(RColorBrewer) # graphs
library(corrplot) 



# load in data 
load("CAR_M&A.RData")

mytheme <- 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# first look at variables
summary(CAR_MA)
# already can see quite different means compared to medians 
# also if max values are far away from 3. quantile
# this is extremely the case for deal_value and bidder size 

# change variable to date type
CAR_MA$yyyymmdd <- ymd(CAR_MA$yyyymmdd)


# check for outliers:
# does not really make sense to check for outliers in dummy variables 
# therefore checking the other variables by plots 

# Deal_value:
plot1 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyy,y=deal_value))+
  geom_point()+
  geom_point(data = CAR_MA[2558,],color="red")

ggplot(CAR_MA, aes(deal_value)) +
  geom_histogram(bins = 30, fill = brewer.pal(11,'PiYG')[2]) +
  mytheme # MA

plot2 <- 
  CAR_MA %>%   
  ggplot(aes(deal_value))+
  geom_histogram(bins = 30,
                 fill = brewer.pal(11,'PiYG')[2]) +
  mytheme # M

grid.arrange(plot1, plot2, ncol=2)
# just by looking at the scatterplot we indicate a huge outlier 
# This could be an error but nonetheless should be removed from the dataset   
# overall there seem to be other "smaller" outliers and we therefore have to winsorize or use log-transformation 
# because of the shape of the historgram we decided to use log-transformations
  


# bidder size
plot3 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyy, y=bidder_size))+
  geom_point()

plot4 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),bidder_size))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot3,plot4,ncol=2)

# there are outliers in the data 
# makes sense to winsorize or log-transform



#relsize
plot11 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyymmdd, y=relsize))+
  geom_point()

plot12 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),relsize))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot11,plot12,ncol=2)

# are kind of outliers 
# but seems fine and bounded at 12,5

plot13 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyy, y=bidder_mtb))+
  geom_point()

plot14 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),bidder_mtb))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot13,plot14,ncol=2)



# adjust data 
CAR_MA <- 
  CAR_MA %>%
  rename(year = yyyy) %>% 
  slice(-(2558)) %>%                    # to get rid of extreme outlier
  mutate(deal_value = log(deal_value),
         bidder_size = log(bidder_size))


# we decided to log-transform the data instead of winsorizing 
# both certainly solve the outlier problem 
# log-transformation seemed more natural and looks more normal-distributed 

# as we see now, log-tranformation solved the outlier problem 
plot1 <- 
  CAR_MA %>% 
  ggplot(aes(x=year,y=deal_value))+
  geom_point()


plot2 <- 
  CAR_MA %>%   
  ggplot(aes(deal_value))+
  geom_histogram(bins = 30,
                 fill = brewer.pal(11,'PiYG')[2]) +
  mytheme # M

grid.arrange(plot1, plot2, ncol=2)
 

# descriptive table 1 
summary1 <- 
  CAR_MA %>% 
  arrange(yyyymmdd) %>% 
  group_by(year) %>%
  mutate(share_private = mean(private),
         share_stock = mean(all_stock)) %>% 
  summarise(avg_deal_size = round(mean(deal_value), digits= 3),
            avg_bidCAR = round(mean(carbidder), digits=3),
            avg_share_private = round(mean(share_private), digits=3),
            avg_share_stock = round(mean(share_stock), digits = 3))

stargazer(summary1,summary = FALSE, type="latex",header = "summary")



# descriptive table 2 
vars.for.model <- names(CAR_MA[!names(CAR_MA) %in% c('yyyymmdd','yyyy', 
                                                     'private', 'all_stock')])

difference.means <- data.frame(matrix(NA,    # Create empty data frame
                                      nrow = 4,
                                      ncol = length(vars.for.model)))

row.names(difference.means) <- c('all.stock', 'not.all.stock', 'differece', 
                                 't-test')
names(difference.means) <- vars.for.model

difference.means[1,] <- lapply(CAR_MA[CAR_MA$all_stock == 1,
                                      names(CAR_MA) %in% vars.for.model], mean)

difference.means[2,] <- lapply(CAR_MA[CAR_MA$all_stock == 0,
                                      names(CAR_MA) %in% vars.for.model], mean)

difference.means[3,] <- difference.means[1,] - difference.means[2,]

for (v in vars.for.model){
  pval <- t.test(CAR_MA[CAR_MA$all_stock == 1,v],
                 CAR_MA[CAR_MA$all_stock == 0,v])$p.value
  difference.means[4,v] <- pval
}

stargazer(difference.means,summary=FALSE,type = "text")


# Check correlations for potential multicolliniarity
correlations <- cor(CAR_MA[!names(CAR_MA) %in% 'yyyymmdd'])
corrplot(correlations, col = COL2('PiYG'), tl.col = 'black')

# run regressions 

# Only public - no controls
model1 <- 
  lm(carbidder ~ all_stock, CAR_MA[CAR_MA$public == 0,]) 
summary(model1)

# Only private - no controls
model2 <- 
  lm(carbidder ~ all_stock,
     CAR_MA[CAR_MA$public == 1,]) 
summary(model2)



# Full sample - no controls
model3 <- 
  lm(carbidder ~ all_stock +
                 public +
                 I(all_stock*public),
                 CAR_MA) 
summary(model3)


# Only public - controls
model4 <- 
  lm(carbidder ~ all_stock +
       deal_value +
       bidder_size +
       bidder_mtb + 
       run_up_bidder +
       bidder_fcf +
       bidder_lev +
       sigma_bidder + 
       relsize +
       horz +
       tender_offer +
       year+
       hostile,
     CAR_MA[CAR_MA$public == 0,])
summary(model4)


# Only private - controls
model5 <- 
  lm(carbidder ~ all_stock +
       deal_value +
       bidder_size +
       bidder_mtb + 
       run_up_bidder +
       bidder_fcf +
       bidder_lev +
       sigma_bidder + 
       relsize +
       horz +
       tender_offer +
       year+
       hostile,
     CAR_MA[CAR_MA$public == 1,])
summary(model5)


# Full sample - controls
model6 <-
  lm(carbidder ~ all_stock +
                 public +
                 I(all_stock*public) +
                 deal_value +
                 bidder_size +
                 bidder_mtb +
                 run_up_bidder + 
                 bidder_fcf +
                 bidder_lev +
                 sigma_bidder +
                 relsize +
                 horz + 
                 tender_offer +
                 year+
                 hostile,
                 CAR_MA)
summary(model6)








stargazer(model1, model2, model3, model4, model5, model6,
          keep.stat = c('n','adj.rsq'),
          type = 'latex', header = FALSE, digits = 3,
          notes.append = TRUE, notes.align = 'l', font.size = 'small',
          notes = 'Notes with explanations?')


















































  
  
  

