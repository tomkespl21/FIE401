# Finacial Econometrics Assignment 1 

rm(list=ls())

# libraries
library(tidyverse)
library(lubridate) # for ymd function
library(gridExtra) # for graphs
library(DescTools) # for winsorize() function 
library(stargazer) # nice tables



# load in data 
load("CAR_M&A.RData")

# first look at variables
summary(CAR_MA)
# already can see quite different means compared to medians 
# this gives a first indication of outliers


CAR_MA <- 
  CAR_MA %>% 
  mutate(yyyymmdd = ymd(yyyymmdd))

# does not really make sense to check for outliers in dummy variables 
# therefore checking the other variables by plots 

# Deal_value:
plot1 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyymmdd,y=deal_value))+
  geom_point()

plot2 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),deal_value))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot1, plot2, ncol=2)
# observation in row 2558 seems to be an outlier with double the amount of value compared to second highest 
# this in absolute values will probably effect the coefficients 
# therefore winsorizing and scaling could make sense 
  

# carbidder: 
plot3 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyymmdd, y=carbidder))+
  geom_point()

plot4 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),carbidder))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot3,plot4,ncol=2)

# does not seem too bad of outliers and scaling, 
# therefore no winsorising or sclaing

# bidder size
plot5 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyymmdd, y=bidder_size))+
  geom_point()

plot6 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),bidder_size))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot5,plot6,ncol=2)

# winsorizing and scaling makes sense

# sigma bidder
plot7 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyymmdd, y=sigma_bidder))+
  geom_point()

plot8 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),sigma_bidder))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot7,plot8,ncol=2)

# 3 highest values seem to outlie a bit 
# otherwise seems fine, not sure


# run-up-bidder
plot9 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyymmdd, y=run_up_bidder))+
  geom_point()

plot10 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),run_up_bidder))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot9,plot10,ncol=2)

# seems fine, already bounded


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

# seems fine, maybe scale

plot13 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyymmdd, y=bidder_mtb))+
  geom_point()

plot14 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),bidder_mtb))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot13,plot14,ncol=2)

# maybe one outliers ? 
# should winsorize and scale
# why all high bidder_mtb in year 2000 ? 



plot15 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyymmdd, y=bidder_fcf))+
  geom_point()

plot16 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),bidder_fcf))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot15,plot16,ncol=2)

# seems fine 
# already bounded




plot17 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyymmdd, y=bidder_lev))+
  geom_point()

plot18 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),bidder_lev))+
  geom_boxplot()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(plot17,plot18,ncol=2)

# seems fine 
# already bounded


# adjust data 
CAR_MA <- 
  CAR_MA %>% 
  mutate(deal_value = scale(deal_value),
         bidder_size = scale(bidder_size),
         bidder_mtb = scale(bidder_mtb),
         relsize = scale(relsize)) %>% 
  mutate(deal_value = Winsorize(deal_value, probs=c(0.01,0.99)),
         bidder_size = Winsorize(bidder_size, probs=c(0.01,0.99)),
         bidder_mtb = Winsorize(bidder_mtb, probs=c(0.01,0.99))) 


# descriptive table 1 
summary1 <- 
  CAR_MA %>% 
  arrange(yyyymmdd) %>% 
  group_by(yyyy) %>%
  mutate(share_private = sum(private)/n(),
         share_stock = sum(all_stock)/n()) %>% 
  summarise(avg_ds = round(mean(deal_value), digits=3),
            avg_bidCAR = round(mean(bidder_size), digits=3),
            avg_share_priv = round(mean(share_private), digits=3),
            avg_share_stock = round(mean(share_stock), digits = 3))

stargazer(summary1,summary = FALSE, type="text")



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



# run regressions 

# regression with private + public without controls
reg1 <- 
  lm(carbidder~all_stock, data = CAR_MA)
summary(reg1)

# regression with public without controls
reg2 <- 
  lm(carbidder~all_stock, data = CAR_MA[CAR_MA$public==1,])
summary(reg2)

# regression with private without controls
reg3 <- 
  lm(carbidder~all_stock+private, data = CAR_MA[CAR_MA$public==0],)
summary(reg3)

# regression with private + public and controls
reg4 <-
  lm(carbidder~all_stock+
       deal_value+
       bidder_size+
       bidder_mtb+
       run_up_bidder+
       bidder_fcf+
       bidder_lev+
       sigma_bidder+
       relsize+
       horz+
       tender_offer+
       hostile+
       I(all_stock*public),
       data = CAR_MA)
summary(reg4)

# regression with public and controls  
reg5 <-
  lm(carbidder~all_stock+
       deal_value+
       bidder_size+
       bidder_mtb+
       run_up_bidder+
       bidder_fcf+
       bidder_lev+
       sigma_bidder+
       relsize+
       horz+
       tender_offer+
       hostile,
     data = CAR_MA[CAR_MA$public ==1,])
summary(reg5)


# regression with private and controls 
reg6 <-
  lm(carbidder~all_stock+
       deal_value+
       bidder_size+
       bidder_mtb+
       run_up_bidder+
       bidder_fcf+
       bidder_lev+
       sigma_bidder+
       relsize+
       horz+
       tender_offer+
       hostile,
     data = CAR_MA[CAR_MA$public ==0,])
summary(reg6)




stargazer(reg1, reg2, reg3, reg4, reg5, reg6,
          keep.stat = c('n','adj.rsq'),
          type = 'latex', header = FALSE, digits = 3,
          notes.append = TRUE, notes.align = 'l', font.size = 'small',
          notes = 'Notes with explanations?')


















































  
  
  

