## ----setup, include=FALSE------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ---- results='asis', message = FALSE------------------------------------------------------------------------------------
# SETUP ------------------------------------------------------------------------

## Data ------------------------------------------------------------------------

portfolio_size <- read.csv("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/equity_size_portfolios_monthly_vw.txt")
portfolio_bm <- read.csv("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/book_market_portfolios_monthly_vw.txt")
portfolio_momentum <- read.csv("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/pricing_factors_monthly.txt")


## Libraries -------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(stargazer)
library(corrplot)
library(tidymodels)
library(knitr)
library(DescTools) # winsorizing 
library(purrr)
library(lubridate) # for ymd function

mytheme <- 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Extract code from Rmd
# knitr::purl(input = 'Assignment2_group_08.Rmd',
#             output = 'Assignment2_group_08.R')


## ----data-preparation----------------------------------------------------------------------------------------------------
# (We made a decision to not winsorize returns; results were not affected) 

# # Winsorizing
# library(DescTools)
# 
# portfolio_size <-
#   portfolio_size %>%
#   mutate(X1..small.size.= Winsorize(X1..small.size., probs = c(0.01, 0.99)))
# 
# 
# portfolio_bm <-
#   portfolio_bm %>%
#   mutate(X2 = Winsorize(X2, probs = c(0.01,0.99)),
#          X9 = Winsorize(X9, probs = c(0.01,0.99)))

# Transform to date format
portfolio_bm$date <- ymd(portfolio_bm$date)
portfolio_size$date <- ymd(portfolio_size$date)
portfolio_momentum$date <- ymd(portfolio_momentum$date)

# range(portfolio_bm$date)

# Rename columns
colnames(portfolio_size)[-c(1:2,11)] <- paste('SIZE', 2:9)
colnames(portfolio_size)[2] <- 'Small.size.1'
colnames(portfolio_size)[11] <- 'Large.size.10'

colnames(portfolio_bm)[-c(1:2,11)] <- paste('BM', 2:9)
colnames(portfolio_bm)[2] <- 'Low.bm.1'
colnames(portfolio_bm)[11] <- 'High.bm.10'

# Combine data
inner_join(portfolio_size,portfolio_bm,by = 'date') %>% 
  inner_join(portfolio_momentum,by = 'date') %>%
  as.data.frame() -> data


## ----exploratory-analysis, results='asis'--------------------------------------------------------------------------------
# SETUP ------------------------------------------------------------------------

## Exploratory analysis --------------------------------------------------------

### Factor correlations --------------------------------------------------------

portfolio_momentum %>%
  select(-date) %>%
  drop_na() %>%
  cor() %>%
  corrplot(col = COL1('YlOrRd'), method = 'number', tl.col = 'black',
           mar=c(0,0,5,0))
mtext('Figure 1: Factor correlation matrix')

### Size portfolios ------------------------------------------------------------

# Summary statistics
stargazer(portfolio_size[!names(portfolio_size) %in% 'date'],
          type = 'latex', header = FALSE,
          title = 'Summary statistics size portfolios',
          summary.stat = c('min','p25','mean','p75', 'max','sd'))

# Density plot of returns of small and big portfolios
portfolio_size %>%
  pivot_longer(Small.size.1:Large.size.10,
               names_to = 'Portfolio',
               values_to = 'Returns') %>%
  filter(Portfolio == 'Small.size.1' |
           Portfolio == 'Large.size.10')  %>%
  ggplot() +
  geom_density(aes(Returns,
                   fill = Portfolio,
                   color = Portfolio),
               alpha = 0.6,
               size = 1)+
  mytheme +
  labs(x = 'Portfolio return desity',
       y = NULL,
       title = 
         'Figure 2: Density plot returns of small and large size portolios') +
  scale_fill_manual(values = c('orange', 'grey'),
                    name = 'Portfolio',
                    breaks = c('Small.size.1', 'Large.size.10'),
                    labels = c('Small size', 'Large size')) +
  scale_color_manual(breaks = c('Small.size.1', 'Large.size.10'),
                     values = c('orange', 'grey'),
                     guide = NULL) +
  theme(axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

### Book-to-market portfolios --------------------------------------------------

# Summary statistics
stargazer(portfolio_bm[!names(portfolio_bm) %in% 'date'],
          type = 'latex', header = FALSE,
          title = 'Summary statistics book-to-market portfolios',
          summary.stat = c('min','p25','mean','p75', 'max','sd'))

# Density plot of returns of low and high portfolios
portfolio_bm %>%
  pivot_longer(Low.bm.1:High.bm.10,
               names_to = 'Portfolio',
               values_to = 'Returns') %>%
  filter(Portfolio == 'Low.bm.1' |
           Portfolio == 'High.bm.10')  %>%
  ggplot() +
  geom_density(aes(Returns,
                   fill = Portfolio,
                   color = Portfolio),
               alpha = 0.6,
               size = 1) +
  mytheme +
  labs(x = 'Portfolio return density',
       y = NULL,
       title = 'Figure 3: Density plot returns of low and high BM portolios') +
  scale_fill_manual(values = c('orange', 'grey'),
                    name = 'Portfolio',
                    breaks = c('Low.bm.1', 'High.bm.10'),
                    labels = c('Low book-to-market', 'High book-to-market')) +
  scale_color_manual(breaks = c('Low.bm.1', 'High.bm.10'),
                     values = c('orange', 'grey'),
                     guide = NULL) +
  theme(axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

### Momentum portfolio ---------------------------------------------------------

stargazer(portfolio_momentum[!names(portfolio_momentum) %in% 'date'],
          type = 'latex', header = FALSE,
          title = 'Summary statistics momentum portfolio',
          summary.stat = c('min','p25','mean','p75', 'max','sd'))

rm(portfolio_momentum, portfolio_size, portfolio_bm)


## ----size-momentum-regressions, results='asis'---------------------------------------------------------------------------
reg_size <- data[c(2:11,25)] %>%
  drop_na()

reg_size %>%
  select(-UMD) %>%
  map_dbl( ~ lm(.x ~ UMD, data = reg_size)$coefficients[2]) -> betas_size

reg_size %>%
  select(-UMD) %>%
  colMeans() -> means_size

bind_rows(betas_size, means_size) %>%
  t() %>%
  as.data.frame() %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point(size = 4, alpha = 0.7, color = 'orange') + 
  geom_smooth(method = 'lm', 
              formula = y ~ x, 
              fill = 'lightgrey', color = 'grey') +
  labs(x = 'Beta',
       y = 'Mean returns',
       title = 'Figure 4: Size portfolio, momentum betas and returns') +
  mytheme

reg_fmb_size <- data[c(2:11)] %>%
  t() 

colnames(reg_fmb_size) <- as.character(data$date)
reg_fmb_size <- cbind(betas_size, reg_fmb_size)  %>%
  as.data.frame()

reg_fmb_size %>%
  select(-betas_size) %>%
  map_dbl( ~ lm(.x ~ betas_size, 
                data = reg_fmb_size)$coefficients[2]) -> gammas_size

# gamma_lm <- lm(gammas_size ~ 1)
# 
# stargazer(gamma_lm, type = 'latex', header = FALSE,
#           title = 'Gamma significance test: Size')

t.test(gammas_size, alternative = 'greater', mu = 0) %>%
  tidy() %>%
  knitr::kable(., caption = 'Gamma significance test: Size')


## ----bm-momentum-regressions, results='asis'-----------------------------------------------------------------------------

reg_bm <- data[c(12:21,25)]

reg_bm %>%
  select(-UMD) %>%
  map_dbl( ~ lm(.x ~ UMD, data = reg_bm)$coefficients[2]) -> betas_BM

reg_bm %>%
  select(-UMD) %>%
  colMeans() -> means_BM

bind_rows(betas_BM, means_BM) %>%
  t() %>%
  as.data.frame() %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point(size = 4, alpha = 0.7, color = 'orange') + 
  geom_smooth(method = 'lm', 
              formula = y ~ x, 
              fill = 'lightgrey', color = 'grey') +
  labs(x = 'Beta',
       y = 'Mean returns',
       title = 'Figure 5: BM portfolios, momentum betas and returns') +
  mytheme

reg_fmb_bm <- data[c(12:21)] %>%
  t() 

colnames(reg_fmb_bm) <- as.character(data$date)
reg_fmb_bm <- cbind(betas_BM, reg_fmb_bm)  %>%
  as.data.frame()

reg_fmb_bm %>%
  select(-betas_BM) %>%
  map_dbl( ~ lm(.x ~ betas_BM, data = reg_fmb_bm)$coefficients[2]) -> gammas_BM

# gamma_lm <- lm(gammas_BM ~ 1)
# 
# stargazer(gamma_lm, type = 'latex', header = FALSE,
#           title = 'Gamma significance test: Book-to-market')

t.test(gammas_BM, alternative = 'greater', mu = 0) %>%
  tidy() %>%
  knitr::kable(., caption = 'Gamma significance test: Book-to-market')


