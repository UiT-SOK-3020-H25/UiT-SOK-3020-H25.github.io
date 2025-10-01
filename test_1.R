

#' ## Chapter 7 - Using Indicator Variables

library(mosaic)

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/utown.def")
#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/utown.rdata"))
#' Table 7.1
head(utown)
noquote(names(utown))

# price	house price, in $1000
# sqft		square feet of living area, in 100's
# age		house age, in years
# utown	=1 if close to university
# pool		=1 if house has pool
# fplace	=1 if house has fireplace


###################################

fit0 <- lm(price~1, data = utown)
summary(fit0)


fit <- lm(price~utown+sqft, data = utown)
summary(fit)

# Q1. How do we inteprete the coefficient of utown? What is the reference group?

# chnage the reference group 
utown$Lutown= 1-utown$utown

fit1 <- lm(price~ Lutown+sqft, data = utown)
summary(fit1)
# Q2. How do we inteprete the coefficient of Lutown? What is the reference group?

# What will happen if we include both utown and Lutown in the model 
fit2 <- lm(price~0+utown+Lutown+sqft, data = utown)
summary(fit2)



