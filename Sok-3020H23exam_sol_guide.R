
rm(list = ls())
library(tidyverse)

# Load the data
heinz <- read_csv("https://raw.githubusercontent.com/oysteinm/data/main/heinz.csv")

# The following variables are in the data:
#' week: Week number (1 to 124).
#' sales: Weekly sales in US$ of Heinz ketchup.
#' price: Average weekly price in US$ of Heinz ketchup.
#' displayonly: A week in which the brand was on display promotion only,
#'               a dummy/indicator variable, 1 if so, 0otherwise.
#' coupononly: A week in which the brand had a coupon promotion only, 
#'               a dummy/indicator variable, 1 if so, 0 otherwise.
#' displaycoupon: A week in which the brand was both on display and had a coupon promotion, 
#'                 a dummy/indicatorvariable, 1 if so, 0 otherwise.

head(heinz)
# Data wrangling
heinz <- heinz %>%
  arrange(week) %>%  # ensure data is ordered by week
  mutate(
    #  Quantity variable
    quantity = sales / price,
    
    # Price in cents
    price = price * 100,
    
    # Difference in price in cents (dprice)
    dprice = price - lag(price, 1),
    
    # Log difference in price in cents (dlnprice)
    dlnprice = log(price) - log(lag(price, 1))
  ) %>%
  # Remove first row with missing lag values
  drop_na()

# Check first few rows
head(heinz)


######################################################
# Questions (related to Model 1) 
################################################

# Q1: Estimate the linear model (i.e., Model 1)
model1 <- lm(sales ~ dprice + displayonly + coupononly + displaycoupon, data = heinz)

# Q1. What signs would you expect, using economic reasoning, on the coeff. of the variables? 
summary(model1)

# Q2. How do you interpreter the estimated coefficient of each variables?
#     For example, coeff of dprice?  displayonly? Intercept?

# Interpretation of:

# coefficient of dprice:
# The coeff. on dprice is a **slope**. For 1 unit(cent) increase in dprice,
# the sales decrease by **-3.17 unit(dollars)**. 

# coefficient of displayonly:
# This is a **dummy** variable. During weeks when displayonly=1, the sales
# **increase** on average by **60.17 unit (dollars)** per week. 

# coefficient on (Intercept):
# The intercept represent the **average sales** in a week when 
# dprice, displayonly, coupononly and displaycoupon are **zero**.
# The unit of the intercept is **dollar** (i.e., the same as the unit of the dep.var)

# Q3. Test the hypothesis: H0: coeff of displayonly =0 (no effect). use 5%
# What test is appropriate to use? (t or F)? or p-value?
#Answer: Just look at the t-value or p-value from the summary table.
#       Reject H0 and conclude that the displayonly variable has 
#       significantly positive effect on sales. 


# Q 4. What proportion of the total variance in sales is explained
#    jointly by the variance in dprice, displayonly, coupononly, and displaycoupon?
names(summary(model1))
summary(model1)$r.squared

# Q5. Find a 95% CI on the average weekly sales in US dollars,
#    at a dprice changes by -5 cents, in weeks with no 
#    displayonly = 0, coupononly = 0, and displaycoupon = 1
library(mosaic)
f <- makeFun(model1)
f(-5, 0,0,1, interval= "confidence",level=0.95)


# Q6. Find the SE used in the calculations of the 95% CI above. And also
# find the 95% critical t-value. 
car::deltaMethod(model1, "b1-5*b2+b5", parameterNames = paste ("b",1:5, sep=""))
# t-value: 
# qt(1-alpha/2, df)
alpha = 0.05
1-alpha/2

# df 
names(model1)
model1$df.residual

qt(1-alpha/2, model1$df.residual)

# 7. Calculate the sales-price elasticity? Find the SE of the sales-price elasticity.
# Elast = slope*x/y = slope*dprice/sales
coef(model1)[2]*mean(heinz$dprice)/mean(heinz$sales)
# Is it elastic or inelastic?

# The SE is:
mean_dprice <- mean(heinz$dprice)
mean_sales <- mean(heinz$sales)
car::deltaMethod(model1, "b2*mean_dprice/mean_sales", parameterNames = paste ("b",1:5, sep=""))

# 8. From the sales model, drive the own-price elasticity? And also find the SE of it
mean_q <- mean(heinz$quantity)
car::deltaMethod(model1, "(b2/mean_q)-1", parameterNames = paste ("b",1:5, sep=""))



##################################################################
# Questions (related to Model 2) 
##################################################################

# Q1 : estimate Model 2: Log-log model
model2 <- lm(log(sales) ~ dlnprice+displayonly+coupononly+displaycoupon, data=heinz)

# Q1.What signs would you expect, using economic reasoning, on the coeff. of the variables? 
summary(model2)

# Q2. Interpret the estimated coefficient of each variables?
#   For example, coeff of dprice?  displayonly? 

# Coefficient of dlnprice:
# The coefficient on dlnprice is ** elasticity**. For 1 % **increase** in dlnprice,
# the sales **decrease* by **-3.29 %**.


# Coefficient of displayonly:
# This is **dummy** variable. During weeks when displayonly=1, the sales
# **increase** on average by 76.239 percentage.
exp(coef(model2)[3])-1
100*(exp(coef(model2)[3])-1)

car::deltaMethod(model2, "100*(exp(b3)-1)", parameterNames = paste ("b",1:5, sep=""))

# Q 3. What proportion of the total variance in  log sales is explained
#    jointly by the variance in dprice, displayonly, coupononly, and displaycoupon?
names(summary(model2))
summary(model2)$r.squared

# Q4. Find the generalized Rg^2 measure for model 2 in levels ( explained variation
#   in sales, not in log sales).
# Rg^2 = cor(sales, pred.sales)^2,
pred.sales.in.log <- predict(model2)
s2 <- deviance(model2)/model2$df.residual # the variance in the model 
# First, find the value of the correction factor
exp(s2/2)
pred.sales <- exp(pred.sales.in.log)*exp(s2/2)

# Rg^2 
cor(heinz$sales, pred.sales)^2



# Q5. Find a 95% CI on the average weekly sales in US dollars, not in log sales
#    at a dprice changes by -5%, in weeks with no 
#    displayonly = o, coupononly = 0, and displaycoupon = 1
library(mosaic)
f <- makeFun(model2)

f(-0.05,0,0,1, interval="confidence", level=0.95 ) # CI in log sales

f(-0.05,0,0,1, interval="confidence", level=0.95 )*exp(s2/2) # CI in sales 

# Q6. Calculate the the dlnprice slope. And also its SE 
# slope = elas* y/x = elas* sales/dprice

coef(model2)[2]* mean(heinz$sales)/mean(heinz$dprice)
coef(model2)[2]* mean(heinz$sales)

# The SE 
mean_sales <- mean(heinz$sales)
mean_dprice <- mean(heinz$dprice) # if we change this to price instead of dprice we get the 
# answer in the solution mannual 
car::deltaMethod(model2, " b2*mean_sales/mean_dprice", parameterNames = paste ("b",1:5, sep=""))

# Q 8. Test the profitability of three different marketing strategies, 
# displayonly, coupononly, and displaycoupon, with weekly costs of 
# 40, 150, and combined cost of the first two, respectively. Use alpha = 5%

# check whether the estimate lies in the CI
car::deltaMethod(Model1, "b3-40", parameterNames = paste("b",1:5, sep = ""),level= 0.95)
# The CI includes zero, hence displayonly is not significantly different 
# from zero at the cost of 40 dollars per week
car::deltaMethod(Model1, "b4-150", parameterNames = paste("b",1:5, sep = ""),level= 0.95)
# coupononly is significantly d/t from zero at cost of 150 dollars per week 

car::deltaMethod(model1, "b5-(40+150)", parameterNames = paste("b",1:5, sep = ""),level= 0.95)
# displaycoupononly is significantly different from zero at cost of 190 dollars per week 
# This marketing activity is the highest profitability 

# Net effect of all jointly  (all costs: 40+150+190=380)
car::deltaMethod(model1, "b3+b4+b5-380", parameterNames = paste("b",1:5, sep = ""),level= 0.95)
# All activities jointly have a significantly positive effect. 


# Alternatively using  the linearHypothesis() function 
car::linearHypothesis(model1, "displayonly-40=0",level=0.95)
car::linearHypothesis(model1, "coupononly-150=0",level=0.95)
car::linearHypothesis(model1, "displaycoupon-40-150=0",level=0.95)
# jointly 
car::linearHypothesis(model1, "displayonly+coupononly+displaycoupon-380=0",level=0.95)

# Alternatively using  glht() function from the multcom package 
library(multcomp)
summary(glht(model1, linfct = c("displayonly=40")))
summary(glht(model1, linfct = c("coupononly=150")))
summary(glht(model1, linfct = c("displaycoupon=190")))
# jointly
summary(glht(model1, linfct = c("displayonly+coupononly+displaycoupon=380")))








