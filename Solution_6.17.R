

#' 6.17  

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/br5.def")

# br5.def
# 
# price sqft bedrooms baths age owner pool traditional fireplace waterfront
# 
# Obs:   900 home sales in Baton Rouge, LA during mid-2005
# 
# price		sale price, thousands of dollars
# sqft		total area in hundreds of square feet
# bedrooms	number of bedrooms
# baths		number of full baths
# age		age in years
# Owner		= 1 if owner occupied at sale; = 0 if vacant or tenant
# Pool		= 1 if pool present
# Traditional	= 1 if traditional style; = 0 if other, such as townhouse, contemporary, etc.
# Fireplace	= 1 if fireplace present
# Waterfront	= 1 if on waterfront

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/br5.rdata"))
head(br5)

dim(br5)

#################################################################################
#' In example we will see how to select best model based on model selection criteria.
#' What are the different selection criterion's we have?
#' Do you suggest any?
#' 
#' Model selection criteria(comparing different model with the same dependent variable): 
#' Residual plots,  R2 and Adjusted R2, AIC, SC (BIC)---These all are based on the principle 
# of minimizing SSE(sum of Squared Error).

#' RMSE (root mean squared error)-a measure of the model's out of sample forecasting ability. 


#'These are additional information to compare models. 
#'They should be treated as devices that provide additional information 
#'We should use them in combination. 
#'There is no best or wrong answer in this case. OK!  

#####################################################################################

library(broom)
#install.packages("Metrics")
library(Metrics)  # rmse() function available from the Metrics package
#https://www.r-bloggers.com/2021/07/how-to-calculate-root-mean-square-error-rmse-in-r/

train <- br5[1:800,] #the first 800 obs were used for estimation 
holdout <- br5[801:900,] # the last 100 obs used for predictive assessment 

# Model 2
fit <- lm(log(price) ~ age + sqft + I(age^2), data = train)
glance(fit)

model2.fit <- glance(fit)


#--------------------------------------------------
library(mosaic)
f <- makeFun(fit)             # predicts in price in levels
f(10,11)
predict(fit,newdata = data.frame(age = 10,sqft = 11)) # predicts log(price)
#To find the prediction in level, use exponential 
exp(predict(fit,newdata = data.frame(age = 10,sqft = 11))) # predicts price in level 
#-------------------------------------------------



# compute the root mean square (RMSE) 
?rmse
rmse(log(holdout$price), predict(fit, holdout)) #look at the log term   

#include RMSE in the data frame mode12.fit
model2.fit <- model2.fit %>% mutate(Model=2,
                                    RMSE=rmse(log(holdout$price), predict(fit, holdout)))
model2.fit

# Model 9
fit <- lm(log(price) ~ age + sqft + I(age^2) + baths, data = train)
model9.fit <- glance(fit)
model9.fit <- model9.fit %>% mutate(Model=9,
                                    RMSE=rmse(log(holdout$price), predict(fit, holdout)))

# combine using the rbind() function - on top of each other 
rbind(model2.fit,model9.fit)

# Model 10
fit <- lm(log(price) ~ age + sqft + I(age^2) + baths + bedrooms, data = train)
model10.fit <- glance(fit)
model10.fit <- model10.fit %>% mutate(Model=10,
                                      RMSE=rmse(log(holdout$price), predict(fit, holdout)))

# Model 11
fit <- lm(log(price) ~ age + sqft + I(age^2) + baths + bedrooms*sqft, data = train)
model11.fit <- glance(fit)
model11.fit <- model11.fit %>% mutate(Model=11,
                                      RMSE=rmse(log(holdout$price), predict(fit, holdout)))

# Model 12
fit <- lm(log(price) ~ age + sqft + I(age^2) + baths + bedrooms*sqft + baths*sqft, data = train)
model12.fit <- glance(fit)
model12.fit <- model12.fit %>% mutate(Model=12,
                                      RMSE=rmse(log(holdout$price), predict(fit, holdout)))

table <- rbind(model2.fit,model9.fit,model10.fit,model11.fit,model12.fit)

table <- table %>% dplyr::select(Model,adj.r.squared,AIC,BIC,RMSE)

table

#' Generate a table with sorted metrics
options(pillar.sigfig = 9)
table %>% arrange(AIC,BIC,RMSE) 

