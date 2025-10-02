
# Q 4.16  page 185 of the book 

rm(list=ls())

library(mosaic)
library(tidyverse)

#' The data defenition file: 
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/newbroiler.def")

# Load the data:
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/newbroiler.rdata"))
head(newbroiler)
tail(newbroiler)


#' The estimated reciprocal model
#'  Q = a_1 + a_2*(1/p) + e 
fit1 <- lm(q~I(1/p), data=newbroiler)
summary(fit1)

plotModel(fit1)

#or 
newbroiler %>% ggplot(aes(x=p, y=q)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y~I(1/x)) 

#' The reciprocal model fits the data relatively well.
#' There is some tendency to overestimate quantity in 
#' the middle range of prices and
#' underestimate quantity at the low and high extreme prices. 


#' It is also possible to create a new variable that is 
#' the reciprocal of price,and use this in a regression.



#' b) compute the elasticity of per capita consumption w.r.t. real price

#' Finding the derivative:
D(expression(1/p), "p")

D(expression(b1+b2*1/p), "p")

f1 <- makeFun(fit1)

#' Elasticity
e <- function(p) {coef(fit1)[2]*-(1/p^2)*p/f1(p)}

median(~p, data=newbroiler) # median p
f1(median(~p, data=newbroiler)) # q at median p

e(median(~p, data=newbroiler)) # elasticity at median p


curve(e(x), 0.9,3) # Higher prices, more elastic


##########################################
# log-log model 
fit_log_log <- lm(log(q) ~log(p), data = newbroiler)
coef(fit_log_log)
coef(fit_log_log)[2]

#'The elasticity found using the log-log model is -1.121 , 
#'a similar, but slightly smaller absolute value than that
#' for the reciprocal model

e(mean(~p, data=newbroiler)) # elasticity at median p
########################################

#' c.  linear-log model 

fit2 <- lm(q~log(p), data=newbroiler)
summary(fit2)

plotModel(fit2)

#' Like the reciprocal model, this linear-log model tends to 
#' under predict for low and high prices and
#' over predict for mid-range prices. 
#' Also, its fit appears slightly worse than that of
#' the reciprocal model. 



#' d). compue the elasticity 

#' Finding the derivative:
D(expression(log(p)), "p")

D(expression(b1+b2*log(p)), "p")

f2 <- makeFun(fit2)

#' Elasticity
e2 <- function(p) {coef(fit2)[2]*(1/p)*p/f2(p)}

median(~p, data=newbroiler) # median p
f2(median(~p, data=newbroiler)) # q at median p
e2(median(~p, data=newbroiler)) # elasticity at median p


#'compare the elasticities:
#' The linear-log model yields a more inealastic 
#' elasticity (in absolute value) than the other model.

curve(e2(x), 0.9,3, col="red") # Linear-log model
curve(e(x), 0.9,3, add = TRUE) # Reciprocal model



#' e. Log-linear function 

fit3 <- lm(log(q)~p, data=newbroiler)
summary(fit3)

f3 <- makeFun(fit3)

#plotModel(fit3) doesnot work here 
xyplot(q~p, data=newbroiler) 
plotFun(f3(p) ~ p, add=TRUE, col="darkred", lwd=2)

#' The model fits well at low prices but over predicts 
#' in the middle range, and under predicts at higher prices. 


#' f. compute the elasticity 
e3 <- function(p) {coef(fit3)[2]*p}

median(~p, data=newbroiler) # median p

f3(median(~p, data=newbroiler)) # q at median p

e3(median(~p, data=newbroiler)) # elasticity at median p

#' The elasticity from this model is lower than for the other models.


#' Plot the elasticities from all all 3 models
curve(e2(x), 0.9,3, col="red", xlab = "price", ylab = "elasticity") # Linear-log model
curve(e(x), 0.9,3, add = TRUE) # Reciprocal model
curve(e3(x), 0.9,3, col="blue", add = TRUE) # Log-lin model
legend("bottomleft", inset=.05, title="Models", #legend: topright
       c("Linear-Log","Reciprocal","Log-Lin"), fill=c("red","black","blue"), horiz=TRUE)

#' The generalized R^2 is:
cor(f3(newbroiler$p),newbroiler$q)^2


# g.  Evalute the suitability of the alternative model


# Plot the residuals 

# Reciprocal model
newbroiler$resid_rec <- resid(fit1)
newbroiler %>% ggplot(aes(x=1/p,y=resid_rec))+geom_point() + labs(title = "Reciprocal Model Residual") 
#'The residual scatter shows a definite spray 
#'pattern, suggesting heteroskedastic errors. 

#log-log 
newbroiler$resid_log_log <- resid(fit_log_log)
newbroiler %>% ggplot(aes(x=log(p),y=resid_log_log))+geom_point() + labs(title = "Log-Log Model Residual") 

#linear_log 
newbroiler$resid_lin_log <- resid(fit2)
newbroiler %>% ggplot(aes(x=log(p),y=resid_lin_log))+geom_point() + labs(title = "Lin-Log Model Residual") 
#'The residual scatter shows a definite spray 
#' pattern, suggesting heteroskedastic errors. 


#'log-linear 
newbroiler$resid_log_lin <- resid(fit3)
newbroiler %>% ggplot(aes(x=p,y=resid_lin_log))+geom_point() + labs(title = "Log-lin Model Residual") 
#' The residual scatter shows a U-shape, suggesting a missed
#'  quadratic or cubic component, or at least a misspecification. 

#All residuals on the same plot 
library(gridExtra)
Recip <-newbroiler %>% ggplot(aes(x=1/p,y=resid_rec))+geom_point() + labs(title = "Reciprocal Model Residual") 
log_log<- newbroiler %>% ggplot(aes(x=log(p),y=resid_log_log))+geom_point() + labs(title = "Log-Log Model Residual") 
lin_log <- newbroiler %>% ggplot(aes(x=log(p),y=resid_lin_log))+geom_point() + labs(title = "Lin-Log Model Residual") 
log_lin <-newbroiler %>% ggplot(aes(x=p,y=resid_lin_log))+geom_point() + labs(title = "Log-lin Model Residual") 


grid.arrange(Recip,log_log,lin_log,log_lin, ncol=2)


#' Given the above diagnostics, the log-log model seems to be
#'  the best choice, based on a less-well defined residual plot. 


library(tseries)
jarque.bera.test(resid(fit1)) #Reciprocal Model
jarque.bera.test(resid(fit_log_log)) #Log-Log Model
jarque.bera.test(resid(fit2)) #Lin-Log Model
jarque.bera.test(resid(fit3)) #Log-lin Model


