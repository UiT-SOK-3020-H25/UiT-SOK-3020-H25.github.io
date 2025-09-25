


##################################################
#' Q 2.17
#########################################
rm(list=ls())
library(tidyverse)
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/collegetown.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"))

head(collegetown)

# Notice: price (in $1,000s) and sqft (in 100s)

#' a) 

collegetown %>%
  ggplot(aes(x=sqft, y=price)) + geom_point() + xlim(c(0,100)) +
  labs(title = "House price vs house size")

#' b)
fit1 <- lm(price ~ sqft, data = collegetown)
summary(fit1)


#' Interpretation of the coefficients:
#' The Slope implies: A 1 unit (i.e., 100sqft) increase of living area will increase
#' the expected home price by 13.40294 unit ($1000)= $ 13, 402.94, holding all else constant. 
#' The intercept implies: A house with zero square feet (i.e., sqft =0) has an expected price 
#' of -115.4236 unit ($1000)= $ -115,423.60. This is has no meaning because, in the sample, 
#' there are no houses that have zero square feet. 

library(mosaic)
favstats(~sqft, data = collegetown)   
#there is no data values with a house size near zero. 

plotModel(fit1)

# use ggplot 
collegetown %>% 
  ggplot(aes(x=sqft, y=price)) + geom_point()+ 
  geom_smooth(method = "lm", se = FALSE) 

#' c)

fit2 <- lm(price ~ I(sqft^2), data = collegetown)
summary(fit2)


slope <- function(sqft) {2*coef(fit2)[2]*sqft}

#' 2000 sqft is 20 when the units are in 100s sqft
1000*slope(20)
#' an additional 100 sqft for a 2000 sqft home will add 7380.76 dollars.


#' d)
collegetown %>% 
  ggplot(aes(x=sqft, y=price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y~I(x^2))+ xlim(c(0,100))

# intercept of tangent at 2000 sqft
f <- makeFun(fit2)

# The intercept 
f(20)-slope(20)*20

# ggplot
tangent <- function(x) {f(20)-slope(20)*20 + slope(20)*x}

collegetown %>%
  ggplot(aes(x=sqft, y=price)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y~I(x^2)) + xlim(c(0,100)) +
  stat_function(fun=tangent, col="red")


#' e)
elasticity <- function(x) {slope(x)*x/f(x)}
elasticity(20)

#' f)

#residual 
collegetown$res1<- resid(fit1)
collegetown$res2 <- resid(fit2)

plot1 <- collegetown %>% 
  ggplot(aes(x= sqft, y=res1))+
  geom_point()+
  ggtitle("Resdiuals from the linear model")


plot2 <- collegetown %>% 
  ggplot(aes(x= sqft, y=res2))+
  geom_point()+
  ggtitle("Resdiuals from the quadratic model")


library(gridExtra)
grid.arrange(plot1,plot2, ncol=2)

#' In both models, the residual patterns do
#' not appear random. The variation in the residuals 
#' increases as SQFT increases, suggesting that the 
#' homoskedasticity assumption may be violated.


#' g) compare the SSE from the two models 
SSE1 <- deviance(fit1); SSE1
SSE2 <- deviance(fit2); SSE2

anova(fit1)
anova(fit2)
anova(fit1,fit2)

#'The sum of square residuals linear relationship is 5,262,846.9.
#' The sum of square residuals for the quadratic relationship is 
#' 4,222,356.3. In this case the quadratic model has the lower SSE. 
#' The lower SSE means that the data values are closer to the fitted
#'  line for the quadratic model than for the linear model.



##########################################################################################
#' Q 2.18
############################################################
rm(list=ls())


#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/collegetown.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"))

#' a)

collegetown %>% ggplot(aes(x=price)) + geom_histogram() +
  ggtitle("Histogram of price")

collegetown %>% ggplot(aes(x=log(price))) + geom_histogram() +
  ggtitle("Histogram of log(price)") # a bit normal than price, by taking the log of the price, we normalized it. 


plot1 <- collegetown %>% ggplot(aes(x=price)) + geom_histogram() +
  ggtitle("Histogram of price")

plot2 <- collegetown %>% ggplot(aes(x=log(price))) + geom_histogram() +
  ggtitle("Histogram of log(price)")

library(gridExtra)
grid.arrange(plot1,plot2, ncol=2)

#' The distribution of PRICE is skewed with a long tail to the right. 
#' The distribution of log(price) is more symmetrical.

#Test normality using the Jarque–Bera test (JB) test from the "moment" package"

#install.packages("moments")
require(moments)
# H0: Normality
jarque.test(collegetown$price)

jarque.test(log(collegetown$price))


#' b) log-linear model

fit <- lm(log(price) ~ sqft, data = collegetown)
summary(fit)

# a 100 square foot increase (1 unit) in house size increases
#predicted price by approximately
100*coef(fit)[2] # percent

#' The estimated intercept tells us that  
#' the predicted price of a zero square foot house is 
exp(coef(fit)[1]) *1000 # multiply by 1000 b/c the unit of sale price is thousands dollar

#' This estimate has little meaning because in the sample there 
#' are no houses with zero square feet of living area. You can see this by running ..
favstats(~sqft, data=collegetown)

f <- makeFun(fit)  # this function gives exp(coef(fit)[1]+coef(fit)[2]*sqft)


# Graph the fitted price against sqft, ggplot
collegetown %>% 
  ggplot(aes(x=sqft, y=price)) + 
  geom_point() +
  stat_function(fun=f, col="red")+ #fitted value here 
  xlim(c(0,100)) 



slope <- function(x) {coef(fit)[2]*f(x)}

#' For a 2000 square foot house the predicted price is: 
f(20)

#' The estimated slope is
slope(20)

#' in units
f(20)*1000
slope(20)*1000
round(slope(20)*1000,2)

#' The predicted price of a house with 2000 square feet of living area is $166,460.10.
#' We estimate that 100 square foot size increase for a house with 2000 square feet of 
#' living area will increase price by $6,000, holding all else fixed.


# ggplot including the tangent line for the curve 
tangent <- function(x) {f(20)-slope(20)*20 + slope(20)*x}

collegetown %>% 
  ggplot(aes(x=sqft, y=price)) +
  geom_point() +
  stat_function(fun=f, col="red") + #prediction 
  stat_function(fun=tangent, col="blue")+ #tangent line 
  xlim(c(0,100)) 


# c) residuals

collegetown$res_loglinear<- fit$residuals

collegetown %>% 
  ggplot(aes(x=sqft, y= res_loglinear))+
  geom_point()+
  ggtitle("Residual from the log-linear model")

#' The residual plot is a little hard to interpret because
#'  there are few very large homes in the sample. 
#'  The variation in the residuals appears to diminish as
#'   house size increases, but that interpretation 
#'   should not be carried too far.

#' d) two-regressions

favstats(~price|close, data = collegetown)
favstats(~sqft|close, data = collegetown)

#' The summary statistics show that there are 189 houses close to LSU and 311 
#' houses not close to LSU in the sample. The mean house price is $10,000 larger 
#' for homes close to LSU, and the homes close to LSU are slightly smaller, by 
#' about 100 square feet. The range of the data is smaller for the homes close 
#' to LSU, and the standard deviation for those homes is half the standard 
#' deviation of homes not close to LSU.


#  what about uisng the filter function 
closer_U <- collegetown %>% filter(close ==1)
Not_closer_U <- collegetown %>% filter(close ==0)

favstats(~price, data = closer_U)


#' e)
fit.0 <- lm(log(price) ~ sqft, data = filter(collegetown, close==0))
coef(fit.0)

fit.1 <- lm(log(price) ~ sqft, data = filter(collegetown, close==1))
coef(fit.1)

#' For homes close to LSU we estimate that an additional 100 square feet of 
#' living space will increase predicted price by about 2.69% and for homes not 
#' close to LSU about 4.02%.

#' Together, and more outputs  
collegetown %>% group_by(close) %>% do(tidy(lm(log(price) ~ sqft, data = .)))
#collegetown %>% group_by(close) %>% do(glance(lm(log(price) ~ sqft, data = .)))

#' plots of the regression model 
f0 <- makeFun(fit.0)
f1 <- makeFun(fit.1)

#' or , only the means 
mean_p <- data.frame(price=mean(~price|close, data = collegetown), close=c(0,1))
mean_p

mean_sqft <- data.frame(sqft=mean(~sqft|close, data = collegetown), close=c(0,1))
mean_sqft

collegetown %>% ggplot(aes(x=sqft, y=price, color=as.factor(close))) + geom_point() +
  stat_function(fun=f0, col="#F8766D") +
  stat_function(fun=f1, col="#00BFC4") + xlim(c(0,100)) + ylim(c(0,1500))


#' f) 
#' Assumption SR1 implies that the data are drawn from the same population. 
#' So the question is, are homes close to LSU and homes not close to LSU in
#' the same population? Based on our limited sample, and using just a simple, 
#' one variable, regression model it is difficult to be very specific.
#' The estimated regression coefficients for the sub-samples are different, 
#' the question we will be able to address later is “Are they significantly
#'  different.” Just looking at the magnitudes is not a statistical test.




# Q.2.27

rm(list=ls())

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/motel.def")
#time days motel_rate comp_rate motel_pct comp_pct repair relprice

# Obs:   25 months
# 
# time           	month, 1=march 2003,.., 25=march 2005
# days           	days in month
# motel_rate      motel room rate, $
#   comp_rate       competitors room rate, $
#   motel_pct       percentage motel occupancy
# comp_pct        percentage competitors occupancy
# repair          = 1 if motel under repair, = 0 otherwise
# relprice        relative price = motel_rate/comp_rate

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"))
names(motel)


#a)
motel %>% 
  ggplot(aes( x= 100*relprice, y= motel_pct))+
  geom_point()

# There seems to be an inverse association between relative price and occupancy rate.

# b)
fit1 <- lm(motel_pct ~ I(100*relprice), data = motel)
summary(fit1)


# c)
names(fit1)
res <- resid(fit1)
res <- residuals(fit1)

motel$res <- res

# plot 
motel %>%
  ggplot(aes(x = time, y = res)) +
  geom_point() +
  geom_vline(xintercept = 17, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 23, linetype = "dashed", color = "red") +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  ggtitle("Residuals over Time") +
  xlab("Time") +
  ylab("Residuals")

#' The residuals are scattered about zero for the first 16 observations 
#' but for observations 17-23 all but one of the residuals is negative. 
#' This suggests that the occupancy rate was lower than predicted by the 
#' regression model for these dates. Randomly scattered time series 
#' residuals should not have strings of consecutive observations with the same sign.


# d)
fit2 <- lm(motel_pct ~repair, data = motel)
summary(fit2)

#' We estimate that during the non-repair period the expected
#' occupancy rate is 79.35%. During the repair period, the expected
#'  occupancy rate is estimated to fall by 13.24%, 
#'  other things held constant, to 66.11%.  




