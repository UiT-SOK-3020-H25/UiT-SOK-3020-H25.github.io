


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




##############################################################
# Q.3.19
#############################################################
rm(list=ls())

library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/motel.def")
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

summary(motel)

# a)

motel %>%
  ggplot() +
  geom_line(aes(x = time, y = motel_pct, color = "Motel")) +
  geom_line(aes(x = time, y = comp_pct, color = "Competitor")) +
  labs(color = "Occupancy rate") +
  theme_minimal()


fit <- lm(motel_pct ~ comp_pct, data = motel)
summary(fit)

# construct  95% confidence interval for the estimated parameter?
confint(fit)
confint(fit, level = 0.95)


# b)


f <- makeFun(fit)
f(comp_pct = 70, interval = "confidence")
f(comp_pct = 70, interval = "prediction")


# c) H0: b2 <=0, vs H1: b2 >0, a = 0.01


# the t-value 
(coef(fit)[2]-0)/0.2027

t <- (coef(fit)[2]-0)/0.2027
t

# the t-critical, 
qt(0.99,dim(motel)[1]-2)
qt(0.99, fit$df.residual)


library(multcomp)
summary(glht(fit, linfct = c("comp_pct <=0")))
# The output gives you a one-sided p-value.
# If p < 0.01, you reject H0 at the 1% level in favor of H1: comp_pct < 0.
# If p ≥ 0.01, you do not reject H0.

# d)
summary(glht(fit, linfct = "comp_pct = 1"))
library(car)
linearHypothesis(fit, "comp_pct =1")

# Define the null hypothesis (H0: comp_pct = 0)
test <- glht(fit, linfct = "comp_pct = 0")

# Two-tailed test (default)
summary(test)
confint(test, level = 0.99)

#e)
motel$res <- resid(fit)

motel %>% 
  ggplot()+
  geom_point(aes(x=time, y=res))+
  geom_hline(yintercept = 0)+
  ggtitle("Restidual plot")


####################################################################
# Q. 3.20
#########################################################


rm(list=ls())

library(mosaic)


#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/motel.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"))

#' a).
count(motel %>% filter(repair==0)) 
count(motel %>% filter(repair==1))

# Alternatively
motel %>% filter(repair==0) %>% summarize(n())  
motel %>% filter(repair==1) %>% summarize(n())   
#'There were 18 time periods during which no repairs were being made.
#' And there were 7 time periods during which repair were being made 

mean(motel_pct ~ repair, data = motel)
diffmean(motel_pct ~ repair, data = motel)

#' The average motel occupancy rate during no repair periods was 79.35%. 
#' When repairs were being made the average occupancy rate was 66.11%,
#' which is a reduction of 13.24 percentage points.  

#' b).

#' repair is an indicator variable taking the value 1 during repair and 
#' 0 otherwise. 

fit <- lm(motel_pct ~ repair, data = motel)
summary(fit)
#' Note that the estimated intercept is the average occupancy
#' rate during the non-repair period, and the estimated coefficient
#' of REPAIR is the difference in the average occupancy rates for the
#' two periods.
#' 

# c.
confint(fit)

#' With 95% confidence, we estimate that during the repair period 
#' there was between a 25.6% and a 0.91% reduction in the expected 
#' motel occupancy rate. This interval is fairly wide, but it does 
#' not include zero or any positive values. We conclude that the motel
#'  likely did suffer a loss of occupancy during the repair period.


# d. H0: gamma >= 0 vs H1: gamma <0

library(multcomp)
summary(glht(fit, linfct = c("repair >= 0"))) # multcomp::glht, one sided test, H0

# critical t
qt(0.05, df=fit$df.residual) 

#' If we reject the null hypothesis we conclude that during the 
#' repair period the model did suffer a statistically significant
#' reduction in occupancy rate,with a small probability, 0.05, of a Type I error.  

#' the p-value 
pt(-2.22, df =fit$df.residual)

# e.
fit2 <- lm(I(motel_pct-comp_pct) ~ repair, data = motel)
summary(fit2)

confint(fit2)

#' We estimate with 95% confidence, that during the repair period 
#' the average difference between the motel’s occupancy rate and 
#' the competitor’s occupancy rate fell by between 22.37% and 5.87%.

# f. Test H=: gamma= 0 vs H1:gamma <0 , alpha=0.001
summary(glht(fit2, linfct = c("repair = 0"))) # multcomp::glht, one sided test, H0
# critical t
qt(0.01, fit2$df.residual) 

#' We reject the null at 1% sig. level. Hence, we conclude that there is a statistically 
#' significant inverse relationship between the average difference between
#' the motel’s occupancy rate & the competitor’s occupancy rate and the repairs. 

#' the p-value 
pt(-3.542, df = fit2$df.residual)


#' -----------------------------------------------------



# 3.28  

rm(list=ls())

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/motel.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"))


motel <- motel %>% mutate(relprice2 = 100*relprice)

favstats(~relprice2, data = motel)
favstats(relprice2~repair, data = motel)

# The relative price of the motel is lower than their competitors over the entire period,
# but significantly lower during repair
motel %>% ggplot(aes(x=time, y=relprice2, color=as.factor(repair), group=1)) + geom_line() + geom_point() +
  geom_hline(yintercept=mean(~relprice2, data = motel)) +
  geom_hline(yintercept=mean(~relprice2, data = filter(motel, repair==1)), color="darkgreen") +
  geom_hline(yintercept=mean(~relprice2, data = filter(motel, repair==0)), color="red")

repair <- lm(relprice2 ~ repair, data = motel)
summary(repair)

# b)
fit <- lm(motel_pct ~ relprice2, data = motel)
summary(fit)

# We estimate that each additional 1% increase in the relative price the average motel occupancy rate falls by
coef(fit)[2] # %. 

# A 95% confidence interval is
confint(fit)[2,]
# This interval is entirely less than zero, so we are 95% confident that there is an inverse relationship. 

# c)
f <- makeFun(fit)
# The 90% interval estimate is
f(80, interval="confidence", level=0.9)
# The interval estimate is about 14% wide which does not pin down the expected occupancy rate very well.
f(80, interval="confidence", level=0.9)[3]-f(80, interval="confidence", level=0.9)[2]

# d) Use 5% level of significance
summary(glht(fit, linfct = c("relprice2 = 0"))) # multcomp::glht, one sided test,  specify the H0 using linfct=
# We do reject the null hypothesis of no relationship and conclude that there is a statistically significant inverse relationship.

# e) Use 5% level of significance
summary(glht(fit, linfct = c("relprice2 = -1"))) # multcomp::glht, one sided test,  specify the H0 using linfct=
qt(0.025,fit$df.residual)
qt(0.975,fit$df.residual)

# This value is in the non-rejection region, so we cannot reject the null hypothesis
# that for each percent higher for the relative price that the motel in question charges,
# it loses one percent of its occupancy rate. The test p-value is 0.708

#' -----------------------------------------------------








