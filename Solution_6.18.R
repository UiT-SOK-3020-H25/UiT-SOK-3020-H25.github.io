#' 6.18

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/rice5.def")

# rice5.def
# 
# firm  year  prod  area  labor  fert
# 
# Obs:   a panel with 44 firms over 2 years (1993-1994)
# total observations = 88
# 
# firm	Firm number  ( 1 to 44)
# year	Year = 1993 to 1994
# prod	Rice production (tonnes)
# area	Area planted to rice (hectares)
# labor	Hired + family labor (person days)
# fert	Fertilizer applied (kilograms)

library(broom)
library(car)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/rice5.rdata"))
head(rice5)

#' Filter out 1994
rice94 <- rice5 %>% filter(year==1994)

#' Estimate model
fit <- lm(log(prod) ~ log(area) + log(labor) + log(fert), data = rice94)

#' a. i. & ii. (i.e, test H0: b2 = 0 vs H1: b2 != 0, and H0:b3=0 vs H1:b3!=0)
summary(fit)
confint(fit)

tidy(fit)
glance(fit)
vif(fit)


#' iii. Joint Hypothesis, 6.7

#' Ho: b2 = b3 = 0 
joint.hyp <- c("log(area)=0","log(labor)=0")
linearHypothesis(fit, joint.hyp)

# Conclusion: 
#' The t-test indicate that when b2 and b3 are tested separately,
#' we are unable to reject these null hypotheses at a 5% significance level.
#' However, when b2 and b3 are tested jointly, we reject the null hypothesis 
#' at a 1% significance level.
#' These results suggest ln(AREA) and ln(LABOR) are important variables,
#' but they are very collinear, as the VIF values show.
#' The correlation between these two variables is
cor(log(area) ~ log(labor), data = rice94)


#' b. Math covered in textbook

#' c.

fit2 <- lm(log(prod/area) ~ log(labor/area) + log(fert/area), data = rice94)
summary(fit2)

#' Using the deltaMethod from `car` on the Restriced model to recover the 
#' coefficient on b2 (in the original model)
#' and its standard error
deltaMethod(fit2, "1-b2-b3", parameterNames= paste("b", 1:3, sep="")) 

#' Alternatively
rice94 <- rice94 %>% mutate(ln_prod_area=log(prod/area),
                            ln_labor_area=log(labor/area),
                            ln_fert_area=log(fert/area))

fit2.alt <- lm(ln_prod_area ~ ln_labor_area + ln_fert_area, data = rice94)

linearHypothesis(fit2.alt, "1 - ln_labor_area - ln_fert_area = 0")

#' d.

#' Use all data
#' Estimate model without constant-returns-to-scale 

fit.wo.crs <- lm(log(prod) ~ log(area) + log(labor) + log(fert), data = rice5)
fit.with.crs <- lm(log(prod/area) ~ log(labor/area) + log(fert/area), data = rice5)

summary(fit.wo.crs)
summary(fit.with.crs)

confint(fit.wo.crs)
confint(fit.with.crs)

deltaMethod(fit.with.crs, "1-b2-b3", parameterNames= paste("b", 1:3, sep="")) 

#' The major change in the estimates from imposition of constant returns to scale
#' is the increase in the elasticity of production with respect to labor which
#' has increased from 0.39967 to 0.48824.
#' The interval estimate has shifted in a corresponding way.
#' There has been a slight decrease in the point estimate and a shift to the left
#' of the interval estimate for the elasticity of production with respect to fertilizer.
#' The standard errors for these two elasticities have decreased slightly
#' after imposing constant returns to scale. There has been little change in the results for AREA

vif(fit.wo.crs)
vif(fit.with.crs)

#' Multicollinearity is no longer a problem when imposing CRS. 
