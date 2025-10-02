

#' Q 5.18

rm(list=ls())
library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/london5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/london5.rdata"))
head(london5)

# a). wfood = b1 +b2*ln(totexp)+ef
#     wcloth = a1 + a2*ln(totexp)+ec
#' A commodity is regarded as luxury if the coeff of ln(totexp) is 
#' positive and a necessity if it is negative. 
# what signs do you expect for b2 and a2?

#Ans: Since both food and clothing are necessities rather than luxuries.
#we would expect both b2 and a1 to have negative signs. 

# b).
london5$nk

london5_1 <- london5 %>% filter(nk==1)

fit_food <- lm(wfood ~ log(totexp), data =london5_1 )
summary(fit_food)

fit_cloth <- lm(wcloth ~ log(totexp), data =london5_1 )
summary(fit_cloth)

#' The coefficient of ln(totexp)  in the equation for food has
#'  the expected negative sign, but the corresponding coefficient 
#'  in the clothing equation is positive, contrary to expectations. 

#' The p-values indicate that the estimates are significantly 
#' different from zero at all conventional significance levels. 

# c). Hypothesis test, 
#'  H0: beta >=0 vs  H1: beta < 0, alpha=1%
#( Note that the summary function gives you a two-tail test )

#' We would set up the hypotheses in this way if we wished to 
#' establish that food was a necessity. 
library(multcomp)
summary(glht(fit1_food,linfct = c("log(totexp)>=0")))

# Alternatively 
london5_1 <- london5_1 %>% mutate(ltotexp = log(totexp))

fit1_food <- lm(wfood ~ ltotexp, data =london5_1 )
summary(fit1_food)

#check with the above model 
summary(fit_food)

# now we can use glht function for the hypothesis testing
summary(glht(fit1_food,linfct = c("ltotexp>=0")))

#Alternatively, using confidence interval approach 
library(car)
deltaMethod(fit1_food, "b2", parameterNames= paste("b", 1:2, sep=""))


# d). Hypothesis test, H0: alpha >=0, H1: alpha <0
fit1_cloth <- lm(wcloth ~ ltotexp, data =london5_1 )
summary(glht(fit1_cloth,linfct = c("ltotexp>=0")))


# e)

head(london5)

london5_2 <- london5 %>% filter(nk==2)

fit_food2 <- lm(wfood ~ log(totexp), data =london5_2 )
summary(fit_food2)

fit_cloth2 <- lm(wcloth ~ log(totexp), data =london5_2 )
summary(fit_cloth2)


# 95% interval estimates 
confint(fit_food) #  1 child HHs
confint(fit_food2) #2-child HHs

confint(fit_cloth) #  1 child HHs
confint(fit_cloth2) #2-child HHs

#' For both food and clothing, the interval estimates overlap
#' suggesting the coefficients could be the same, particularly 
#' for clothing. A simple inspection of interval estimates is not 
#' definitive, however.


# f) use all observations to estimate

# wfood = a1 +a2 ln(totexp)+a3 nk + a4 nk*ln(totexp) +e
# wcloth = a1 +a2 ln(totexp)+a3 nk + a4 nk*ln(totexp) +e

fit_food3 <- lm(wfood ~ log(totexp) + nk+ nk*log(totexp), data =london5 )
summary(fit_food3)

fit_cloth3 <- lm(wcloth ~ log(totexp) + nk + nk*log(totexp), data =london5 )
summary(fit_cloth3)

# A 95% significance level 

#' For both commodities, to test whether the coefficient of totexp 
#' is the same for both one-child and two-child households,
#' we test whether the coefficient of NK*totexp is significantly 
#' different from zero.

#For Food, H0: gamma_4 =0 vs H1: gmma_4 = ! 0 

# look at the t-static and p-value from the summary of the model
# since we have a two tail test 
#conclusion: reject H0

#For clothing, H0: gamma_4 =0 vs H1: gamma_4 = ! 0 
#conclusion: fail to reject H0
#' i.e., There is no evidence to suggest that, for the clothing 
#' equation, one- and two-child households have different 
#' coefficients for log(totexp).
#' 

##############################################################

mydata <- mutate(london5,one=ifelse(nk==1,1,0))

m1 <- lm(wfood ~log(totexp)+one+one*log(totexp), data = mydata)
summary(m1)


#############################################################


# g). mathematical derivation, go by yourself




