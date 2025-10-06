

#' ## Chapter 7 - Using Indicator Variables
library(tidyverse)
library(mosaic)


#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/utown.def")
#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/utown.rdata"))
#' Table 7.1
head(utown)
names(utown)

# price	house price, in $1000
# sqft		square feet of living area, in 100's
# age		house age, in years
# utown	=1 if close to university
# pool		=1 if house has pool
# fplace	=1 if house has fireplace


###################################
# Question 1: Compute the mean price of the houses in the sample 

# select only relevant variables and save 
utown <- utown %>% select(price, sqft,utown)

mean(utown$price)

# using regression 
fit0 <- lm(price~1, data = utown)
summary(fit0)
#############################################

# Question 2: Compute the mean of the house in the two locations 

# consider the dummy variable utown 
View(utown)
utown$utown
unique(utown$utown)
#the dummy variable utown is a grouping variable in this case 


mean(price~utown, data = utown)
diffmean(price ~ utown, data = utown)

#using regression 
m2 <- lm(price ~ utown, data = utown)
summary(m2)
###############################################

# Question 3. What will happen if we change the reference group/base group 
utown$lutown <- 1-utown$utown
View(utown)

m3 <- lm(price~lutown, data = utown)
summary(m3)

# Question 4. What will happen if we include both
#             utown and lutown in the regression 
m4 <- lm(price~utown+lutown, data = utown)
summary(m4)
# dummy variable trap, either one of them need to 
# be excluded from the model to avoid dummy variable trap

# Or exclude the intercept 
m5 <- lm(price~0+utown+lutown, data = utown)
summary(m5)
# which is basically the same as 
mean(price~utown, data = utown)
####################################################

# load the data again
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/utown.rdata"))
names(utown)


fit1 <- lm(price~sqft, data = utown)
summary(fit1)

# Q1. Now let us add the effect of location on the house price
#     This can be done by including dummy variable. 
fit2 <- lm(price~utown+sqft, data = utown)
summary(fit2)

# Q2. How do you interpret the coefficient of utown?
# Solution: The effect of the inclusion of an indicator variable into the 
#           regression model is best seen by examine the regression function
#           in the two locations,  i.e., E (Price/sqft, utown)

# E(price|sqft, utown)= 5.68 +8.36 sqft + 60.369 utown 

# E(price|sqft , utown = 0)= 5.68 +8.36 sqft + 60.369 *0
# E(price|sqft , utown = 1)= 5.68 +8.36 sqft + 60.369 *1
# From the first model, we can see that location premium for lots not close to the university is 5.68 *1000 $
# From the second model, we can see that location premium for lots close to the university has additional value of 60.369*1000 $ 
f <- makeFun(fit2)

# The average price of a house 
f(sqft = 0, utown = 0)
f(sqft = 0, utown = 1)

# The location premium for lots near the university, 
# calculated as the difference b/n the expected values, aka. the coefficient of utown. 
f(sqft = 0, utown = 1)-f(sqft = 0, utown = 0)

confint(fit2)


#########################################
# Slope-indicator variables
############################################

# Let us assume that "additional sqft " of a house has different effect on the price 
# of the house based on the house location 

fit3 <- lm(price~sqft+I(sqft*utown), data = utown)
summary(fit3)
# The coefficient of the interaction variable captures the 
#interaction effects of location and house size on house price. 

# Interpretation: 
# The change in the expected price for a house:

coef(fit3)[2]+coef(fit3)[3] # close to the university 
coef(fit3)[2] # not close to the university

##############################################
##############################################

# If we assume that house location affects both intercept 
# and slope, then both effects can be incorporated into 
# a single model
fit4 <- lm(price~utown+sqft+I(sqft*utown), data = utown)
summary(fit4)


###############################
# Now let us include other variables into our model 

# age = house age, in years
# pool =1 if house has pool
# fplace = 1 if house has fireplace

fit5 <- lm(price~ utown+sqft+I(sqft*utown)+age+pool+fplace, data = utown)
summary(fit5)

# Note that pool and fplace are intercept dummy variables.
# By introducing these variables, we are asking whether
# and by how much, these features change house price.
# Because these variables stand alone, and are not interacted with 
# sqft or age, we are assuming that they affect
# the regression intercept, but not the slope.

# Interprate the coefficients:

f <-makeFun(fit5)

# 1). The intercept, the expected price, when all explanatory variables
# are zero. 
f(sqft = 0, utown = 0,age = 0,pool = 0, fplace = 0)

# 2). The location premium for lots near the university, 
#     (calculated as the difference b/n the expected values)

f(sqft = 0, utown = 1,age = 0,pool = 0, fplace = 0)-
f(sqft = 0, utown = 0,age = 0,pool = 0, fplace = 0)
# which is the coeff. on "utown"

# 3). The change in expected price per additional square foot
#    (remember sqft is given in 100 sqft)is:

(coef(fit5)[3]+ coef(fit5)[4])*10 # $ for house near the university,
coef(fit5)[3]*10  # $ for houses in other areas.

# 4). interpretation of other variables

# House depreciate by 
coef(fit5)[5]*1000 # $ per year 

# A pool increases the value of a home by $
coef(fit5)[6]*1000 

# A fireplace increases the value by 
coef(fit5)[7]*1000 

# Q. House near the university with 25000 square feet,
#    being 10 years old, with no pool and fireplace 
#   is sold for $
f(utown = 1, sqft = 250,age = 10, pool = 0, fplace = 0)*1000


#########################################################
##########################################################

#'  Example 7.2 The Effects of Race and Sex on Wage

rm(list=ls())
library(tidyverse)
library(mosaic)

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/cps5_small.def")

#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"))
head(cps5_small)
noquote(names(cps5_small))

cps5_small <- cps5_small %>% mutate(black.female=black*female)

fit <- lm(wage~educ+black+female+black.female, data = cps5_small)
#fit <- lm(wage~educ+black+female+I(black*female), data = cps5_small)
#' Table 7.3
summary(fit)

#' Holding the effect of education (educ) constant, a black male earn
coef(fit)[3]
#' per hour less than a white male, white females earn
coef(fit)[4]
#' per hour less than a white male, and black females earn
coef(fit)[3]+coef(fit)[4]+coef(fit)[5]
#' per hour less than a white male.


#'  Test the joint significance of all the qualitative factors (see p. 324)
#' The model is: 
#'  wage= b1 Intercept + b2 educ + b3 black + b4 female + b5 black.female

#' 3-joint hypothesis, from chapter 6.7
#' H0: b3 = 0, and
#' H0: b4 = 0, and
#' H0: b5 = 0
library(car)

linearHypothesis(fit, c("black=0", "female=0", "black.female=0"))

# Alternatively
Hypothesis <- matrix(c(0,0,1,0,0,
                       0,0,0,1,0,
                       0,0,0,0,1), 3, 5, byrow=TRUE)
RHS <- c(0,0,0)

colnames(Hypothesis) <- c("b1", "b2", "b3", "b4", "b5")
rownames(Hypothesis) <- c("eqtn 1", "eqtn 2", "eqtn 3")
Hypothesis

linearHypothesis(fit, Hypothesis, rhs=RHS)
#' Thus we conclude that a workers race and/or sex affect the wage equation

###########################################################
##########################################################

#' Example 7.3 A wage equation with regional indicators
 names(cps5_small)
#View(cps5_small)
fit2 <- lm(wage~educ+black+female+black.female+south+midwest+west, data = cps5_small)
#' Table 7.4
summary(fit2)

#' Test of no regional effects, joint test on the regional dummies.
linearHypothesis(fit2, c("south=0", "midwest=0", "west=0"))

# Alternatively 
Hypothesis <- matrix(c(0,0,0,0,0,1,0,0,
                       0,0,0,0,0,0,1,0,
                       0,0,0,0,0,0,0,1), 3, 8, byrow=TRUE)
RHS <- c(0,0,0)

colnames(Hypothesis) <- c("b1","b2","b3","b4","b5","b6","b7","b8")
rownames(Hypothesis) <- c("eqtn 1", "eqtn 2", "eqtn 3")
Hypothesis

linearHypothesis(fit2, Hypothesis, rhs=RHS)
#' We keep the H0, no regional effects.

#############################################
##############################################


#' Example 7.4 
#' Testing the equivalence of two regressions (the chow test) 
noquote(names(cps5_small))

cps5_small <- cps5_small %>% mutate(educ.south=educ*south,
                                    black.south=black*south,
                                    female.south=female*south,
                                    black.female.south=black*female*south)

fit3 <- lm(wage~educ+black+female+black.female+south+educ.south+black.south+female.south+black.female.south,
           data = cps5_small)

#' Table 7.5, Whole sample
summary(fit3)

#' Table 7.5, Nonsouth
fit4 <- lm(wage~educ+black+female+black.female, 
           data = filter(cps5_small, south==0))
summary(fit4)

#' Table 7.5, South
fit5 <- lm(wage~educ+black+female+black.female, 
           data = filter(cps5_small, south==1))
summary(fit5)

#' Verify that the coefficients from the whole sample is equal to the separate models.

#' In the nonsouth, a black female has
coef(fit4)[5]
#' This is the same coefficient as the whole sample
coef(fit3)[5]

#' In the south, a black female has
coef(fit5)[5]
#' This is the same coefficient as the whole sample
coef(fit3)[5]+coef(fit3)[10]

#' However, their differences can only be tested on the whole sample,
#' using a joint hypothesis

#' Example 7.4 Test the joint hypothesis
#' Note that we are using the names of the variables instead of the matrix approach above!
joint.hyp <- c("south=0",
               "educ.south=0",
               "black.south=0",
               "female.south=0",
               "black.female.south=0")

linearHypothesis(fit3,joint.hyp)

#' We fail to reject the H0, the regression in the south is no different from that of the whole country



###############################################
#' Example 7.5 Indicator Variables in Log-Linear Models
###############################################

fit <- lm(log(wage)~educ+female, data=cps5_small)
summary(fit)

approx <- 100*coef(fit)[3]
exact <- 100*(exp(coef(fit)[3])-1)

paste(approx,exact)








#############################################
##############################################
#' Project STAR, an application of the simple difference estimator
#' 

rm(list=ls())

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/star.def")

# star.def
# 
# id schid  tchid  tchexper absent  readscore mathscore  totalscore boy 
# white_asian black  tchwhite tchmasters  freelunch  schurban schrural 
# small regular aide              
# 
# Obs:   5786 observations
# 
# id 		student id
# schid	school id
# tchid	teacher id
# tchexper	teacher years of experience
# absent	days absent
# readscore	reading score
# mathscore	math score
# totalscore	combined math and reading score
# boy		male student
# white_asian	white or asian student
# black	black student
# tchwhite	white teacher
# tchmasters	teacher with masters degree
# freelunch	free lunch provided
# schurban	school urban or inner city
# schrural	school rural
# small	small class
# regular	regular class
# aide		regular class with aide
# 
# Data source: http://www.heros-inc.org/star.htm 
# 
# 
# 
# Variable |       Obs        Mean    Std. Dev.       Min        Max
# -------------+--------------------------------------------------------
#   id |      5786    15593.06    2694.317      10133      21580
# schid |      5786    211001.8    38381.93     112038     264945
# tchid |      5786    2.11e+07     3838193   1.12e+07   2.65e+07
# tchexper |      5766    9.306452    5.767684          0         27
# absent |      5765    10.27511     9.27064          0         79
# -------------+--------------------------------------------------------
#   readscore |      5786    436.7297    31.71347        315        627
# mathscore |      5786     485.599    47.69394        320        626
# totalscore |      5786    922.3287     73.7466        635       1253
# boy |      5786    .5134808    .4998614          0          1
# white_asian |      5786    .6766333    .4678018          0          1
# -------------+--------------------------------------------------------
#   black |      5786    .3209471    .4668809          0          1
# tchwhite |      5786    .8354649    .3707925          0          1
# tchmasters |      5786     .351711    .4775456          0          1
# freelunch |      5786    .4816799    .4997074          0          1
# schurban |      5786    .3128241    .4636834          0          1
# -------------+--------------------------------------------------------
#   schrural |      5786    .4709644    .4991994          0          1
# small |      5786    .3003802    .4584629          0          1
# regular |      5786    .3465261    .4759043          0          1
# aide |      5786    .3530937    .4779728          0          1


#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"))

head(star)
noquote(names(star))

# Obs:   5786 observations
# 
# id 		student id
# schid	school id
# tchid	teacher id
# tchexper	teacher years of experience
# absent	days absent
# readscore	reading score
# mathscore	math score
# totalscore	combined math and reading score
# boy		male student
# white_asian	white or asian student
# black	black student
# tchwhite	white teacher
# tchmasters	teacher with masters degree
# freelunch	free lunch provided
# schurban	school urban or inner city
# schrural	school rural
# small	small class
# regular	regular class
# aide		regular class with aide

library(tidyverse)
library(stargazer)

star %>% filter(regular==1) %>%
  select(totalscore,small,tchexper,boy,freelunch,white_asian,tchwhite,tchmasters,schurban,schrural) %>%
  stargazer(. , type="text", header=FALSE,
            title="Table 7.6a Dataset 'star' Summary Statistics for Regular Sized Classes",
            summary.stat = c("n","mean", "sd", "min", "max"))

star %>% filter(small==1) %>%
  select(totalscore,small,tchexper,boy,freelunch,white_asian,tchwhite,tchmasters,schurban,schrural) %>%
  stargazer(. , type="text", header=FALSE,
            title="Table 7.6b Dataset 'star' Summary Statistics for Small Sized Classes",
            summary.stat = c("n","mean", "sd", "min", "max"))

#' Example 7.9 The Difference Estimator with Additional Controls
#' Table 7.7
star.data <- star %>% filter(regular==1 | small==1)

fit <- lm(totalscore~small, data = star.data)
summary(fit)          

fit2 <- lm(totalscore~small+tchexper, data = star.data)
summary(fit2)          

#' School Effects is a dummy variable on schid, computed bu making schid a factor
fit3 <- lm(totalscore~small+as.factor(schid), data = star.data)
summary(fit3)             

#' fixed effects    
fit4 <- lm(totalscore~small+tchexper+as.factor(schid), data = star.data)
summary(fit4)             

stargazer(fit, fit2, fit3, fit4, type = "text", omit = "schid", intercept.bottom = FALSE,
          title="Table 7.7 Project STAR: Kindergarten")

library(broom)
#' Confidence interval for variable small in model 4
confint_tidy(fit4, conf.level = 0.95)[2,]

# ------------------------------------------------------



rm(list=ls())

#'  ## Example 7.12 Estimating the effect of a minimum wage change, the DiD estimator, p. 340
browseURL("https://www.scimagojr.com/journalrank.php?area=2000")

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/njmin3.def")
#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/njmin3.rdata"))

head(njmin3)
noquote(names(njmin3))

# Obs:   820
# 
# nj           = 1 if new jersey
# d            = 1 if after nj min wage increase
# d_nj         nj*d interaction
# fte          full time-equivalent employees
# bk           = 1 if burger king
# kfc          = 1 if kentucky fried chicken
# roys         = 1 if roy rodgers
# wendys       = 1 if wendys
# co_owned     = 1 if company owned
# centralj     = 1 if in central nj
# southj       = 1 if in southern nj
# pa1          = 1 if in pa, northeast suburbs of phila
# pa2          = 1 if in pa, easton etc
# demp         change in full time employment

#' Difference in difference estimator
#browseURL("https://www.wolframalpha.com/input/?i=%28%28b1%2Bb2%2Bb3%2Bd%29-%28b1%2Bb3%29%29-%28%28b1%2Bb2%29-b1%29")

#' Table 7.8
#' Pennsylvania
favstats(fte~d, data = filter(njmin3, nj==0))
#' New Jersey (NJ)
favstats(fte~d, data = filter(njmin3, nj==1))


fit <- lm(fte~nj+d+d*nj, data=njmin3)
fit2 <- lm(fte~nj+d+d*nj+kfc+roys+wendys+co_owned, data=njmin3)
fit3 <- lm(fte~nj+d+d*nj+kfc+roys+wendys+co_owned+southj+centralj+pa1, data=njmin3)

stargazer(fit,fit2,fit3, type="text",
          title="Table 7.9 Difference in Differences Regressions",
          header=FALSE, keep.stat="n",digits=2, single.row=TRUE, intercept.bottom=FALSE)

#' The coefficient on the term nj:d in Table 7.9 is delta, our difference-in-differences estimator. 
#' The t-statistic for delta in model 1 is:
tidy(fit)
summary(fit)

library(multcomp)
summary(glht(fit, linfct = c("nj:d >= 0"))) # multcomp::glht, one sided test, H0: delta >=0
#' Keep H0, no significant reduction in employment

#' The figure below displays the change of the effect of minimum wage from the period before (d=0) to the period after
#' the change in minimum wage (d=1) for both the treatment and the control groups.
#' The line labeled "counterfactual" shows how the treatment group would have changed in the absence of the treatment,
#' assuming its change would mirror the change in the control group. The graph is plotted using the first model (fit).
fit
b1 <- coef(fit)[1]
b2 <- coef(fit)[2]
b3 <- coef(fit)[3]
delta <- coef(fit)[4]
#' Using the notation from Figure 7.3, p. 339
C <- b1+b2+b3+delta
E <- b1+b3
B <- b1+b2
A <- b1
D <- E+(B-A)
#' Difference-in-Differences Plot
#' Pennsylvania is the control, and NJ is the treated
plot(1, type="n", xlab="period", ylab="fte", xaxt="n", xlim=c(-0.01, 1.01), ylim=c(18, 24),
     main="Difference-in-Differences")
segments(x0=0, y0=A, x1=1, y1=E, lty=1, col=2, lwd=2) #control
segments(x0=0, y0=B, x1=1, y1=C, lty=3, col=3, lwd=2) #treated
segments(x0=0, y0=B, x1=1, y1=D, lty=4, col=4, lwd=2) #counterfactual
legend("topright", legend=c("control", "treated", "counterfactual"), lty=c(1,3,4), col=c(2,3,4))
axis(side=1, at=c(0,1), labels=NULL)
text(0, 23.5, "A") ; text(0, 20.6, "B") ; text(1, 20.8, "C") ; text(1, 18.5, "D") ; text(1, 21.5, "E")

#' Example 7.13 Using Panel Data, using the differenced data
fit4 <- lm(demp~nj, data=njmin3)
summary(fit4)
#' The value of the estimated difference-in-differences coefficient is very close to the one we estimated before.
#' Its  t-statistic is still positive, indicating that the null hypothesis
#' H0: "an increase in minimum wage increases employment" cannot be rejected.



