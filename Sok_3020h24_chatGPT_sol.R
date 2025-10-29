
#' As we discussed in our last class, ChatGPT occasionally produced errors, 
#' particularly when constructing confidence intervals using the deltaMethod. 
#' Apart from that, it generally provided correct and well-structured answers.
#' In the following code, I have included both the solution proposed by ChatGPT and 
#' the correct answers to Item 3 of last year exam. The solution suggested by ChatGPT appears between 
#' large comment lines for reference.
#' Additionally, I have inserted my own comments between two dashed lines to help you follow my reasoning.

#' For the remaining questions, ChatGPT’s solutions were accurate.





# Item 3

######################################################################
# step 2: R code: 
# Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/means.rdata"))

# Estimate the regression model
model1 <- lm(avgprice ~ d, data = means)
summary(model1)

#Step 3. Compute estimates and interpretation
# Extract coefficients
coefs <- coef(summary(model1))
b0 <- coefs["(Intercept)", "Estimate"]       # avg price 1990
b1 <- coefs["d", "Estimate"]                 # difference 2000 - 1990
se_b1 <- coefs["d", "Std. Error"]            # std. error of the difference

# Average prices
avg_1990 <- b0
avg_2000 <- b0 + b1

# Percentage difference
perc_diff <- (b1 / b0) * 100

# Print results
cat("Average house price in 1990:", round(avg_1990, 2), "dollars\n")
cat("Average house price in 2000:", round(avg_2000, 2), "dollars\n")
cat("Percentage difference:", round(perc_diff, 2), "%\n")
cat("Standard error of the difference:", round(se_b1, 2), "\n")
########################################################################
#---------------------------------------------------------------------------
# My comments:
#ChatGPT is correct except on the standard error if the difference,
# which the correct answer here is:
car::deltaMethod(model1, "(b1/b0) * 100", parameterNames=paste("b",0:1, sep = ""), level=0.90)
# Alternatively (notice the notation, 0:1 and 1:2 in the codes)
car::deltaMethod(model1, "(b2/b1)*100", parameterNames=paste("b",1:2, sep = ""))
#----------------------------------------------------------------------------------



# Item 3: Construct a 95% confidence interval on the average house price in 1990
############################
# Using model1 from Question 1
confint(model1, "(Intercept)", level = 0.95)
# mannually
b0 <- coef(summary(model1))["(Intercept)", "Estimate"]
se_b0 <- coef(summary(model1))["(Intercept)", "Std. Error"]
ci_1990 <- c(b0 - 1.96 * se_b0, b0 + 1.96 * se_b0)
ci_1990
#########################################################
#-------------------------------------------------------------------------------
# My comments: 
# chatGPt gave us two different solutions, one using the function confinit() and another using 
# the mannual approach. The answer from the manual approach is correct since it uses z(alpha=5%)=1.96,
# the function confit() use t-distribution

# deltaMethod also uses the z-distribution to construct CI
car::deltaMethod(model1, "b1", parameterNames=paste("b",1:2, sep = ""), level=0.95)
#-----------------------------------------------------------------------------------


# Item 3: Construct a 95% confidence interval on the average house price in 2000....
######################################################################
# Using model1 from Question 1
library(multcomp)
ci_2000 <- confint(glht(model1, linfct = c("(Intercept) + d = 0")))
ci_2000

# Mannually 
b0 <- coef(model1)[1]
b1 <- coef(model1)[2]
vc <- vcov(model1)
se_2000 <- sqrt(vc[1,1] + vc[2,2] + 2*vc[1,2])
ci_2000 <- c(b0 + b1 - 1.96*se_2000, b0 + b1 + 1.96*se_2000)
ci_2000
#####################################################################

#----------------------------------------------------
# My comments: Again only the manuall option is correct 

# or using delta Method
car::deltaMethod(model1, "b1+b2", parameterNames=paste("b",1:2, sep = ""), level=0.95)
# It seems there is a bit discrepancy between the manual option and delta Method, but that is okay, it is just slight diff
#--------------------------------------------------------------------------------------------------

# Item 3: Construct a 90% confidence interval for the percentage.....
##########################################################################
# Coefficients
b0 <- coef(model1)[1]
b1 <- coef(model1)[2]

# Variance-covariance matrix
vc <- vcov(model1)

# Standard error using delta method
se_pct <- 100 * sqrt( vc[2,2]/b0^2 + (b1^2 * vc[1,1])/b0^4 - 2*b1*vc[1,2]/b0^3 )

# Point estimate of percentage difference
pct_diff <- (b1 / b0) * 100

# 90% confidence interval
ci_90 <- c(pct_diff - 1.645*se_pct, pct_diff + 1.645*se_pct)
ci_90
###########################################################################

#--------------------------------------------------------------
# My comments: the answer from chatGPT is correct 
# Or simply using the delta MEthod (notice the level of confidence here, i.e., level = 0.90)
car::deltaMethod(model1, "(b2/b1)*100", parameterNames=paste("b",1:2, sep = ""), level= 0.90)
#----------------------------------------------------------------------------------


# Item 3: using the means dataset, estimate a simple log-linear.....
###############################################################################
# Log-linear regression
model_log <- lm(lnprice ~ d, data = means)
summary(model_log)

# Extract coefficient for year dummy
b1 <- coef(summary(model_log))["d", "Estimate"]
se_b1 <- coef(summary(model_log))["d", "Std. Error"]
pval_b1 <- coef(summary(model_log))["d", "Pr(>|t|)"]
r2 <- summary(model_log)$r.squared

# Approximate percentage change
pct_change <- b1 * 100

# Print results
cat("Percentage change from 1990 to 2000:", round(pct_change,2), "%\n")
cat("Significant at 5% level:", ifelse(pval_b1 < 0.05, "Yes", "No"), "\n")
cat("R-squared:", round(r2,3), "\n")
######################################################################################
#-------------------------------------------------------------------------------------------
# My comments: The Percentage change from 1990 to 2000 obtained using chatGPT(i.e., 25.58 %) is not correct.
# Remember the question here is on the interpretation of the coefficient of a dummy variable in log-linear model.
# The correct value should be computed as: 100*(exp(b2)-1), using deltaMethod, 

# the percentage diff
car::deltaMethod(model_log, "100*(exp(b2)-1)", parameterNames=paste("b",1:2, sep = ""), level=0.90)
car::deltaMethod(model_log, "100*(exp(b2)-1)", parameterNames=paste("b",1:2, sep = ""), level=0.95)
#-------------------------------------------------------------------------------------------------------


# Item 3: Construct a 90% confidence interval for the percentage difference between the average....
####################################
b1 <- coef(summary(model_log))["d", "Estimate"]
se_b1 <- coef(summary(model_log))["d", "Std. Error"]

# 90% CI for beta1
ci_beta <- b1 + c(-1,1) * 1.645 * se_b1

# Transform to percentage
ci_pct <- (exp(ci_beta) - 1) * 100
ci_pct
####################################################

#----------------------------------------------------------------
# My comments: chatGPT used t-distribution to combute the CI
# Using the delta Method the correct solution (is will be:
car::deltaMethod(model_log, "100*(exp(b2)-1)", parameterNames=paste("b",1:2, sep = ""), level=0.90)
#-------------------------------------------------------------------------------------------------

# Item 3: the last question
#######################################################################
# Create sales variable
means$sales <- means$avgprice * means$units

# Average sales across all cities and years (in billions)
avg_sales <- mean(means$sales) / 1e9
avg_sales

##################################################################

#------------------------------------------------------------------------------------------
# My comments:  the variable "avgprice" and "units" are integers, you can see this by 
# running the class() function as: 
# Create sales variable
class(means$avgprice)
class(means$units)
# It seems like R does not  produce results when we multiply two integers, but when 
# we convert to numeric using the function "as.numeric()" as you can see below, it produce a result
means$sales <- as.numeric(means$avgprice) * as.numeric(means$units)
# Average sales across all cities and years (in billions)
avg_sales <- mean(means$sales) / 1e9
avg_sales
#----------------------------------------------------------------------------------------




# Here is the complete solution of H2024 exams using chatGPT
browseURL("https://chatgpt.com/share/6901ca38-2330-8003-a799-9e7ac28edff3")

