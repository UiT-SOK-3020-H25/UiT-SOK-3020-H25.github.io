

##############################
# Q. 3.20
#################################


rm(list=ls())

library(mosaic)

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

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"))

#' a).
#' 
motel %>% filter(repair==0) %>% summarize(n())  
motel %>% filter(repair==1) %>% summarize(n())   
#'There were 18 time periods during which no repairs were being made.
#' And there were 7 time periods during which repair were being made 

mean(motel_pct ~ repair, data = motel)
diffmean(motel_pct ~ repair, data = motel)

#' The average motel occupancy rate during no repair periods was 79.35%. 
#' When repairs were being made the average occupancy rate was 66.11%,
#' which is a reduction of 13.24 percentage points. 
#' 

motel_before <- motel %>% filter(repair==0)
motel_during<- motel %>% filter(repair==1)

#compute the average occupancy rate before and during
mean(motel_before$motel_pct) # average occupancy rate before repair (in percentage )
mean(motel_during$motel_pct)  # the average occupancy rate during the repair (in percentage)

# For a 200 unit motel the revenue before the repair per 30 days become 
(mean(motel_before$motel_pct)/100)*200*mean(motel_before$motel_rate)*30 

# During the repair 
(mean(motel_during$motel_pct)/100)*200*mean(motel_before$motel_rate)*30 

# loss due to repair per month 
(mean(motel_before$motel_pct)/100)*200*mean(motel_before$motel_rate)*30-(mean(motel_during$motel_pct)/100)*200*mean(motel_before$motel_rate)*30 

# loss due to repair in 7 month 
7*((mean(motel_before$motel_pct)/100)*200*mean(motel_before$motel_rate)*30-(mean(motel_during$motel_pct)/100)*200*mean(motel_before$motel_rate)*30) 

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



# # Simple numeric example on motel problem   
#For a 100 unit motel, the Revenue per month and in 7 months 
# rooms = 100, motel_rate = $50, days = 30.

# Suppose pre motel_pct average = 80 --> pre revenue = (80/100)100*30*50 = $120,000.

# During repair months, observed motel_pct = 66 --> actual = (66/100)100*30*50 = $99,000.

# Loss/month = 120,000 − 99,000 = $21,000 --> the loss in 7 months --> $21,000*7 =  $147,000.

# If competitor occupancy rose 5% during repairs (comp_ratio = 1.05),
# then competitor-adjusted motel_pct_cf = 80 * 1.05 = 84 -->
# revenue_cf = (84/100)100*30*50 = $126,000 --> Loss/month = 27,000 --> total loss in 7 months = 27,000*7= $189,000.

