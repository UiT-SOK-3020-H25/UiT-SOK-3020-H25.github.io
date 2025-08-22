library(TeachingDemos)

# Demonstration of the Central Limit Theorem

#' Takes samples of size n from 4 different distributions and plots histograms of the
#' means along with a normal curve with matching mean and standard deviation.
#' Creating the plots for different values of n demonstrates the Central Limit Theorem.
#' 
#' Running the function with n=1 will show the populations.
clt.examp(1)

#' Run the function again with n at higher values to show that the sampling distribution
#' of the uniform quickly becomes normal and the exponential and 
#' beta distributions eventually become normal (but much slower than the uniform).
clt.examp(5)

clt.examp(30)

# ------------------------------------------------------------------------------

#' This next function creates a scatterplot of the data,
#' then adds colored rectangles between the points and the mean of x and y (both close to zero) to represent
#' the deviations from mean that the covariance and correlation coefficient is calculated from.

## low correlation
x <- rnorm(25)
y <- rnorm(25)

cor(x,y)
cor.rect.plot(x,y)

## Positive correlation
x <- rnorm(25)
y <- x + rnorm(25,3, .5)
cor(x,y)
cor.rect.plot(x,y)

## negative correlation
x <- rnorm(25)
y <- rnorm(25,10,1.5) - x
cor(x,y)
cor.rect.plot(x,y)

## zero correlation but a definite non-linear relationship
x <- -5:5
y <- x^2
cor(x,y)
cor.rect.plot(x,y)

# ------------------------------------------------------------------------------

# Example C.8 from the book
ci.examp(mean.sim = 10, sd = sqrt(10), n = 30, reps = 100, conf.level = 0.95, method = "t")

