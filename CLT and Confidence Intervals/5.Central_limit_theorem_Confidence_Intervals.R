# free memory
rm(list = ls())
gc()

#https://stackoverflow.com/questions/40307510/central-limit-theorem-in-r
#The CLT states that given i.i.d. samples from a distribution with mean and variance, the sample mean (as a random variable) has a distribution that converges to a Gaussian as the number of samples n increase. Here, I will assume that you want to generate r sample sets containing n samples each to create r samples of the sample mean. Some code to do that is as follows:

set.seed(123) ## set the seed for reproducibility
r <- 10000
n <- 20      ## I use 200 instead of 20 to enhance convergence to Gaussian

## this function computes the r samples of the sample mean from the 
## r*n original samples
sample.means <- function(samps, r, n) {
  rowMeans(matrix(samps,nrow=r,ncol=n))
}
#For generating the plots, we use ggplot2 and Aaron's qqplot.data function from here. We also use gridExtra to plot multiple plots in one frame.

library(ggplot2)
library(gridExtra)
qqplot.data <- function (vec) {
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  d <- data.frame(resids = vec)

  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int, colour="red") + ggtitle("Q-Q plot")  
}

generate.plots <- function(samps, samp.means) {
  p1 <- qplot(samps, geom="histogram", bins=30, main="Sample Histogram")
  p2 <- qplot(samp.means, geom="histogram", bins=30, main="Sample Mean Histogram")
  p3 <- qqplot.data(samp.means)
  grid.arrange(p1,p2,p3,ncol=2)
}
#Then we can use these functions with the uniform distribution:

samps <- runif(r*n)  ## uniform distribution [0,1]
# compute sample means
samp.means <- sample.means(samps, r, n)
# generate plots
generate.plots(samps, samp.means)

#Or, with the poisson distribution with mean = 3:

samps <- rpois(r*n,lambda=3)
# compute sample means
samp.means <- sample.means(samps, r, n)
# generate plots
generate.plots(samps, samp.means)
