# free memory
rm(list = ls())
gc()

#Problem 1
Suppose the seller says that the mean life of a fan is more than 15,000 hours. In a sample of 40 fans, it was found that they only last 14,900 hours on average. Assume the population standard deviation is 110 hours. At .05 significance level, can we reject the claim by the seller? 

#The null hypothesis is µ = 15000. 
#Begin with computing the test statistic.
xbar = 14900
#sample mean
mu0 = 15000 
# hypothesized value 
sigma = 110 # population standard deviation
n = 40 # sample size 
z = (xbar-mu0)/(sigma/sqrt(n)) 
z # test statistic 
#[1] -5.749596
pnorm(-5.749596)
#[1] 4.472847e-09
#The critical value at .05 significance level
alpha = .05 
z.alpha = qnorm(1-alpha) 
-z.alpha # critical value 
#[1] -1.6449

#The test statistic -4.5644 is less than the critical value of
#-1.6449. Hence, at .05 significance level, 
#We reject the claim that mean life of a fan is above 15,000 hours.

#Problem 2:
Suppose the chocolate wrapper states that there is at most 4 grams of saturated fat in a single chocolate. In a sample of 70 chocolates , it is found that the mean amount of saturated fat per chocolate is 4.2 grams. Assume that the population standard deviation is 0.50 grams. At .05 significance level, can we reject the claim on wrapper? 


#######
#The null hypothesis is that µ = 4. 
#We begin with computing the test statistic.
xbar1=4.2 # sample mean
mu01=4 # Hypothesized Value
sigma1=0.50 # population standard deviation
n1=70 # sample size
z1=(xbar1-mu01)/(sigma1/sqrt(n1))
z1
#[1] 3.34664
#We apply the pnorm function to compute the
#upper tail p-value of the test statistic. 
pnorm(z1)
#[1] 0.999591
1-pnorm(z1)
#[1] 0.0004089867
pnorm(z1,lower.tail=FALSE)
#[1] 0.0004089867
# As it turns out to be less than the .05 
# significance level, we reject the null 
# hypothesis that µ = 4.
###Or#######
alpha=0.05
z.alpha = qnorm(1-alpha)
z.alpha
# The test statistic 3.344>critical value of
# 1.6449. Hence, at .05 significance level, 
# we reject the claim that there is at most 
# 4 grams of saturated fat in a chocolate.

## Problem 3 ###
Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. Assume the population standard deviation is 2.5 kg. At .05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year? 


#The null hypothesis is that µ = 15.4. 
xbar = 14.6 # sample mean
mu0 = 15.4 # hypothesized value
sigma = 2.5 # population standard deviation
n = 35 # sample size
z = (xbar-mu0)/(sigma/sqrt(n))
z
#-1.893146
alpha = .05 
z.half.alpha = qnorm(1-alpha/2) 
c(-z.half.alpha, z.half.alpha)
#[1] -1.959964  1.959964

##The test statistic -1.8931 lies between the critical values
##-1.9600 and 1.9600. 
##Hence, at .05 significance level, 
##we do not reject the null hypothesis 
##that the mean penguin weight does not differ from last year.

##
##We apply pnorm to compute 2-tailed p-value
##of the test statistic. 
##It doubles the lower tail p-value as the 
##sample mean is less than the hypothesized value.
##Since it turns out to be > .05 significance level, 
##we do not reject the null hypothesis that µ = 15.4.
pval = 2 * pnorm(z)
pval
#[1] 0.05833852

###--------------------
## R code for Cardio problem

setwd("")
mydata=read.csv("CardioGoodFitness.csv", header = TRUE)
attach(mydata)
names(mydata)
summary(mydata)
t.test(Miles~Gender, var.equal=TRUE)
dim(mydata)

####
#Hypothesis Testing-Two sample Test-Luggage

Independent t-test: two sample

#A hotel manager looks to enhance the initial impressions that hotel guests have when they check in. Contributing to initial impressions is the time it takes to deliver a guest's luggage to the room after check-in. A random sample of 20 deliveries on a particular day were selected in Wing A of the hotel, and a random sample of 20 deliveries were selected in Wing B. The results are stored in Luggage.csv. Analyze the data and determine whether there is a difference between the mean delivery times in the two wings of the hotel. (Use a=0.05)

setwd("C:\\Users\\gmanish\\Dropbox\\latest\\openminds\\slides\\ProbStats\\")
Mydata=read.csv("Luggage.csv",header = TRUE)
attach(Mydata)
t.test(WingA,WingB, var.equal = TRUE, alternative = "two.sided")
t.test(WingA,WingB)
boxplot(WingA,WingB, 
col = c("Red","Pink"), horizontal = TRUE)

####
Paired t-test.
#The file Concrete1.csv contains the compressive strength, in thousands of pounds per square inch (psi), of 40 samples of concrete taken two and seven days after pouring. At 0.01 level of significance, is there evidence that the mean strength is lower at two days than at seven days?

# R code for concrete problem
setwd("")
mydata=read.csv("Concrete1.csv", header = TRUE)
mydata
attach(mydata)
names(mydata)
t.test(TwoDays, SevenDays, paired = TRUE, conf.level = 0.99, alternative = "greater")

####
#Example problem-chi square Test

A company is considering an organizational change involving the use of self-managed work teams. To assess the attitudes of employees of the company toward this change, a sample of 400 employees is selected and asked whether they favor the institution of self-managed work teams in the organization. Three responses are permitted: favor, neutral, or oppose. The  results of the survey, cross-classified by type of job and attitude toward self managed work teams, are summarized as follows: 



TYPE OF JOB        Favor     Neutral    Oppose   Total
Hourly worker        108       46        71        225
Supervisor            18       12        30         60
Middle management     35       14        26         75
Upper management      24        7         9         40
Total                185       79       136        400

At the 0.05 level of significance, is there evidence of a relationship between attitude toward self-managed work teams and type of job? 

Favor =c(108,18,35,24)
Neutral =c(46,12,14,7)
Oppose =c(71,30,26,9)
mydata=data.frame(Favor, Neutral, Oppose, 
row.names =c("HW","Sup.","MM","UM"))
mydata
chisq.test(mydata)
chisq.test(mydata)$expected
qchisq(0.95,6)

# 95th percentile of chisq distribution at 6 degrees of freedom

round(chisq.test(mydata)$residuals,3)
install.packages('corrplot')
library(corrplot)
corrplot(chisq.test(mydata)$residuals, is.cor = FALSE)
##For a given cell, the size of the circle
##is proportional to the amount of the cell contribution.
##The sign of the standardized residuals is also 
##very important to interpret the association 
##between rows and columns as explained in the block below.
##Positive residuals are in blue. Positive values in cells
##specify an attraction (positive association) 
##between the corresponding row and column variables.
##Negative residuals are in red. This implies a repulsion 
#(negative association) 
##between the corresponding row and column variables
#The contribution (in %) of a given cell to the total 
#Chi-square score
#is calculated as follow:

contrib <- 100*chisq.test(mydata)$residuals^2/chisq.test(mydata)$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)
chisq.test(mydata)$p.value






============

setwd("D:/Batch41/CSE7315c/Day05")

# Confidence Intervals in t-distribution

dosage <- c(98.6, 102.1, 100.7, 102, 97, 103.4, 98.9, 101.6, 102.9, 105.2)
dosage
sd(dosage)
ttest <- t.test(dosage, conf.level = 0.95, mu = 100)
ttest
ttest$statistic
ttest$parameter
ttest$conf.int
ttest$p.value
ttest$estimate
ttest$null.value
ttest$alternative
ttest$method
ttest$data.name
pt(1.5824,9)

# Critical t value at a specified confidence level and degrees of freedom
conf.level = 0.90
df = 17
qt((1-conf.level)/2, df)
qt(0.025,24)
qt(0.025, 19)
qt(0.975, 19)

z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
xbar = 80
sdx = 4
c(xbar - z * sdx, xbar + z * sdx)

# 2-sample UNPAIRED t-test

rifampicinT <- c(1.15, 1.15, 0.92, 1.28, 0.72, 0.67, 1, 0.79,
                 0.76, 0.95, 0.67, 0.82, 1.06, 1.21, 0.82)
rifampicinC <- c(0.81, 0.56, 0.46, 1.06, 0.45, 0.43, 0.43,
                 0.88, 0.37, 0.54, 0.73, 0.73, 0.68, 0.43, 0.93)
ttest2U <- t.test(rifampicinT,rifampicinC, conf.level = 0.95,
                 var.equal = TRUE)
ttest2U

# 2-sample PAIRED t-test

treatmentA <- c(63, 54, 79, 68, 87, 84, 92, 57, 66, 53, 76, 63)
treatmentB <- c(55, 62, 108, 77, 83, 78, 79, 94, 69, 66, 72, 77)
ttest2P <- t.test(treatmentA, treatmentB, paired = TRUE, conf.level = 0.95)
ttest2P

# Critical chi-square value at a specified confidence level and degrees of freedom
qchisq(0.95,4)
qchisq(0.99,4)

# Critical F value at a specified confidence level and degrees of freedom
qf(0.025,9,11,lower.tail = FALSE)
qf(0.025,9,11)
qf(0.975,9,11,lower.tail = FALSE)
qf(0.975,9,11)
qf(0.9,2,24)

# Generate F tables (OPTIONAL)
# Code from http://wiki.socr.umich.edu/index.php/SMHS_ProbabilityDistributions#Generating_Probability_Tables
# Define the right-tail probability of interest, alpha
right_tail_p <- 0.5

# Define the vectors storing the indices corresponding to numerator (n1)
# and denominator (n2, row) degrees of freedom for F(alpha, n1, n2).
# Note the Inf corresponds to Infinity.

n1 <- c(1,2,3,4,5,6,7,8,9,10,11,12,15,20,24,30,40,60,120,Inf)
n2 <- c(1:30,40,60,120,Inf)

# Define precision (4-decimal point accuracy)
options(digits=4)

# Generate an empty matrix of critical F-values
f_table <- matrix(ncol = length(n1), nrow = length(n2))

# Use the F-Distribution quantile function to fill in the matrix values in a nested 2-loop
# Recall that the density (df), distribution function (pf), quantile function (qf) and
# random generation (rf) for the F-distribution

for (i in 1:length(n2)){
    for (j in 1:length(n1)){
        f_table[i,j] <- qf(right_tail_p, n1[j], n2[i], lower.tail = FALSE)
    }
}

# Print results
f_table

# Label rows and columns
rownames(f_table) <- n2; colnames(f_table) <- n1
f_table

# Save results to a file
# write.table(f_table, file="C:\\User\\f_table.txt")

==============================

#http://www.r-tutor.com/elementary-statistics/hypothesis-testing/lower-tail-test-population-mean-known-variance

Lower Tail Test of Population Mean with Known Variance
Problem
Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. Assume the population standard deviation is 120 hours. At .05 significance level, can we reject the claim by the manufacturer?

Solution
The null hypothesis is that μ ≥ 10000. We begin with computing the test statistic.

> xbar = 9900            # sample mean 
> mu0 = 10000            # hypothesized value 
> sigma = 120            # population standard deviation 
> n = 30                 # sample size 
> z = (xbar−mu0)/(sigma/sqrt(n)) 
> z                      # test statistic 
[1] −4.5644
We then compute the critical value at .05 significance level.

> alpha = .05 
> z.alpha = qnorm(1−alpha) 
> −z.alpha               # critical value 
[1] −1.6449
Answer
The test statistic -4.5644 is less than the critical value of -1.6449. Hence, at .05 significance level, we reject the claim that mean lifetime of a light bulb is above 10,000 hours.

Alternative Solution
Instead of using the critical value, we apply the pnorm function to compute the lower tail p-value of the test statistic. As it turns out to be less than the .05 significance level, we reject the null hypothesis that μ ≥ 10000.

> pval = pnorm(z) 
> pval                   # lower tail p−value 
[1] 2.5052e−06



Upper Tail Test of Population Mean with Known Variance

Problem
Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams. Assume that the population standard deviation is 0.25 grams. At .05 significance level, can we reject the claim on food label?

Solution
The null hypothesis is that μ ≤ 2. We begin with computing the test statistic.

> xbar = 2.1             # sample mean 
> mu0 = 2                # hypothesized value 
> sigma = 0.25           # population standard deviation 
> n = 35                 # sample size 
> z = (xbar−mu0)/(sigma/sqrt(n)) 
> z                      # test statistic 
[1] 2.3664
We then compute the critical value at .05 significance level.

> alpha = .05 
> z.alpha = qnorm(1−alpha) 
> z.alpha                # critical value 
[1] 1.6449
Answer
The test statistic 2.3664 is greater than the critical value of 1.6449. Hence, at .05 significance level, we reject the claim that there is at most 2 grams of saturated fat in a cookie.

Alternative Solution
Instead of using the critical value, we apply the pnorm function to compute the upper tail p-value of the test statistic. As it turns out to be less than the .05 significance level, we reject the null hypothesis that μ ≤ 2.

> pval = pnorm(z, lower.tail=FALSE) 
> pval                   # upper tail p−value 
[1] 0.0089802



Two-Tailed Test of Population Mean with Known Variance

Problem
Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. Assume the population standard deviation is 2.5 kg. At .05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year?

Solution
The null hypothesis is that μ = 15.4. We begin with computing the test statistic.

> xbar = 14.6            # sample mean 
> mu0 = 15.4             # hypothesized value 
> sigma = 2.5            # population standard deviation 
> n = 35                 # sample size 
> z = (xbar−mu0)/(sigma/sqrt(n)) 
> z                      # test statistic 
[1] −1.8931
We then compute the critical values at .05 significance level.

> alpha = .05 
> z.half.alpha = qnorm(1−alpha/2) 
> c(−z.half.alpha, z.half.alpha) 
[1] −1.9600  1.9600
Answer
The test statistic -1.8931 lies between the critical values -1.9600 and 1.9600. Hence, at .05 significance level, we do not reject the null hypothesis that the mean penguin weight does not differ from last year.

Alternative Solution
Instead of using the critical value, we apply the pnorm function to compute the two-tailed p-value of the test statistic. It doubles the lower tail p-value as the sample mean is less than the hypothesized value. Since it turns out to be greater than the .05 significance level, we do not reject the null hypothesis that μ = 15.4.

> pval = 2 * pnorm(z)    # lower tail 
> pval                   # two−tailed p−value 
[1] 0.058339


Lower Tail Test of Population Mean with Unknown Variance

Problem
Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. Assume the sample standard deviation is 125 hours. At .05 significance level, can we reject the claim by the manufacturer?

Solution
The null hypothesis is that μ ≥ 10000. We begin with computing the test statistic.

> xbar = 9900            # sample mean 
> mu0 = 10000            # hypothesized value 
> s = 125                # sample standard deviation 
> n = 30                 # sample size 
> t = (xbar−mu0)/(s/sqrt(n)) 
> t                      # test statistic 
[1] −4.3818
We then compute the critical value at .05 significance level.

> alpha = .05 
> t.alpha = qt(1−alpha, df=n−1) 
> −t.alpha               # critical value 
[1] −1.6991
Answer
The test statistic -4.3818 is less than the critical value of -1.6991. Hence, at .05 significance level, we can reject the claim that mean lifetime of a light bulb is above 10,000 hours.

Alternative Solution
Instead of using the critical value, we apply the pt function to compute the lower tail p-value of the test statistic. As it turns out to be less than the .05 significance level, we reject the null hypothesis that μ ≥ 10000.

> pval = pt(t, df=n−1) 
> pval                   # lower tail p−value 
[1] 7.035e−05


Upper Tail Test of Population Mean with Unknown Variance

Problem
Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams. Assume that the sample standard deviation is 0.3 gram. At .05 significance level, can we reject the claim on food label?

Solution
The null hypothesis is that μ ≤ 2. We begin with computing the test statistic.

> xbar = 2.1             # sample mean 
> mu0 = 2                # hypothesized value 
> s = 0.3                # sample standard deviation 
> n = 35                 # sample size 
> t = (xbar−mu0)/(s/sqrt(n)) 
> t                      # test statistic 
[1] 1.9720
We then compute the critical value at .05 significance level.

> alpha = .05 
> t.alpha = qt(1−alpha, df=n−1) 
> t.alpha                # critical value 
[1] 1.6991
Answer
The test statistic 1.9720 is greater than the critical value of 1.6991. Hence, at .05 significance level, we can reject the claim that there is at most 2 grams of saturated fat in a cookie.

Alternative Solution
Instead of using the critical value, we apply the pt function to compute the upper tail p-value of the test statistic. As it turns out to be less than the .05 significance level, we reject the null hypothesis that μ ≤ 2.

> pval = pt(t, df=n−1, lower.tail=FALSE) 
> pval                   # upper tail p−value 
[1] 0.028393


Two-Tailed Test of Population Mean with Unknown Variance

Problem
Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. Assume the sample standard deviation is 2.5 kg. At .05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year?

Solution
The null hypothesis is that μ = 15.4. We begin with computing the test statistic.

> xbar = 14.6            # sample mean 
> mu0 = 15.4             # hypothesized value 
> s = 2.5                # sample standard deviation 
> n = 35                 # sample size 
> t = (xbar−mu0)/(s/sqrt(n)) 
> t                      # test statistic 
[1] −1.8931
We then compute the critical values at .05 significance level.

> alpha = .05 
> t.half.alpha = qt(1−alpha/2, df=n−1) 
> c(−t.half.alpha, t.half.alpha) 
[1] −2.0322  2.0322
Answer
The test statistic -1.8931 lies between the critical values -2.0322, and 2.0322. Hence, at .05 significance level, we do not reject the null hypothesis that the mean penguin weight does not differ from last year.

Alternative Solution
Instead of using the critical value, we apply the pt function to compute the two-tailed p-value of the test statistic. It doubles the lower tail p-value as the sample mean is less than the hypothesized value. Since it turns out to be greater than the .05 significance level, we do not reject the null hypothesis that μ = 15.4.

> pval = 2 ∗ pt(t, df=n−1)  # lower tail 
> pval                      # two−tailed p−value 
[1] 0.066876


Lower Tail Test of Population Proportion

Problem
Suppose 60% of citizens voted in last election. 85 out of 148 people in a telephone survey said that they voted in current election. At 0.5 significance level, can we reject the null hypothesis that the proportion of voters in the population is above 60% this year?

Solution
The null hypothesis is that p ≥ 0.6. We begin with computing the test statistic.

> pbar = 85/148          # sample proportion 
> p0 = .6                # hypothesized value 
> n = 148                # sample size 
> z = (pbar−p0)/sqrt(p0∗(1−p0)/n) 
> z                      # test statistic 
[1] −0.6376
We then compute the critical value at .05 significance level.

> alpha = .05 
> z.alpha = qnorm(1−alpha) 
> −z.alpha               # critical value 
[1] −1.6449
Answer
The test statistic -0.6376 is not less than the critical value of -1.6449. Hence, at .05 significance level, we do not reject the null hypothesis that the proportion of voters in the population is above 60% this year.

Alternative Solution 1
Instead of using the critical value, we apply the pnorm function to compute the lower tail p-value of the test statistic. As it turns out to be greater than the .05 significance level, we do not reject the null hypothesis that p ≥ 0.6.

> pval = pnorm(z) 
> pval                   # lower tail p−value 
[1] 0.26187
Alternative Solution 2
We apply the prop.test function to compute the p-value directly. The Yates continuity correction is disabled for pedagogical reasons.

> prop.test(85, 148, p=.6, alt="less", correct=FALSE) 
 
        1−sample proportions test without continuity 
        correction 
 
data:  85 out of 148, null probability 0.6 
X−squared = 0.4065, df = 1, p−value = 0.2619 
alternative hypothesis: true p is less than 0.6 
95 percent confidence interval: 
 0.0000 0.63925 
sample estimates: 
      p 
0.57432


Upper Tail Test of Population Proportion

Problem
Suppose that 12% of apples harvested in an orchard last year was rotten. 30 out of 214 apples in a harvest sample this year turns out to be rotten. At .05 significance level, can we reject the null hypothesis that the proportion of rotten apples in harvest stays below 12% this year?

Solution
The null hypothesis is that p ≤ 0.12. We begin with computing the test statistic.

> pbar = 30/214          # sample proportion 
> p0 = .12               # hypothesized value 
> n = 214                # sample size 
> z = (pbar−p0)/sqrt(p0∗(1−p0)/n) 
> z                      # test statistic 
[1] 0.90875
We then compute the critical value at .05 significance level.

> alpha = .05 
> z.alpha = qnorm(1−alpha) 
> z.alpha                # critical value 
[1] 1.6449
Answer
The test statistic 0.90875 is not greater than the critical value of 1.6449. Hence, at .05 significance level, we do not reject the null hypothesis that the proportion of rotten apples in harvest stays below 12% this year.

Alternative Solution 1
Instead of using the critical value, we apply the pnorm function to compute the upper tail p-value of the test statistic. As it turns out to be greater than the .05 significance level, we do not reject the null hypothesis that p ≤ 0.12.

> pval = pnorm(z, lower.tail=FALSE) 
> pval                   # upper tail p−value 
[1] 0.18174
Alternative Solution 2
We apply the prop.test function to compute the p-value directly. The Yates continuity correction is disabled for pedagogical reasons.

> prop.test(30, 214, p=.12, alt="greater", correct=FALSE) 
 
        1−sample proportions test without continuity 
        correction 
 
data:  30 out of 214, null probability 0.12 
X−squared = 0.8258, df = 1, p−value = 0.1817 
alternative hypothesis: true p is greater than 0.12 
95 percent confidence interval: 
 0.10563 1.00000 
sample estimates: 
      p 
0.14019


Two-Tailed Test of Population Proportion

Problem
Suppose a coin toss turns up 12 heads out of 20 trials. At .05 significance level, can one reject the null hypothesis that the coin toss is fair?

Solution
The null hypothesis is that p = 0.5. We begin with computing the test statistic.

> pbar = 12/20           # sample proportion 
> p0 = .5                # hypothesized value 
> n = 20                 # sample size 
> z = (pbar−p0)/sqrt(p0∗(1−p0)/n) 
> z                      # test statistic 
[1] 0.89443
We then compute the critical values at .05 significance level.

> alpha = .05 
> z.half.alpha = qnorm(1−alpha/2) 
> c(−z.half.alpha, z.half.alpha) 
[1] −1.9600  1.9600
Answer
The test statistic 0.89443 lies between the critical values -1.9600 and 1.9600. Hence, at .05 significance level, we do not reject the null hypothesis that the coin toss is fair.

Alternative Solution 1
Instead of using the critical value, we apply the pnorm function to compute the two-tailed p-value of the test statistic. It doubles the upper tail p-value as the sample proportion is greater than the hypothesized value. Since it turns out to be greater than the .05 significance level, we do not reject the null hypothesis that p = 0.5.

> pval = 2 ∗ pnorm(z, lower.tail=FALSE)  # upper tail 
> pval                   # two−tailed p−value 
[1] 0.37109
Alternative Solution 2
We apply the prop.test function to compute the p-value directly. The Yates continuity correction is disabled for pedagogical reasons.

> prop.test(12, 20, p=0.5, correct=FALSE) 
 
        1−sample proportions test without continuity 
        correction 
 
data:  12 out of 20, null probability 0.5 
X−squared = 0.8, df = 1, p−value = 0.3711 
alternative hypothesis: true p is not equal to 0.5 
95 percent confidence interval: 
  0.38658 0.78119 
sample estimates: 
  p 
0.6


Type II Error

In hypothesis testing, a type II error is due to a failure of rejecting an invalid null hypothesis. The probability of avoiding a type II error is called the power of the hypothesis test, and is denoted by the quantity 1 - β .

In the following tutorials, we demonstrate how to compute the power of a hypothesis test based on scenarios from our previous discussions on hypothesis testing. The approach is based on a parametric estimate of the region where the null hypothesis would not be rejected. The probability of a type II error is then derived based on a hypothetical true value.

Type II Error in Lower Tail Test of Population Mean with Known Variance

Problem
Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. Assume actual mean light bulb lifetime is 9,950 hours and the population standard deviation is 120 hours. At .05 significance level, what is the probability of having type II error for a sample size of 30 light bulb?

Solution
We begin with computing the standard deviation of the mean, sem.

> n = 30                # sample size 
> sigma = 120           # population standard deviation 
> sem = sigma/sqrt(n); sem   # standard error 
[1] 21.909
We next compute the lower bound of sample means for which the null hypothesis μ ≥ 10000 would not be rejected.

> alpha = .05           # significance level 
> mu0 = 10000           # hypothetical lower bound 
> q = qnorm(alpha, mean=mu0, sd=sem); q 
[1] 9964
Therefore, so long as the sample mean is greater than 9964 in a hypothesis test, the null hypothesis will not be rejected. Since we assume that the actual population mean is 9950, we can compute the probability of the sample mean above 9964, and thus found the probability of type II error.

> mu = 9950             # assumed actual mean 
> pnorm(q, mean=mu, sd=sem, lower.tail=FALSE) 
[1] 0.26196
Answer
If the light bulbs sample size is 30, the actual mean light bulb lifetime is 9,950 hours and the population standard deviation is 120 hours, then the probability of type II error for testing the null hypothesis μ ≥ 10000 at .05 significance level is 26.2%, and the power of the hypothesis test is 73.8%.

Type II Error in Upper Tail Test of Population Mean with Known Variance

Problem
Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. Assume the actual mean amount of saturated fat per cookie is 2.09 grams, and the population standard deviation is 0.25 grams. At .05 significance level, what is the probability of having type II error for a sample size of 35 cookies?

Solution
We begin with computing the standard deviation of the mean, sem.

> n = 35                # sample size 
> sigma = 0.25          # population standard deviation 
> sem = sigma/sqrt(n); sem   # standard error 
[1] 0.042258
We next compute the upper bound of sample means for which the null hypothesis μ ≤ 2 would not be rejected.

> alpha = .05           # significance level 
> mu0 = 2               # hypothetical upper bound 
> q = qnorm(alpha, mean=mu0, sd=sem, lower.tail=FALSE); q 
[1] 2.0695
Therefore, so long as the sample mean is less than 2.0695 in a hypothesis test, the null hypothesis will not be rejected. Since we assume that the actual population mean is 2.09, we can compute the probability of the sample mean below 2.0695, and thus found the probability of type II error.

> mu = 2.09             # assumed actual mean 
> pnorm(q, mean=mu, sd=sem) 
[1] 0.31386
Answer
If the cookies sample size is 35, the actual mean amount of saturated fat per cookie is 2.09 grams and the population standard deviation is 0.25 grams, then the probability of type II error for testing the null hypothesis μ ≤ 2 at .05 significance level is 31.4%, and the power of the hypothesis test is 68.6%.

Type II Error in Two-Tailed Test of Population Mean with Known Variance

Problem
Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. Assume the actual mean population weight is 15.1 kg, and the population standard deviation is 2.5 kg. At .05 significance level, what is the probability of having type II error for a sample size of 35 penguins?

Solution
We begin with computing the standard deviation of the mean, sem.

> n = 35                # sample size 
> sigma = 2.5           # population standard deviation 
> sem = sigma/sqrt(n); sem   # standard error 
[1] 0.42258
We next compute the lower and upper bounds of sample means for which the null hypothesis μ = 15.4 would not be rejected.

> alpha = .05           # significance level 
> mu0 = 15.4            # hypothetical mean 
> I = c(alpha/2, 1-alpha/2) 
> q = qnorm(I, mean=mu0, sd=sem); q 
[1] 14.572 16.228
Therefore, so long as the sample mean is between 14.572 and 16.228 in a hypothesis test, the null hypothesis will not be rejected. Since we assume that the actual population mean is 15.1, we can compute the lower tail probabilities of both end points.

> mu = 15.1             # assumed actual mean 
> p = pnorm(q, mean=mu, sd=sem); p 
[1] 0.10564 0.99621
Finally, the probability of type II error is the probability between the two end points.

> diff(p)               # p[2]-p[1] 
[1] 0.89056
Answer
If the penguin sample size is 35, the actual mean population weight is 15.1 kg and the population standard deviation is 2.5 kg, then the probability of type II error for testing the null hypothesis μ = 15.4 at .05 significance level is 89.1%, and the power of the hypothesis test is 10.9%.

Type II Error in Lower Tail Test of Population Mean with Unknown Variance

Problem
Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. Assume in a random sample of 30 light bulbs, the standard deviation of the lifetime is 125 hours. If actual mean light bulb lifetime is 9,950 hours, what is the probability of type II error for a hypothesis test at .05 significance level?

Solution
We begin with computing the standard error estimate, SE.

> n = 30                # sample size 
> s = 125               # sample standard deviation 
> SE = s/sqrt(n); SE    # standard error estimate 
[1] 22.822
We next compute the lower bound of sample means for which the null hypothesis μ ≥ 10000 would not be rejected.

> alpha = .05           # significance level 
> mu0 = 10000           # hypothetical lower bound 
> q = mu0 + qt(alpha, df=n-1) * SE; q 
[1] 9961.2
Therefore, so long as the sample mean is greater than 9961.2 in a hypothesis test, the null hypothesis will not be rejected. Since we assume that the actual population mean is 9950, we can compute the probability of the sample mean above 9961.2, and thus found the probability of type II error.

> mu = 9950             # assumed actual mean 
> pt((q - mu)/SE, df=n-1, lower.tail=FALSE) 
[1] 0.31329
Answer
If the light bulbs sample size is 30, the sample standard variance is 125 hours and the actual mean light bulb lifetime is 9,950 hours, then the probability of type II error for testing the null hypothesis μ ≥ 10000 at .05 significance level is 31.3%, and the power of the hypothesis test is 68.7%.

Type II Error in Upper Tail Test of Population Mean with Unknown Variance

Problem
Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. Assume in a random sample of 35 cookies, the standard deviation of saturated fat is 0.3 grams. If actual mean amount of saturated fat per cookie is 2.09 grams, what is the probability of type II error for a hypothesis test at .05 significance level?

Solution
We begin with computing the standard error estimate, SE.

> n = 35                # sample size 
> s = 0.3               # sample standard deviation 
> SE = s/sqrt(n); SE    # standard error estimate 
[1] 0.050709
We next compute the upper bound of sample means for which the null hypothesis μ ≤ 2 would not be rejected.

> alpha = .05           # significance level 
> mu0 = 2               # hypothetical upper bound 
> q = mu0 + qt(alpha, df=n-1, lower.tail=FALSE) * SE; q 
[1] 2.0857
Therefore, so long as the sample mean is less than 2.0857 in a hypothesis test, the null hypothesis will not be rejected. Since we assume that the actual population mean is 2.09, we can compute the probability of the sample mean below 2.0857, and thus found the probability of type II error.

> mu = 2.09             # assumed actual mean 
> pt((q - mu)/SE, df=n-1) 
[1] 0.46681
Answer
If the cookies sample size is 35, the sample standard deviation of saturated fat per cookie is 0.3 grams and the actual mean amount of saturated fat per cookie is 2.09 grams, then the probability of type II error for testing the null hypothesis μ ≤ 2 at .05 significance level is 46.7%, and the power of the hypothesis test is 53.3%.

Type II Error in Two-Tailed Test of Population Mean with Unknown Variance

Problem
Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. Assume in a random sample 35 penguins, the standard deviation of the weight is 2.5 kg. If actual mean penguin weight is 15.1 kg, what is the probability of type II error for a hypothesis test at .05 significance level?

Solution
We begin with computing the standard error estimate, SE.

> n = 35                # sample size 
> s = 2.5               # sample standard deviation 
> SE = s/sqrt(n); SE    # standard error estimate 
[1] 0.42258
We next compute the lower and upper bounds of sample means for which the null hypothesis μ = 15.4 would not be rejected.

> alpha = .05           # significance level 
> mu0 = 15.4            # hypothetical mean 
> I = c(alpha/2, 1-alpha/2) 
> q = mu0 + qt(I, df=n-1) * SE; q 
[1] 14.541 16.259
Therefore, so long as the sample mean is between 14.541 and 16.259 in a hypothesis test, the null hypothesis will not be rejected. Since we assume that the actual population mean is 15.1, we can compute the lower tail probabilities of both end points.

> mu = 15.1             # assumed actual mean 
> p = pt((q - mu)/SE, df=n-1); p 
[1] 0.097445 0.995168
Finally, the probability of type II error is the probability between the two end points.

> diff(p)               # p[2]-p[1] 
[1] 0.89772
Answer
If the penguin sample size is 35, the sample standard deviation of penguin weight is 2.5 kg and the actual mean population weight is 15.1 kg, then the probability of type II error for testing the null hypothesis μ = 15.4 at .05 significance level is 89.8%, and the power of the hypothesis test is 10.2%.


Inference About Two Populations

Population Mean Between Two Matched Samples

Example
In the built-in data set named immer, the barley yield in years 1931 and 1932 of the same field are recorded. The yield data are presented in the data frame columns Y1 and Y2.

> library(MASS)         # load the MASS package 
> head(immer) 
  Loc Var    Y1    Y2 
1  UF   M  81.0  80.7 
2  UF   S 105.4  82.3 
    .....
Problem
Assuming that the data in immer follows the normal distribution, find the 95% confidence interval estimate of the difference between the mean barley yields between years 1931 and 1932.

Solution
We apply the t.test function to compute the difference in means of the matched samples. As it is a paired test, we set the "paired" argument as TRUE.

> t.test(immer$Y1, immer$Y2, paired=TRUE) 
 
           Paired t-test 
 
data:  immer$Y1 and immer$Y2 
t = 3.324, df = 29, p-value = 0.002413 
alternative hypothesis: true difference in means is not equal to 0 
95 percent confidence interval: 
  6.122 25.705 
sample estimates: 
mean of the differences 
                 15.913
Answer
Between years 1931 and 1932 in the data set immer, the 95% confidence interval of the difference in means of the barley yields is the interval between 6.122 and 25.705.


#https://www.r-bloggers.com/paired-students-t-test/

Comparison of the means of two sets of paired samples, taken from two populations with unknown variance.

A school athletics has taken a new instructor, and want to test the effectiveness of the new type of training proposed by comparing the average times of 10 runners in the 100 meters. Are below the time in seconds before and after training for each athlete.

Before training: 12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3
After training: 12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1
In this case we have two sets of paired samples, since the measurements were made on the same athletes before and after the workout. To see if there was an improvement, deterioration, or if the means of times have remained substantially the same (hypothesis H0), we need to make a Student’s t-test for paired samples, proceeding in this way:



a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)

t.test(a,b, paired=TRUE)

    Paired t-test

data: a and b
t = -0.2133, df = 9, p-value = 0.8358
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
    -0.5802549 0.4802549
sample estimates:
mean of the differences
    -0.05
The p-value is greater than 0.05, then we can accept the hypothesis H0 of equality of the averages. In conclusion, the new training has not made any significant improvement (or deterioration) to the team of athletes.
Similarly, we calculate the t-tabulated value:


qt(0.975, 9)
[1] 2.262157
t-computed < t-tabulated, so we accept the null hypothesis H0.


Suppose now that the manager of the team (given the results obtained) fired the coach who has not made any improvement, and take another, more promising. We report the times of athletes after the second training:

Before training: 12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3
After the second training: 12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0
Now we check if there was actually an improvement, ie perform a t-test for paired data, specifying in R to test the alternative hypothesis H1 of improvement in times. To do this simply add the syntax alt = "less" when you call the t-test:


a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0)

t.test(a,b, paired=TRUE, alt="less")

    Paired t-test

data: a and b
t = 5.2671, df = 9, p-value = 0.9997
alternative hypothesis: true difference in means is less than 0
95 percent confidence interval:
    -Inf 2.170325
sample estimates:
mean of the differences
     1.61
With this syntax we asked R to check whether the mean of the values contained in the vector a is less of the mean of the values contained in the vector b. In response, we obtained a p-value well above 0.05, which leads us to conclude that we can reject the null hypothesis H0 in favor of the alternative hypothesis H1: the new training has made substantial improvements to the team.

If we had written: t.test (a, b, paired = TRUE, alt = "greater"), we asked R to check whether the mean of the values contained in the vector a is greater than the mean of the values contained in the vector b. In light of the previous result, we can suspect that the p-value will be much smaller than 0.05, and in fact:


a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0)

t.test(a,b, paired=TRUE, alt="greater")

    Paired t-test

data: a and b
t = 5.2671, df = 9, p-value = 0.0002579
alternative hypothesis: true difference in means is greater than 0
95 percent confidence interval:
    1.049675 Inf
sample estimates:
mean of the differences
    1.61
	
	
	
	
	
	
Population Mean Between Two Independent Samples

Example
In the data frame column mpg of the data set mtcars, there are gas mileage data of various 1974 U.S. automobiles.

> mtcars$mpg 
 [1] 21.0 21.0 22.8 21.4 18.7 ...
Meanwhile, another data column in mtcars, named am, indicates the transmission type of the automobile model (0 = automatic, 1 = manual).

> mtcars$am 
 [1] 1 1 1 0 0 0 0 0 ...
In particular, the gas mileage for manual and automatic transmissions are two independent data populations.

Problem
Assuming that the data in mtcars follows the normal distribution, find the 95% confidence interval estimate of the difference between the mean gas mileage of manual and automatic transmissions.

Solution
As mentioned in the tutorial Data Frame Row Slice, the gas mileage for automatic transmission can be listed as follows:

> L = mtcars$am == 0 
> mpg.auto = mtcars[L,]$mpg 
> mpg.auto                    # automatic transmission mileage 
 [1] 21.4 18.7 18.1 14.3 24.4 ...
By applying the negation of L, we can find the gas mileage for manual transmission.

> mpg.manual = mtcars[!L,]$mpg 
> mpg.manual                  # manual transmission mileage 
 [1] 21.0 21.0 22.8 32.4 30.4 ...
We can now apply the t.test function to compute the difference in means of the two sample data.

> t.test(mpg.auto, mpg.manual) 
 
        Welch Two Sample t-test 
 
data:  mpg.auto and mpg.manual 
t = -3.7671, df = 18.332, p-value = 0.001374 
alternative hypothesis: true difference in means is not equal to 0 
95 percent confidence interval: 
 -11.2802  -3.2097 
sample estimates: 
mean of x mean of y 
   17.147    24.392
Answer
In mtcars, the mean mileage of automatic transmission is 17.147 mpg and the manual transmission is 24.392 mpg. The 95% confidence interval of the difference in mean gas mileage is between 3.2097 and 11.2802 mpg.

Alternative Solution
We can model the response variable mtcars$mpg by the predictor mtcars$am, and then apply the t.test function to estimate the difference of the population means.

> t.test(mpg ~ am, data=mtcars) 
 
        Welch Two Sample t-test 
 
data:  mpg by am 
t = -3.7671, df = 18.332, p-value = 0.001374 
alternative hypothesis: true difference in means is not equal to 0 
95 percent confidence interval: 
 -11.2802  -3.2097 
sample estimates: 
mean in group 0 mean in group 1 
         17.147          24.392
		 

		 
Comparison of Two Population Proportions

Example
In the built-in data set named quine, children from an Australian town is classified by ethnic background, gender, age, learning status and the number of days absent from school.

> library(MASS)         # load the MASS package 
> head(quine) 
  Eth Sex Age Lrn Days 
1   A   M  F0  SL    2 
2   A   M  F0  SL   11 
    .....
In effect, the data frame column Eth indicates whether the student is Aboriginal or Not ("A" or "N"), and the column Sex indicates Male or Female ("M" or "F").

In R, we can tally the student ethnicity against the gender with the table function. As the result shows, within the Aboriginal student population, 38 students are female. Whereas within the Non-Aboriginal student population, 42 are female.

> table(quine$Eth, quine$Sex) 
 
     F  M 
  A 38 31 
  N 42 35
Problem
Assuming that the data in quine follows the normal distribution, find the 95% confidence interval estimate of the difference between the female proportion of Aboriginal students and the female proportion of Non-Aboriginal students, each within their own ethnic group.

Solution
We apply the prop.test function to compute the difference in female proportions. The Yates’s continuity correction is disabled for pedagogical reasons.

> prop.test(table(quine$Eth, quine$Sex), correct=FALSE) 
 
        2-sample test for equality of proportions 
        without continuity correction 
 
data:  table(quine$Eth, quine$Sex) 
X-squared = 0.0041, df = 1, p-value = 0.949 
alternative hypothesis: two.sided 
95 percent confidence interval: 
 -0.15642  0.16696 
sample estimates: 
 prop 1  prop 2 
0.55072 0.54545
Answer
The 95% confidence interval estimate of the difference between the female proportion of Aboriginal students and the female proportion of Non-Aboriginal students is between -15.6% and 16.7%.
