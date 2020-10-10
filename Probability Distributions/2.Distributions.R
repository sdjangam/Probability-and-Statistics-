# free memory
rm(list = ls())
gc()

#Command library loads the package MASS (for Modern Applied Statistics with S) into memory. Command data() will list all the datasets in loaded packages. The command data(phones) will load the data set phones into memory.
data("bodyfat", package="TH.data")
library(MASS)
data()

str(iris)
dim(iris)
names(iris)
attributes(iris)
summary(iris)

iris[1:5,]
head(iris)
tail(iris)

iris[1:10, "Sepal.Length"]
iris$Sepal.Length[1:10]

##########################################################################################################################################################

Uniform

Problem: Select ten random numbers between one and three.

Solution
We apply the generation function runif of the uniform distribution to generate ten random numbers between one and three.

> runif(10, min=1, max=3) 
[1] 1.6121 1.2028 1.9306 2.4233 1.6874 1.1502 2.7068 
[8] 1.4455 2.4122 2.2171

Exercise:

Generating random numbers. Set your seed to 1 and generate 10 random numbers using runif and save it in an object called random_numbers.

set.seed(1)
random_numbers <- runif(10)

Exercise:

We can simulate rolling a die in R with runif. Save in an object called die_roll 1 random number with min = 0 and max = 6. This means that we will generate a random number between 1 and 6.

Apply the function ceiling to die_roll. Don’t forget to set the seed to 1 before calling runif.

set.seed(1)
(die_roll <- runif( n = 1, min = 0, max = 6))
## [1] 1.593052
(ceiling(die_roll))
## [1] 2

Exercise:

Generating dice rolls Set your seed to 1 and generate 30 random numbers using runif. Save it in an object called random_numbers. Then use the ceiling function to round the values. These values represent rolling dice values.

if (!'MASS' %in% installed.packages()) install.packages('MASS')
library(MASS)

set.seed(1)
(random_numbers <- runif(30, min = 0, max = 6))
## [1] 1.5930520 2.2327434 3.4371202 5.4492467 1.2100916 5.3903381 5.6680516
## [8] 3.9647868 3.7746843 0.3707176 1.2358474 1.0593405 4.1221371 2.3046223
## [15] 4.6190485 2.9861955 4.3057110 5.9514366 2.2802111 4.6646713 5.6082314
## [22] 1.2728551 3.9100426 0.7533306 1.6033240 2.3166846 0.0803420 2.2943277
## [29] 5.2181451 2.0420940
ceiling(random_numbers)
## [1] 2 3 4 6 2 6 6 4 4 1 2 2 5 3 5 3 5 6 3 5 6 2 4 1 2 3 1 3 6 3


#####################################################################################################################################################

Normal
x <- seq(-3,3,0.1)
plot(x=x, y=dnorm(x, mean=0, sd=1), type='l')

Problem
Assume that the test scores of a college entrance exam fits a normal distribution. Furthermore, the mean test score is 72, and the standard deviation is 15.2. What is the percentage of students scoring 84 or more in the exam?

Solution
We apply the function pnorm of the normal distribution with mean 72 and standard deviation 15.2. Since we are looking for the percentage of students scoring higher than 84, we are interested in the upper tail of the normal distribution.

> pnorm(84, mean=72, sd=15.2, lower.tail=FALSE) 
[1] 0.21492
Answer
The percentage of students scoring 84 or more in the college entrance exam is 21.5%.

Exercise: Simulate normal distribution values. Imagine a population in which the average height is 1.70 m with an standard deviation of 0.1, using rnorm simulate the height of 100 people and save it in an object called heights.

To get an idea of the values of heights applying the function summaryto it.

set.seed(1)
heights <- rnorm(n = 100, mean = 1.70, sd = .1)
summary(heights)
## Min. 1st Qu. Median Mean 3rd Qu. Max. 
## 1.479 1.651 1.711 1.711 1.769 1.940

Exercise:

a) What’s the probability that a person will be smaller or equal to 1.90 m ? Use pnorm
b) What’s the probability that a person will be taller or equal to 1.60 m? Use pnorm

pnorm(1.90, mean = 1.70, sd = .1)
## [1] 0.9772499
1 - pnorm(1.60, mean = 1.70, sd = .1)
## [1] 0.8413447


Exercise:

90% of the population is smaller than ____________?

qnorm(.90, mean = 1.70, sd = .1)
## [1] 1.828155

Exercise:

Which percentage of the population is bigger than 1.60 m?

pnorm(1.60, mean = 1.70, sd = .1, lower.tail = F)
## [1] 0.8413447



Exercise:

Simulate normal distribution values. Imagine a population in which the average height is 1.70 m with a standard deviation of 0.1. Use rnorm to simulate the height of 1000 people and save it in an object called heights.

a) Plot the density of the simulated values.
b) Generate 10000 values with the same parameters and plot the respective density function on top of the previous plot in red to differentiate it.

This plot will show you how much a sample with 10000 simulations approximate to the real normal distribution.

set.seed(1)
heights <- rnorm(n = 1000, mean = 1.70, sd = .1)
plot(density(heights), main = 'Simulating Heights')


set.seed(1)
heights2 <- rnorm(n = 10000, mean = 1.70, sd = .1)
lines(density(heights2), col = 'red', lwd =2)

Exercise:

Find the 90% interval of a population with mean = 1.70 and standard deviation = .1

c(qnorm(.05, mean = 1.70, sd = .1),qnorm(.95, mean = 1.70, sd = .1))
## [1] 1.535515 1.864485


#####################################################################################################################################################

Binomial
set.seed(1)
x <- seq(0,20,1)
plot(x=x, y=dbinom(x,20,0.2),type='l')
lines(dbinom(x,20,0.4), col = 'red', lwd =2)
lines(dbinom(x,20,0.7), col = 'green', lwd =2)

Problem: Suppose there are twelve multiple choice questions in an English class quiz. Each question has five possible answers, and only one of them is correct. Find the probability of having four or less correct answers if a student attempts to answer every question at random.

Solution: Since only one out of five possible answers is correct, the probability of answering a question correctly by random is 1/5=0.2. We can find the probability of having exactly 4 correct answers by random attempts as follows.

> dbinom(4, size=12, prob=0.2) 
[1] 0.1329
To find the probability of having four or less correct answers by random attempts, we apply the function dbinom with x = 0,…,4.

> dbinom(0, size=12, prob=0.2) + 
+ dbinom(1, size=12, prob=0.2) + 
+ dbinom(2, size=12, prob=0.2) + 
+ dbinom(3, size=12, prob=0.2) + 
+ dbinom(4, size=12, prob=0.2) 
[1] 0.9274
Alternatively, we can use the cumulative probability function for binomial distribution pbinom.

> pbinom(4, size=12, prob=0.2) 
[1] 0.92744
Answer: The probability of four or less questions answered correctly by random in a twelve question multiple choice quiz is 92.7%.

Exercise:

Using the function ifelse and the object random_numbers simulate coin tosses. Hint: If random_numbers is bigger than .5 then the result is head, otherwise is tail.

Another way of generating random coin tosses is by using the rbinom function. Set the seed again to 1 and simulate with this function 10 coin tosses. Note: The value you will obtain is the total number of heads of those 10 coin tosses.

(coin_tosses_1 <- ifelse(random_numbers>.5, 'head', 'tail'))
## [1] "tail" "tail" "head" "head" "tail" "head" "head" "head" "head" "tail"
set.seed(1)
(coin_tosses <- rbinom(n = 1, size = 10, prob = .5))
## [1] 4

Exercise:

Using the function rbinom to generate 10 unfair coin tosses with probability success of 0.3. Set the seed to 1.

set.seed(1)
(coin_tosses_unfair <- rbinom(n = 1, size = 10, prob = .3))

## [1] 2

Exercise: 

Let’s assume that we want to simulate a game in which we throw an unfair coin (success probability is 0.48) 10 times and you win $10 every time the result is tails and lose $10 when the result is heads. Simulate this game 1000 times using rbinom, and find the expected amount of money you will gain or lose in this game using the simulated values.

The game is such that you toss the coin 10 times. So you have 10 opportunities to win or lose. In the first toss, you have (0.52-0.48)=0.04 prob of losing. Since each toss costs $10, you lose on average $0.4 (=0.04*10) on every toss. Now since 10 tosses are done, overall on average you should lose 4 dollars. So, the expected answer should be close to -$4 (minus sign indicates loss).
In fact that is what we see using our code. The answer is -3.86 which is close to -4.

The solution should be 
set.seed(1)
coin_tosses <- rbinom(n = 1000, prob = .48, size = 10)
mean(coin_tosses-5) * 20
## [1] -1.93

########################################################################################################
Poisson

x <- seq(0, 20, 1)
plot (x, dpois(x, 3))
points(dpois(x,5), col = 'red', lwd =2)
points(dpois(x,10), col = 'green', lwd =2)