# Reading Survey Data and libraries
gameofthrones <- read.csv('GOT_CSV.csv')
library(dplyr)
head(gameofthrones)

##displaying the number of rows and columns in the dataset
nrow(gameofthrones)
ncol(gameofthrones)

library(funModeling)
library(Hmisc)

df_status(gameofthrones)

# Exploratory Data Analysis
##1
viewers <- gameofthrones %>% select(8)
viewers$US.viewers..million.[viewers$US.viewers..million.>=5] = 'Most Viewed'
viewers$US.viewers..million.[viewers$US.viewers..million.< 5] = 'less viewed'
freq(viewers)
##2
Director <- freq(gameofthrones$Director)
##3
NotableDeathCount <- freq(gameofthrones$Notable.Death.Count)
##4
hist(gameofthrones$IMDB.votes, xlim =c(2000,160000), breaks = 10)


#######################################
##One sample test
#######################################
qqnorm(gameofthrones$Imdb.Rating)

hist(gameofthrones$Imdb.Rating)

h_0 : mu = 9.0
h_a : mu != 9.0

## p-value
## Two-sided based on our choice of alternate hypothesis
# the parts of the test statistic
# sample mean
x_bar <- mean(gameofthrones$Imdb.Rating)
# null hypothesized population mean
mu_0 <- 9.0
# sample st. dev
s <- sd(gameofthrones$Imdb.Rating)
# sample size
n <- length(gameofthrones$Imdb.Rating)
# t-test test statistic
t <- (x_bar - mu_0)/(s/sqrt(n))
# two-sided p-value so multiply by 2
two_sided_t_pval <- pt(q = t, df = n-1, lower.tail = FALSE)*2
two_sided_t_pval

qt(0.025, n-1)

# lower bound
x_bar+(qt(0.025, n-1)*(s/sqrt(n))) # alternately you can use x_bar-(qt(0.975, n-1)*(s/sqrt(n)))

# upper bound
x_bar+(qt(0.975, n-1)*(s/sqrt(n))) # alternately you can use x_bar-(qt(0.025, n-1)*(s/sqrt(n)))

t.test(gameofthrones$Imdb.Rating,alternative = "two.sided",mu = 9)

# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = gameofthrones$Imdb.Rating,size = n,replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Average IMDB rating for Game of thrones episodes', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(26, 33, 1000), dnorm(seq(26, 33, 1000), mean = x_bar, sd = 0.42/sqrt(67)))

# Shift the sample so that the null hypothesis is true
rating_H0_true <- gameofthrones$Imdb.Rating - mean(gameofthrones$Imdb.Rating) + mu_0
# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 10000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results_given_H0_true[i] <- mean(sample(x = rating_H0_true,
                                          size = n,
                                          replace = TRUE))
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE, main='Sampling Distribution of the Sample Mea
n, Given Null Hypothesis is True', xlab = 'Average IMDB rating for Game of thrones episodes', ylab = 'Density')
# add line to show values more extreme on upper end
abline(v=x_bar, col = "red")
# add line to show values more extreme on lower end
low_end_extreme <- mean(results_given_H0_true)+(mean(results_given_H0_true)-x_bar)
abline(v=low_end_extreme, col="red")

# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= low_end_extreme)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= x_bar)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
bootstrap_pvalue

# two sided t p-value
two_sided_t_pval

# need the standard error which is the standard deviation of the results
bootstrap_SE_X_bar <- sd(results)
# an estimate is to use the formula statistic +/- 2*SE
c(x_bar - 2*bootstrap_SE_X_bar, x_bar + 2*bootstrap_SE_X_bar)

# you can also use the 5th and 95th quantiles to determine the bounds:
c(quantile(results, c(.025, .975)))

# compare to our t-methods
c(x_bar+(qt(0.025, n-1)*(s/sqrt(n))), x_bar+(qt(0.975, n-1)*(s/sqrt(n))))

########################################
## One sample proportion
########################################
## One sample test of prop
##What proportion of episodes are most viewed? 

MostViewer <- gameofthrones %>% select(8, 12)
MostViewer$US.viewers..million.[MostViewer$US.viewers..million. >= 5.0] = 'Most Viewed'
MostViewer$US.viewers..million.[MostViewer$US.viewers..million. < 5.0] = 'less viewed'
MostViewer

freq(MostViewer$US.viewers..million.)

##67 episodes, out of which 42 are most viewed
p_hat <- .42
n_p_hat <- 67*(42/67)
n*(1-p_hat) <- 67*(25/67)

H_0 :P_0  = .5
H_A :P_A > .5

z <- (.42 - .5) / sqrt((.5*(1-.5)) / 67)
z

binom.test(x=42, n = 67, p=(.5), alternative="greater")

pnorm(-1.30, lower.tail = FALSE)

##Confidence Interval
cat("exact binomial test")

binom.test(x=42, n = 67, p=(.5), alternative="greater")$conf.int

cat("normal approx")

c(.42 - (0.09)*sqrt(((.42)*(1-.42))/67), 1)

table(MostViewer)

views <- rep(c(1, 0), c(42, 67-42))
views

##This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 100000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = views,
                            size = 67,
                            replace = TRUE))
}
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Proportion', xlab = 'Proportion of Views', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.35, .80, .001), dnorm(seq(.35, .80, .001), mean = mean(results), sd = sd
                                     (results)))

##Using this sampling distribution to find the 5th and 95th percentiles and compare to the other methods.

cat("Bootstrap Confidence Interval")

c(quantile(results, c(.05, 1)))


cat("exact binomial test")


binom.test(x=42, n = 67, p=(.5), alternative="greater")$conf.int


cat("normal approx")
c(.42 - (0.09)*sqrt(((.42)*(1-.42))/67), 1)

# Under the assumption that the null hypothesis is true, we have 50% most viewed
MostViewer <- rep(c(1, 0), c(50, 100-50))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = views,
                            size = 67,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Proportion under H
_0:p=0.5', xlab = 'Proportion of Views', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.35, .90, .001), dnorm(seq(.35, .90, .001), mean = mean(results), sd = sd
                                     (results)))
abline(v=.42, col="red")

count_of_more_extreme_upper_tail <- sum(results >= .42)
bootstrap_pvalue <- count_of_more_extreme_upper_tail/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue
cat("Exact Binomial p-value")
binom.test(x=42, n = 67, p=(.5), alternative="greater")$p.value
cat("Normal Approximation p-value")
pnorm(-1.39, lower.tail = FALSE)

#################################################
## Two sample difference in means
#################################################
ViewervsRatings <- gameofthrones %>% select(8, 12)
ViewervsRatings$US.viewers..million.[ViewervsRatings$US.viewers..million. >= 5.0] = 'Most Viewed'
ViewervsRatings$US.viewers..million.[ViewervsRatings$US.viewers..million. < 5.0] = 'less viewed'
ViewervsRatings

qqnorm(ViewervsRatings$Imdb.Rating)

qqnorm(ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="Most Viewed"])

qqnorm(ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="less viewed"])

##p-value and confidence interval
##Two-sided based on the alternate hypothesis
t.test(ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="less viewed"], ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="Most Viewed"])

##bootstrap approach.
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  mean_mostviewed <- mean(sample(x = ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="Most Viewed"],
                                 size = 67,
                                 replace = TRUE))
  mean_lessviewed<- mean(sample(x = ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="less viewed"],
                                size = 67,
                                replace = TRUE))
  results[i] <- mean_mostviewed - mean_lessviewed
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Average Difference view vs IMDBratings', ylab = 'Density')

# Bootstrap one-sided CI
c(quantile(results, c(.025, .975)))

# compare to our t-methods
t.test(ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="Most Viewed"], ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="less viewed"])$conf.int

# Check out the transform function used to shuffle
transform(ViewervsRatings, US.viewers..million.=sample(US.viewers..million.))

num_sims <- 10000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_groups <- transform(ViewervsRatings, Imdb.Rating=sample(Imdb.Rating))
  mean_mostviewed <- mean(shuffled_groups$Imdb.Rating[shuffled_groups$US.viewers..million.=="Most Viewed"])
  
  mean_lessviewed <- mean(shuffled_groups$Imdb.Rating[shuffled_groups$US.viewers..million.=="less viewed"])
  results_given_H0_true[i] <- mean_mostviewed - mean_lessviewed
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Means Under Null',
     xlab = 'Average Difference viewers vs IMDBratings under Null',
     ylab = 'Density')
diff_in_sample_means <- mean(ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="Most Viewed"]) - mean(ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="less viewed"])
abline(v=diff_in_sample_means, col = "blue")
abline(v=abs(diff_in_sample_means), col = "red")

# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= diff_in_sample_means)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= abs(diff_in_sample_means))
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
cat("Bootstrap p-value")

bootstrap_pvalue

cat("t-test p-value")

t.test(ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="Most Viewed"], ViewervsRatings$Imdb.Rating[ViewervsRatings$US.viewers..million.=="less viewed"])$p.value


####################################################
## Two sample difference in test proportions
####################################################
ViewervsRatings <- gameofthrones %>% select(8, 12)
ViewervsRatings$US.viewers..million.[ViewervsRatings$US.viewers..million. >= 5.0] = 'Most Viewed'
ViewervsRatings$US.viewers..million.[ViewervsRatings$US.viewers..million. < 5.0] = 'less viewed'
ViewervsRatings

##Sample Statistic
##Distribution of the test statistic

# the parts of the test statistic
# sample props
p_mostviewedrating_hat <- length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating >= 9.0))/length(ViewervsRatings$Imdb.Rating)
p_lessviewedrating_hat <- length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating < 9.0))/length(ViewervsRatings$Imdb.Rating)
# null hypothesized population prop difference between the two groups
p_0 <- 0
# sample size
n_m <- length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating >= 9.0))
n_l <- length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating < 9.0))
# sample variances
den_p_m <- (p_mostviewedrating_hat*(1-p_mostviewedrating_hat))/n_m
den_p_l <- (p_lessviewedrating_hat*(1-p_lessviewedrating_hat))/n_l
# z-test test statistic
z <- (p_mostviewedrating_hat - p_lessviewedrating_hat - p_0)/sqrt(den_p_m + den_p_l)
# two sided p-value
two_sided_diffprop_pval <- pnorm(q = z, lower.tail = FALSE)*2
two_sided_diffprop_pval
# confidence interval
# lower bound
(p_mostviewedrating_hat - p_lessviewedrating_hat)+(qnorm(0.025)*sqrt(den_p_m + den_p_l))

# upper bound
(p_mostviewedrating_hat - p_lessviewedrating_hat)+(qnorm(0.975)*sqrt(den_p_m + den_p_l))

##bootstrap aproach
# Make the data
mostviews <- rep(c(1, 0), c(length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating >= 9)), length(ViewervsRatings$Imdb.Rating)-n_m))
lessviews <- rep(c(1,0), c(length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating < 9)), length(ViewervsRatings$Imdb.Rating)-n_l))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  prop_mostviewed <- mean(sample(x=mostviews,
                                 size = n_m,
                                 replace = TRUE))
  prop_lessviewed <- mean(sample(x = lessviews,
                                 size = n_l,
                                 replace = TRUE))
  results[i] <- prop_mostviewed - prop_lessviewed
}
# Finally plot the results
hist(results, freq = FALSE, main='Dist. of the Diff in Prop', xlab = 'Difference in Prop. of ratings', ylab = 'Density')

cat("Bootstrap")
c(quantile(results, c(.025, .975)))
cat("Normal Approximation")
c((p_mostviewedrating_hat - p_lessviewedrating_hat)+(qnorm(0.025)*sqrt(den_p_m + den_p_l)), 
  (p_mostviewedrating_hat - p_lessviewedrating_hat)+(qnorm(0.975)*sqrt(den_p_m + den_p_l)))

#################################################
##chi-square test
################################################
DeathCounts <- gameofthrones %>% select(1, 13)
DeathCounts

prop.table(table(DeathCounts))
##P1, P2, P3, P4, P5, P6, P7 = 0.14
##Sample size = 67

67/0.14

sum(((table(DeathCounts) - 478.57)^2)/478.57)

pchisq(36716.18, df = 7-1, lower.tail = FALSE)

num_sims <- 100
# A vector to store my results
chisq_stats_under_H0 <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  new_samp <- sample(DeathCounts, 67, replace = T)
  new_samp <- new_samp[!is.na(new_samp)]
  chisq_stats_under_H0[i] <- sum(((table(new_samp) - 478.57)^2)/478.57)
}

hist(chisq_stats_under_H0, freq = FALSE,
     main='Dist. of the Chi-Square Statistic Under Null',
     xlab = 'Chi-Square Stat under Null',
     ylab = 'Density')
abline(v=sum(((table(DeathCounts) - 478.57)^2)/478.57), col="red")

sum(chisq_stats_under_H0 >= sum(((table(DeathCounts) - 478.57)^2)/478.57))/num_sims

table(DeathCounts)

##################################################
##difference in proportion
##################################################
ViewervsRatings <- gameofthrones %>% select(8, 12)
ViewervsRatings$US.viewers..million.[ViewervsRatings$US.viewers..million. >= 5.0] = 'Most Viewed'
ViewervsRatings$US.viewers..million.[ViewervsRatings$US.viewers..million. < 5.0] = 'less viewed'
ViewervsRatings

##Sample Statistic

##Distribution of the test statistic

# the parts of the test statistic
# sample props
p_mostviewedrating_hat <- length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating >= 9.0))/length(ViewervsRatings$Imdb.Rating)
p_lessviewedrating_hat <- length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating < 9.0))/length(ViewervsRatings$Imdb.Rating)
# null hypothesized population prop difference between the two groups
p_0 <- 0
# sample size
n_m <- length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating >= 9.0))
n_l <- length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating < 9.0))
# sample variances
den_p_m <- (p_mostviewedrating_hat*(1-p_mostviewedrating_hat))/n_m
den_p_l <- (p_lessviewedrating_hat*(1-p_lessviewedrating_hat))/n_l
# z-test test statistic
z <- (p_mostviewedrating_hat - p_lessviewedrating_hat - p_0)/sqrt(den_p_m + den_p_l)
# two sided p-value
two_sided_diffprop_pval <- pnorm(q = z, lower.tail = FALSE)*2
two_sided_diffprop_pval

# confidence interval
# lower bound
(p_mostviewedrating_hat - p_lessviewedrating_hat)+(qnorm(0.025)*sqrt(den_p_m + den_p_l))

# upper bound
(p_mostviewedrating_hat - p_lessviewedrating_hat)+(qnorm(0.975)*sqrt(den_p_m + den_p_l))

##bootstrap aproach
# Make the data

mostviews <- rep(c(1, 0), c(length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating >= 9)), length(ViewervsRatings$Imdb.Rating)-n_m))
lessviews <- rep(c(1,0), c(length(subset(ViewervsRatings$Imdb.Rating, ViewervsRatings$Imdb.Rating < 9)), length(ViewervsRatings$Imdb.Rating)-n_l))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  prop_mostviewed <- mean(sample(x=mostviews,
                                 size = n_m,
                                 replace = TRUE))
  prop_lessviewed <- mean(sample(x = lessviews,
                                 size = n_l,
                                 replace = TRUE))
  results[i] <- prop_mostviewed - prop_lessviewed
}
# Finally plot the results
hist(results, freq = FALSE, main='Dist. of the Diff in Prop', xlab = 'Difference in Prop. of ratings', ylab = 'Density')

cat("Bootstrap")
c(quantile(results, c(.025, .975)))
cat("Normal Approximation")
c((p_mostviewedrating_hat - p_lessviewedrating_hat)+(qnorm(0.025)*sqrt(den_p_m + den_p_l)), (p_mostviewedrating_hat - p_lessviewedrating_hat)+(qnorm(0.975)*sqrt(den_p_m + den_p_l)))


