##############################################################################
## Project:  Designing, running, analyzing, and writing-up your own quantitative research study
## INSC 571: Quantitative Methods in Information Science 
## Winter 2019
## 
## Author: Aakash Agrawal, Christina Mauri
## References : Wobbrock, J. O. (2018). Statistical Analysis & Reporting in R
#############################################################################

# Our Hypotheses for the research :
# H0: There is no difference in depression scores of students that engage in more phone calls and those that engage in fewer phone calls.
# H1: There is a difference in depression scores of students that engage in more phone calls and of those that engage in fewer phone calls.


## Load, view and prepare the dataset

deprcall <- read.csv("depression_and_call.csv") # Read in the dataset
View(deprcall) # verify
deprcall$Subject <- factor(deprcall$Subject) # recoding the factor
summary(deprcall) # summary of the data
summary(deprcall$CallerGroup)

## Exploratory Data Analysis (EDA) using descriptive statistics and plots

# Summary Statistics
library(plyr)
ddply(deprcall, ~CallerGroup, function(data) summary(data$DepressionScore))  # summary statistics by CallerGroup
ddply(deprcall, ~CallerGroup, summarise, DepressionScore.mean = mean(DepressionScore), DepressionScore.sd = sd(DepressionScore))  # summarizing mean and standard deviation

# Visualizations
par(mfrow=c(2,1)) # plot two histograms stacked

hist(deprcall[deprcall$CallerGroup == 'More',]$DepressionScore,
     xlim=c(0,25),
     ylim=c(0,10), 
     xlab="Depression Score", 
     ylab="Frequency", 
     main="CallerGroup: More", 
     las=1, 
     col="lightblue")
hist(deprcall[deprcall$CallerGroup == 'Less',]$DepressionScore,
     xlim=c(0,25),
     ylim=c(0,10), 
     xlab="Depression Score", 
     ylab="Frequency", 
     main="CallerGroup: Less", 
     las=1, 
     col="lightgreen")


par(mfrow=c(1,1)) # return to plotting in one panel

plot(DepressionScore ~ CallerGroup, # boxplots
     data = deprcall,
     xlab="CallerGroup", 
     ylab="Depression Score", 
     main="CallerGroup - DepressionScore", 
     las=1, 
     col=c("lightblue","lightgreen")) 


## With a median of 6 and a mean depression score of 6.57 (SD = 4.87) for the Less Caller Group and 
## Median and Mean of 5 and 6.70 respectively (SD = 5.26) for the More caller group, 
## these two caller groups did not appear to be significantly different in the depression scores.
## We also observe an outlier in th data for the CallerGroup 'More' which has one of the values of 
## depression score as 22 which is 4 units greater than the Q3 + 1.5 * IQR (= 18). However, after 
## running the analysis without the outlier separately, we say that neither the presence nor absence 
## of the outlier in the graph above would affect the assumptions or the distribution of rest of  the data.
## So, keeping the data as it is with the outlier.
 
## The distribution of depression scores of the two Caller Groups appear to be log-normal, and so, 
## violations of normality are probably present, recommending either a log-transform of the response 
## or the use of a non-parametric statistical test.


## Testing for ANOVA assumptions

# Shapiro-Wilk normality test on the DepressionCore response for each caller group
# A signifiacnt result means departure from a normal distribution

shapiro.test(deprcall[deprcall$CallerGroup == 'More',]$DepressionScore)
# for Caller group 'More' we do have departure from normality ; W = 0.84519, p-value = 0.009064
shapiro.test(deprcall[deprcall$CallerGroup == 'Less',]$DepressionScore)
# for Caller group 'Less' we do have departure from normality ;W = 0.88171, p-value = 0.01572

## A Shapiro-Wilk test of normality on the response (DepressionScore) for each level of factor (CallerGroup)
## showed a statistically significant deviation from Normality for both the CallerGroups More (W=.845, p<.01)
## and Less (W=.881, p<.05).

# Although not required, but conducting a Shapiro-Wilk test on residuals
mAnova = aov(DepressionScore ~ CallerGroup, data = deprcall) # fit model
shapiro.test(residuals(mAnova)) # test residuals
# W = 0.89738, p-value = 0.002152
qqnorm(residuals(mAnova)); qqline(residuals(mAnova))
# based on the qqplot we can say that the data is not normally distributed, or there is a departure from normality


# Brown-Forsythe test of homoscedasticity (Homogenity of varience)

library(car)
leveneTest(DepressionScore ~ CallerGroup, data = deprcall, center = median)          
# F-value : 0.3167, p-value : 0.5771 ; no departure from homoscedasticity
## A Brown-Forsythe homoscedasticity test on DepressionScore by CallerGroup showed no violation of 
## homoscedasticity (F(1, 36) = .317, p=.577).


# To test if lognormailty is a better fit to our phone-call-depression-rate data
# Conducting a Kolmogorov-Smirnov goodness-of-fit test to estimate fit parameters

library(MASS)

fitMore = fitdistr(deprcall[deprcall$CallerGroup == "More",]$DepressionScore, "lognormal")$estimate
ks.test(deprcall[deprcall$CallerGroup == "More",]$DepressionScore, "plnorm", meanlog=fitMore[1], sdlog=fitMore[2], exact = TRUE)
#  D = 0.1398, p-value = 0.8495
# Since p-value is > 0.05, this means that we do not detect significant departure from lognormality

fitLess = fitdistr(deprcall[deprcall$CallerGroup == "Less",]$DepressionScore, "lognormal")$estimate
ks.test(deprcall[deprcall$CallerGroup == "Less",]$DepressionScore, "plnorm", meanlog=fitLess[1], sdlog=fitLess[2], exact = TRUE)
#  D = 0.16705, p-value = 0.5458
# Since p-value is > 0.05, this means that we do not detect significant departure from lognormality

## Lognormal distributions were fit to the response of each level of CallerGroup. Kolmogorov-
## Smirnov goodness-of-fit tests were then run, showing no departure from log-normality
## for More (D=.134, p=.849), and Less (D=.167, p=.546). 


## Data Transformation
# log-transformed Time response

deprcall$logDepScore <- log(deprcall$DepressionScore)  # new column containg log of depressionScore response 
View(deprcall)
ddply(deprcall, ~CallerGroup, function(data) summary(data$logDepScore))  # summary statistics by CallerGroup
ddply(deprcall, ~CallerGroup, summarise, logDepScore.mean = mean(logDepScore), logDepScore.sd = sd(logDepScore))  # summarizing mean and standard deviation

# checking for normality
shapiro.test(deprcall[deprcall$CallerGroup == "More",]$logDepScore) # Shapiro-Wilk normality test
# W = 0.97201, p-value = 0.8526
shapiro.test(deprcall[deprcall$CallerGroup == "Less",]$logDepScore)
# W = 0.90601, p-value = 0.04583 ; departure from log-normality

# A log-transform was applied to the response data and the normality of this transformed response was checked 
# with Shapiro-Wilk tests, showing no departure from normality for
# More (W=.972, p=.852), and a significant departure for Less (W=.906, p<0.05)

# Conducting Brown-Forsythe test of homoscedasticity (Homogenity of varience) on log-transformed response
leveneTest(logDepScore ~ CallerGroup, data = deprcall, center = median)
## A Brown-Forsythe homoscedasticity test on the log-transformed depression score by CallerGroup showed no violation of 
## homoscedasticity (F(1, 36) = 1.268, p=.267).


## The above tests show that even the log-transformed distribution of the response by levels of the D.V
## conform to the assumptions of ANOVA. So using the non-parametric Mann-Whitney U statistical test to
## check for significance of depression scores among the two levels of the factor.


## Non-parametric test
# Mann-Whitney U test

library(coin)
wilcox_test(DepressionScore ~ CallerGroup, data = deprcall, distribution = "exact")
wilcox_test(logDepScore ~ CallerGroup, data = deprcall, distribution = "exact") # same result for the log-transformed response
# Z = -0.10341, p-value = 0.9244

## A Mann-Whitney test indicated no statistical significant difference among the levels of CallerGroup 
## - More (Mdn =5), Less (Mdn =6) and DepressionScore,  Z = -0.103, p = .924, r = .02. 

# Since the p-value is  0.9244, we fail to reject the null hypothesis and conclude that there is no difference 
# in depression scores of students that engage in more phone calls and those that engage in fewer phone calls.



######################################################################################
## Although, there is a violation of assumptions of ANOVA, conducting an independent-samples t-test 
## on the log-transformed DepressionScore response to get an idea of the p-value and confidence interval

t.test(logDepScore ~ CallerGroup, data = deprcall, var.equal = TRUE)
# t = -0.27507, df = 36, p-value = 0.7848
# t(36) = -0.28, p = n.s.
# Also, the 95 percent confidence interval: -0.6452149  0.4910948  contains 0, 
# which favors in the null hypothesis being true
######################################################################################


