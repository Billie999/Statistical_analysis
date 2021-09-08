# Statistical Analysis

# Question 1
# The lifetime of a particular type of TV
# follows a normal distribution with 𝜇=4800 hours, and = 𝜎=400 hours. 

# (a) Find the probability that a single randomly-chosen TV 
# will last less than 4,500 hours.

# Calculating a probability of a random variable
# p(x < 4500) #x random variable 
prob1<-pnorm(4500, mean = 4800, sd = 400, lower.tail = TRUE, log.p = FALSE) #prob1 is the name of the function
print(prob1)

# b) Find the probability that the mean lifetime
# of a random sample of 16 TVs is less than 4,500 hours. 
# Generating a random sample (y) of 16 TVs from a normal distribution
set.seed(1) # to reconstruct the original set of random numbers
y <- rnorm(16, mean=4800, sd=400) # y (random sample of 16) is the name of the function 
print(y) # print function
mean(y) # mean of y
sd(y) #standard deviation of y
# p(y < 4500)
prob2<-pnorm(4500, mean = mean(y), sd = sd(y), lower.tail=TRUE, log.p = FALSE)
print(prob2)

# (c) Compare answers from (a) and (b). 
# Generate standard normal Distribution with 23% probability

par(mfrow=c(1,2)) # to query graphical parameters
x=seq(-4,4,length=1001) # produces 1001 equally spaced values between -4 and 4 
y=dnorm(x) # produces values of d.n.f for normal distribution
plot(x,y,type="l",lwd=2,col="red", # plots y versus x
     main = "Standard Normal Distribution with # title   
     (Z < -0.75)=23% Probability",
     xlab="z", ylab="f(z)", # x and y labels
     xlim=c(-3,3), ylim=c(0,0.45), yaxs="i") # creates a common axis 
x=seq(-3,-0.75,length=500) # produces 500 equally spaced values btw -3 and -0.85 
y=dnorm(x) # produces values of d.n.f for normal distribution
polygon(c(-3,x,-0.75),c(0,y,0),col="blue") # draws a polygon in a plot

# Generate standard normal distribution with 19% probability
x=seq(-4,4,length=1001) # produces 1001 equally spaced values between -4 and 4 
y=dnorm(x) # produces values of d.n.f for normal distribution
plot(x,y,type="l",lwd=2,col="red", # plots y versus x
     main = "Standard Normal Distribution with # title
     (Z < -0.85)=20% Probability",
     xlab="z", ylab="f(z)", # x and y labels
     xlim=c(-3,3), ylim=c(0,0.45), yaxs="i") # produces values of d.n.f 
x=seq(-3,-0.85,length=500) # produces 500 equally spaced values btw -3 and -0.85 
y=dnorm(x) # produces values of d.n.f for normal distribution
polygon(c(-3,x,-0.85),c(0,y,0),col="blue") # draws a polygon in a plot

# Question 2
getwd() # Get working directory in R
# Beta is the name of “data frame”
beta <-read.csv("beta.csv") # load the data into R 
View(beta)  # check the data have been loaded correctly
str(beta)  # data structure compact display 

# 2. Perform some exploratory data analysis procedures
# Check the summary statistics of the data
summary(beta) # summarize a dataset with summary
# Scatter plot of post-workout beta-endorphin concentrations versus pre-workout beta-endorphin concentrations (pg/ml)
plot(beta$pre, beta$post, pch=19, col="blue", 
     xlab = "Pre-workout B-E concentration (pg/ml)", ylab = "Post-workout B-E Concentration (pg/ml)",
     main = "Changes in Beta-Endorphin Concentrations") # the most used plotting function in R for plotting R objects

# Generate Q-Q plot of the differenced data to check the normality
qqnorm(beta$dif) # draw the Q-Q plot
qqline(beta$dif, col="blue") # draw the line of Q-Q plot

# Box plot of differenced data
boxplot(beta$dif,ylab="Beta-Endorphin Concentration (pg/ml)", # box plot function
        xlab="Differences in Beta-Endorphin Concentrations", # x-axis label
        col=c("green")) # colour function

# Shapiro-Wilk normality test for the differenced data
with(beta, shapiro.test(beta$pre)) # Shapito-Wilk test for “pre” variable

with(beta, shapiro.test(beta$post)) # Shapiro-Wilk test for the “post” variable

with(beta, shapiro.test(beta$dif)) #Shapiro-Wilk test for the differenced data

# Data in 2 numeric vectors
# Beta-endorphin concentration before exercise
pre<-c(beta$pre) # create vector for pre data subset
print(pre) #print function

# Beta endorphin concentration after the treatment
post<-c(beta$post) # create vector for post data subset
print(post) #print function

# Wilcoxin Test in R
wilcox.test(beta$pre, beta$post, paired = TRUE, alternative = "two.sided")

# Question 3
# Data import datafile “PlantGrowth” from R datasets

data("PlantGrowth") # load specified dataset
View(PlantGrowth) # check the data have been loaded correctly
str(PlantGrowth) # data structure compact display

# Summary statistics of the data
summary(PlantGrowth) # summarize a dataset with summary
mean(PlantGrowth[which(PlantGrowth$group=='ctrl'),]$weight)
mean(PlantGrowth[which(PlantGrowth$group=='trt1'),]$weight)
mean(PlantGrowth[which(PlantGrowth$group=='trt2'),]$weight)
sd(PlantGrowth[which(PlantGrowth$group=='ctrl'),]$weight)
sd(PlantGrowth[which(PlantGrowth$group=='trt1'),]$weight)
sd(PlantGrowth[which(PlantGrowth$group=='trt2'),]$weight)

# Separate the data into 3 groups: trt1, trt2 and ctrl
trt1<-subset(PlantGrowth, group=="trt1") # select trt1 of data frame
print(trt1) # print function
trt2<-subset(PlantGrowth, group=="trt2") # select trt2 of data frame
print(trt2) # print function
ctrl<-subset(PlantGrowth,group=="ctrl") # select ctrl of data frame
print(ctrl) # print function

# Generate 3 Box plots of the observed groups (trt1, trt2 and a control group)

labels<-rep(c("trt1","trt2", "ctrl"), each=nrow(trt1)) # returns the variable name
treatment<-c(trt1$weight, trt2$weight, ctrl$weight) # create a vector
boxplot(treatment~labels) # plotting function for boxplot

# Generate  3 Q-Q plots to check the normality of trt,  trt2 and ctrl
par(mfrow=c(1,3)) # set up 3 plots on one page
qqnorm(trt1$weight) # draw plot for trt1
qqline(trt1$weight, col="blue") # draw line
qqnorm(trt2$weight) # draw plot of trt2
qqline(trt2$weight, col="blue") # draw line
qqnorm(ctrl$weight) # draw plot for trt1
qqline(ctrl$weight, col="blue") # draw line

# Compute F-test to assess whether the variances of two groups are equal
cntl<-PlantGrowth[which(PlantGrowth$group=='ctrl'),]$weight
trt2<-PlantGrowth[which(PlantGrowth$group=='trt2'),]$weight

res.ftest <- var.test(weight ~ group, data = subset(PlantGrowth,PlantGrowth$group != 'trt1'))
print(res.ftest)# function to compare two variances 

# Calculate Two Sample t-test
res<-t.test(cntl, trt2, var.equal=TRUE) # t-test function
print(res) # print function







