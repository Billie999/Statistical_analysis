# Question 1

# a. Create a data frame which divides up responsibility for employees to manage a store.
# The data frame should contain 5 columns: • Column 1 is the shift number – this
# number should be the same as the row number. • Column 2 is the day as a factor, 1-5
# being Monday, Tuesday, … Friday respectively. • Column 3 is the store location, either
# A or B. • Column 4 is the starting time, either 800 or 1300. • Column 5 is the employee
# number, 1, 2, 3 or 4.

# You should use the functions seq and rep to produce the appropriate values for the
# columns 1-4 so that every possible store location, day and starting time are covered. You
# may then assign the shifts to the employees however you like, but any single employee
# cannot work more than once a day.
store<-c(rep("A",10),rep("B",10))
start<-rep(c("0800","1300"),10)
employee<-c(rep(1,5),rep(2,5),rep(3,5),rep(4,5))
day<-rep(c("Mon","Tue","Wed","Thur","Fri"),4)
shiftnum<-1:20
Shifts<-data.frame(Shift.Number=shiftnum,Day=day,Store=store,Starting.Time=start,Employee=employee)

# b. Now use logical selection, along with the length() function, to count the number of
# times that employee 2 works at store B.
length(which(Shifts$Employee==2 & Shifts$Store=="B"))

# c. Use logical selection to find where employee 3 is working on Wednesday.
# Employee 3 is working at store B
Shifts$Store[which(Shifts$Employee==3 & Shifts$Day=="Wed")]

# Question 2
# a. Using R, compute the log to base 10 of 50
log10(50)
# b. Using R, compute the sqrt of 20
sqrt(20)

# Question 3
# a. Import the data plantGrowthExpt.csv file to Rstudio and check that the data imported
# correctly using the View command.
z <- read.csv("plantGrowthExpt.csv")
View(plantGrowthExpt) # for checking 

# b. Produce a histogram of height.cm
hist(z$height.cm)

# c. Produce a Q-Q plot of height.cm
qqnorm(z$height.cm)
qqline (z$height.cm)

# d. Produce a boxplot of height.cm
boxplot(z$height.cm,ylab="height (cm)")

# e. Produce side-by-side boxplots to compare how the distribution of height.cm differs
# between varieties A and B.
boxplot(z$height.cm~z$variety,ylab="height (cm)")

# f. Produce a scatterplot with height.cm on the y-axis and biomass.gm on the x-axis.
# Colour code the points to distinguish each variety.
plot(z$biomass.gm, z$height.cm,
pch=19, col="blue",
xlab="biomass (gm)",ylab="height (cm)"
)

#superimpose variety A coloured in red
points(z$biomass.gm[z$variety=="A"],
z$height.cm[z$variety=="A"],
pch=19, col="red",
xlab="biomass (gm)",
ylab="height (cm)",
)

plot(z$biomass.gm,z$height.cm,xlab="biomass (gm)",ylab="height (cm)")

# g. Compute the minimum, 1st quartile, median, mean, 3rd quartile and maximum for
# height.cm and biomass.cm, separately for each variety A and B.

# Uses logical indexing to extract the different varieties
summary(z[z$variety=="A",])
summary(z[z$variety=="B",])


