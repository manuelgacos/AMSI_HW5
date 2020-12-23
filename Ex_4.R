# Set Working directory
setwd('/home/noble_mannu/Documents/PhD/First/STAT_2131_Applied_Statistical_Methods_I/HW5')

# Exercise 4 Homework 5

# Read the data
data <- read.table('steam_text.txt', header = TRUE)

### Part (a) ###

# Regressing steam (Y) onto fat (X1) and glycerine (X2) (FULL MODEL)
m1 <- lm(steam ~ fat + glycerine, data = data)

# PLotting errors as a funtion of fitted values
plot(m1$residuals ~ m1$fitted.values)
abline(h=0)
# PLotting errors as a funtion of fitted fat
plot(m1$residuals ~ data$fat)
abline(h=0)
# PLotting errors as a funtion of fitted glycerine
plot(m1$residuals ~ data$glycerine)
abline(h=0)

# Making the reduced model with coefficients for fat and glycerine equal to 0
m1_reduced <- lm(steam ~ 1, data = data)

# Conducting the F-test for null hypothesis (fat and glycerine equal to 0)
# I did this in two ways

# First way, doing all the computations separately
x <- m1$fitted.values - mean(data$steam)
SSR <- x%*%x
MSR <- SSR/(3-1)
y <- m1$fitted.values - data$steam
SSE <- y%*%y
MSE <- SSE/(length(data$steam)-3)
f <- MSR/MSE

# For the p-value
quant1 <- qf(.95, df1=2, df2=22)
p_value1 <- 1 - pf(f, df1=2, df2=22) 

# Second way, using an ANOVA table
anova(m1,m1_reduced)

# Plotting the variable 'temp' against the residuals
plot(m1$residuals ~ data$temp)
abline(h=0)

### Part (b) ###

# Regressing steam (Y) onto fat (X1), glycerine (X2) and temp(X3) (FULL MODEL)
m2 <- lm(steam ~ fat + glycerine + temp, data = data)

# Making the reduced model with coefficients for fat and glycerine equal to 0
m2_test1 <- lm(steam ~ temp, data = data)

# Conducting the F-test for null hypothesis (fat and glycerine equal to 0)
# I did this just using the anova(,) function
anova(m2,m2_test1)

# For the p-value
quant2 <- qf(.95, df1=2, df2=21)
p_value2 <- 1 - pf(10.934, df1=2, df2=21)

# Conducting the F-test for other reduced models
m2_test2 <- lm(steam ~ fat + temp, data = data)
anova(m2,m2_test2)
m2_test3 <- lm(steam ~ glycerine + temp, data = data)
anova(m2,m2_test3)