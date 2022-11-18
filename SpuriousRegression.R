----------------------------------
#Problem Set #1
  
  #Author: Anthony Carr
  #Class: Time-Series Analysis
----------------------------------

################
#PROBLEM 1
pacman::p_load(ggplot2, tidyverse, urca)
set.seed(8675309)
nobs <- 200
problem_one_series <- rep(0,nobs)
epsilon_t <- rnorm(nobs,0,2)
phi_1 <- 1.1
phi_2 <- -.25
alpha <- 0
problem_one_series[1] <- epsilon_t[1]
problem_one_series[2] <- alpha + problem_one_series[1] + epsilon_t[2]
for (i in 3:nobs) {
  problem_one_series[i] <- alpha +
    phi_1*problem_one_series[i-1] +
    phi_2*problem_one_series[i-2] +
    epsilon_t[i]
}
plot(problem_one_series , ylab = "Problem One Series", xlab = "Time", type = 'l')


#testing for stationarity with Augmented Dickey Fuller Test
df_test=ur.df(problem_one_series, type = 'none', lags = 1)
df_test@testreg

#Using the sourced function to get the latter two tests for stationarity
source("/Users/anthonycarr/OneDrive - The George Washington University/Second Year/Second Semester/Time Series/intord.R")
Last_two=intord(problem_one_series)

 ################
#PROBLEM 2
matrix={}
set.seed(202)
for (z in 1:100){
  reps=100
  observations = 50
  #Making the error term
  stochastic_1= rnorm(observations, 0,1)
  stochastic_2= rnorm(observations, 0,1)
  #Making the data vector
  Y_series= X_series = rep(0,observations)
  #Adding Random First Entry to First series
  Y_series[1] = stochastic_1[1]
  #Adding Random First Entry to Second Series
  X_series[1]=stochastic_2[1]
  #Inputting the dependency on previous observation on 
  #First Series and Second Series
  for (i in 2:observations){
    Y_series[2]= Y_series[i-1]+stochastic_1[2]
    X_series[2]= X_series[i-1]+stochastic_2[2]
  }
  x=lm(Y_series ~ X_series)
  T_stats=summary(x)[["coefficients"]][, "t value"]
  matrix=rbind(T_stats, matrix)
}
#Getting t-stat alone and converting to DF
matrix=matrix[,2]
matrix=data.frame(matrix)

#Plotting the T-Stats in a Histogram
T_Hist= matrix %>% ggplot(aes(x= matrix))+geom_density(fill='light blue', alpha=.5)+labs(y="Density", x="T-Statistic", title="Monte Carlo: Regression Density Plot - Spurious Regression")
T_Hist

#finding all entries below +-1.96
mean(matrix < -1.96 | matrix > 1.96)
#__________________________________________________


#Now doing the same for a random series
matrix1={}
set.seed(203)
for (z in 1:100){
  observations = 50
  #Making the  new error term equal to series
  Y_series_2= rnorm(observations, 0,1)
  X_series_2= rnorm(observations, 0,1)
  #running the regression
  lr2=lm(Y_series_2 ~ X_series_2)
  T_stats1=summary(lr2)[["coefficients"]][, "t value"]
  matrix1=rbind(T_stats1, matrix1)
}
#getting t-stat alone and converting to df
matrix1=matrix1[,2]
matrix1=data.frame(matrix1)
#Plotting the T-Stats in a Histogram
T_Hist2= matrix1 %>% ggplot(aes(x= matrix1))+geom_density(fill='red', alpha=.5)+labs(y="Density", x="T-Statistic", title="Monte Carlo: Regression Density Plot - Spurious Regression 2")
T_Hist2
#finding all entries below +-1.96
mean(matrix1 < -1.96 | matrix1 > 1.96)



################
#PROBLEM 3

#Making the Y series
set.seed(8675309)
time <- 125
y_series <- rep( 0, time)
error= rnorm(time, 0, 1)
y_series[1] <- .025 + error[1]
y_series[2] <- (.025*2) +(.75*y_series[1]) + (.25*error[1]) + error[2]
for (i in 3:time){
  y_series[i] <- (.025*i) +(.75*y_series[i-1]) - (.20*y_series[i-2]) + (.25*error[i-1]) + error[i]
}
plot(y_series, type="l",xlab="Time", ylab="Y Series")
abline(h = mean(y_series[1:50]), col="red", lwd=3, lty=2)
abline(h = mean(y_series[50:100]), col="red", lwd=3, lty=2)

#Making the Z series
set.seed(8675309)
z_series <- rep(0,time)
error_z=rnorm(time, mean = 0, sd = sqrt(2))
z_series[1]=.025+error_z[1]
for (i in 2:time){
  z_series[i]=.025*i + .35*z_series[i-1]+error_z[i]
}
plot(z_series, type="l", xlab="Time", ylab="Z Series")
abline(h = mean(z_series[1:50]), col="red", lwd=3, lty=2)
abline(h = mean(z_series[50:100]), col="red", lwd=3, lty=2)



#Stationarity tests on Level Series
  #Y-Series
p3_test_null=ur.df(y_series, type = 'none', lags = 1)
p3_test_null
p3_test_null@testreg
  #Z-Series
p3_test_null1=ur.df(z_series, type = 'none', lags = 1)
p3_test_null1
p3_test_null1@testreg

#intord function for differenced SD and the ACF 
  #Y-Series
intord(y_series)
  #finding percent change in sd after differencing
print(((2.2852-1.0758)/2.2852)*100)
  #Z-series
intord(z_series)
  #finding percent change in sd after differencing
print((1.861-1.6165)/1.861)




#Regression number 1
y_z_regression=lm(y_series~z_series)
summary(y_z_regression)




#Stationarity tests on differenced series

#Mean line test 
  #Y-series
par(mfrow=c(1,2))
plot(diff(y_series), type='l', xlab="Time", ylab="Y-Series")
abline(h=mean(diff(y_series[1:60])), col='red')
abline(h=mean(diff(y_series[60:120])), col='blue', lwd = 1)
  #Z-series
plot(diff(z_series), type='l', xlab="Time", ylab="Z-Series")
abline(h=mean(diff(z_series[1:60])), col='red')
abline(h=mean(diff(z_series[60:120])), col='blue', lwd = 1)
#Variance line tests
  #Y-series
par(mfrow=c(1,2))
plot(diff(y_series), type='l', xlab="Time", ylab="Y-Series")
abline(h=max(diff(y_series)), col='red')
abline(h=min(diff(y_series)), col='blue', lwd = 1)
  #Z-series
plot(diff(z_series), type='l', xlab="Time", ylab="Z-Series", reset.par = FALSE)
abline(h=max(diff(z_series)), col='red')
abline(h=min(diff(z_series)), col='blue', lwd = 1)
#Augmented Dickey Fuller tests
  #Y series
p3_test=ur.df(diff(y_series), type = 'none', lags = 1)
p3_test
p3_test@testreg
  #Z series
p3_test2=ur.df(diff(z_series), type = 'none', lags = 1)
p3_test2
p3_test2@testreg


#Running alternate regessions as a solution
  #Regression with a series included
trend_reg= lm(y_series ~ z_series + seq(1:125))
summary(trend_reg)
  #Regression with differenced series
diff_y=diff(y_series, differences = 1 )
diff_z=diff(z_series, differences = 1)
summary(lm(diff_y~diff_z))
diff_regression= lm(diff(y_series, differences = 1)~diff(z_series, differences = 1))
summary(diff_regression)
plot(diff(y_series), type="l")
plot(diff(z_series), type="l")
  #Regression plot
plot(y=diff(y_series), x=diff(z_series), xlab="Differenced Z-Series", ylab="Differenced Y-Series")
abline(a=diff_regression, col="red")


