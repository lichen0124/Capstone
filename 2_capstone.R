############ capstone ############

#### import packages
library(car)
library(glmnet)
library(randomForest)


#### 1. data preparation 
data <- read.csv('/Users/apple/Documents/UCDSmurfit/0_Capstone/data/data_duration.csv')
names(data)
dim(data)
data <- subset(data, select=-c(times, events, duration_s))
summary(data)
dim(data)
names(data)
data <- subset(data, data$week != 0)
data <- subset(data, data$semester != 3)
data <- subset(data, data$hour >= 10 & data$hour < 20)
data$hour <- data$hour - min(data$hour)
data <- subset(data, data$weekday <= 5)
data <- subset(data, data$number_of_tutors != 0)

# exclude rows with rare subjects which occur less than 10 times.
freq <- table(data$subject)
name <- names(freq)[freq<10]
for (i in name){
  data <- subset(data,data$subject != i)
}
dim(data)

# remove the null data
sum(is.na(data))
data <- na.omit(data)
dim(data)

data <- subset(data, data$duration_m > 0 & data$duration_m <= 120)

dim(data)
head(data)               
summary(data)

#### 2. data visualization
hist(data$duration_m, col='grey', main='Distribution of Entire Time Spent with Tutors', xlab='Duration')
hist(data$duration_m[data$duration_m<=40], col='light blue', main='Time Spent with Tutors <= 40', xlab='Duration')

mean(data$duration_m) #35.81125

# pairs(data)

plot(data$year, data$duration_m)
boxplot(data$duration_m~data$year, xlab = 'Year', ylab ='Duration(min)', main = 'Time Spent with Tutors for Year')
duration_year <- tapply(data$duration_m, data$year, FUN = mean, na.rm = TRUE)
barplot(duration_year, xlab = 'Year', ylab ='Duration(min)', main = 'Average Time Spent with Tutors for Year', col=rainbow(9))

plot(data$month, data$duration_m)
hist(data$month, main='Duration Distribution for Month', xlab='Month', col='grey')
boxplot(data$duration_m~data$month,xlab = 'Month', ylab ='Duration(min)', main = 'Time Spent with Tutors' )
duration_month <- tapply(data$duration_m, data$month, FUN = mean, na.rm = TRUE)
barplot(duration_month, xlab = 'Month', ylab ='Duration(min)', main = 'Average Time Spent with Tutors for Month', col=rainbow(9))

plot(data$date, data$duration_m)
boxplot(data$duration_m~data$date,xlab = 'Date', ylab ='Duration(min)', main = 'Time Spent with Tutors' )
duration_date <- tapply(data$duration_m, data$date, FUN = mean, na.rm = TRUE)
barplot(duration_date, xlab = 'Date', ylab ='Duration(min)', main = 'Average Time Spent with Tutors', col='light blue')

plot(data$academic_year, data$duration_m)
boxplot(data$duration_m~data$academic_year,xlab = 'Academic Year', ylab ='Duration(min)', main = 'Time Spent with Tutors for Acamedic Year' )
duration_academic_year <- tapply(data$duration_m, data$academic_year, FUN = mean, na.rm = TRUE)
barplot(duration_academic_year, xlab = 'Academic Year', ylab ='Duration(min)', main = 'Average Time Spent with Tutors  for Acamedic Year', col=rainbow(9))

plot(data$semester, data$duration_m)
boxplot(data$duration_m~data$semester,xlab = 'Semester', ylab ='Duration(min)', main = 'Time Spent with Tutors for Semester' )
duration_semester <- tapply(data$duration_m, data$semester, FUN = mean, na.rm = TRUE)
barplot(duration_semester, xlab = 'Semester', ylab ='Duration(min)', main = 'Average Time Spent with Tutors', col=rainbow(9))

plot(data$week, data$duration_m)
hist(data$week, main='Duration Distribution for Week', xlab='Week', col='grey')
boxplot(data$duration_m~data$week,xlab = 'Week', ylab ='Duration(min)', main = 'Time Spent with Tutors' )
duration_week <- tapply(data$duration_m, data$week, FUN = mean, na.rm = TRUE)
barplot(duration_week, xlab = 'Week', ylab ='Duration(min)', main = 'Average Time Spent with Tutors for Week', col=rainbow(9))
#transform week to get linear relationship with duration_m
data$week2 <- (data$week-8)^2+1
duration_week2 <- tapply(data$duration_m, data$week2, FUN = mean, na.rm = TRUE)
barplot(duration_week2, xlab = '(week-8)^2+1', ylab ='Duration(min)', main = 'Average Time Spent with Tutors for Tansformed Week', col=rainbow(9))

plot(data$weekday, data$duration_m)
hist(data$weekday, main='Duration Distribution for Weekday', xlab='Weekday', col='grey')
boxplot(data$duration_m~data$weekday,xlab = 'Weekday', ylab ='Duration(min)', main = 'Time Spent with Tutors' )
duration_weekday <- tapply(data$duration_m, data$weekday, FUN = mean, na.rm = TRUE)
barplot(duration_weekday, xlab = 'Weekday', ylab ='Duration(min)', main = 'Average Time Spent with Tutors for Weekday', col=rainbow(9))
# transform weekday to get linear relationship with duration_m
data$weekday2 <- (data$weekday-3)^2+1
duration_weekday2 <- tapply(data$duration_m, data$weekday2, FUN = mean, na.rm = TRUE)
barplot(duration_weekday2, xlab = 'Transformed Weekday', ylab ='Duration(min)', main = 'Average Time Spent with Tutors for Weekday', col=rainbow(9))

plot(data$hour, data$duration_m)
hist(data$hour, main='Duration Distribution for Hour', xlab='Hour', col='grey')
boxplot(data$duration_m~data$hour,xlab = 'Hour', ylab ='Duration(min)', main='Time Spent with Tutors' )
duration_hour <- tapply(data$duration_m, data$hour, FUN = mean, na.rm = TRUE)
barplot(duration_hour, xlab = 'Hour', ylab ='Duration(min)', main = 'Average Time Spent with Tutors', col=rainbow(9))

data$subject
barplot(table(data$subject), las=2, cex.names=0.6, main='Duration Distribution for Subject', xlab='Subject', col='grey')
plot(data$subject, data$duration_m)
boxplot(data$duration_m~data$subject,xlab = 'Hour', ylab ='Duration(min)', main='Time Spent with Tutors' )
duration_subject <- tapply(data$duration_m, data$subject, FUN = mean, na.rm = TRUE)
barplot(duration_subject, xlab = 'Subject', ylab ='Duration(min)', cex.names=0.6, main = 'Average Time Spent with Tutors', col=rainbow(9))

barplot(table(data$student_level), main='Duration Distribution for Student Level', xlab='Student Level', col='grey')
plot(data$student_level, data$duration_m)
boxplot(data$duration_m~data$student_level,xlab = 'Student Level', ylab ='Duration(min)', main = 'Time Spent with Tutors for Student Level' )
duration_level <- tapply(data$duration_m, data$student_level, FUN = mean, na.rm = TRUE)
barplot(duration_level, xlab = 'Student Level', ylab ='Duration(min)', main = 'Average Time Spent with Tutors', col=rainbow(9))

hist(data$number_of_tutors, main='Duration Distribution for Number of Tutors', xlab='Number of Tutors', col='grey')
plot(data$number_of_tutors, data$duration_m)
boxplot(data$duration_m~data$number_of_tutors,xlab = 'Number of Tutors', ylab ='Duration(min)', main = 'Time Spent with Tutors' )
duration_tutors <- tapply(data$duration_m, data$number_of_tutors, FUN = mean, na.rm = TRUE)
barplot(duration_tutors, xlab = 'Number of Tutors', ylab ='Duration(min)', main = 'Average Time Spent with Tutors', col=rainbow(9))
#transform number_of_tutors to get linear relationship with duration_m
data$number_of_tutors2 <- (data$number_of_tutors-3)^2
duration_tutors2 <- tapply(data$duration_m, data$number_of_tutors2, FUN = mean, na.rm = TRUE)
barplot(duration_tutors2, xlab = 'Number of Tutors', ylab ='Duration(min)', main = 'Average Time Spent with Tutors', col=rainbow(9))


####### modelling

# split dataset into training data and test data
set.seed(124)
dataPartition <- sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))
data.train <- data[dataPartition == 1,]
data.test <-  data[dataPartition == 2,]

x.train <- data.matrix(subset(data.train, select=-c(duration_m, week, weekday, number_of_tutors)))
x.test <- data.matrix(subset(data.test, select=-c(duration_m, week, weekday, number_of_tutors)))
y.train <- data.matrix(subset(data.train, select = c(duration_m)))
y.test <- data.matrix(subset(data.test, select = c(duration_m)))

#### linear modelling with y
# model 1: MLR model with 5 predictors
lm.fit1 <- lm(duration_m~factor(academic_year)+factor(semester)+factor(week2)+factor(weekday2)+number_of_tutors2,data = data.train)
summary1 <- summary(lm.fit1) 
summary1
r.squared.model1 <- summary1$r.squared # r-squared:0.01169738
adj.r.squared.model1 <- summary1$adj.r.squared # adjusted r-squared: 0.009711415
coef(lm.fit1)
prediction.lm1 <- predict(lm.fit1, newdata = data.test)
error.lm1 <- prediction.lm1 - data.test$duration_m
mse.lm1 <- mean(error.lm1^2)
mse.lm1 #849.4205
plot(data.test$duration_m, main='Comparison with Model 1', col='black')
points(prediction.lm1, col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))

# model 2: MLR model with 6 predictors
lm.fit2 <- lm(duration_m~factor(month)+factor(week2)+factor(academic_year)+factor(semester)+factor(weekday2)+number_of_tutors2,data = data.train)
summary2 <- summary(lm.fit2)
summary2
r.squared.model2 <- summary2$r.squared # r-squared:0.01538653
adj.r.squared.model2 <- summary2$adj.r.squared # adjusted r-squared: 0.01241571
coef(lm.fit2)
prediction.lm2 <- predict(lm.fit2, newdata = data.test)
error.lm2 <- prediction.lm2 - data.test$duration_m
mse.lm2 <- mean(error.lm2^2)
mse.lm2 #848.3645
plot(data.test$duration_m, main='Comparison with Model 2', col='black')
points(prediction.lm2, col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))

# model 3: MLR model with 10 predictors
lm.fit3 <- lm(duration_m~factor(month)+factor(date)+factor(academic_year)+factor(semester)+factor(week2)+factor(weekday2)+factor(hour)+factor(subject)+factor(student_level)+number_of_tutors,data = data.train)
summary3 <- summary(lm.fit3) 
summary3
r.squared.model3 <- summary3$r.squared # r-squared:0.05620804
adj.r.squared.model3 <- summary3$adj.r.squared # adjusted r-squared:0.04485189
coef(lm.fit3)
prediction.lm3 <- predict(lm.fit3, newdata = data.test)
error.lm3 <- prediction.lm3 - data.test$duration_m
mse.lm3 <- mean(error.lm3^2)
mse.lm3 #819.5708
plot(data.test$duration_m, main='Comparison with Model 3', col='black')
points(prediction.lm3, col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))

#### linear modelling with log_y
# model 4: MLR model with 5 predictors and 1 transformed response 
lm.fit.log1 <- lm(log(duration_m)~factor(academic_year)+factor(semester)+factor(week2)+factor(weekday2)+number_of_tutors2,data = data.train)
summary.log1 <- summary(lm.fit.log1) 
summary.log1
r.squared.model.log1 <- summary.log1$r.squared # r-squared:0.009890074
adj.r.squared.model.log1 <- summary.log1$adj.r.squared # adjusted r-squared: 0.007900475
coef(lm.fit.log1)
prediction.lm.log1 <- predict(lm.fit.log1, newdata = data.test)
error.lm.log1 <- exp(prediction.lm.log1) - data.test$duration_m
mse.lm.log1 <- mean(error.lm.log1^2)
mse.lm.log1 #1005.871
plot(data.test$duration_m, main='Comparison with Model 4', col='black')
points(exp(prediction.lm.log1), col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))

# model 5: MLR model with 6 predictors and 1 transformed response 
lm.fit.log2 <- lm(log(duration_m)~factor(month)+factor(week2)+factor(academic_year)+factor(semester)+factor(weekday2)+number_of_tutors2,data = data.train)
summary.log2 <- summary(lm.fit.log2) 
summary.log2
r.squared.model.log2 <- summary.log2$r.squared # r-squared:0.01233427
adj.r.squared.model.log2 <- summary.log2$adj.r.squared # adjusted r-squared: 0.009354239
coef(lm.fit.log2)
prediction.lm.log2 <- predict(lm.fit.log2, newdata = data.test)
error.lm.log2 <- exp(prediction.lm.log2) - data.test$duration_m
mse.lm.log2 <- mean(error.lm.log2^2)
mse.lm.log2 #1002.601
plot(data.test$duration_m, main='Comparison with Model 5', col='black')
points(exp(prediction.lm.log2), col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))

# model 6: MLR model with 10 predictors and 1 transformed response 
lm.fit.log3 <- lm(log(duration_m)~factor(month)+factor(date)+factor(academic_year)+factor(semester)+factor(week2)+factor(weekday2)+factor(hour)+factor(subject)+factor(student_level)+number_of_tutors,data = data.train)
summary.log3 <- summary(lm.fit.log3) 
summary.log3
r.squared.model.log3 <- summary.log3$r.squared # r-squared:0.0437293
adj.r.squared.model.log3 <- summary.log3$adj.r.squared # adjusted r-squared:0.032223 
coef(lm.fit.log3)
prediction.lm.log3 <- predict(lm.fit.log3, newdata = data.test)
error.lm.log3 <- exp(prediction.lm.log3) - data.test$duration_m
mse.lm.log3 <- mean(error.lm.log3^2)
mse.lm.log3 #968.1345
plot(data.test$duration_m, main='Comparison with Model 6', col='black')
points(exp(prediction.lm.log3), col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))


#### model 7: ridge regression
grid <- 10^seq(10, -2, length = 100)
# ridge regression with duration_m
ridge.mod <- cv.glmnet(x.train,y.train,alpha=0, lambda =grid, thresh=1e-12)
plot(ridge.mod)
bestlam <- ridge.mod$lambda.min
ridge.coef <- predict(ridge.mod, type = 'coefficients', s = bestlam)[1:12,]
ridge.coef
prediction.ridge <- predict(ridge.mod, s=bestlam, newx = x.test) 
error.ridge <- prediction.ridge - data.test$duration_m
mse.ridge <- mean(error.ridge^2)
mse.ridge #853.0396
r.squared.ridge = 1-sum(error.ridge^2)/sum((y.test-mean(y.test))^2)
r.squared.ridge # 0.00797408
adj.r.squared.ridge <- 1-(1-r.squared.ridge^2)*(dim(data.test)[1]-1)/(dim(data.test)[1]-11-1)
adj.r.squared.ridge # -0.003547478
plot(data.test$duration_m, main='Comparison with Ridge Model', col='black')
points(prediction.ridge, col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))

#### model 8: lasso regression
grid <- 10^seq(10, -2, length = 100)
# lasso regression with duration_m
lasso.mod <- cv.glmnet(x.train, y.train, alpha = 1, lambda = grid)
plot(lasso.mod)
bestlam <- lasso.mod$lambda.min
lasso.coef <- predict(lasso.mod, type = 'coefficients', s = bestlam)[1:12,]
lasso.coef
prediction.lasso <- predict(lasso.mod, s=bestlam, newx = x.test) 
mse.lasso <- mean((prediction.lasso-y.test)^2)
mse.lasso # 853.0975
r.squared.lasso = 1-sum((prediction.lasso-y.test)^2)/sum((y.test-mean(y.test))^2)
r.squared.lasso # 0.007906791
adj.r.squared.lasso <- 1-(1-r.squared.lasso^2)*(dim(data.test)[1]-1)/(dim(data.test)[1]-11-1)
adj.r.squared.lasso # -0.00354855
plot(data.test$duration_m, main='Comparison with Lasso Model', col='black')
points(prediction.lasso, col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))

#### model 9: elastic net regression
grid.elastic <- 10^seq(0, -2, length = 100)
# elastic net regression with duration_m
elastic.mod <- cv.glmnet(x.train, y.train, alpha = grid.elastic)
bestlam <- elastic.mod$lambda.min
prediction.elastic <- predict(elastic.mod, s=bestlam, newx = x.test) 
elastic.coef <- predict(elastic.mod, type = 'coefficients', s = bestlam)[1:12,]
elastic.coef
mse.elastic <- mean((prediction.elastic-y.test)^2)
mse.elastic # 853.1618
r.squared.elastic = 1-sum((prediction.elastic-y.test)^2)/sum((y.test-mean(y.test))^2)
r.squared.elastic # 0.007831975
adj.r.squared.elastic <- 1-(1-r.squared.elastic^2)*(dim(data.test)[1]-1)/(dim(data.test)[1]-11-1)
adj.r.squared.elastic #  -0.003549732
plot(data.test$duration_m, main='Comparison with ElasticNet Model', col='black')
points(prediction.elastic, col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))

#### model 10: random forest model
# run the model 50 times to get the average results
repetition <- 50
mse.rf.vector <- rep(1, repetition)
r.squared.rf.vector <- rep(0, repetition)
adj.r.squared.rf.vector <- rep(0, repetition)

for (i in 1:repetition) {
  rf.fit <- randomForest(duration_m~month+date+academic_year+semester+week+weekday+hour+subject+student_level+number_of_tutors, data=data.train, importance = TRUE)
  prediction.rf <- predict(rf.fit, data.test)
  summary(prediction.rf)
  error.rf <- prediction.rf - data.test$duration_m
  mse.rf.vector[i] <- mean(error.rf^2)
  r.squared.rf.vector[i] = 1-sum(error.rf^2)/sum((data.test$duration_m-mean(data.test$duration_m))^2)
  adj.r.squared.rf.vector[i] <- 1-(1-r.squared.rf.vector[i]^2)*(dim(data.test)[1]-1)/(dim(data.test)[1]-11-1)
}

mse.rf <- mean(mse.rf.vector) # 827.0943
r.squared.rf <- mean(r.squared.rf.vector) # 0.03814671
adj.r.squared.rf <- mean(adj.r.squared.rf.vector) # -0.002149191
#min(mse.rf.vector)
#max(r.squared.rf)
#max(adj.r.squared.rf)

plot(data.test$duration_m, main='Comparison with Random Forest', col='black')
points(prediction.rf, col='red')
legend('topright', c('True Value', 'Prediction'), fill =c('black', 'red'))
plot(data.test$duration_m, prediction.rf,xlim=c(0,120), ylim=c(0,120),main="Model 10")


##### plot the comparisons between predictions and true values in 10 models.
x=c(1:120)
y=c(1:120)
par(mfrow=c(2,4))
plot(data.test$duration_m, prediction.lm1,xlim=c(0,120), xlab='Actual Value', ylab='Prediction', ylim=c(0,120),main="Model 1")
abline(lm(y~x), lwd=1, col="red")
plot(data.test$duration_m, prediction.lm2,xlim=c(0,120), xlab='Actual Value', ylab='Prediction', ylim=c(0,120),main="Model 2")
abline(lm(y~x), lwd=1, col="red")
plot(data.test$duration_m, prediction.lm3,xlim=c(0,120), xlab='Actual Value', ylab='Prediction', ylim=c(0,120),main="Model 3")
abline(lm(y~x), lwd=1, col="red")
plot(data.test$duration_m, exp(prediction.lm.log1), xlab='Actual Value', ylab='Prediction', xlim=c(0,120), ylim=c(0,120),main="Model 4")
abline(lm(y~x), lwd=1, col="red")
plot(data.test$duration_m, exp(prediction.lm.log2), xlab='Actual Value', ylab='Prediction', xlim=c(0,120), ylim=c(0,120),main="Model 5")
abline(lm(y~x), lwd=1, col="red")
plot(data.test$duration_m, exp(prediction.lm.log3), xlab='Actual Value', ylab='Prediction', xlim=c(0,120), ylim=c(0,120),main="Model 6")
abline(lm(y~x), lwd=1, col="red")
plot(data.test$duration_m, prediction.ridge, xlab='Actual Value', ylab='Prediction', xlim=c(0,120), ylim=c(0,120),main="Model 7")
abline(lm(y~x), lwd=1, col="red")
plot(data.test$duration_m, prediction.lasso, xlab='Actual Value', ylab='Prediction', xlim=c(0,120), ylim=c(0,120),main="Model 8")
abline(lm(y~x), lwd=1, col="red")
plot(data.test$duration_m, prediction.elastic, xlab='Actual Value', ylab='Prediction', xlim=c(0,120), ylim=c(0,120),main="Model 9")
abline(lm(y~x), lwd=1, col="red")
plot(data.test$duration_m, prediction.rf, xlab='Actual Value', ylab='Prediction', xlim=c(0,120), ylim=c(0,120),main="Model 10")
abline(lm(y~x), lwd=1, col="red")

