- 👋 Hi, I’m Subha
- 👀 I’m interested in Business Analytics
- 🌱 I’m currently learning R
- 💞️ I’m looking to collaborate on different Regression Topics
- 📫 How to reach me 

<!---
subhachak9681/subhachak9681 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at [rental bikes.readme.pdf](https://github.com/subhachak9681/subhachak9681/files/8126316/rental.bikes.readme.pdf)
your changes.
--->
[rental bikes.readme.pdf](https://github.com/subhachak9681/subhachak9681/files/8126348/rental.bikes.readme.pdf)
[day.csv](https://github.com/subhachak9681/subhachak9681/files/8126351/day.csv)
[rental bikes.readme.pdf](https://github.com/subhachak9681/subhachak9681/files/8126356/rental.bikes.readme.pdf)
#################################
###  CASE STUDY ANALSYSIS
#################################
##### Importing data
data1=read.csv(###your code here)

  

  
### Fit the model with the selected variables and evaluate its performance

###your code here
##### Importing data
bike1=read.csv("day.csv",header=TRUE,sep=",")
head(bike1)


## Excluding Casual and Registered Column as they are reductant
bike2<-subset(bike1,select=-c(casual,registered))

### Install and activate package 'ggplot2' needed for histogram and box plot
install.packages("ggplot2")
library(ggplot2)

##Changing the names of the Columns

names(bike2)<-c('rec_id','datetime','season','year','month','holiday','weekday','workingday','weather_condition','temp','atemp','humidity','windspeed','total_count')
head(bike2)
### Histogram of the response variable ###

qplot(bike2$total_count,geom="histogram",main="Histogram for count",xlab="count",bins=30)
### Boxplot of the response variable ###

ggplot(bike2,aes(y=total_count))+geom_boxplot()

### Obtaining descriptive statistics ###
library(pastecs)
stat.desc(bike2$total_count)

## formation of Train_data(90%) and test_data (10%)
train_index<-sample(1:nrow(bike2),0.9*nrow(bike2))
train_data<-bike2[train_index,]
test_data<-bike2[-train_index,]
dim(train_data)
dim(test_data)
head(train_data)

### Fitting the main multiple linear regression (MLR) model with all relevant predictors ###
mod1=lm((total_count)~as.factor(season)+as.factor(year)+as.factor(month)+as.factor(holiday)+as.factor(weekday)+as.factor(workingday)+as.factor(weather_condition)+temp+atemp+humidity+windspeed,data=train_data)
summary(mod1)
### Cross validation of the model using out-of-sample data ###
pred_val=predict(mod1,test_data)
  
# cor() is a function that finds correlation between two variables, here between actual values in the testdata and pred_val which indicates values predicted by the model
cor(test_data$total_count,pred_val)
    
###calculate root mean squared error of the residuals (RMSE) ###
sqrt(mean(test_data$total_count-pred_val)^2)
      
###plot the QQ-plot, residual plot and standardized residual plot for our analysis.
plot(mod1)

###perfrom shapiro test

shapiro.test(mod1$residuals)

###  Durbin-Watson test ###
install.packages("lmtest")
library(lmtest)
dwtest(mod1)

### Multicollinearity check 
library(car)
vif(mod1)
mod2=lm((total_count)~as.factor(season)+as.factor(year)+as.factor(holiday)+as.factor(weekday)+as.factor(workingday)+as.factor(weather_condition)+temp+atemp+humidity+windspeed,data=train_data)
summary(mod2)
vif(mod2)
### Fit the model with the selected variables and evaluate its performance

mod4=lm((total_count)~as.factor(season)+as.factor(year)+as.factor(holiday)+as.factor(workingday)+as.factor(weather_condition)+atemp+humidity+windspeed,data=train_data)
vif(mod4)

### Variable selection

library(MASS)
step.model=stepAIC(mod1, direction = "both", trace = FALSE)
summary(step.model)
summary(mod4)
