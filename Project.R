setwd("C:/Users/hp/Desktop/Soumya/Project/BSc Sem 6 Project")
#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

#All Necessary Libraries
library(MASS)
library(rcompanion)
library(mctest)
library(polycor)
library(caret)
library(glmnet)

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

x <- read.csv("Dataset_train.csv",header=TRUE) #;View(x)
x.test<- read.csv("Dataset_fit.csv", header = TRUE)

fivenum(x$Age)
fivenum(x$Number.of.Dependents)
fivenum(x$Number.of.Referrals)
fivenum(x$Tenure.in.Months)
fivenum(x$Avg.Monthly.Long.Distance.Charges)
fivenum(x$Avg.Monthly.GB.Download)
fivenum(x$Monthly.Charge)                   #Only this column gives absurd values
fivenum(x$Total.Charges)
fivenum(x$Total.Refunds)
fivenum(x$Total.Extra.Data.Charges)
fivenum(x$Total.Long.Distance.Charges)
fivenum(x$Total.Revenue)

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

x<-x[-c(which(x$Monthly.Charge<0)),]                #We remove the rows which contain absurd values 
y<-x$Tenure.in.Months


#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

pct1<-round(((table(x$Offer))/sum(table(x$Offer))*100), digits = 2);pie(table(x$Offer),main="Pie Chart of Offers",labels = pct1)
pie(table(x$Internet.Type),main="Pie Chart of Internet Type")
pie(table(x$Contract),main="Pie Chart of Contract")
pie(table(x$Customer.Status),main="Pie Chart of status of the customer at the end of the quarter")
barplot(table(x$Multiple.Lines),main="Barplot to represent customer subscribes Multiple Line")
barplot(table(x$Online.Security),main="Barplot to represent customer subscribes Online Security", col = c("red", "green"))
barplot(table(x$Online.Backup),main="Barplot to represent customer subscribes Online Backup", col = c("red", "green"))
barplot(table(x$Device.Protection.Plan),main="Barplot to represent customer subscribes Device Protection Plan")
barplot(table(x$Premium.Tech.Support),main="Barplot to represent customer subscribes Premium Tech Support", col = c("red", "green"))
barplot(table(x$Streaming.TV),main="Barplot to represent customer uses Streaming TV", col = c("seashell3", "seashell2"))
barplot(table(x$Streaming.Movies),main="Barplot to represent customer uses Streaming Movies", col = c("red", "green"))
barplot(table(x$Streaming.Music),main="Barplot to represent customer uses Streaming Music", col = c("red", "green"))
barplot(table(x$Unlimited.Data),main="Barplot to represent customer has paid for Unlimited data",, col = c("red", "green"))
plot(x$Monthly.Charge,y,main="Scatterplot for Monthly charges vs tenure in months")
plot(x$Total.Charges,y,main="Scatterplot for Total charges vs tenure in months")
plot(x$Total.Refunds,y,main="Scatterplot for total Refunds vs tenure in months")
plot(x$Total.Extra.Data.Charges,y,main="Scatterplot for Total Extra Data charges vs tenure in months")
plot(x$Total.Long.Distance.Charges,y,main="Scatterplot for Total Long Distance charges vs tenure in months")
plot(x$Total.Revenue,y,main="Scatterplot for Total Revenue vs tenure in months")
plot(x$Age,y)

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

#Numerical Regressor Variables

Age<-x$Age
Dependents<-x$Number.of.Dependents
Referrals<-x$Number.of.Referrals
Monthly.Long.Dis.Charges<-x$Avg.Monthly.Long.Distance.Charges
Mon.GB.Download<-x$Avg.Monthly.GB.Download
Monthly.Charge<-x$Monthly.Charge
Total.Charges<-x$Total.Charges
Total.Refunds<-x$Total.Refunds
Total.Extra.Data.Charges<-x$Total.Extra.Data.Charges
Total.Long.Distance.Charges<-x$Total.Long.Distance.Charges
Total.Revenue<-x$Total.Revenue
Gender<-x$Gender
Married<-x$Married
Offer<-x$Offer
#X15<-x$Phone.Service         #All Categories are Yes
Multiple.Lines<-x$Multiple.Lines
#X17<-x$Internet.Service        #All Categories are Yes
Internet.Type<-x$Internet.Type
Online.Security<-x$Online.Security
Online.Backup<-x$Online.Backup
Device.Protection.Plan<-x$Device.Protection.Plan
Premium.Tech.Support<-x$Premium.Tech.Support
Streaming.TV<-x$Streaming.TV
Streaming.Movies<-x$Streaming.Movies
Streaming.Music<-x$Streaming.Music
Unlimited.Data<-x$Unlimited.Data
Contract<-x$Contract
#X28<-x$Paperless.Billing
#X29<-x$Payment.Method
data<-data.frame(y,Age,Dependents,Referrals,Monthly.Long.Dis.Charges,Mon.GB.Download,Monthly.Charge,Total.Charges,Total.Refunds,Total.Extra.Data.Charges,Total.Long.Distance.Charges,Total.Revenue,Gender,Married,Offer,Multiple.Lines,Internet.Type,Online.Security,Online.Backup,Device.Protection.Plan,Premium.Tech.Support,Streaming.TV,Streaming.Movies,Streaming.Music,Unlimited.Data,Contract); View(data)



#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

X<-as.matrix(data[,c(2:12)]); #X<-as.matrix(X)       #data matrix
View(X)

#VIF Calculations
fit<-lm(y ~ 1+Age+Dependents+Referrals+Monthly.Long.Dis.Charges+Mon.GB.Download+Monthly.Charge+Total.Charges+Total.Refunds+Total.Extra.Data.Charges+Total.Long.Distance.Charges+Total.Revenue+Gender+Married+Offer+Multiple.Lines+Internet.Type+Online.Security+Online.Backup+Device.Protection.Plan+Premium.Tech.Support+Streaming.TV+Streaming.Movies+Streaming.Music+Unlimited.Data+Contract, data = data)
fit
summary(fit)
#install.packages("mctest")
library(mctest)
imcdiag(fit, method = "VIF")                #Need to include mctest lib
#Found multicollinearity in X7-X11 variables. Let's check by Eigen values

#Eigen Value System
eigen_values<-(eigen(t(X)%*%X)$values);eigen_values   
k<-max(eigen_values)/(eigen_values);k                 #we have highest value of CI in X11. delete X11
X.1<-X[,-11]

#Dependency in  between Monthly.Charge & Internet.Type
for(i in 1:length(Monthly.Charge))
{
  if(Monthly.Charge[i]<70){ mon_cat[i]<- "Below 70"}
  else if(Monthly.Charge[i]>=70 & Monthly.Charge[i]<80){ mon_cat[i]<- "Low Mid"}
  else if(Monthly.Charge[i]>=80 & Monthly.Charge[i]<100){ mon_cat[i]<- "High Mid"}
  else{ mon_cat[i] <- "Above 100"}
}
table(mon_cat)

table(mon_cat, Internet.Type)
library(rcompanion)
cramerV(mon_cat, Internet.Type)     #Delete Internet.Type


fit.2<-lm(y ~ 1+Age+Dependents+Referrals+Monthly.Long.Dis.Charges+Mon.GB.Download+Monthly.Charge+Total.Charges+Total.Refunds+Total.Extra.Data.Charges+Total.Long.Distance.Charges+Gender+Married+Offer+Multiple.Lines+Internet.Type+Online.Security+Online.Backup+Device.Protection.Plan+Premium.Tech.Support+Streaming.TV+Streaming.Movies+Streaming.Music+Unlimited.Data+Contract, data = data)
imcdiag(fit.2, method = "VIF")        
fit.3<-lm(y ~ 1+Age+Dependents+Referrals+Monthly.Long.Dis.Charges+Mon.GB.Download+Monthly.Charge+Total.Charges+Total.Refunds+Total.Extra.Data.Charges+Total.Long.Distance.Charges+Gender+Married+Offer+Multiple.Lines+Online.Security+Online.Backup+Device.Protection.Plan+Premium.Tech.Support+Streaming.TV+Streaming.Movies+Streaming.Music+Unlimited.Data+Contract, data = data)
imcdiag(fit.3, method = "VIF")
summary(fit.3)
plot(fit.3, which = 2)
hist(rnorm(717, mean(resid(fit.3)), sd(resid(fit.3))), col = "red",ylim=c(0,300),xlim=c(-25,25),xlab="Values"); hist(resid(fit.2), col= rgb(0,0,0,0.5), add = TRUE); legend(6,250, legend = c("Histogram of \nResidual of model\n", "Histogram of\nNormal Distribution"), fill= c(rgb(0,0,0,0.5), "red"))
#install.packages("rcompanion")
library(rcompanion)
plotNormalHistogram(resid(fit.3), prob = FALSE, length = 717, col = "lightgoldenrod1", main = "Histogram of the Residuals", xlab = "Residuals"); legend(4, 160, legend = c("Residulas of the model", "Normal Distribution Curve"), fill = c("lightgoldenrod1", "blue")); par(bg = "gray89") 
ks.test(resid(fit.3), "pnorm", mean(resid(fit.2)), sd(resid(fit.2)))

#StepAIC
library(MASS)
stepAIC(fit.3)
fit.4<-lm(y ~ Monthly.Long.Dis.Charges + Mon.GB.Download + Monthly.Charge + Total.Charges + Total.Long.Distance.Charges + Gender + Married + Offer + Multiple.Lines + Device.Protection.Plan + Contract, data = data)
summary(fit.4)
plot(fit.4, which = 2)
plotNormalHistogram(resid(fit.4), prob = FALSE, length = 717, col = "lightgoldenrod1", main = "Histogram of the Residuals", xlab = "Residuals"); legend(4, 160, legend = c("Residulas of the model", "Normal Distribution Curve"), fill = c("lightgoldenrod1", "blue")); par(bg = "gray89") 
ks.test(resid(fit.4), "pnorm", mean(resid(fit.4)), sd(resid(fit.4)), alternative = "two.sided")
fit.4
summary(fit.4)

#Box Cox Transformation
# bc<-boxcox(fit.4)
# best.lam<- bc$x[which(bc$y==max(bc$y))]; best.lam
# fit.5<-lm(y^best.lam ~ Monthly.Long.Dis.Charges + Mon.GB.Download + Monthly.Charge + Total.Charges + Total.Long.Distance.Charges + Gender + Married + Offer + Multiple.Lines + Device.Protection.Plan + Contract, data = data)
# summary(fit.5)
# plot(fit.5, which = 2)
# plotNormalHistogram(resid(fit.5), prob = FALSE, length = 717, col="red", main = "Histogram of the Residuals"); legend(4, 160, legend = c("Residulas of the model", "Normal Distribution Curve"), fill = c("red", "blue"))
# ks.test(resid(fit.5), "pnorm", mean(resid(fit.5)), sd(resid(fit.5)), alternative = "two.sided")

model.coeff <- coefficients(fit.4); model.coeff

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------



for(i in  1:length(x$Age))
{
  if(Age[i]<=22){ age_ca[i] <- "Child"}
  else if(Age[i]>22 & Age[i]<=45){ age_ca[i] <- "Youth"}
  else if(Age[i]>45 & Age[i]<=60){ age_ca[i] <- "Adult"}
  else{ age_ca[i] <- "Old"}
}
table(age_ca)

for(i in 1:717)
{
  if(y[i]<=10){ y_cat[i] <- "Low"}
  else if(y[i]>10 & y[i]<=31){ y_cat[i] <- "Low Mid"}
  else if(y[i]>31 & y[i]<=57){ y_cat[i] <- "High Mid"}
  else{ y_cat[i] <- "High"}
}
table(y_cat)
table(y_cat, age_ca)
cramerV(y_cat, age_ca)

table(y_cat, x$Internet.Type)

table(y_cat, x$Contract)
cramerV(y_cat, x$Contract)

table(x$Married, y_cat)
cramerV(x$Married, y_cat)

table(y_cat, x$Gender)
cramerV(y_cat, x$Gender)

table(y_cat, x$Premium.Tech.Support)
cramerV(y_cat, x$Premium.Tech.Support)

table(y_cat, x$Streaming.TV)
cramerV(y_cat, x$Streaming.TV)

table(y_cat, x$Streaming.Music)
cramerV(y_cat, x$Streaming.Music)

table(y_cat, x$Streaming.Movies)
cramerV(y_cat, x$Streaming.Movies)

table(y_cat, x$Online.Security)
cramerV(y_cat, x$Online.Security)

table(y_cat, x$Online.Backup)
cramerV(y_cat, x$Online.Backup)




#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

observed.values<-x.test$Tenure.in.Months

x.test1<-read.csv("test_dataset.csv", header = TRUE); View(x.test1)

X4.t<-x.test1$Avg.Monthly.Long.Distance.Charges
X5.t<-x.test1$Avg.Monthly.GB.Download
Monthly.Charge.t<-x.test1$Monthly.Charge
X7.t<-x.test1$Total.Charges
X10.t<-x.test1$Total.Long.Distance.Charges
X12.t<-x.test1$Gender
X13.t<-x.test1$Married
X14.tA<-x.test1$Offer.A
X14.tB<-x.test1$Offer.B
X14.tC<-x.test1$Offer.C
X14.tD<-x.test1$Offer.D
X14.tE<-x.test1$Offer.E
X16.t<-x.test1$Multiple.Lines
X21.t<-x.test1$Device.Protection.Plan
X27.tone<-x.test1$ContractOne
X27.ttwo<-x.test1$ContractTwo
intercept<-rep(1,312)

data.t<-data.frame(intercept,X4.t,X5.t,Monthly.Charge.t,X7.t,X10.t,X12.t,X13.t,X14.tA,X14.tB,X14.tC,X14.tD,X14.tE,X16.t,X21.t,X27.tone,X27.ttwo)
View(data.t)
test.data.matrix<- as.matrix(data.t)
model.coeff<-as.vector(model.coeff)
predicted.values <- (test.data.matrix)%*%(model.coeff)
plot(observed.values,predicted.values, main = "Observed vs Predicted", col = "red3")

pred.obs<-as.matrix(data.frame(y.observed, y.predicted))
mse <- mean((predicted.values - observed.values)^2)
mse
View(as.matrix(data.frame(y.observed, y.predicted)))

data.frame( R2 = R2(y.predicted, y.observed),
            RMSE = RMSE(y.predicted, y.observed),
            MAE = MAE(y.predicted, y.observed))

par(mfrow=c(2,3))
boxplot(y[which(x$Offer == "None" )], main = "None", col = "skyblue3")
boxplot(y[which(x$Offer == "Offer A" )], main = "Offer A", col = "skyblue3")
boxplot(y[which(x$Offer == "Offer B" )], main = "Offer B", col = "skyblue3")
boxplot(y[which(x$Offer == "Offer C" )], main = "Offer C", col = "skyblue3")
boxplot(y[which(x$Offer == "Offer D" )], main = "Offer D", col = "skyblue3")
boxplot(y[which(x$Offer == "Offer E" )], main = "Offer E", col = "skyblue3")
mtext("Boxplots of Tenure in months for different categories of Offers", side = 1, line = -20, outer = TRUE, col = "red")

summary(y[which(x$Offer == "None" )])
summary(y[which(x$Offer == "Offer A" )])

pie(table(x$Offer))


for(i in 1:717)
{
  if(x$Gender[i] == "Male") { gen_num[i] <- 1}
  else {gen_num[i] <- 0}
}
gen_num
cor.test(y, gen_num)

library(polycor)
polyserial(y, x$Gender)

library(MASS)
library(msm)
library(ltm)
biserial.cor(y, x$Gender)
cor(y, gen_num)

values<-matrix(c(table(x$Streaming.TV), table(x$Streaming.Movies), table(x$Streaming.Music), table(x$Premium.Tech.Support), table(x$Device.Protection.Plan), table(x$Online.Security), table(x$Unlimited.Data)), byrow = FALSE, ncol = 7)
names<- c("Streaming\n TV", "Streaming\n Movies", "Streaming \nMusic", "Premium\n Tech Support", "Device \nProtection Plan", "Online\n Security", "Unlimited\n Data")
barplot(values, col = c("springgreen3", "yellowgreen"), beside = TRUE, names.arg = names, main = "Barplots of different provided features")
legend(1,600, legend = c("Yes", "No"), fill = c("yellowgreen","springgreen3"))

polyserial(y, x$Unlimited.Data)


write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(pred.obs,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(dat)

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

# Ridge and Lasso

X <- read.csv("Dataset.csv", header = TRUE); View(X)
X <- data.frame(X)
X<- X[-c(which(X$Monthly.Charge<0)),]                #We remove the rows which contain absurd values 
y <- X[,8]
X <- X[,-8]
X <- as.matrix(X)
#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

set.seed(123)  # Set seed for reproducibility

# Create a train-test split
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

# Ridge Regression

cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0); cv_ridge

# Assuming X_train and y_train are your training data
ridge_model <- glmnet(X_train, y_train, alpha = 0)  # alpha = 0 for Ridge
ridge_model
# Plot the regularization path for Ridge regression
plot(ridge_model, xvar = "lambda", label = TRUE)

ridge_pred <- predict(ridge_model, s = 0.01, newx = X_test)  # 's' is the lambda value, adjust accordingly

ridge_mse <- mean((ridge_pred - y_test)^2)
print(paste("Ridge MSE:", ridge_mse))

# Perform cross-validation for Ridge (alpha = 0)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0)

# Best lambda value
best_lambda_ridge <- cv_ridge$lambda.min; best_lambda_ridge
print(paste("Best lambda for Ridge:", best_lambda_ridge))

# Re-fit model with the best lambda
ridge_pred_best <- predict(cv_ridge, s = best_lambda_ridge, newx = X_test)
ridge_mse_best <- mean((ridge_pred_best - y_test)^2)
print(paste("Best Ridge MSE:", ridge_mse_best))

# Ridge coefficients
coef(ridge_model, s = best_lambda_ridge)

# Fit Ridge regression model with the optimal lambda
ridge_model_best <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda_ridge)
predictions <- predict(ridge_model_best, newx = X_test)

plot(ridge_pred_best, y_test, xlab = "Predicted Values", ylab = "Observed Values")
plot(ridge_pred, y_test)
plot(predictions, y_test)

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

# Lasso Regression

# Fit Lasso regression model (alpha = 1 for Lasso)
lasso_model <- glmnet(X_train, y_train, alpha = 1)

# Make predictions on the test set
lasso_pred <- predict(lasso_model, s = 0.01, newx = X_test)  # 's' is the lambda value, adjust accordingly

# Evaluate the model performance (Mean Squared Error)
lasso_mse <- mean((lasso_pred - y_test)^2)
print(paste("Lasso MSE:", lasso_mse))

# Perform cross-validation for Lasso (alpha = 1)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
cv_lasso
# Best lambda value
best_lambda_lasso <- cv_lasso$lambda.min
print(paste("Best lambda for Lasso:", best_lambda_lasso))

# Re-fit model with the best lambda
lasso_pred_best <- predict(cv_lasso, s = best_lambda_lasso, newx = X_test)
lasso_mse_best <- mean((lasso_pred_best - y_test)^2)
print(paste("Best Lasso MSE:", lasso_mse_best))

lasso_model_best <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda_lasso)
coef(lasso_model, s = best_lambda_lasso)

plot(lasso_pred_best, y_test, xlab = "Predicted values", ylab = "Observed values")

plot(lasso_model, xvar = "lambda", label = TRUE)

