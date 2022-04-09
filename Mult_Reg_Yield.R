#### Multiple Linear Regression ####
#dependent variable is regressed upon two or more predictor variables

# Reading the data and variables
data = read.csv('Mult_Reg_Yield.csv',header = T,sep = ",")
mydata= data[,-1] # Removing SL.NO. Column 
mydata
attach(mydata)
# Computing Correlation Matrix
cor(mydata)

# Fitting Multiple Linear Regression
model = lm(X.Yield ~ Temperature + Time) 
summary(model)

X.Yield = -67.88436 - 0.06419*Temperature + 0.90609*Time
X.Yield
# Temperature is NOT a causal variable. 

# Regression Model Performance
anova(model)

# From the ANOVA Table we can say only time is related to % yield as p value < 0.05, so we modify our model
# Fitting Linear Regression Model with Time as the only Predictor variable

model_m = lm(X.Yield ~  Time) 
summary(model_m)

# X.Yield = -81.6205 + 0.9065*Time 

# Regression Model Performance
anova(model_m)

# Residual Analysis
pred = fitted(model_m) 
Res = residuals(model_m) 
plot(Res)
qqnorm(Res)
