load("../input/wage2015-inference/wage2015_subsample_inference.Rdata")
dim(data)

str(data)

# construct matrices for estimation from the data 
Y <- log(data$wage)
n <- length(Y)
Z <- data[-which(colnames(data) %in% c("wage","lwage"))]
p <- dim(Z)[2]

cat("Number of observations:", n, '\n')
cat( "Number of raw regressors:", p)

# generate a table of means of variables 
library(xtable) 
Z_subset <- data[which(colnames(data) %in% c("lwage","sex","shs","hsg","scl","clg","ad","mw","so","we","ne","exp1"))]
table <- matrix(0, 12, 1)
table[1:12,1]   <- as.numeric(lapply(Z_subset,mean))
rownames(table) <- c("Log Wage","Sex","Some High School","High School Graduate","Some College","College Graduate", "Advanced Degree","Midwest","South","West","Northeast","Experience")
colnames(table) <- c("Sample mean")
tab<- xtable(table, digits = 2)
tab

# 1. basic model
basic <- lwage~ (sex + exp1 + shs + hsg+ scl + clg + mw + so + we +occ2+ind2)
regbasic <- lm(basic, data=data) # perform ols using the defined model
regbasic # estimated coefficients
cat( "Number of regressors in the basic model:",length(regbasic$coef), '\n') # number of regressors in the Basic Model


# 2. flexible model
flex <- lwage ~ sex + shs+hsg+scl+clg+mw+so+we+occ2+ind2 + (exp1+exp2+exp3+exp4)*(shs+hsg+scl+clg+occ2+ind2+mw+so+we)
regflex <- lm(flex, data=data)
regflex # estimated coefficients
cat( "Number of regressors in the flexible model:",length(regflex$coef)) # number of regressors in the Flexible Model


# Flexible model using Lasso
library(hdm)
lassoreg<- rlasso(flex, data=data) 
sumlasso<- summary(lassoreg)

# Assess predictive performance
sumbasic <- summary(regbasic)
sumflex <- summary(regflex)

# R-squared and adjusted R-squared
R2.1 <- sumbasic$r.squared
cat("R-squared for the basic model: ", R2.1, "\n")
R2.adj1 <- sumbasic$adj.r.squared
cat("adjusted R-squared for the basic model: ", R2.adj1, "\n")

R2.2 <- sumflex$r.squared
cat("R-squared for the flexible model: ", R2.2, "\n")
R2.adj2 <- sumflex$adj.r.squared
cat("adjusted R-squared for the flexible model: ", R2.adj2, "\n")

R2.L <- sumlasso$r.squared
cat("R-squared for the lasso with flexible model: ", R2.L, "\n")
R2.adjL <- sumlasso$adj.r.squared
cat("adjusted R-squared for the flexible model: ", R2.L, "\n")



# MSE and adjusted MSE
MSE1 <- mean(sumbasic$res^2)
cat("MSE for the basic model: ", MSE1, "\n")
p1 <- sumbasic$df[1] # number of regressors
MSE.adj1 <- (n/(n-p1))*MSE1
cat("adjusted MSE for the basic model: ", MSE.adj1, "\n")

MSE2 <-mean(sumflex$res^2)
cat("MSE for the flexible model: ", MSE2, "\n")
p2 <- sumflex$df[1]
MSE.adj2 <- (n/(n-p2))*MSE2
cat("adjusted MSE for the lasso flexible model: ", MSE.adj2, "\n")


MSEL <-mean(sumlasso$res^2)
cat("MSE for the lasso flexible model: ", MSEL, "\n")
pL <- length(sumlasso$coef)
MSE.adjL <- (n/(n-pL))*MSEL
cat("adjusted MSE for the lasso flexible model: ", MSE.adjL, "\n")

# Output the table
library(xtable)
table <- matrix(0, 3, 5)
table[1,1:5]   <- c(p1,R2.1,MSE1,R2.adj1,MSE.adj1)
table[2,1:5]   <- c(p2,R2.2,MSE2,R2.adj2,MSE.adj2)
table[3,1:5]   <- c(pL,R2.L,MSEL,R2.adjL,MSE.adjL)
colnames(table)<- c("p","$R^2_{sample}$","$MSE_{sample}$","$R^2_{adjusted}$", "$MSE_{adjusted}$")
rownames(table)<- c("basic reg","flexible reg", "lasso flex")
tab<- xtable(table, digits =c(0,0,2,2,2,2))
tab

# splitting the data
set.seed(1) # to make the results replicable (we will generate random numbers)
random <- sample(1:n, floor(n*4/5))
# draw (4/5)*n random numbers from 1 to n without replacing them
train <- data[random,] # training sample
test <- data[-random,] # testing sample

# basic model
# estimating the parameters in the training sample
regbasic <- lm(basic, data=train)

# calculating the out-of-sample MSE
trainregbasic <- predict(regbasic, newdata=test)
y.test <- log(test$wage)
MSE.test1 <- sum((y.test-trainregbasic)^2)/length(y.test)
R2.test1<- 1- MSE.test1/var(y.test)

cat("Test MSE for the basic model: ", MSE.test1, " ")

cat("Test R2 for the basic model: ", R2.test1)

# flexible model
# estimating the parameters
options(warn=-1) # ignore warnings 
regflex <- lm(flex, data=train)

# calculating the out-of-sample MSE
trainregflex<- predict(regflex, newdata=test)
y.test <- log(test$wage)
MSE.test2 <- sum((y.test-trainregflex)^2)/length(y.test)
R2.test2<- 1- MSE.test2/var(y.test)

cat("Test MSE for the flexible model: ", MSE.test2, " ")

cat("Test R2 for the flexible model: ", R2.test2)

# flexible model using lasso
library(hdm) # a library for high-dimensional metrics
reglasso <- rlasso(flex, data=train, post=FALSE) # estimating the parameters

# calculating the out-of-sample MSE
trainreglasso<- predict(reglasso, newdata=test)
MSE.lasso <- sum((y.test-trainreglasso)^2)/length(y.test)
R2.lasso<- 1- MSE.lasso/var(y.test)

cat("Test MSE for the lasso on flexible model: ", MSE.lasso, " ")

cat("Test R2 for the lasso flexible model: ", R2.lasso)

# Output the comparison table
table2 <- matrix(0, 3,2)
table2[1,1]   <- MSE.test1
table2[2,1]   <- MSE.test2
table2[3,1]   <- MSE.lasso
table2[1,2]   <- R2.test1
table2[2,2]   <- R2.test2
table2[3,2]   <- R2.lasso

rownames(table2)<- c("basic reg","flexible reg","lasso regression")
colnames(table2)<- c("$MSE_{test}$", "$R^2_{test}$")
tab2 <- xtable(table2, digits =3)
tab2
