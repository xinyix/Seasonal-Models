## Seasonal Models (U.S. Retail Sales of Passenger Cars)

## Read Data
We are given the monthly U.S. retail sales of passenger cars (in thousands) for the period January 1955 to December 1977
```
## read response data
y <- read.table("dat.txt", header=F, sep=" ")

## clean response data 
y <- as.vector(ts(y[-length(y)]))
> y
  [1]  440  477  637  652  661  681  647  659  655  576  509  631  432  448  545  564  560  540  535  568  421  424  404  514  437  439  573  549  556  517  543  492  495
 [34]  464  409  512  382  334  401  418  424  411  400  371  317  321  335  511  421  425  498  575  584  586  567  534  458  535  429  431  430  494  597  647  647  596
 [67]  547  525  459  548  543  544  414  375  480  496  544  572  501  471  371  550  558  526  506  473  592  635  644  602  614  540  374  678  638  644  554  498  624
[100]  759  715  692  709  553  404  715  640  712  612  552  637  812  781  754  724  649  565  659  564  757  667  631  799  896  841  842  834  767  590  746  794  909
[133]  733  715  913  820  745  809  688  668  565  855  798  726  612  558  736  774  815  855  696  594  615  735  684  673  702  700  851  815  900  872  829  728  663
[166]  980  864  750  730  726  809  855  901  898  761  655  808  923  797  720  624  686  746  799  811  925  762  639  581  755  539  538  694  749  899  883  889  957
[199]  819  724  884 1050  958  741  721  814  913  900 1032 1025  905  813  877 1070 1030  848  875  920 1142 1024 1144 1084  960  836  873  977  911  694  679  683  778
[232]  816  882  811  811  811  726  757  604  508  578  684  670  660  741  770  794  684  726  889  744  701  679  758  947  914  921  956  865  762  792  868  840  807
[265]  725  811 1084 1027 1054 1118  913  931  829 1014  881  795
```

### Globally Constant Linear Trend Model with Seasonal Indicators
Since we are given monthly data, the number of seasons s=12. The globally constant linear trend with seasonal indicators thus looks like
 $$z_{t}=\beta_0+\beta_1t+\sum_{i=1}^{11}\delta_i IND_{ti} + \varepsilon_t$$. We now fit our data to this model to estimate the coefficients. We take the first 168 data points as the training set.

```
## construct design matrix for Global Constant Linear Trend Model with Seasonal Indicators 
x <- matrix(data=NA, nrow=276, ncol=13)

x[, 1] <- rep(1, 276)
x[, 2] <- 1:276

block <- matrix(NA, nrow=12, ncol=11)
block[1:11, 1:11] <- diag(1, 11, 11)		
block[12, ] <- rep(0, 11)

for (i in 0:22) {
	x[(1+12*i):(12+12*i), 3:13] <- block 	# i=0, 2, ..., 22
}

## first few rows of x
## > x
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
##  [1,]    1    1    1    0    0    0    0    0    0     0     0     0     0
##  [2,]    1    2    0    1    0    0    0    0    0     0     0     0     0
##  [3,]    1    3    0    0    1    0    0    0    0     0     0     0     0
##  [4,]    1    4    0    0    0    1    0    0    0     0     0     0     0
##  [5,]    1    5    0    0    0    0    1    0    0     0     0     0     0
##  [6,]    1    6    0    0    0    0    0    1    0     0     0     0     0
##  [7,]    1    7    0    0    0    0    0    0    1     0     0     0     0
##  [8,]    1    8    0    0    0    0    0    0    0     1     0     0     0
##  [9,]    1    9    0    0    0    0    0    0    0     0     1     0     0
## [10,]    1   10    0    0    0    0    0    0    0     0     0     1     0


## build a data frame to host the entire dataset
df <- data.frame(y, x)

## split data into training and testing set, use the first 168 points as training
dftrain <- df[1:168, ]
xtrain <- df[1:168, 2:14]
ytrain <- y[1:168]
xtest <- df[-(1:168), 2:14]
ytest <- y[-(1:168)]

## run ordinary least square regression on training set, predict with testing set
mylm <- lm(y~., data=dftrain)
pred <- predict(mylm, newdata=xtest)

## Coefficients: b0, b1, delta1, delta2, ..., delta11
## > mylm

## Call:
## lm(formula = y ~ . - 1, data = dftrain)

## Coefficients:
##        X1         X2         X3         X4         X5         X6         X7         X8         X9        X10        X11        X12        ## X13  
## 449.6181     2.0201   -84.7787  -102.7274    21.2525    57.0181    55.3551    47.0493     9.6720   -43.4195  -128.7968     0.1831   
## -45.9085  

par(mfrow=c(1, 2))
## plot prediction and observation
plot(169:276, ytest, type="l", col="blue", xlab="Months since Jan 1955", ylab="Monthly Sales (in thousands) of US Cars", main="GLR Indicator")
lines(169:276, pred, type="l", col="red")
legend("topright", legend=c("Observation", "Prediction"), col=c("blue", "red"), lty=1:2)

## plot acf of fitted residuals 
fit <- predict(mylm)
res <- ytrain-fit
acf(res, main="ACF of Fitted Residuals")

## calculate prediction mse
MSE <- mean(sum(res^2))
> MSE
[1] 1264229
```
![original resid dist](https://github.com/xinyix/Seasonal-Models/blob/master/GLR_indicator.png?raw=true)

The fitted residual has strong correlations at some of the small lags, this suggests us to try other models such as the auto-regressive models. The prediction MSE will be compared to that of other models, for end results, jump to "Conclusion".

### Globally Constant Linear Trend Model with Sinusoidal Harmonics
As suggested by the book (page 149) that in discrete time series, we can consider at most m=s/2 harmonics. Since s=12, we take m=6. So the model we propose looks like $$z_{t}=\beta_0+\beta_1t+\sum_{i=1}^{6} \beta_{1i}sin(f_{i}t)+\beta_{2i}cos(f_{i}t) + \varepsilon_t$$. Again taking the first 168 points as training set
```
## construct design matrix
x <- matrix(data=NA, nrow=276, ncol=14)
x[, 1] <- rep(1, 276)
x[, 2] <- 1:276

for (i in 1:276) {
	for (j in 1:6) {
		x[i, (2*j+1)] <- sin(pi*j*i/6)
		x[i, (2*j+2)] <- cos(pi*j*i/6)
	}
}

## first few rows of x
## > x
##        [,1] [,2]          [,3]          [,4]          [,5] [,6]          [,7]          [,8]          [,9] [,10]         [,11]         [,12]         [,13] [,14]
##  [1,]    1    1  5.000000e-01  8.660254e-01  8.660254e-01  0.5  1.000000e+00  6.123234e-17  8.660254e-01  -0.5  5.000000e-01 -8.660254e-01  1.224647e-16    -1
##  [2,]    1    2  8.660254e-01  5.000000e-01  8.660254e-01 -0.5  1.224647e-16 -1.000000e+00 -8.660254e-01  -0.5 -8.660254e-01  5.000000e-01 -2.449294e-16     1
##  [3,]    1    3  1.000000e+00  6.123234e-17  1.224647e-16 -1.0 -1.000000e+00 -1.836970e-16 -2.449294e-16   1.0  1.000000e+00  1.194340e-15  3.673940e-16    -1
##  [4,]    1    4  8.660254e-01 -5.000000e-01 -8.660254e-01 -0.5 -2.449294e-16  1.000000e+00  8.660254e-01  -0.5 -8.660254e-01 -5.000000e-01 -4.898587e-16     1
##  [5,]    1    5  5.000000e-01 -8.660254e-01 -8.660254e-01  0.5  1.000000e+00  1.194340e-15 -8.660254e-01  -0.5  5.000000e-01  8.660254e-01  2.388680e-15    -1
##  [6,]    1    6  1.224647e-16 -1.000000e+00 -2.449294e-16  1.0  3.673940e-16 -1.000000e+00 -4.898587e-16   1.0  2.388680e-15 -1.000000e+00 -7.347881e-16     1
##  [7,]    1    7 -5.000000e-01 -8.660254e-01  8.660254e-01  0.5 -1.000000e+00 -4.286264e-16  8.660254e-01  -0.5 -5.000000e-01  8.660254e-01  8.572528e-16    -1
##  [8,]    1    8 -8.660254e-01 -5.000000e-01  8.660254e-01 -0.5 -4.898587e-16  1.000000e+00 -8.660254e-01  -0.5  8.660254e-01 -5.000000e-01 -9.797174e-16     1
##  [9,]    1    9 -1.000000e+00 -1.836970e-16  3.673940e-16 -1.0  1.000000e+00  5.510911e-16 -7.347881e-16   1.0 -1.000000e+00  8.578717e-16  1.102182e-15    -1
## [10,]    1   10 -8.660254e-01  5.000000e-01 -8.660254e-01 -0.5  2.388680e-15 -1.000000e+00  8.660254e-01  -0.5  8.660254e-01  5.000000e-01 -4.777360e-15     1
 
## build a data frame to host the entire dataset
df <- data.frame(y, x)

## split data into training and testing set, use the first 168 points as training
dftrain <- df[1:168, ]
xtrain <- df[1:168, 2:15]
ytrain <- y[1:168]
xtest <- df[-(1:168), 2:15]
ytest <- y[-(1:168)]

## run ordinary least square regression on training set, predict with testing set
mylm <- lm(y~.-1, data=dftrain)
pred <- predict(mylm, newdata=xtest)

par(mfrow=c(1, 2))
## plot prediction and observation
plot(169:276, ytest, type="l", col="blue", xlab="Months since Jan 1955", ylab="Monthly Sales (in thousands) of US Cars", main="GLR Trig")
lines(169:276, pred, type="l", col="red")
legend("topright", legend=c("Observation", "Prediction"), col=c("blue", "red"), lty=1:2)

## plot acf of fitted residuals
fit <- predict(mylm)
res <- ytrain-fit
acf(res, main="ACF of Fitted Residuals")

## calculate prediction mse
MSE <- mean(sum(res^2))
> MSE
[1] 1262280
```
![original resid dist](https://github.com/xinyix/Seasonal-Models/blob/master/GLR_trig.png?raw=true)

The prediction MSE is a bit smaller than using the Globally Constant Linear Trend Model with Seasonal Indicator.

### General Exponential Smoothing for the Locally Constant Linear Trend Model with Seasonal Indicator
We apply the method introduced in Section 4.3.1 of the book on our data. The utility functions are defined in the Appendix, these functions include Locally_Constant_Indicator_Model(), Seasonal_Indicator_F(), Seasonal_Indicator_L() and Locally_Constant_Indicator_Optimal()
```
opt_omega <- Locally_Constant_Indicator_Optimal(y, 12, NULL)
> opt_omega
[1] 0.2348485

mymodel <- Locally_Constant_Indicator_Model(y, 12, opt_omega, NULL)

par(mfrow=c(1, 2))
## plot prediction and observation
plot(1:276, y, type="l", col="blue", xlab="Months since Jan 1955", ylab="Monthly Sales (in thousands) of US Cars", main="LLR Indicator")
lines(1:276, mymodel$one_step_predictions, type="l", col="red")
legend("topright", legend=c("Observation", "Prediction"), col=c("blue", "red"), lty=1:2)

## plot acf of prediction residuals
acf(mymodel$one_step_errors, main="ACF of Prediction Residuals")

> mymodel$MSE
[1] 1753289
```
![original resid dist](https://github.com/xinyix/Seasonal-Models/blob/master/LLR_indicator.png?raw=true)

The optimal smoothing constant opt_omega=0.2348485 is chosen so that MSE is minimized. We can see the prediction follows closely to the observations.

### General Exponential Smoothing for the Locally Constant Linear Trend Model with Sinusoidal Harmonics
```
opt_omega <- Locally_Constant_Trigonometric_Optimal(y, 12, 6, NULL)
mymodel <- Locally_Constant_Trigonometric_Model(y, 12, 6, opt_omega, NULL)

par(mfrow=c(1, 2))
## plot prediction and observation
plot(1:276, y, type="l", col="blue", xlab="Months since Jan 1955", ylab="Monthly Sales (in thousands) of US Cars", main="LLR Trig")
lines(1:276, mymodel$one_step_predictions, type="l", col="red")
legend("topright", legend=c("Observation", "Prediction"), col=c("blue", "red"), lty=1:2)

## plot acf of prediction residuals
acf(mymodel$one_step_errors, main="ACF of Prediction Residuals")
```
After execution, we are getting the error
```
Error in solve.default(F, f_0) : 
  system is computationally singular: reciprocal condition number = 9.8206e-29
```
which means the matrix F is non-invertible. We proceed to the next method.

###











### Appendix
Seasonal_Indicator_F(), Seasonal_Indicator_L(), Locally_Constant_Indicator_Model() and Locally_Constant_Indicator_Optimal()
```
############################ Indicator F ##############################################
## See page 159 for this algorithm 
Seasonal_Indicator_F <- function (s, omega) {
	n <- 2 + (s - 1)		# number of variables in model
	F <- matrix(0, n, n)
	
	oneOverAlpha <- 1 / (1-omega)
	F[1, 1] <- oneOverAlpha
	F[2, 2] <- omega * (1+omega) * oneOverAlpha^3
	F[1, 2] <- -omega * oneOverAlpha^2
	F[2, 1] <- F[1, 2]
	
	omegaFrac <- 1 / (1-omega^s)
	
	for (j in 3:(s+1)) {
		F[1, j] <- (omega^(s+2-j)) * omegaFrac
		F[j, 1] <- F[1, j]
		F[j, j] <- F[1, j]
	}
	
	omegaFrac <- omegaFrac * omegaFrac
	for (j in 3:(s+1)) {
		F[2, j] <- -(omega^(s+2-j)) * ((s+2-j)+(j-2)*omega^s) * omegaFrac
		F[j, 2] <- F[2, j]
	}
	
	return(F)
}
############################ Indicator L ##############################################
## See page 148 for this algorithm
Seasonal_Indicator_L <- function (k, s) {
	library(abind)
	
	L_11 <- matrix(0, k+1, k+1)
	for (jj in 1:(k+1)) {
		for (ii in jj:(k+1)) {
			L_11[ii, jj] <- 1 / factorial(ii-jj)
		}
	}
	
	L_21 <- matrix(0, s-1, k+1)
	L_21[1, 1] <- 1
	
	L_22 <- matrix(0, s-1, s-1)
	for (jj in 1:(s-1)) {
		L_22[1, jj] <- -1
	}
	for (ii in 2:(s-1)) {
		L_22[ii, ii-1] <- 1
	}
	
	top <- abind(L_11, matrix(0, k+1, s-1), along=2)
	bottom <- abind(L_21, L_22, along=2)
	L <- abind(top, bottom, along=1)
	
	return(L)	
}

############################ Indicator Model #########################################
Locally_Constant_Indicator_Model <- function (Z, s, omega, beta_0) {
	n <- length(Z)
	
	## 1) get initial guess beta_0, if not provided estimate from the entrire data
	if (is.null(beta_0)) {
		x <- matrix(data=NA, nrow=276, ncol=2+(s-1))		## 2 linear components

		x[, 1] <- rep(1, 276)

		x[, 2] <- 1:276

		block <- matrix(NA, nrow=12, ncol=11)

		block[1:11, 1:11] <- diag(1, 11, 11)		

		block[12, ] <- rep(0, 11)

		for (i in 0:22) {
			x[(1+12*i):(12+12*i), 3:13] <- block 	# i=0, 2, ..., 22
		}

		## build a data frame to host the derived data points
		df <- data.frame(Z, x)

		mylm <- lm(Z~.-1, data=df)
		beta_0 <- mylm$coeff
		beta_0 <- as.matrix(beta_0)
	}
	
	## 2) get F and L matrices
	F <- Seasonal_Indicator_F(s, omega)
	k <- 1	# in this problem we only consider linear model
	L <- Seasonal_Indicator_L(k, s)
	f_0 <- matrix(0, 2+s-1, 1)
	f_0[1] <- 1
	FinvOnf_0 <- solve(F, f_0)
	f_1 <- as.matrix(df[1, -1])
	
	## 3) iterate over the time series (the first point is calculated manually)
	beta_hats <- matrix(0, 2+s-1, n)
	z_hat_1 <- matrix(0, n, 1)
	z_hat_1[1] <- f_1 %*% beta_0
	beta_hats[, 1] <- t(L) %*% beta_0 + FinvOnf_0 %*% (Z[1] - z_hat_1[1])
	
	for (ii in 2:n) {
		z_hat_1[ii] <- f_1 %*% beta_hats[, ii-1]
		incr <- FinvOnf_0 %*% (Z[ii]-z_hat_1[ii])
		beta_hats[, ii] <- t(L) %*% beta_hats[, ii-1] + incr
	}
	
	et <- Z - z_hat_1
	MSE <- mean(sum(et^2))
	
	rs <- list(one_step_predictions=z_hat_1, one_step_errors=et, MSE=MSE)
	return(rs)
}

############################ Seasonal Indicator Optimal Omega ###########################
Locally_Constant_Indicator_Optimal <- function (Z, s, omega_grid) {
	if (is.null(omega_grid)) {
		omega_grid <- seq(from=0.1, to=0.99, length.out=100)
	}
	
	MSE_omega <- matrix(0, 1, length(omega_grid))
	for (ii in 1:length(omega_grid)) {
		out <- Locally_Constant_Indicator_Model(Z, s, omega_grid[ii], NULL)
		MSE_omega[ii] <- out$MSE
	}
	
	return(omega_grid[which.min(MSE_omega)])
}
```

Seasonal_Trigonometric_F(), Seasonal_Trigonometric_L(), Locally_Constant_Trigonometric_Model() and Locally_Constant_Trigonometric_Optimal()
```
############################ Seasonal Trigonometric F #############################################
## See page 164 for this algorithm 
Seasonal_Trigonometric_F <- function (s, m, omega) {
	f <- 2*pi/s
	
	F <- matrix(0, 2+2*m, 2+2*m)
	F[1, 1] <- 1 / (1-omega)
	F[1, 2] <- -omega / ((1-omega)^2)
	F[2, 2] <- omega * (1+omega) / ((1-omega)^3)
	F[2, 1] <- F[1, 2]
	
	for (ii in 1:m) {
		f_ii <- f*ii
		g_1 <- 1 - 2 * omega * cos(f_ii) + omega^2
		F[1, 1+2*ii] <- -omega * sin(f_ii) / g_1
		F[1, 2+2*ii] <- (1-omega*cos(f_ii)) / g_1
		F[2, 1+2*ii] <- omega * (1-omega^2) * sin(f_ii) / (g_1^2)
		F[2, 2+2*ii] <- - (omega*(1+omega^2)*cos(f_ii)-2*omega^2) / (g_1^2)
		
		F[1+2*ii, 1] <- F[1, 1+2*ii]
		F[2+2*ii, 1] <- F[1, 2+2*ii]
		F[1+2*ii, 2] <- F[2, 1+2*ii]
		F[2+2*ii, 2] <- F[2, 2+2*ii]
	}
	
	for (ii in 1:m) {
		f_ii <- f * ii
		for (kk in 1:m) {
			f_kk <- f * kk
			g_2 <- 1 - 2 * omega * cos(f_ii-f_kk) + omega^2
			g_3 <- 1 - 2 * omega * cos(f_ii+f_kk) + omega^2
			
			F[1+2*ii, 1+2*kk] <- 0.5 * ((1-omega*cos(f_ii-f_kk))/g_2-(1-omega*cos(f_ii+f_kk))/g_3)
			F[2+2*ii, 2+2*kk] <- 0.5 * ((1-omega*cos(f_ii-f_kk))/g_2+(1-omega*cos(f_ii+f_kk))/g_3)
			F[1+2*ii, 2+2*kk] <- -0.5 * (omega*sin(f_ii-f_kk)/g_2+omega*sin(f_ii+f_kk)/g_3)
			
			F[1+2*kk, 1+2*ii] <- F[1+2*ii, 1+2*kk]
			F[2+2*kk, 2+2*ii] <- F[2+2*ii, 2+2*kk]
			F[2+2*kk, 1+2*ii] <- F[1+2*ii, 2+2*kk]
		}
	}
	
	return(F)
}

############################ Seasonal Trigonometric L #############################################
## See page 153 for this algorithm
Seasonal_Trigonometric_L <- function (k, s, m) {
	library(abind)
	
	## polynomial portion of L
	L_1 <- matrix(0, k+1, k+1)
	for(jj in 1:(k+1)) {
		for (ii in jj:(k+1)) {
			L_1[ii, jj] <- 1/factorial(ii-jj)
		}
	}
	
	## trigonometric portion of L
	f <- 2*pi/s
	
	L_2 <- matrix(0, 2*m, 2*m)
	for (ii in 0:(m-1)) {
		iip1 <- ii+1
		top <- abind(cos(f*iip1), sin(f*iip1), along=2)
		bottom <- abind(-sin(f*iip1), cos(f*iip1), along=2)
		L_2i <- abind(top, bottom, along=1)
		
		L_2[2*ii+1, 2*ii+1] <- L_2i[1, 1]
		L_2[2*ii+2, 2*ii+1] <- L_2i[2, 1]
		L_2[2*ii+1, 2*ii+2] <- L_2i[1, 2]
		L_2[2*ii+2, 2*ii+2] <- L_2i[2, 2]
	}
	
	top <- abind(L_1, matrix(0, k+1, 2*m), along=2)
	bottom <- abind(matrix(0, 2*m, k+1), L_2, along=2)
	L <- abind(top, bottom, along=1)
	
	return(L)
}

############################ Seasonal Trigonometric Model #########################################
Locally_Constant_Trigonometric_Model <- function (Z, s, m, omega, beta_0) {
	n <- length(Z)
	
	## 1) get initial guess beta_0, if not provided estimate from the entrire data
	if (is.null(beta_0)) {
		x <- matrix(data=NA, nrow=276, ncol=2+2*m)
		x[, 1] <- rep(1, 276)
		x[, 2] <- 1:276

		for (i in 1:276) {
			for (j in 1:m) {
				x[i, (2*j+1)] <- sin(pi*j*i/6)
				x[i, (2*j+2)] <- cos(pi*j*i/6)
			}
		}

		df <- data.frame(Z, x)

		mylm <- lm(Z~.-1, data=df)
		beta_0 <- mylm$coeff
		beta_0 <- as.matrix(beta_0)
	}
	
	## 2) get F and L matrices
	F <- Seasonal_Trigonometric_F(s, m, omega)
	k <- 1	# in this problem we only consider linear model
	L <- Seasonal_Trigonometric_L(k, s, m)
	f_0 <- matrix(0, 2+2*m, 1)
	f_0[1] <- 1
	f_0[seq(4, 2+2*m, by=2)] <- 1
	FinvOnf_0 <- solve(F, f_0)
	f_1 <- as.matrix(df[1, -1])
	
	## 3) iterate over the time series (the first point is calculated manually)
	beta_hats <- matrix(0, 2+2*m, n)
	z_hat_1 <- matrix(0, n, 1)
	z_hat_1[1] <- f_1 %*% beta_0
	beta_hats[, 1] <- t(L) %*% beta_0 + FinvOnf_0 %*% (Z[1] - z_hat_1[1])
	
	for (ii in 2:n) {
		z_hat_1[ii] <- f_1 %*% beta_hats[, ii-1]
		incr <- FinvOnf_0 %*% (Z[ii]-z_hat_1[ii])
		beta_hats[, ii] <- t(L) %*% beta_hats[, ii-1] + incr
	}
	
	et <- Z - z_hat_1
	MSE <- mean(sum(et^2))
	
	rs <- list(one_step_predictions=z_hat_1, one_step_errors=et, MSE=MSE)
}

############################ Seasonal Trigonometric Optimal Omega #####################################
Locally_Constant_Trigonometric_Optimal <- function (Z, s, m, omega_grid) {
	if (is.null(omega_grid)) {
		omega_grid <- seq(from=0.1, to=0.99, length.out=100)
	}
	
	MSE_omega <- matrix(0, 1, length(omega_grid))
	for (ii in 1:length(omega_grid)) {
		out <- Locally_Constant_Trigonometric_Model(Z, s, m, omega_grid[ii], NULL)
		MSE_omega[ii] <- out$MSE
	}
	
	return(omega_grid[which.min(MSE_omega)])
}
```


<script type="text/javascript" async
  src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
