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
$$z_{t}=\beta_0+\beta_1t+\sum_{i-1}^{11}\delta_i IND_{ti} + \varepsilon_t$$. We now fit our data to this model to estimate the coefficients. We take the first 168 data points as the training set.

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

## First few rows of x
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

### Globally Constant Linear Trend Model with Seasonal Indicators



















<script type="text/javascript" async
  src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
