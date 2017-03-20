## Seasonal Models (U.S. Retail Sales of Passenger Cars)

## Read Data
```
## read response data
y <- read.table("dat.txt", header=F, sep=" ")

## clean response data 
y <- as.vector(ts(y[-length(y)]))
```

### Globally Constant Linear Trend Model with Seasonal Indicators
$$z_{t}=\beta_0+\beta_1t+\sum_{i-1}^{11}\delta_i IND_ti$$
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





















<script type="text/javascript" async
  src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
