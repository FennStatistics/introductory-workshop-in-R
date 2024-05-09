

### siedler von catan
d1 <- round(x = runif(n = 1000, min = 1, max = 6), digits = 0)
d2 <- round(x = runif(n = 1000, min = 1, max = 6), digits = 0)


table(d1+d2)
barplot(height = table(d1+d2))


########### see: http://rstudio-pubs-static.s3.amazonaws.com/240670_37c5f4ada9a5402ab5474bb66514e667.html

# MLE for Binomial Distribution
y <-c(0,1,0,1,0,1,0,1,0,1)

sum(y) / length(x = y)

#############
x <- 5
p <- seq(0, 0.99, 0.01)
out <- choose(n = 10, k = x)* p^x * (1-p)^(10-x)
plot(out)
abline(v = which(out == max(out)), col = "red")
p[which(out == max(out))]
#############


n<-1 # length(y) here's the change!
# formulation for the log likelihood for the binomial
logL <- function(p) sum(log(dbinom(y, n, p)))
# again we can test the function for one value of p
logL(0.7)
#plot logL
p.seq <- seq(0, 0.99, 0.01)
plot(p.seq, sapply(p.seq, logL), type="l")
#optimum:
fit_optimize <- optimize(logL, lower=0, upper=1, maximum=TRUE)
abline(v = fit_optimize$maximum, col = "red")
fit_optimize$maximum



############
# use MLE, see: https://www.r-bloggers.com/2013/08/fitting-a-model-by-maximum-likelihood/
############
library(stats4)
###### to fit a normal distribution

x <- rnorm(n = 1000, mean = 2, sd = 2)
hist(x)
mean(x = x)
sd(x = x)

LL <- function(mu, sigma) {
  R = dnorm(x, mu, sigma)

  out <- -sum(log(R))

  print(out)
  out
}

mle(LL, start = list(mu = 1, sigma=1), method = "L-BFGS-B", lower = c(-Inf, 0),
    upper = c(Inf, Inf))





###### to fit a simple linear regression
### simulate data
set.seed(seed = 111)
N <- 100

x <- runif(n = N)
y <- 3 + 5 * x + rnorm(n = N, mean = 0, sd = 1)

df <- data.frame(x = x, y = y)
rm(x); rm(y)


plot(df)
cor(df)
abline(a = 3, b = 5, col = "green")



lm_out <- lm(y ~ x, data=df)
summary(lm_out)
abline(a = lm_out, col = "red")



LL <- function(beta0, beta1, mu, sigma) {
  # Find residuals
  R = df$y - df$x * beta1 - beta0

  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  R = suppressWarnings(dnorm(R, mu, sigma))
  # Sum the log likelihoods for all of the data points
  -sum(log(R))
}


fit <- mle(LL, start = list(beta0 = 2, beta1 = 1.5, sigma=1), fixed = list(mu = 0),
             nobs = nrow(x = df))
summary(fit)

AIC(object = fit)
AIC(object = lm_out)


############
# use optim, see: https://www.r-bloggers.com/2013/08/fitting-a-model-by-maximum-likelihood/
############
#define function to minimize residual sum of squares
min_residuals <- function(data, par) {

  with(data, sum((par[1] + par[2] * x - y)^2))
}

#find coefficients of linear regression model
fit_optim <- optim(par=c(0, 1), fn=min_residuals, data=df, hessian = TRUE)
fit_optim$par



