##############################################
##############################################
# problem1
# linear congruentual generator
# a
lcg = function(n) {
    rng = c()
    a = 110351245
    m = 2 ^ 32
    c = 12345
    z = as.numeric(Sys.time()) * 1000 # seed value
    for(i in 1:n) {
      z = (a * z + c) %% m
      rng[i] = z / m
    }
    return(rng)
}
# b
u100 = lcg(100)
u500 = lcg(500)
u1000 = lcg(1000)
# c
par(mfrow = c(1, 3))
hist(u100)
hist(u500)
hist(u1000)
# d
ks.test(u100, "punif")
ks.test(u500, "punif")
ks.test(u1000, "punif")
##############################################
##############################################
# problem2
# a
x = seq(-5, 10, 0.1)
fx = dnorm(x, 3, 2)
# b
par(mfrow = c(1, 1))
plot(x, fx)
# c
x = rnorm(1000, 3, 2)
summary(x)
hist(x)
##############################################
##############################################
# problem3
# a
x = rpois(1000, 10)
hist(x)
boxplot(x)
summary(x)
mean(x)
var(x)
# b
x = rexp(1000, rate = 1 / 5)
hist(x)
boxplot(x)
summary(x)
mean(x)
var(x)
##############################################
##############################################
# problem4
# a
library(MASS)
x = rnorm(1000, 10, 2)
fitdistr(x, "normal")
# b
x = rweibull(1000, shape = 2.1, scale = 1.1)
fitdistr(x, "weibull")
# c
x = rgamma(1000, rate = 0.5, shape = 3.5)
fitdistr(x, "gamma")
##############################################
##############################################
# problem5
# a
library(MASS)
invt.Weib = function(n, beta, eta){
    R = runif(n)
    x = eta * (-log(1 - R)) ^ (1 / beta)
    return(x)
}
# b
x = invt.Weib(n = 100, beta = 2, eta = 10)
fitdistr(x, "weibull")
# c
n = seq(10, 3000, 50)
beta.true = 2
eta.true = 10
beta.hat = c()
eta.hat = c()

for(i in 1:length(n)){
    x = invt.Weib(n[i], beta.true, eta.true)
    beta.hat[i] = as.numeric(fitdistr(x, "weibull")$estimate[1])
    eta.hat[i] = as.numeric(fitdistr(x, "weibull")$estimate[2])
}
plot(n, beta.hat, main="Plot of the MLE of beta")
plot(n, eta.hat, main="Plot of the MLE of eta")
##############################################
##############################################
# problem6
# a
mu = 2
sigma = 2
n = 10
nsim = 10

x.bar = c()
x.var = c()
for(i in 1:nsim) {
    x = rnorm(n, mu, sigma)
    x.bar[i] = mean(x)
    x.var[i] = var(x)
}
# b 
expecte.mean = mean(x.bar)
# c
mu = 2
sigma = 2
n = 10
nsim = 1000

x.bar = c()
x.var = c()
for(i in 1:nsim){
    x = rnorm(n, mu, sigma)
    x.bar[i] = mean(x)
    x.var[i] = var(x)
}
expecte.mean = mean(x.bar)
par(mfrow = c(1, 2))
hist(x.bar)
qqnorm(x.bar)
qqline(x.bar)
# d
mu = 2
sigma = 2
n1 = 10
n2 = 100
n3 = 1000
nsim = 1000
xbar1 = c()
xbar2 = c()
xbar3 = c()
xvar1 = c()
xvar2 = c()
xvar3 = c()
for(i in 1:nsim){
    x1 = rnorm(n1, mu, sigma)
    x2 = rnorm(n2, mu, sigma)
    x3 = rnorm(n3, mu, sigma)
    xbar1[i] = mean(x1)
    xbar2[i] = mean(x2)
    xbar3[i] = mean(x3)
    xvar1[i] = var(x1)
    xvar2[i] = var(x2)
    xvar3[i] = var(x3)
}
####(e)####
par(mfrow = c(3, 1))
hist(xbar1)
hist(xbar2)
hist(xbar3)

par(mfrow = c(3, 1))
qqnorm(xbar1)
qqline(xbar1)
qqnorm(xbar2)
qqline(xbar2)
qqnorm(xbar3)
qqline(xbar3)
# f
mu = 2
sigma = 2
n = 100
nsim = 1000

low95 = c()
up95 = c()
low99 = c()
up99 = c()

for(i in 1:nsim){
    x = rnorm(n, mu, sigma)

    low95[i] = mean(x) - 1.96 * sqrt(sigma ^ 2 / n)
    up95[i]= mean(x) + 1.96 * sqrt(sigma ^ 2 / n)
    low99[i] = mean(x) - 2.58 * sqrt(sigma ^ 2 / n)
    up99[i]= mean(x) + 2.58 * sqrt(sigma ^ 2 / n)
}
ave.ci95 = mean((low95 <= 2) & (2 <= up95))
ave.ci99 = mean((low99 <= 2) & (2 <= up99))
# g
mu = 2
sigma = 2
n = 100
nsim = 1000

low95 = c()
up95 = c()
low99 = c()
up99 = c()

for(i in 1:nsim){
  x <- rnorm(n, mu, sigma)

  low95[i] = mean(x) - qt(0.975, df = n - 1) * sqrt(var(x) / n)
  up95[i] = mean(x) + qt(0.975, df = n - 1) * sqrt(var(x) / n)
  low99[i] = mean(x) - qt(0.995, df = n - 1) * sqrt(var(x) / n)
  up99[i] = mean(x) + qt(0.995, df = n - 1) * sqrt(var(x) / n)
}

ave.ci95 = mean((low95 <= 2) & (2 <= up95))
ave.ci99 = mean((low99 <= 2) & (2 <= up99))