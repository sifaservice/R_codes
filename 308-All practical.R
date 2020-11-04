###====* problem: 01 *=====##

######(a)#####

lcg = function(n){
  r = c()
  a = 1103515245
  m = 2**32
  c = 12345
  z = as.numeric(Sys.time()) * 1000
  
  for (i in 1:n) {
    z = (a * z + c) %% m
    r[i] = z / m
  } 
  
  return(r)
}

#####(b)#####

U100 = lcg(100)
U100
U500 = lcg(500)
U500
U1000 = lcg(1000)
U1000

#####(c)####
par(mfrow=c(1,3))
hist(U100,breaks=10,main="Histogram of U100")
hist(U500,breaks=10,main="Histogram of U500")
hist(U1000,breaks=10,main="Histogram of U1000")

######(d)######
ks.test(U100, 'punif')
ks.test(U500, 'punif')
ks.test(U1000,'punif')

################################################
###====* problem : 02 *=====##
################################################
######(a)####
x <- seq(-5, 10, 0.1)
x
fx = dnorm(x, 3, 2)


####(b)#####
plot(x, fx, main = "PDF of N(3,4)", type = "l", lwd = 2)

#####(c)#####
x<-rnorm(1000,3,2)
summary(x)
hist(x)

################################################
###====* problem : 03 *=====##
################################################
#####(a)###
x <- rpois(1000, 4)
hist(x)
boxplot(x)
summary(x)
mean(x)
var(x)

#####(b)###
y <- rexp(1000, 0.2) # mean = 1 / .2
hist(y)
boxplot(y)
summary(y)
mean(y)
var(y)



################################################
###====* problem : 04 *=====##
################################################

# Problem : 04
# Md. Sifat HOssain, 1711024137
#####(a)#####

library(MASS)
nor37 = rnorm(1000, mean = 10, sd = 2)
fitdistr(nor37, "normal")

####(b)####
wei37 = rweibull(1000, shape = 2.1, scale = 1.1)
fitdistr(wei37, "weibull")

#######(c)#####
gamm37 = rgamma(1000, rate = 0.5, shape = 3.5)
fitdistr(gamm37,"gamma")


################################################
###====* problem : 05 *=====##
################################################

# Problem : 05
# Md. Sifat HOssain, 1711024137

#####(a)#####
invt.Weib = function(n, beta, eta) {
  u37 = runif(n)
  x37 = eta * (-log(1 - u37)) ^ (1 / beta)
  return(x37)
}

####(b)####
library(MASS)
x <- invt.Weib(n = 100, beta = 2, eta = 10)
fitdistr(x, "weibull")

#####(c)####
n <- seq(10, 3000, 50); b_t <- 2.0; eta_t <- 10.0
b_hat <- c(); eta_hat <- c()
for (i in 1:length(n)) {
  x <- invt.Weib(n[i], b_t, eta_t)
  b_hat[i] <- as.numeric(fitdistr(x, "weibull")$estimate[1])
  eta_hat[i] <- as.numeric(fitdistr(x,"weibull")$estimate[2])
}
par(mfrow = c(1, 2))
plot(n, b_hat, main = "Plot of the MLE of beta")
plot(n, eta_hat, main = "Plot of the MLE of eta")




################################################
###====* problem : 06 *=====##
################################################
#####problem____06####

# Problem : 06
# Md. Sifat HOssain, 1711024137

#####(a)######
mu = 2; sigma = 2; n = 10; nsim = 10
x.bar = rep(NA, nsim); x.var = rep(NA, nsim)

for (i in 1:nsim) {
  x <- rnorm(n, mu, sigma)
  x.bar[i] = mean(x);   x.var[i] = var(x)
}

mean.var = cbind(x.bar, x.var)
colnames(mean.var) = c("Mean", "Variance"); mean.var

#####(b)#####
nsim = 500
x.bar = rep(NA, nsim)
for (i in 1:nsim) {
  x <- rnorm(n, mu, sigma)
  x.bar[i] = mean(x)
}

s.mean = mean(x.bar); s.mean

#####(c)######
nsim = 1000; x.bar = rep(NA, nsim); x.var = rep(NA, nsim)
n = 10
for (i in 1:nsim) {
  x <- rnorm(n, mu, sigma)
  x.bar[i] = mean(x)
  x.var[i] = var(x)
}

s.mean = mean(x.bar)
s.mean

par(mfrow = c(1,2))
hist(x.bar)
qqnorm(x.bar)
qqline(x.bar)

####(d)####
mu = 2; sigma = 2; n10 = 10; n100 = 100; n1000 = 1000; nsim = 1000
xbar10 = rep(NA, nsim); xbar100 = rep(NA, nsim); xbar1000 = rep(NA, nsim)
xvar10 = rep(NA, nsim); xvar100 = rep(NA, nsim); xvar1000 = rep(NA, nsim)
for (i in 1:nsim) {
  x10 <- rnorm(n10, mu, sigma)
  x100 <- rnorm(n100, mu, sigma)
  x1000 <- rnorm(n1000, mu, sigma)
  xbar10[i] = mean(x10); xbar100[i] = mean(x100); xbar1000[i] = mean(x1000)
  xvar10[i] = var(x10); xvar100[i] = var(x100); xvar1000[i] = var(x1000)
}

samp.mean37 = c(mean(xbar10), mean(xbar100), mean(xbar1000))
samp.var37 = c(var(xbar10), var(xbar100), var(xbar1000))
var.mean = cbind(samp.mean37, samp.var37)
colnames(var.mean) = c("Mean", "Variance")
rownames(var.mean) = c("n = 10", "n = 100", "n = 1000"); var.mean

####(e)####
par(mfrow = c(3, 1))
hist(xbar10)
hist(xbar100)
hist(xbar1000)
par(mfrow = c(3, 1))
qqnorm(xbar10)
qqline(xbar10)
qqnorm(xbar100)
qqline(xbar100)
qqnorm(xbar1000)
qqline(xbar1000)

####(f)####
mu = 10; sigma = 2; n = 100; nsim = 1000
low95 = rep(NA, nsim); upp95 = rep(NA, nsim)
low99 = rep(NA, nsim); upp99 = rep(NA, nsim)
margin.err95 = qnorm(1 - .05 / 2) * sqrt(sigma ^ 2 / n)
margin.err99 = qnorm(1 - .01 / 2) * sqrt(sigma ^ 2 / n)
for (i in 1:nsim) {
  x <- rnorm(n, mu, sigma)
  low95[i] = mean(x) - margin.err95
  upp95[i] = mean(x) + margin.err95
  low99[i] = mean(x) - margin.err99
  upp99[i] = mean(x) + margin.err99
  
  cat(low95[i],upp95[i],low99[i],upp99[i],"\n")
}
(average_ci95 = mean((low95 <= mu) & (mu <= upp95)))
(average_ci99 = mean((low99 <= mu) & (mu <= upp99)))