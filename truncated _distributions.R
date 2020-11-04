# truncated binomial 
x = 1:8
f = c(9, 16, 25, 42, 30, 11, 6, 2)

x.bar = weighted.mean(x, f)
n = length(x)
p = c()
p[1] = x.bar / n

a = function(p){
	x.bar*n/p - (n^2-x.bar*n)/(1-p) - (n^2*(1-p)^(n-1))/(1-(1-p)^n)
}

express.a = expression(x.bar*n/p - (n^2-x.bar*n)/(1-p) - (n^2*(1-p)^(n-1))/(1-(1-p)^n))
D(express.a, "p")

b = function(p){
	-(x.bar * n/p^2 + (n^2 - x.bar * n)/(1 - p)^2 - (n^2 * ((1 - 
    p)^((n - 1) - 1) * (n - 1))/(1 - (1 - p)^n) + (n^2 * (1 - 
    p)^(n - 1)) * ((1 - p)^(n - 1) * n)/(1 - (1 - p)^n)^2))
}


for(i in 1:20){
	p[i + 1] = p[i] - a(p[i])/b(p[i])
	cat(i, p[i], "\n")
}

expected.b = function(p) {
	-(n * p * n/p^2 + (n^2 - n * p * n)/(1 - p)^2 - (n^2 * ((1 - 
    p)^((n - 1) - 1) * (n - 1))/(1 - (1 - p)^n) + (n^2 * (1 - 
    p)^(n - 1)) * ((1 - p)^(n - 1) * n)/(1 - (1 - p)^n)^2))
}

var.p = 1/(-expected.b(p[20]))
se.p = sqrt(var.p)

lower.confi = p[1] - 1.96 * se.p
higher.confi = p[1] + 1.96 * se.p



# truncated poisson
x = 1:10
f = c(203, 383, 525, 532, 408, 273, 139, 49, 29, 14)
x.bar = weighted.mean(x, f)
lambda = c()
lambda[1] = x.bar

a = function(lambda){
	x.bar / lambda - 1 / (1 - exp(-lambda))
}

express.a = expression(x.bar / lambda - 1 / (1 - exp(-lambda)))
D(express.a, "lambda")

b = function(lambda){
	-(x.bar/lambda^2 - exp(-lambda)/(1 - exp(-lambda))^2)

}

for(i in 1:20){
	lambda[i + 1] = lambda[i] - a(lambda[i]) / b(lambda[i])
	cat(i, lambda[i], "\n")
}

expected.b = function(lambda){
	-(1/lambda - exp(-lambda)/(1 - exp(-lambda))^2)
}

var.lambda = 1 / (-expected.b(lambda[20]))
se.lambda = sqrt(var.lambda)

lower.confi = lambda[1] - 1.96 * se.lambda
higher.confi = lambda[1] + 1.96 * se.lambda

# MLE of the parameters of the weibull distribution 
x <- c(18.75, 17.6, 16.65, 6.55, 4.92, 6.88, 10.20, 
	18.21, 9.87, 8.91, 8.75)

parm = c()
weibull_loglik <- function(parm){
  n <- length(x)
  gamma <- parm[1]
  lambda <- parm[2]
  loglik <- sum(dweibull(x, shape = gamma, scale = lambda, log = TRUE))
  return(-loglik) # return negative log likelihood function
}

weibull <- nlm(weibull_loglik, p = c(1, 1), hessian = TRUE) 
#(-loglik) er  Non linear minimization 
# i.e optimization

weibull$estimate

