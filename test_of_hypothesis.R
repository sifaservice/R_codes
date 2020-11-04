#############################################################
#############################################################

# problem1
# (b)

mu = 7725 # given
d.intake = c(5260, 5470, 5640, 6180, 6392, 6515, 6805, 7515, 
	7515, 8230, 8770) # data

x.bar = mean(d.intake)
s = sd(d.intake)
n = length(d.intake)
t.stat = (x.bar - mu) / (s / sqrt(n))
t.stat # calculated t statistic


# (c)

df = n - 1 # degrees of freedom
# two tail
p.value = 2 * pt(t.stat, df) # pt means cdf of t distribution

# by using built in function
# one tail: right
t.test(x = d.intake, mu = 7725, alt = "greater")

# one tail: left
t.test(x = d.intake, mu = 7725, alt = "less")

# two tail
t.test(x = d.intake, mu = 7725)

#############################################################
#############################################################

# problem2
# data input

expend = c(9.21, 7.53, 7.48, 8.08, 8.09, 10.15, 8.40, 
	10.88, 6.13, 7.90, 11.51, 12.79, 7.05, 11.85, 
	9.97, 7.48, 8.79, 9.69, 9.68, 7.58, 9.19, 8.11)
stature = c("obese", rep("lean", 9), "obese", "obese", "lean",
	"obese", "obese", "lean", rep("obese", 3), "lean", 
	"obese", "lean")

# for unequal variances
exp.lean = expend[stature == "lean"] # data extraction
exp.obese = expend[stature == "obese"] # data extraction

x1.bar = mean(exp.lean)
x2.bar = mean(exp.obese)

var1 = var(exp.lean)
var2 = var(exp.obese)

n1 = length(exp.lean)
n2 = length(exp.obese)

numerator = x1.bar - x2.bar
denominator = sqrt(var1 / n1 + var2 / n2)

t.stat = numerator / denominator
t.stat # calculated t statistic

numer = (var1 / n1 + var2 / n2) ^ 2
denom = ((var1 / n1) ^ 2) / (n1 - 1) + (
	(var2 / n2) ^ 2) / (n2 - 1)
df = numer / denom
p.value = 2 * pt(t.stat, df)

# by using built in function
t.test(expend~stature)

# for equal variances
df = n1 + n2 - 2
s = sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / df)
t.stat = numerator / (s * sqrt(1 / n1 + 1 / n2))
p.value = 2 * pt(t.stat, df)

# by using built in function
t.test(expend~stature, var.equal = T)

#############################################################
#############################################################

# problem3
# (i) 
theta.null = 3.2
sigma = sqrt(.5)
crit.value = 4.2
crit.z = (crit.value - theta.null) / sigma
alpha = 1 - pnorm(crit.z)

# (ii)

power.f = function(crit.value, theta){
	z = (crit.value - theta) / sigma
	power = 1 - pnorm(z)
	return(power)
}

power.f(4.2, 4)

theta1 = seq(3, 6, length = 100)
power = power.f(4.2, theta1)
plot(power, type = "l", main = "Power Function", xlab = "theta")

#############################################################
#############################################################

# problem4
library(MASS)
contin.tabl = table(survey$Smoke, survey$Exer)  

chisq.test(contin.tabl)

#############################################################
#############################################################
	
# problem5

library(stats4) # loading library

h = c(115, 117, 120, 123, 126, 129, 132, 139, 
	142, 146, 150, 154, 159, 164, 135)
w = c(58:64, 66:72, 65) # data
n = length(w)
# function writing
loglik.H1 = function(b0, b1, sigma2){
	loglik = -n/2 * log(sigma2) - 1 / (2 * sigma2) * sum((w - b0 - b1 * h) ^ 2)
	return(-loglik)
}

# using mle() of stats4 library
est.H1 = mle(minuslog = loglik.H1, 
	start = list(b0 = 5, b1 = 5, sigma2 = 5))

summary(est.H1)

loglik.H0 = function(b0, sigma2){
	loglik = -n/2 * log(sigma2) -1 / (2 * sigma2) * sum((w - b0) ^ 2)
	return(-loglik)
}

est.H0 = mle(minuslog = loglik.H0, 
	start = list(b0 = 5, sigma2 = 5))

summary(est.H0)

chisq.LRT = -2 * (logLik(est.H0) - logLik(est.H1))
p.value = 1 - pchisq(chisq.LRT, df = 2)

#############################################################
#############################################################

# problem6
# equal variances
x1 = c(2.40, 1.50, 2.00, 1.90, 2.30, 2.90, 2.80, 2.60, 3.50, 
	4.50, 3.70, 5.50, 2.80, 6.50, 5.60)
n1 = length(x1)
x1.bar = mean(x1)

x2 = c(2.60, 1.50, 1.30, 2.40, 2.90, 3.00, 3.10, 3.90, 
	2.90, 3.90, 4.80)
n2 = length(x2)
x2.bar = mean(x2)

var1 = var(x1)
var2 = var(x2)

df = n1 + n2 - 2
s.sq = (((n1 - 1) * var1 + (n2 - 1) * var2) / df)
s.sq

t.stat = (x1.bar - x2.bar) / sqrt(s.sq * (1 / n1 + 1 / n2))
t.stat


p.value = 2 * pt(t.stat,  23.912, T)
p.value

# unequal variances
t.stat = (x1.bar - x2.bar) / sqrt(var1 / n1 + var2 / n2)
t.stat
numerator = (var1 / n1 + var2 / n2) ^ 2
denominator = (var2 / n1) ^ 2 / (n1 - 1) + (var2 / n2) ^ 2/ (n2 - 1)
df = numerator / denominator                  

p.value = 2 * pt(t.stat, df, T)
p.value

t.test(x1, x2)
#############################################################
#############################################################

# problem7
alpha = .1
beta = .1
A = (1 - beta) / alpha
B = beta / (1 - alpha)


x = c(73.23, 63.62, 57.45, 85.32, 87.44, 65.80, 71.03, 74.58, 
	75.60, 71.53, 69.27, 55.54, 70.04, 48.99, 87.11, 81.21,
	70.47, 65.78, 79.89, 94.43)

n = length(x)

cum.x = cumsum(x)
mu0 = 75
mu1 = 78
sigma2 = 100

low = c()
up = c()
x.bar = c()

for(m in 1:n){
	x.bar[m] = cum.x[m] / m
	
	temp1 = log(B) + m / (2 * sigma2) * (mu1 ^ 2 - mu0 ^ 2)
	temp2 = log(A) + m / (2 * sigma2) * (mu1 ^ 2 - mu0 ^ 2)
	temp3 = m / sigma2 * (mu1 - mu0)

	low[m] = temp1 / temp3
	up[m] = temp2 / temp3

	cat(low[m], x.bar[m], up[m], "\n")
}

##########################################################################
#for(m in 1:n){
	#x.bar[m] = cum.x[m] / m
	
	#temp1 = log(B) + m / (2 * sigma2) * (mu1 ^ 2 - mu0 ^ 2)
	#temp2 = log(A) + m / (2 * sigma2) * (mu1 ^ 2 - mu0 ^ 2)
	#temp3 = m / sigma2 * (mu1 - mu0)

	#low[m] = temp1 / temp3
	#up[m] = temp2 / temp3

	#cat(c(round(low[m], 2), "\t", round(x.bar[m], 2), "\t", round(up[m], 2), "\n"))

	#if(up[m] < x.bar[m] | low[m] > x.bar[m]){
		#break
	#}
#}

##########################################################################
####  OC function   ###
# under null,mu=mu0
OC.0=function(mu1,mu0,mu=mu0){
    h0=(mu1+mu0-2*mu)/(mu1-mu0)
    L0=(A**h0-1)/(A**h0-B**h0)
    return(L0)
}
oc.0=OC.0(78,75)
oc

# under alternative ,mu=mu1
OC.1=function(mu1,mu0,mu=mu1){
    h1=(mu1+mu0-2*mu)/(mu1-mu0)
    L1=(A**h1-1)/(A**h1-B**h1)
    return(L1)
}
oc.1=OC.1(78,75)
oc.1


#### ASN funtion ######
# Under null,mu=mu0
sigma=10
ASN.0=function(mu0){
    expct.z=(mu1-mu0)*(2*mu0-mu1-mu0)/(2*sigma^2)
    asn.0=(oc.0*log(B)+(1-oc.0)*log(A))/expct.z
    return(asn.0)
}
ASN.0(75)

# Under alternative,mu=mu1
sigma=10
ASN.1=function(mu1){
    expct.z=(mu1-mu0)*(2*mu1-mu1-mu0)/(2*sigma^2)
    asn.1=(oc.1*log(B)+(1-oc.1)*log(A))/expct.z
    return(asn.1)
}
ASN.1(78)
#############################################################
#############################################################

# problem8

x = c(58, 59, 67, 68, 70, 112, 74, 75, 76, 78, 80, 82, 83, 84, 86, 88, 
	90, 92, 93, 94, 97, 98, 104, 110)
z = (x - 85) / 15
ks.test(z, "pnorm")


#############################################################
#############################################################

# problem9
x = c(206, 238, 224, 257, 230)
y = c(236, 209, 287, 276, 252, 251)

ks.test(x, y)