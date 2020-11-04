library(MASS)
data = c(0, 0, .5, 0, 0, 0, .5, 0, 0, 0, 0, 1, 1, 1, 0, 0)

p1 = matrix(data, nrow = 4)
p1 = fractions(p1)

p2 = p1 %*% p1
p2 = fractions(p2)

p3 = p2 %*% p1
p3 = fractions(p3)

p4 = p3 %*% p1
p4 = fractions(p4)

#ii
reducible

#iii
periodic

#iv
period 3

#v
not ergodic


f1 = matrix(0, 4, 4)
f2 = matrix(0, 4, 4)
f3 = matrix(0, 4, 4)
f4 = matrix(0, 4, 4)

sumF = c()
mu = c()
for(i in 1:4){

  f1[i, i] = p1[i, i]
  f2[i, i] = p2[i, i] - f1[i, i] * p1[i, i]
  f3[i, i] = p3[i, i] - f1[i, i] * p2[i, i] - f2[i, i] * p1[i, i]
  f4[i, i] = p4[i, i] - f1[i, i] * p3[i, i] - f2[i, i] * p2[i, i] - f3[i, i] * p1[i, i]
  #f5[i, i] = p5[i, i] - f1[i, i] * p4[i, i] - f2[i, i] * p3[i, i] - f3[i, i] * p2[i, i] - f4[i, i] * p1[i, i]


  sumF[i] = f1[i, i] + f2[i, i] + f3[i, i] + f4[i, i]
  mu[i] = f1[i, i] + 2 * f2[i, i] + 3 * f3[i, i] + 4 * f4[i, i]
}
sumF
mu




# poission process
m1 = 4
t1 = 4


pnt = function(n){
	(exp(-m1 * t1) * (m1 * t1) ^ n) / factorial(n)
}

# (i) exactly 4
pnt(4)

# (ii) higher than 4

s = 0
for(i in 0:4){
	s = s + pnt(i)
}

1 - s
# alternative
1 - (pnt(0) + pnt(1) + pnt(2) + pnt(3) + pnt(4))

# less that 4

s = 0
for(i in 0:3){
	s = s + pnt(i)
}
