################################################
##########    Practical Session VI   ###########
##########           Group 2         ###########
##########  Kolmogorov-Smirnov Test  ###########
################################################  

set.seed(137) # setting seed
x = runif(30) # generating numbers from U(0, 1)
Fobs = sort(x) # arranging the random numbers

# writing function for emperical values
emp.uni = function(n){
  z = array() # declaring array
  for(i in 1:n) 
    z[i] = (i - 1) / n
  return(z)
}

Fexp = emp.uni(30) # getting emperical values
# applying the formula to get KS-statistic
Dn = max(abs(Fexp - Fobs))
print(Dn)

# using R built-in function from stats package
ks.test(x, "punif")

# graphical view of Kolmogorov-Smirnov Test
plot(Fobs, type = 's', lw = 3, col = 2)
lines(Fexp, type = 's', lw = 3, col = 3)
legend("bottomright", inset = .005, 
       lwd = 3, legend = c("Observed", 
       "Emperical"), col = 2:3)

################################################
#############     Happy Coding     #############
################################################