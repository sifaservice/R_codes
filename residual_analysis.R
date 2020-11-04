############ Practical_Problem0001 ########
# The amount of time required by the route 
# driver in selling soft drinks:
########  Fitting model  ########


# entering data into R environment
data = read.csv(file.choose())
data

### here y = delivery time (minutes)
### here x = distance (feet)

y = data$y # variable extraction
x = data$x

model1 = lm(y ~ x) # fitting model
model1
res = resid(model1) # getting residuals

summary(model1) # getting model summary
anova(model1) # analysis of variance


####02
plot(x, y) # Scatter Diagram
abline(model1) # getting the regression line

######### 03

h = hat(x) # getting the hat matrix
n = length(x) # getting the number of observation

p = 2 # no. of parameters
l = 2 * p / n # twice mean rule
df1 = data.frame(x, y, res, h) # making a data frame
hlv = h[h>l] # getting the leverage value
hlv ## high leverage value
df1[h > l,] # getting the rows or corresponding data

# standardized residuals
### error df is (n-p)

abs.res = abs(res) # getting absolute errors
SSRes = sum(res ^ 2) # sum of squares of residuals
MSRes = SSRes / (n - p) # mean sum of squares of residuals

di = abs.res /sqrt(MSRes) # standardized residuals

out = di[di > 2]
df2 = data.frame(x, y, res, di)
df2[di>2, ]

##### studentized residuals

ri = abs.res / sqrt(MSRes *(1-h))
outl = ri[ri > 2]
df3 = data.frame(x, y, res, ri, di)
df3[ri > 2, ]
####### Cook's Distance

CDi = h * ri ^ 2 / (p * (1 - h))
IO = CDi[CDi > 1]
df4 = data.frame(x, y, res, ri, di, CDi)
df4[CDi > 1, ]

######################