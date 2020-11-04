# SRSWOR with sample() function

data = c(12, 32, 13, 45, 88, 54, 67, 69, 76, 76, 71, 56, 97, 91, 70, 93,
	83, 74, 57, 65, 56, 79, 64, 67, 37, 67, 81, 76, 43, 67, 85, 59, 55,
	54, 64, 73, 89, 65, 46, 78) # input data

N = length(data) # pop size
n = 13
samp = sample(data, n)

# (i)
# sample mean = y_bar = estimate of pop mean
x_bar = mean(samp)
x_bar

# (ii)
# sampling variance = variance of sample mean
f = n / N # sampling fraction
s_sq = var(samp) # sample variance

var_y_bar = (1 - f) * s_sq / n

# (iii)
# finding the standard error

se = sqrt(var_y_bar)

# (iv)
# coefficient of variation
cv = se / x_bar

# (v)
z_tab95 = 1.96 # tabulated value of std norm at 95% confidence interval

margin_err = se * z_tab95

# (vi)
# finding the 95% confidence intervals
lower = x_bar - margin_err # lower value of CI at 95%
higher = x_bar + margin_err # higher value of CI at 95%