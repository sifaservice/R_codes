data = c(12, 32, 13, 45, 88, 54, 67, 69, 76, 76, 71, 56, 97, 91, 70, 93,
	83, 74, 57, 65, 56, 79, 64, 67, 37, 67, 81, 76, 43, 67, 85, 59, 55,
	54, 64, 73, 89, 65, 46, 78) # input data


# writing function
# NB: no need to provide any kind of comments, starting 
# with hash tag (#), in the exam hall
# srs_37 is the function name; reader may change it
# function name should be valid or ANSI recognized
# reader may use = as an alternative of <-

srs_37 = function(pop, n){

# SRSWOR with sample() function
# function name = srs_37
# parameters = pop, n
# first parameter: pop = population
# second parameter: n = sample size
# function using guide: srs_37(pop = MyData as vector form, n = sample size)
# example: srs_37(data, 13)

	# pop = na.omit(pop) # omitting NA values	
	N = length(pop) # pop size
	
	samp = sample(pop, n) # taking sample from pop
	x_bar = mean(samp) # sample mean = y_bar = estimator of pop mean

	# sampling variance = variance of sample mean
	f = n / N # sampling fraction
	s_sq = var(samp) # sample variance
	var_x_bar = (1 - f) * s_sq / n # sampling variance 
	se = sqrt(var_x_bar) # finding the standard error
	cv = se / x_bar # coefficient of variation

	# alpha = 1 - CI # CI = confidence interval
	# z_tab = qnorm(1 - alpha / 2)
	z_tab95 = 1.96 # tabulated value of std norm at 95% confidence interval
	
	margin_err = se * z_tab95 # margin of error of sample mean

	# finding the 95% confidence intervals
	lower = x_bar - margin_err # lower value of CI at 95%
	higher = x_bar + margin_err # higher value of CI at 95%

	result_list = list("Estimated population mean" = x_bar,
		"Estimated sampling variance" = var_x_bar,
		"Standard Error" = se,
		"Coefficient of Variation" = cv,
		"Margin of Error" = margin_err,
		"95% confidence interval: Lower" = lower,
		"95% confidence interval: Higher" = higher)

	return(result_list)		
}

# calling the function by it's name & passing the arguments to get the results
srs_37(pop = data, n = 13)


# reference
cat("Coded by\nSFT\n1711024137\nHappy Codding!\n")