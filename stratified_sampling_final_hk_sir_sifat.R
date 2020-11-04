# NB: no need to provide any kind of comments, starting 
# with hash tag (#), in the exam hall
# strata.samp is the function name; reader may change it
# function name should be valid or ANSI recognized
# reader may use = as an alternative of <-

strata.samp <- function(df, n)
{
	# df and n are passed arguments
	# df is the data frame having the k columns as strata and 
	# n is the sample size
	
	# writing function for the stratified sampling
	# where we need to pass the strata as data frame and 
	# sample size as arguments to 
	# find the population mean, srs mean, stratified mean,
	# variance of srs mean,
	# variance of stratified sampling mean,
	# standard error of stratified sampling mean and
	# confidence interval of stratified sampling mean 
	
	# finding the number of strata 
	k <- ncol(df)									
	
	st.size <- array() # making array of the strata sizes
	pop <- vector() # making a vector of all population
	
	for (i in 1:k)
	{
		# finding the length of ith stratum
		st.size[i] <- length(na.omit(df[, i])) 
		pop <- c(pop, na.omit(df[, i])) # merging the strata
	}
	
	pop.size <- length(pop) # population size
	
	samp.st.size <- array() # making array for sample strata sizes
	samp.mean.st <- array() # array of sample means of the strata
	var.samp.st <- array() # array of sample variances of  strata
	
	for (i in 1:k) {
	      # i is the iterative variable
		# finding sample size of ith stratum
		samp.st.size[i] <- round(n * st.size[i] / pop.size) 
		# taking sample from ith stratum & storing them to temp
		set.seed(0000)
		temp = sample(na.omit(df[, i]), samp.st.size[i]) 
		
		samp.mean.st[i] <- mean(temp) # sample mean of ith stratum
		var.samp.st[i] <- var(temp) # sample variance of ith stratum 
	}
	
	pop.mean <- mean(pop) # population mean
	
	set.seed(0000)
	srs <- sample(pop, n) # simple random sampling without replacement
	mean.srs <- mean(srs) # finding srs mean
	
	# finding the variance of srswor mean using formula
	var.srs.mean <- (1 - n / pop.size) * var(srs) / n
	
	# finding stratified mean
	final.samp.mean.st <- weighted.mean(x = samp.mean.st, w = samp.st.size)
	
	# finding the variance of stratified sampling mean
	# proportions of the sizes of strata and population size
	w <- st.size / pop.size 
	# proportions of sample unit and population unit under strata
	f <- samp.st.size / st.size
	
	# initialization because we need to 
	# increment the variable var.st.mean
	var.st.mean <- 0 
	
	# loop for finding the variance of stratified 
	# sampling mean to maintain the iterative sum
	# i is iterative variable
	
	for (i in 1:k) 
	{ 
		# formula to find the variance of
		# stratified sampling mean
		
		# passing iterative integer variable i
		# within square brackets
		# which lies between 1 to k
		# to index the arrays
		
		var.st.mean <- var.st.mean + 
			w[i] ^ 2 * (1 - f[i]) * var.samp.st[i] / samp.st.size[i]
	}
	
	se.st.mean <- sqrt(var.st.mean) # standard error
	margin.err.st.mean <- 1.96 * se.st.mean # margin of error
	
	# lower value at 95% confidence interval
	lower.confi <- final.samp.mean.st - margin.err.st.mean
	
	# higher value at 95% confidence interval
	higher.confi <- final.samp.mean.st + margin.err.st.mean
	
	# 95% confidence interval
	confi.interv <- list("at 95% Lower" = lower.confi, 
						 "at 95% Higher" = higher.confi)
	# precision
	precision = (var.srs.mean - var.st.mean) / var.st.mean

	# making the list of all expected answers
	list.results <- list("Population Mean" = pop.mean, 
						 "SRS Mean" = mean.srs, 
						 "Variance of SRS Mean" = var.srs.mean,
						 "Stratified Sampling Mean" = final.samp.mean.st, 
						 "Variance of Stratified Sampling Mean" = 
						 	var.st.mean,
						 "Standard Error of Stratified Sampling Mean" = 
						 	se.st.mean, 
					     "Confidence Interval of Stratified Sampling Mean" = 
						 	confi.interv,
			"Relative Precision due to stratification over SRS"
						 = precision)
	
	# returning the result as list 
	return(list.results)
}

# reading the given csv data as data frame
dat <- read.csv(file.choose())
# dat <- read.csv(file = "hk_sir_stratified_data_25_03_2019.csv")
# dat # printing the data frame

# calling the function by it's name & passing the arguments
strata.samp(df = dat, n = 10)

# reference
cat("Coded by\nSFT\n1711024137\nHappy Codding!\n")