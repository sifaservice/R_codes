# NB: no need to provide any kind of comments, starting 
# with hash tag (#), in the exam hall

# sys.samp is the function name; reader may change it
# function name should be valid or ANSI recognized
# reader may use = as an alternative of <-

sys.samp <- function(pop, n){
    # n = sample size (a single value)
    # pop = population (a vector or factor or array, i.e., 
    # any kind of one dimensional data object of R)
    # pop and n are the passed arguments
    
    
	# writing function for systematic random sampling both 
	# linear and circular form
	# where we need to pass population and sample size
	# to find the systematic random sample of given size
	
	
	
	pop <- na.omit(pop) # omitting the NA values if present
	N <- length(pop) # finding the size of population
	
	# using this formulae: nk = N
	# round is for get integer number if results fractional number
	k = round(N / n) 
	i = sample(k, 1) # taking a number from 1 to k randomly
	
	# samp will contain our selected sample
	samp <- array() # one dimensional array declaration
	
	for (j in 1:n) {
	    # loop for getting sample
	    # j is the iterative variable for executing loop several times
	    
	    # for better understanding see the formula from book:
	    # Daroga Singh
	    # taking (i + (n - 1) k)th values
	    
		samp[j] <- pop[i + (j - 1) * k] 
		
		# if (i + (n - 1) k) excedes N, then the last element can't 
		# be found, then it will take value from first
		if (is.na(samp[j])){ # checking if circular systematic sampling
		
		    # get the sample from first
			samp[j] <- pop[(i + (j - 1) * k) - N] 
		}
	}
	
	sys.mean = mean(samp) # systematic sampling mean

	sw = 0 # initializing
	for(p in 1:k){
		for (j in 1:n) {	  
  
			samp[j] <- pop[p + (j - 1) * k] 
	
			if (is.na(samp[j])){ 
				# checking if circular systematic sampling
			   	# get the sample from first
				samp[j] <- pop[(p + (j - 1) * k) - N] 
			}
		}

		sw = sw + (mean(samp) - mean(pop)) ^ 2
	}

	# systematic sampling variance
	var.sys.mean = sw / N
	se = sqrt(var.sys.mean) # finding the standard error
	
	# alpha = 1 - CI # CI = confidence interval
	# z_tab = qnorm(1 - alpha / 2)
	z_tab95 = 1.96 # tabulated value of std norm at 95% confidence interval
	
	margin_err = se * z_tab95 # margin of error of sample mean

	# finding the 95% confidence intervals
	lower = sys.mean - margin_err # lower value of CI at 95%
	higher = sys.mean + margin_err # higher value of CI at 95%

	result_list = list("Systematic sample mean" 
		= sys.mean,
		"Estimated systematic sampling variance" = var.sys.mean,
		"Standard Error" = se,
		"Margin of Error" = margin_err,
		"95% confidence interval: Lower" = lower,
		"95% confidence interval: Higher" = higher)

	return(result_list)
}

# data will be given, for exercise I provide pop1
pop1 <- c(7192.5, 8836.5, 7398, 8014.5, 7809, 6576, 8220, 7192.5, 8425.5, 
          7603.5, 6370.5, 9247.5, 6987, 8425.5, 10069.5, 6165, 6165, 6576, 
          6987, 7295.25, 10069.5, 6987, 8425.5, 7809, 8425.5, 7192.5, 8425.5, 
          8220, 6987, 8425.5, 6987, 6576, 7603.5, 6165, 7192.5, 6987, 6165, 
          6987, 6370.5, 7398, 8014.5, 7603.5, 6987, 6576, 9247.5, 8220, 
          6987, 6165, 8014.5, 7603.5, 6165, 7398, 6165, 6165, 8425.5, 7603.5, 
          8014.5, 6987, 6165, 8220, 10069.5, 6165, 6987, 6165, 6987, 7603.5, 
          6165, 6576, 9247.5, 8220, 9247.5, 7398, 6987, 7603.5, 6165, 6987, 
          6987, 7295.25, 6370.5, 6987, 6987, 6165, 6987, 6987, 8425.5, 
          6370.5, 8220, 8425.5, 6576, 6987, 8220, 7192.5, 6165, 10069.5, 
          8220, 7192.5, 6165, 7809, 10069.5, 6987) # getting data


# calling the function by it's name to get the systematic sampling 
sys.samp(pop1, 15)

# reference
cat("Coded by\nSFT\n1711024137\nsifat.stat@gmail.com\nHappy Codding!\n")