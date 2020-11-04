sys.samp <- function(pop, n)
{
    # writing function for systematic random sampling
    # where we need to pass population as two dimensional array and sample size
    # to find the systematic sample of given size
    
    # n = sample size
    # pop = population in two dimension

    ml = nrow(pop) # number of rows in the pop
    nk = ncol(pop) # number of columns in the pop
    
    
    N <- ml * nk # finding the size of population
    
    l = trunc(ml * (n / N)) 
    i = sample(1:l, 1) # taking a number from 1 to l randomly
    
    k = trunc(nk * (n / N)) 
    j = sample(k, 1);j # taking a number from 1 to k randomly
    
    
    samp <- matrix() # array declaration
    
    for (p in 1:ml) {
        for (q in 1:nk) { # loop for getting sample 
            if ((i + (p - 1) * l) <= ml & (j + (q - 1) * k) <= nk) 
                samp[p, q] <- pop[i + (p - 1) * l, j + (q - 1) * k] 
            }
                
            if (is.na(samp[p, q])) {
                    samp[p, q] <- pop[(i + (p - 1) * l) <= ml, 
                                      (j + (q - 1) * k) - nk] # get the sample from first
            }
            
            if ((i + (p - 1) * l) > ml) {
                samp[p, q] <- pop[(i + (p - 1) * l) - ml, q] # get the sample from first
            }
            
            if (j + (q - 1) * k > nk) {
                samp[p, q] <- pop[p, (j + (q - 1) * k) - nk] # get the sample from first
            }
        }
        
    }
    
    return(samp) # to get the sample 
}


pop = matrix(1:100, ncol = 10)
# calling the function to get the systematic sampling 
print(sys.samp(pop, n = 15))
