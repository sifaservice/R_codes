cluster.samp = function(N, m.vec, y, total = T, M = NA) {
    
	# N = number of cluster in pop
	# M = number of elements in the pop
	# m.vec = vector of the cluster size in sample
	# y = either a vector of totals per cluster, or 
	# a list of the observations per cluster (this is set by total)

	n = length(m.vec) # finding the length

	# If M is unkhown, m.bar is estimated with the mean of m.vec
	if (is.na(M)) {
		m.bar = mean(m.vec)
	} else {
		m.bar = M / N
	}
	
	# If there are not totals of observations they are computed
	if(total == F) {
		y = unlist(lapply(y, sum))
	}

	mu.hat = sum(y) / sum(m.vec)
	s2.c = sum((y - (mu.hat * m.vec)) ** 2) / (n - 1)
	var.mu.hat = ((N - n) / (N * n * m.bar ** 2)) * s2.c
	B = 2 * sqrt(var.mu.hat)
	cbind(mu.hat, s2.c, var.mu.hat, B)
}

# example
m = c(55, 60, 63, 58, 71, 78, 69, 58, 52, 71, 73, 64, 69, 58, 63, 75, 78, 51, 67, 70)
y = c(2210, 2390, 2430, 2380, 2760, 3110, 2780, 2370, 1990, 2810, 2930, 2470, 2830, 2370, 2390, 2870, 3210, 2430, 2730, 2880)