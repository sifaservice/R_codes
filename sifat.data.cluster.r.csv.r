dat <- read.csv(file.choose())
dat1 <- as.matrix(dat)

## finding mean
M <- 4
N <- 103
n <- 15

mean.clust <- array()
for(i in 1:n){
	mean.clust[i] <- mean(dat1[i,])
}

mean.clust <- mean(mean.clust)
final.clust.samp.mean <- mean(mean.clust)

## finding variance
temp <- 0
for(i in 1:n){
	temp <- temp + (mean.clust[i] - final.clust.samp.mean)^2
}


var.clust.final <- (1 / n - 1 / N) * (1 / (n-1)) * temp

