
# loading package
library(sampling)

# generating data as vectors
x1 <- rnorm(50, mean = 10, sd = 1)
print(x1)
x2 <- rnorm(50, mean = 20, sd = 1)
print(x2)
x3 <- rnorm(50, mean = 30, sd = 1)
print(x3)

# making all strata as a simple vector
dat <- c(x1, x2, x3)
print(dat)

# creating label as vector
lab <- c(rep(1, 50), rep(2, 50), rep(3, 50))
print(lab)

# creating data frame of data vector and lable vector
dat_lab <- cbind(dat, lab)
# setting collumn names of the data frame
colnames(dat_lab) <- c("Index", "Group")
print(dat_lab)

# stratified sampling 
# with method simple random sampling without replacement
sampl_dat_lab = strata(dat_lab, "Group", size =c(10,10,10),
    method = "srswor")
print(sampl_dat_lab)

# extracting indices of the sample units
sampl_indx <- sampl_dat_lab[ ,2]
print(sampl_indx)

# extracting the sample units and it will be final sampling
final_sampl <- dat[sampl_indx]
print(final_sampl)

# finding sample mean
x_bar <- mean(final_sampl)
print(x_bar)

# finding sample variance
sampl_var <- var(final_sampl)
print(sampl_var)

# finding sample standard deviation
sampl_sd <- sd(final_sampl)
print(sampl_sd)

# coded by
# Sifat Hossain
# 1711024137
# Thank you and happy coding!
