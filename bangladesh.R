a = 'bangladesh' # input
n = nchar(a) # size

temp1 = seq(1, n, by = 2) # a variable to control even-odd
indx = 1:n # index

# loop to exchange position 
for(i in temp1){
  temp2 = indx[i] # saving value in a temporary variable
  indx[i] = indx[i + 1]
  indx[i + 1] = temp2 
}

chars = c() # initialization

# loop to extract characters one by one
for(i in 1:n){
  chars[i] = substr(a, i, i) 
}

a_new = chars[indx] # exchanging characters
aa = paste(a_new, sep = '', collapse = "") # merging
aa # output

#### Happy Coding ####