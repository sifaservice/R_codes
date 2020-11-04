# https://www.urionlinejudge.com.br/judge/en/problems/view/1138

# a function to count the no. of digits
digit_counter = function(N){
  if(N == 0) return(1) # log10 never accept 0
  else return(trunc(log10(N)) + 1)
}

# loop for extracting the digits
xtract_digit = function(x){
  p = digit_counter(x)
  vec1 = c()
  
  for(z in 1:p){ 
    n = digit_counter(x)
    m = trunc(x / (10^(n - 1)))
    vec1[z] = m
    x = x - m * 10^(n - 1)
  }
  return(vec1)
}

while(1){
  a_num_freq = rep(0, 10)
  # input
  A = scan()
  B = scan()
  
  # loop control
  if(A == 0 & B == 0) break
  
  # initialize
  mat1 = matrix(0, nrow = length(A:B), ncol = 10)
  
  for(i in A:B){
    vec2 = xtract_digit(i)
    
    a = table(vec2) # for controling the repeatation of the no.
    a_num = as.numeric(rownames(a))
    a_num_freq[a_num + 1] = a_num_freq[a_num + 1] + as.numeric(a)
  }
  print(a_num_freq)
}