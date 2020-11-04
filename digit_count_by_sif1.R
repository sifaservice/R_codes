# https://www.urionlinejudge.com.br/judge/en/problems/view/1138

while(1){
  # input
  A = scan()
  B = scan()
  
  # loop control
  if(A == 0 & B == 0) break
  
  # a function to count the no. of digits
  digit_counter = function(N){
    if(N == 0) return(1) # log10 never accept 0
    else return(trunc(log10(N)) + 1)
  }
  
  # initialize
  vec1 = c()
  mat1 = matrix(0, nrow = length(A:B), ncol = 10)
  
  for(i in A:B){
    x = i # putting value on a temp variable x
    p = digit_counter(x) # no. of iteration of the following loop
    
    # loop for extracting the digits
    for(z in 1:p){ 
      n = digit_counter(x)
      m = trunc(x / (10^(n - 1)))
      vec1[z] = m
      x = x - m * 10^(n - 1)
    }
    
    a = table(vec1) # for controling the repeatation of the no.
    a_num_freq = as.numeric(a)
    a_num = as.numeric(rownames(a))
    mat1[i - A + 1, a_num + 1] = a_num_freq
  }
  print(colSums(mat1))
}