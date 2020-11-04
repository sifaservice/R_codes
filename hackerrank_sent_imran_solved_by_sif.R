# problem: https://www.hackerrank.com/challenges/ginorts/problem?fbclid=IwAR2vbDk4JqgKOyVoEXGjVo_zzV1DrNF3PpqXueEaMNJ7uqHtxTJ3f_as-hA

s = scan(what = "character") # scanning the string

nums = sort(utf8ToInt(s)) # character to ascii code
n = nchar(s) # finding the length of the string

# initialization of the vectors and iterative variables
j = 1; k = 1; l = 1 
nums_a = c(); nums_A = c(); nums_0 = c()


# loop for separation of small-capital letters and numbers
for(i in 1:n){
  # to extract the small letters
  if(nums[i] >= 97 & nums[i] <= 122) {
    nums_a[j] = nums[i]
    j = j + 1
  }
  
  # to extract the capital letters
  if(nums[i] >= 65 & nums[i] <= 90) {
    nums_A[k] = nums[i]
    k = k + 1
  }
  
  # to extract the numbers
  if(nums[i] >= 48 & nums[i] <= 57) {
    nums_0[l] = nums[i]
    l = l + 1
  }
}

new_nums = c(nums_a, nums_A, nums_0) # getting them a vector
print(intToUtf8(new_nums)) # getting back as string

# Happy Coding!
# sifat.stat