## operators
5 + 7 # addition
5 * 5 # multiplication
7 - 9 # subtraction
22 / 7 # division
5 %% 2 # modulus 
5 %/% 2 # integer division
n <- 2.2 # assigment operator
n = 2.2 # assignment operator
5 ** 5 # power by double asteriks
5 ^ 5 # power by caret
pi

## mathematical functions
abs(x) # absolute value 
sqrt(x) # square root 
ceiling(x) # ceiling(3.475) is 4 
floor(x) # floor(3.475) is 3 
trunc(x) # trunc(5.99) is 5 
round(x, digits=n) # round(3.475, digits=2) is 3.48 
signif(x, digits=n) # signif(3.475, digits=2) is 3.5 
cos(x) # trigonometrical cosine function
sin(x) # trigonometrical sine function
tan(x) # trigonometrical tan function
log(x) # natural logarithm
log10(x) # 10 base logarithm
exp(x) # exponential # (e^x)


## comparison Operators
5 > 6 # greater than
v1 <- c(1, 2, 3) # cocatenation of vector
v2 <- c(10, 20, 30) # cocatenation of vector
v1 < v2 # checking less than or not
6 >= 6 # checking greater than or not
5 != 5 # checking not equal or equal 
5 == 5 # checking equality
v <- c(1, 2, 3, 4, 5)
v < 2 # checking the all the elements of vector are less than 2 or not


## R data types
n <- 2.2 # assigment operator
n = 2.2 # assignment operator
t <- TRUE # boolean variable
char <- "Hello World!" # character with double quote
c <- 'Single Quote Char' # character with single quote
class(t) # know the class of R objects
class(char) # know the class of R objects
class(c) # know the class of R objects
class(n) # know the class of R objects


## vector
x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1) # concatenation
y <- 1:10 # natural sequence
z <- seq(from = 0, to = 10, by = .3) # sequence by increment
z1 <- seq(from = 0, to = 10, length = 200) # sequence of expected length 


## functions with vectors
sum(x) # sum
cumsum(x) # cumulative sum of the elements of x
length(x) # finding the number of element of x
v <- c(12, 45, 100, 2) # vector
sd(v) # standard deviation
var(v) # variance
max(v) # maximum element
min(v) # minimum element
prod(v1) # product of elements
prod(v2) # product of elements


## indexing vector by names
v <- 1:10 # vector
names(v) <- c('a','b','c','d') # setting names to access the elements
names(v) <- letters[1:4] # setting names to access the elements
v['b'] # accessing the element having name 'b'
v[c('c', 'a')] # accessing the element having name 'c' & 'a'
temps <- c(72, 71, 68, 73, 69, 75, 71) # vector
names(temps) <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun') # setting name
temps # printing temps
days <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
temps2 <- 1:7 # vector of numbers 1 through 7
names(temps2) <- days  # setting name
temps2 # printing temps2


## creating data frames
days <- c('mon','tue','wed','thu','fri') # vectors of days
temp <- c(22.2, 21, 23, 24.3, 25) # vector
rain <- c(TRUE, TRUE, FALSE, FALSE, TRUE) # creating logical vector
df <- data.frame(days, temp, rain ) # pass the vectors
str(df) # check structure
summary(df) # to calculate the summary of the data frame
df[1,] #  everything from first row by indexing data frame
df[5,] # grab Friday data
df$days # extracting days vector from data frame
empty <- data.frame() # empty data frame
c1 <- 1:10 # vector of integers
c2 <- letters[1:10] # vector of strings
df <- data.frame(col.name.1 = c1, col.name.2 = c2) # data frame
nrow(df) #rows and columns counting
ncol(df) # number of columns in an object
colnames(df) # column names
rownames(df) # row names


## accessing data frames
data_doctors1 <- data.frame(
    name = c("Maeen", "GR", "Yasin", "Mahmudul", "Sefatullah"),
    nationality = c("Japanese", "Australian", "Somali", 
                    "American","Austrian"),
    having_AIDS = c(rep("no", 4), "yes")) # creating data frame
data_doctors2 <- data.frame(
    surname = c("Molla", "Bayezid", "Molla", "Akash", "Sefuda"),
    expertised_area = c("Experimental Design", "Bioinformatics", 
                        "Demography", "Regression Diagnostics", 
                        "Simulation and Modeling"),
    having_papers = c(T, T, F, T, F)) # creating data frame
final_data_doctors = data.frame(data_doctors1[, 1], data_doctors2[, 1], 
                  data_doctors1[, -1], data_doctors2[, -1]) # accessing


## factor ## categorical
grade  <- c('B+','A-','A+','A-','A-', 'B+', 'B-', 
            'A-', 'A+', 'A-') # vector of characters
id <- 1:10 # vector
fact_id <- factor(id) # passing vector as factor
fact_id1 <- as.factor(id) # passing vector as factor
fact_grade <- factor(grade) # converting as factor
order_grade <- c('A+','A-','B+', 'B-') # generate an order
fact_grade2 <- factor(grade, ordered = TRUE, 
                      levels = order_grade) # converting factor by ordering
summary(fact_grade)  # frequency distrubution table
summary(fact_grade2)# frequency distrubution table


## matrix arithmetic
mat <- matrix(1:50, byrow = T, nrow = 5) # creating matrix
dim(mat) # to know the dimension of an array, matrix or data frame
mat1 <- array(1:50, dim = c(3, 2)) # two dimensional array is matrix
mat1 # print
2*mat # scalar multiplication
1/mat # division (reciprocal)
mat/2 # division
mat ^ 2 # power
mat > 17 # checking all the elements of matrix greater than 17
mat + mat # addition
mat / mat # division corresponding elements by corresponding elements
mat ^ mat # power corresponding elements by corresponding elements
mat*mat # multiplication corresponding elements by corresponding elements
mat2 <- matrix(c(12, 15, -8, rep(3,3), 7:9), 
               nrow = 3, ncol = 3, byrow = T) # creating 3X3 matrix
mat2 %*% mat2 # matrix multiplication, if satisfies the condition
solve(mat2)  # inverse of matrix, if satisfies the condition
det(mat2) # finding determinant
var1 <- c(450,451,452,445,468) # vector
var2 <- c(230,231,232,236,228) # vector
stocks <- c(var1, var2) # concatenating two vectors
stock_matrix <- matrix(stocks, byrow = T, nrow = 2) # creating matrix
day <- c('Mon','Tue','Wed','Thu','Fri') # character vector of days
product_name <- c('product1','product2') 
colnames(stock_matrix) <- day # setting column names
rownames(stock_matrix) <- product_name # setting row names
stock_matrix # printing matrix
colSums(stock_matrix) # sum of the columns
rowSums(stock_matrix) # sum of all the elements of a row
rowMeans(stock_matrix) # mean of all the elements of a row