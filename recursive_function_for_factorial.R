# factorial with loop
fact <- function(a)
{
    mult <- 1
    
    if (a > 1)
        for (i in 1:a)
            mult <- mult * i
    return(mult)
}

fact(5)


# factorial with recursive function
fact1 <- function(a)
{
    mult <- 1
    if (a > 1)
        mult <- a * fact1(a - 1)
    return(mult)
}

fact1(10)