##################################################################################
############################ Practical Session-III, Group-4 ######################
####################### Practical Problem-4, Logistic Regression #################
##################################################################################


# RStudio is the best preferable IDE for this R file

# writing function for fitting logit model on the given data
LogitMod4 = function(Xi, Ni, ni){
    
    
    Pi_hat = ni / Ni
    Li_hat = log(Pi_hat / (1 - Pi_hat))
    Wi = sqrt(Ni * Pi_hat * (1 - Pi_hat))
    Li_star = Wi * Li_hat # transformed Li_hat
    Xi_star = Wi * Xi # transformed Xi_hat
    
    n = length(Xi) # no. of observations
    
    # covariance between Li_star and Xi_star
    Cov_XL = sum(Li_star * Xi_star) / n - mean(Li_star) * mean(Xi_star) 
    # another approach
    # cov(Xi_star, Li_star)
    
    Var_X = sum(Xi_star ^ 2) / n - (mean(Xi_star)) ^ 2 # variance of Xi_star
    # another approach
    # var(Xi_star)
    
    # another approach
    # logit_model = lm(Li_star ~ Xi_star)
    
    # regression coefficient beta hat or slope of the fitted line
    b = Cov_XL / Var_X 
    # another approach
    # coef(logit_model)[2]
    
    # regression coefficient alpha hat or intercept of the fitted line
    a = mean(Li_star) - b * mean(Xi_star) 
    # another approach
    # coef(logit_model)[1]
    
    fit.Li_star = a + b * Xi_star # fitted value of the dependent variable
    # another approach
    # fitted.values(logit_model)
    
    res = Li_star - fit.Li_star # residuals
    # another approach
    # res = resid(logit_model)
    
    SSTot = sum((Li_star - mean(Li_star)) ^ 2) # sum of squares of total
    SSRes = sum(res ^ 2) # sum of squares of residuals
    SSReg = sum((fit.Li_star - mean(Li_star)) ^ 2) # sum of squares of regression
    # another approach 
    # SSReg = SSTot - SSRes
    
    # another approach
    # aov(logit_model)
    
    # coefficient of determination
    R_sq = 1 - SSRes / SSTot
    # another approach
    # R_sq = SSReg / SSTot
    
    p = 2 # no. of regression coefficients
    
    # mean sum of squares of residuals
    MSRes = SSRes / (n - p) # estimate of residual variance
    var_b = MSRes / (sum(Xi_star ^ 2) - ((sum(Xi_star)) ^ 2) / n)
    # another approach
    # var_b = MSRes / (n * Var_X)
    
    list.retults = list("Intercept (Alpha Hat)" = a, 
                 "Slope (Beta Hat)" = b,
                 "Coefficient of Determination (R Square)" = R_sq, 
                 "Mean Sum of Squares of Residuals (MSRes)"
                 = MSRes, "Variance of Beta Hat" = var_b)
    
    return(list.retults)
    
}

# writing function for fitting probit model on the given data
ProbitMod4 = function(Xi, Ni, ni){
    
    
    Pi_hat = ni / Ni

    Ii_hat = qnorm(Pi_hat)
    Ki = Ii_hat + 5
    n = length(Xi) # no. of observations
    
    # covariance between Ki and Xi
    Cov_XK = sum(Ki * Xi) / n - mean(Ki) * mean(Xi) 
    # another approach
    # cov(Xi, Ki)
    
    Var_X = sum(Xi ^ 2) / n - (mean(Xi)) ^ 2 # variance of Xi
    # another approach
    # var(Xi)
    
    # another approach
    # probit_model = lm(Ki ~ Xi)
    
    # regression coefficient beta hat or slope of the fitted line
    b = Cov_XK / Var_X 
    # another approach
    # coef(probit_model)[2]
    
    # regression coefficient alpha hat or intercept of the fitted line
    a = mean(Ki) - b * mean(Xi) 
    # another approach
    # coef(probit_model)[1]
    
    fit.Ki = a + b * Xi # fitted value of the dependent variable
    # another approach
    # fitted.values(probit_model)
    
    res = Ki - fit.Ki # residuals
    # another approach
    # res = resid(probit_model)
    
    SSTot = sum((Ki - mean(Ki)) ^ 2) # sum of squares of total
    SSRes = sum(res ^ 2) # sum of squares of residuals
    SSReg = sum((fit.Ki - mean(Ki)) ^ 2) # sum of squares of regression
    # another approach 
    # SSReg = SSTot - SSRes
    
    # another approach
    # aov(probit_model)
    
    # coefficient of determination
    R_sq = 1 - SSRes / SSTot
    # another approach
    # R_sq = SSReg / SSTot
    
    p = 2 # no. of regression coefficients
    
    # mean sum of squares of residuals
    MSRes = SSRes / (n - p) # estimate of residual variance
    var_b = MSRes / (sum(Xi ^ 2) - ((sum(Xi)) ^ 2) / n)
    # another approach
    # var_b = MSRes / (n * Var_X)
    
    list.retults = list("Intercept (Alpha Hat)" = a, 
                        "Slope (Beta Hat)" = b,
                        "Coefficient of Determination (R Square)" = R_sq, 
                        "Mean Sum of Squares of Residuals (MSRes)"
                        = MSRes, "Variance of Beta Hat" = var_b)
    
    return(list.retults)
}


# getting data into R environment
# df = read.csv(file = "group4_SAM_sir_practical_problem4_logit_model_probit_model.csv")
# another approach
# mannually select the csv file
df = read.csv(file.choose()) 

# extracting variables
X = df$X
N = df$N
n = df$n

# fitting logit model by using our user defined function LogitMod4()
Logit_Model = LogitMod4(X, N, n)

# fitting probit model by using our user defined function ProbitMod4()
Probit_Model = ProbitMod4(X, N, n)

# presenting the differences between two models
result_df = cbind(Logit_Model, Probit_Model)
print(result_df)

##################################################################################
################################### Happy Coding #################################
##################################################################################