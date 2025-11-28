## Exercise 3 


# 1. 
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
head(df)



#2

nll_lm <- function(data, par) {
  
  n <- nrow(data) 
  betas <- par[1:4] # beta 0, 1, 2, 3
  s2 <- par[5] # sigma
  y <- data$y
  X <- cbind(1, data$x1, data$x2, data$x3)
  mu <- X %*% betas
  res <- y - mu

  llik_yt <- sum(dnorm(x = res, mean = 0, sd = s2, log=TRUE))
  return(-llik_yt)
}


# 3
inits <- c(mean(df$y), 0.1, 0.1, 0.1, 0.1)

optim(par = inits, 
      fn = nll_lm, 
      data = df, 
      method = "L-BFGS-B", 
      lower = -Inf, 
      upper = Inf)

# 5
y <- df$y
x <- cbind(1, df$x1, df$x2, df$x3)

beta_hats <- solve(crossprod(x), crossprod(x, y))
beta_hats

#6

n <- nrow(df) # number of rows in df
p <- ncol(df) # number of parameters

RSS <- y - x %*% beta_hats  

sqrt(crossprod(RSS)/n) # RSS/n
sqrt(crossprod(RSS)/(n - p)) # RSS/(n-p)


#8

err_est <- optim(par = inits, 
      fn = nll_lm, 
      data = df, 
      method = "L-BFGS-B", 
      lower = -Inf, 
      upper = Inf, 
      hessian = TRUE)

cv_matrix <- solve(err_est$hessian)
sqrt(diag(cv_matrix))[1:4]


## Exercise 4 

fit <- lm(y ~., data= df)
fit$coefficients
fit$residuals
summary(fit)

