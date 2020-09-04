x <- 1:100
r <- runif(100, min=0, max=100)

lin_regression_params <- function(x,r,N)
{
 w1 = (sum(x*r)-mean(x)*mean(r)*N)/(sum(x*x)-N*mean(x)*mean(x))
 w0 = mean(r) - w1*mean(x)
 return(c(w0,w1))
}

calc_line <- function(params,samples)
{
  y<-params[2]*samples+params[1]
  return(y)
}

line_vals = calc_line(lin_regression_params(x,r,100),r)


plot(x,r)
points(x,line_vals,col="blue")
abline(lm(r~x), col="red")

