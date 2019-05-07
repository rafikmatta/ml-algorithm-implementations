density_func <- function(d,sigma_mat,mu,x)
{
  det_sigma = det(sigma_mat)
  factor1 <- ((2*pi)^(d/2)*det_sigma^(.5))
  
  final_result = c()
  for(i in 1:length(x[,1]))
  {
    xmut = c()
    for(j in 1:d)
    {
      xmut[j] = x[i,j] - mu[j]
    }
    factor2 <- xmut%*%ginv(sigma_mat)%*%matrix(xmut)
    factor3 <- exp((-.5)*factor2)
    final_result[i] = factor3/factor1
  }
  return (final_result)
}