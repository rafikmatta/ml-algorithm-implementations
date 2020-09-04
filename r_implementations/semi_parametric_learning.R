---
title: "Lab 8 Implementation- DS8002"
author: "Rafik Matta"
output:
  html_document: default
pdf_document: default
word_document: default
---
  
#same samples was what's in the book
samples = c(0.5,0.6,0.7,1.9,2.4,6.2,6.3,7.3)

#my data
x = seq(from=0,to=8,by=0.01)

gaussian_kernel_estimator <- function(x,xt,h)
{
   first_term = (length(xt)*h)
   normal_term = 1/sqrt(2*pi)
   base_term = ((x-xt)/h)
   actual_term = sum(exp(-(base_term^2)/2))
   k = normal_term * actual_term 
   final_result = k/first_term
   return(final_result)
}

#bin length 1
result_vector = vector()
for(i in 1:length(x))
{
  result_vector[i] = gaussian_kernel_estimator(x[i],samples,1)
}
plot(x,result_vector,type='l',main="bin = 1")


#bin length 0.5
result_vector = vector()
for(i in 1:length(x))
{
  result_vector[i] = gaussian_kernel_estimator(x[i],samples,0.5)
}
plot(x,result_vector,type='l',main="bin = 0.5")

#bin length 0.25
result_vector = vector()
for(i in 1:length(x))
{
  result_vector[i] = gaussian_kernel_estimator(x[i],samples,0.25)
}
plot(x,result_vector,type='l',main="bin = 0.25")

