library("mvtnorm")
library("MASS")

#picking our centers 
x1 = 1
y1 = 2

x2 = 2
y2 = 3


#as this is a toy example, we can pick our mean/sd ahead of time since we 
#control the data
# in this case our centers are our means, and we choose the sd

x1_train = rnorm(20,x1,0.1)
y1_train = rnorm(20,y1,0.1)

x2_train = rnorm(20,x2,0.1)
y2_train = rnorm(20,y2,0.1)

x = c(x1_train,x2_train)
y = c(y1_train,y2_train)

r1 = c(rep(1,20),rep(0,20))
r2 = c(rep(0,20),rep(1,20))


#plot to see it visually
plot(0:4, 0:4, xlab = "x", ylab = "y",type = "n") 
points(x1,y1,col='red')
points(x2,y2,col='green')
points(x,y)

#Prepare the data
X = data.frame(x,y,r1,r2)

#shuffle
X_shuffled <- X[sample(nrow(X)),]

#split into train/test
X_train = X_shuffled[0:32,]
X_test = X_shuffled[33:40,]

#mean estimator function
mean_estimate <- function(x,class)
{
  class_list = x[class]
  m1 = sum(class_list*x$x)/sum(class_list)
  m2 = sum(class_list*x$y)/sum(class_list)
  return(c(m1,m2))
}

m1 = mean_estimate(X_train,'r1')
m2 = mean_estimate(X_train,'r2')

m_vec = rbind(m1,m2)

#covar matrix estimator function
sd_estimate <- function(x,m_vec,class_index,class)
{
  numerator = matrix(0,ncol=2,nrow=2)
  
  for(i in 1:dim(x)[1])
  {
    temp = x[i:i,1:2] - m_vec[(class_index-1):class_index,]
    v1 = c(temp$x,temp$y)
    v2 = t(v1)
    result = v1%*%v2
    numerator = numerator + result*x[class][i,1]
  }
  
  return(numerator/sum(x[class]))
}

sd_estimate1 = sd_estimate(X_train,m_vec,1,'r1')
sd_estimate2 = sd_estimate(X_train,m_vec,2,'r2')


#prior term calculated for each class
p_c1 = log(sum(X_train$r1)/dim(X_train)[1])
p_c2 = log(sum(X_train$r2)/dim(X_train)[1])

#discriminant function
discrim_func <- function(x,p_ci,sigma_mat,m_vec,class_index,d)
{

  first_term = (-d/2)*log(2*pi)
  second_term = 0.5*log(det(sigma_mat))
  print(second_term)
  temp = x[1:1,1:2] - m_vec[(class_index-1):class_index,]
  v1 = c(temp$x,temp$y)
  third_term = 0.5*(t(v1)%*%ginv(sigma_mat)%*%v1)
  print(third_term)
  return (p_ci - first_term-second_term-third_term)
}


#prediction task using discriminant functions as seen before
class_vec = c()
for (i in 1:dim(X_test)[1])
{
  discriminant_1 = discrim_func(X_test[i:i,],p_c1,sd_estimate1,m_vec,1,2)
  discriminant_2 = discrim_func(X_test[i:i,],p_c2,sd_estimate2,m_vec,2,2)
  if(discriminant_1 >discriminant_2){
    class_vec = c(class_vec,'r1')
  }
  else{
    class_vec = c(class_vec,'r2')
  }
    
}

#report on error (confusion matrix can work here too)
correct_count = 0
incorrect_count = 0
for(i in 1:length(class_vec))
{
  if(X_test[class_vec[i]][i:i,] == 1){
    correct_count = correct_count +1
  }
  else{
    incorrect_count = incorrect_count +1
  }
}


test = lda(X_train$r1~X_train$x+X_train$y)
