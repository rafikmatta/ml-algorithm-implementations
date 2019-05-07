likeli <- function(x,mean,sd){
  (1/(sqrt(2*pi)*sd))*(exp(-((x-mean)^2)/(2*sd^2)))
} 

posterior1 <- function(x,m1,m2,sd1,sd2,r1,r2){
  ((likeli(x,m1,sd1)*sum(r1))/(likeli(x,m1,sd1)*sum(r1)+likeli(x,m2,sd2)*sum(r2)))
}

posterior2 <- function(x,m1,m2,sd1,sd2,r1,r2){
  ((likeli(x,m2,sd2)*sum(r2))/(likeli(x,m1,sd1)*sum(r1)+likeli(x,m2,sd2)*sum(r2)))
}

x1 = rnorm(1000,mean=-4,sd=4)
x2 = rnorm(800,mean=4,sd=2)

r1 = rep(1,length(x1))
r2 = rep(1,length(x2))

N=length(x1)+length(x2)

mean1 = sum(x1*r1)/sum(r1)
mean2 = sum(x2*r2)/sum(r2)

var1 = sum((x1-mean1)^2*r1)/sum(r1)
std_dev1 = sqrt(var1)
var2 = sum((x2-mean2)^2*r2)/sum(r2)
std_dev2 = sqrt(var2)

likeli1 <- likeli(x1,mean1,std_dev1)
likeli2 <- likeli(x2,mean2,std_dev2)

plot(x2,likeli2,xlim=c(min(x1),max(x1)),ylim=c(0,max(likeli2)),col="red")
points(x1,likeli1,col="blue")

priors1 = sum(r1)/N
priors2 = sum(r2)/N

discrim1 = -log(std_dev1)-((x1-mean1)^2)/(2*var1)+log(priors1)
discrim2 = -log(std_dev2)-((x2-mean2)^2)/(2*var2)+log(priors2)

post1<- posterior1(x1,mean1,mean2,std_dev1,std_dev2,r1,r2)
post2<- posterior2(x2,mean1,mean2,std_dev1,std_dev2,r1,r2)

plot(x1,post1,xlim=c(min(x1),max(x1)),col="red")
points(x2,post2,col="blue")


