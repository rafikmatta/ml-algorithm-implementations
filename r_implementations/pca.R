#calc means of each dimension
mean_v1 = mean(pca_data$V1)
mean_v2 = mean(pca_data$V2)

#calculation x[i]-m[i] for each dimension and make matrix out of it
normalized_mat = matrix(c(pca_data$V1-mean_v1,
         pca_data$V2-mean_v2),
       dim(pca_data)[1],dim(pca_data)[2])

#calculate covariance matrix
cov_mat = cov(normalized_mat)

#Calculation of eigen values using built in eigen function
#no need here to do our own eigen
eig <- eigen(cov_mat)

#verify with prcomp from R (principal components function)
prcomp(pca_data)

Z = c()
for (i in 1:dim(normalized_mat)[1]){
  temp_vals = t(eig$vectors) %*% normalized_mat[i:i,]
  Z = rbind(Z,c(temp_vals[1],temp_vals[2]))
}

plot(-1:2, -1:2, xlab = "x", ylab = "y",type = "n") 
points(pca_data$V1,pca_data$V2,col="red")
points(Z[,1],Z[,2],col="green")
