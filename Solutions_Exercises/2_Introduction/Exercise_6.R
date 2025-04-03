n_sim <- 1000
cor_vector <- numeric(n_sim)
for(i in 1:n_sim){
 # Generate X from a symmetric t-distribution (df=3 for example)
 n <- 100
 X <- rt(n, df=3)  
 Y <- X^2  
 cor_vector[i] <- cor(X, Y)
}

cor_vector

plot(X,Y)

mean(cor_vector)
hist(cor_vector, breaks=50, 
     main="Histogram of correlation between X and Y", 
     xlab="Correlation", col="lightblue", border="black")

# X,Y are independent -> cor(X,Y) = 0
# the other way around does not hold in general

# IF X,Y are from a bivariate normal distribution:
# cor(X,Y) = 0 ->  X,Y are independent

