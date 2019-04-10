##########
# Generate 100 observations ,each with 10 predictors and standard gaussian values
##########
N = 100
M = 10
X = matrix(rnorm(N*M,mean=0,sd=1), N, M) 

##########
# Generate response variables with sigma^2 = 1
response = rnorm(N,mean = 0, sd = 1)

##########
#Combine into data fram
##########
data = as.data.frame(cbind(X,response))
#########
# 1 terminal node
#########


