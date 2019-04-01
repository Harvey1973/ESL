require(stats); require(graphics)
library(splines)
library(ggplot2)
OZ=read.table("/Users/harvey/Desktop/MS_cs/MS_spring_2019/MA751/Data_Assignment_2/ozone.data",sep="", header = TRUE)

train = OZ[,c(2,3,4)]


yTrain=OZ$ozone

yTrain = scale(yTrain,center = TRUE, scale = TRUE)
X=cbind(ns(train$radiation,4),ns(train$temperature,4),ns(train$wind,4))
X = scale(X,center = TRUE, scale = TRUE)
X
df_2=data.frame(yTrain,X)


regression = lm(yTrain~.,data = df_2)
B = cbind(1,X)
N = length(yTrain)
p = 4*3+1
#########################
#    Yhat, theta_hat    #
yhat = B%*%(solve(t(B)%*%B))%*%(t(B))%*%(yTrain)
theta_hat = (solve(t(B)%*%B))%*%(t(B))%*%(yTrain)
#########################
#        J              #
J = (solve(t(B)%*%B))%*%(t(B))

#########################

sigma_val = (((t(yTrain - yhat))%*%(yTrain - yhat))/(N-p-1))[1]
V_y = sigma_val * diag(N)
#########################
# Sigma_hat             #
Sigma_hat = (J%*%V_y)%*%(t(J))
#########################

#########################
#        Radiation      #
h_1 = B[,2:5]
f_1=h_1%*%theta_hat[2:5]
sigma_hat = Sigma_hat[2:5,2:5]
ycov=h_1%*%sigma_hat%*%t(h_1)
yvar=diag(ycov)
low=f_1-2*sqrt(yvar)
high=f_1+2*sqrt(yvar)
plot_df = as.data.frame(cbind(scale(train$radiation,center = TRUE,scale = TRUE),
                              f_1,high,low,yTrain))
ggplot() + geom_line(aes(V1,V2),plot_df)+ geom_line(aes(V1,V3),plot_df,color = "red")+
  geom_line(aes(V1,V4),plot_df,color = "red")+
  labs(y= "f(radiation)", x = "radiation") 

#########################
#        Temperture     #
h_2 = B[,6:9]
f_2=h_2%*%theta_hat[6:9]
sigma_hat = Sigma_hat[6:9,6:9]
ycov=h_2%*%sigma_hat%*%t(h_2)
yvar=diag(ycov)
low=f_2-2*sqrt(yvar)
high=f_2+2*sqrt(yvar)
plot_df = as.data.frame(cbind(scale(train$temperature,center = TRUE,scale = TRUE),
                              f_2,high,low,yTrain))
ggplot() + geom_line(aes(V1,V2),plot_df)+ geom_line(aes(V1,V3),plot_df,color = "red")+
  geom_line(aes(V1,V4),plot_df,color = "red")+
  labs(y= "f(temperature)", x = "temperature") 

#########################
#        Wind           #
h_3 = B[,10:13]
f_3=h_3%*%theta_hat[10:13]
sigma_hat = Sigma_hat[10:13,10:13]
ycov=h_3%*%sigma_hat%*%t(h_3)
yvar=diag(ycov)
low=f_3-2*sqrt(yvar)
high=f_3+2*sqrt(yvar)
plot_df = as.data.frame(cbind(scale(train$wind,center = TRUE,scale = TRUE),
                              f_3,high,low,yTrain))
ggplot() + geom_line(aes(V1,V2),plot_df)+ geom_line(aes(V1,V3),plot_df,color = "red")+
  geom_line(aes(V1,V4),plot_df,color = "red")+
  labs(y= "f(wind)", x = "wind") 






