library(ggplot2)
OZ=read.table("C:/Users/Harvey/Desktop/MA751/Data_Assignment_2/OZone.data",sep="", header = TRUE)

train = OZ$radiation
yTrain = OZ$ozone
N = length(train)
yTrain_sorted = OZ[order(OZ$radiation),]$ozone
xTrain_sorted = OZ[order(OZ$radiation),]$radiation

smooth_1 <- smooth.spline(train,yTrain,cv = FALSE,all.knots = TRUE)
smooth_1$lambda
x_value = smooth_1$x
fitted_y = smooth_1$y   # for distinct x values
yhat = predict(smooth.spline(train,yTrain,cv=FALSE,all.knots = TRUE),sort(train))$y

L = matrix(0,nrow = length(train), ncol = length(train))

for(j in 1:length(train)){
  yi = rep_len(0, length(train))
  yi[j] = 1
  L[,j] = predict(smooth.spline(sort(train), yi, lambda  = smooth_1$lambda, cv=FALSE,
                                all.knots = TRUE), sort(train))$y
}

###############################################
# Comparison check the y = Sy is equal to smooth_1$y  Note : both model returns the y sorted based on x
fromsm = L%*%(yTrain_sorted)

###############################################
# Variance computation
sigma = (t(yTrain_sorted - yhat)%*%(yTrain_sorted - yhat))/N
V_y = sigma[1]* diag(N)

###############################################
# Var(f)
var_f = (L%*%V_y)%*%t(L)
f_var = diag(var_f)
###############################################
# 90% confidence band z-score : 1.65
low = yhat - 1.65*sqrt(f_var)
high = yhat + 1.65*sqrt(f_var)


################################################

plot_df = as.data.frame(cbind(xTrain_sorted,
                              yhat,high,low,yTrain_sorted))

#plot_df

ggplot() + geom_point(aes(xTrain_sorted,yTrain_sorted),plot_df)+ geom_line(aes(xTrain_sorted,yhat),plot_df,color = "red")+
  geom_line(aes(xTrain_sorted,low),plot_df,color = "blue")+geom_line(aes(xTrain_sorted,high),plot_df,color = "blue")+
  labs(y= "ozone", x = "radiation") 




###################################################################

train = OZ$temperature
yTrain = OZ$ozone
N = length(train)
yTrain_sorted = OZ[order(OZ$temperature),]$ozone
xTrain_sorted = OZ[order(OZ$temperature),]$temperature

smooth_1 <- smooth.spline(train,yTrain,cv = FALSE,all.knots = TRUE)
smooth_1$lambda
x_value = smooth_1$x
fitted_y = smooth_1$y   # for distinct x values
yhat = predict(smooth.spline(train,yTrain,cv=FALSE,all.knots = TRUE),sort(train))$y

L = matrix(0,nrow = length(train), ncol = length(train))

for(j in 1:length(train)){
  yi = rep_len(0, length(train))
  yi[j] = 1
  L[,j] = predict(smooth.spline(sort(train), yi, lambda  = smooth_1$lambda, cv=FALSE,
                                all.knots = TRUE), sort(train))$y
}

###############################################
# Comparison check the y = Sy is equal to smooth_1$y  Note : both model returns the y sorted based on x
fromsm = L%*%(yTrain_sorted)

###############################################
# Variance computation
sigma = (t(yTrain_sorted - yhat)%*%(yTrain_sorted - yhat))/N
V_y = sigma[1]* diag(N)

###############################################
# Var(f)
var_f = (L%*%V_y)%*%t(L)
f_var = diag(var_f)
###############################################
# 90% confidence band z-score : 1.65
low = yhat - 1.65*sqrt(f_var)
high = yhat + 1.65*sqrt(f_var)


################################################

plot_df = as.data.frame(cbind(xTrain_sorted,
                              yhat,high,low,yTrain_sorted))

#plot_df

ggplot() + geom_point(aes(xTrain_sorted,yTrain_sorted),plot_df)+ geom_line(aes(xTrain_sorted,yhat),plot_df,color = "red")+
  geom_line(aes(xTrain_sorted,low),plot_df,color = "blue")+geom_line(aes(xTrain_sorted,high),plot_df,color = "blue")+
  labs(y= "ozone", x = "temperature") 


###################################################################


train = OZ$wind
yTrain = OZ$ozone
N = length(train)
yTrain_sorted = OZ[order(OZ$wind),]$ozone
xTrain_sorted = OZ[order(OZ$wind),]$wind

smooth_1 <- smooth.spline(train,yTrain,cv = FALSE,all.knots = TRUE)
smooth_1$lambda
x_value = smooth_1$x
fitted_y = smooth_1$y   # for distinct x values
yhat = predict(smooth.spline(train,yTrain,cv=FALSE,all.knots = TRUE),sort(train))$y

L = matrix(0,nrow = length(train), ncol = length(train))

for(j in 1:length(train)){
  yi = rep_len(0, length(train))
  yi[j] = 1
  L[,j] = predict(smooth.spline(sort(train), yi, lambda  = smooth_1$lambda, cv=FALSE,
                                all.knots = TRUE), sort(train))$y
}

###############################################
# Comparison check the y = Sy is equal to smooth_1$y  Note : both model returns the y sorted based on x
fromsm = L%*%(yTrain_sorted)

###############################################
# Variance computation
sigma = (t(yTrain_sorted - yhat)%*%(yTrain_sorted - yhat))/N
V_y = sigma[1]* diag(N)

###############################################
# Var(f)
var_f = (L%*%V_y)%*%t(L)
f_var = diag(var_f)
###############################################
# 90% confidence band z-score : 1.65
low = yhat - 1.65*sqrt(f_var)
high = yhat + 1.65*sqrt(f_var)


################################################

plot_df = as.data.frame(cbind(xTrain_sorted,
                              yhat,high,low,yTrain_sorted))

#plot_df

ggplot() + geom_point(aes(xTrain_sorted,yTrain_sorted),plot_df)+ geom_line(aes(xTrain_sorted,yhat),plot_df,color = "red")+
  geom_line(aes(xTrain_sorted,low),plot_df,color = "blue")+geom_line(aes(xTrain_sorted,high),plot_df,color = "blue")+
  labs(y= "ozone", x = "wind") 
