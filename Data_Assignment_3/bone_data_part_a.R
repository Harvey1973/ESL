# Load dataset 
bone_data=read.table("C:/Users/Harvey/Desktop/MA751/Data_Assignment_3/bone.data",sep="", header = TRUE)

# load training examples (predictor :age)
age = bone_data$age
# load responese varaibles (spnbmd)
bmd = bone_data$spnbmd
N = length(age)
# Scale predictor and response variables
#train = scale(age,center = TRUE, scale = TRUE)
#yTrain = scale(bmd,center = TRUE, scale = TRUE)
#yTrain
train = age
yTrain = bmd
yTrain_sorted = bone_data[order(bone_data$age),]$spnbmd
xTrain_sorted = bone_data[order(bone_data$age),]$age
##############################################
smooth_1 <- smooth.spline(train,yTrain,cv = FALSE,all.knots = TRUE)
smooth_1$lambda
x_value = smooth_1$x
fitted_y = smooth_1$y   # for distinct x values
yhat = predict(smooth.spline(train,yTrain,cv=FALSE,all.knots = TRUE),sort(train))$y

##############################################
# smoothing_matrix calculation

L = matrix(0,nrow = length(train), ncol = length(train))

for(j in 1:length(train)){
  yi = rep_len(0, length(train))
  yi[j] = 1
  L[,j] = predict(smooth.spline(train, yi, lambda  = smooth_1$lambda, cv=FALSE,
                                all.knots = TRUE), sort(age))$y
}

###############################################
# Comparison check the y = Sy is equal to smooth_1$y  Note : both model returns the y sorted based on x
fromsm = L%*%(yTrain_sorted)
#fromsm_2 = L%*%(yTrain)
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

ggplot() + geom_line(aes(xTrain_sorted,yTrain_sorted),plot_df)+ geom_line(aes(xTrain_sorted,yhat),plot_df,color = "red")+
  geom_line(aes(xTrain_sorted,low),plot_df,color = "blue")+geom_line(aes(xTrain_sorted,high),plot_df,color = "blue")+
  labs(y= "spnbmd", x = "age") 


#################################################
# Part b 

H = ns(train,knots = sort(train)[1:484])
df_2=data.frame(yTrain,H)
regression = lm(yTrain~.,data = df_2)
yhat = predict(regression,data.frame(cbind(1,H)))
sigma = (t(yTrain - yhat)%*%(yTrain - yhat))/N
tao = 0.000939
prior_sigma = 1*diag(N)
posterior_mean = H%*%(solve(t(H)%*%(H) + 
                    (sigma[1]/tao)*solve(prior_sigma)))%*%t(H)%*%yTrain
posterior_cov = H%*%(solve(t(H)%*%(H) + 
                    (sigma[1]/tao)*solve(prior_sigma)))%*%t(H)*sigma[1]
################################################
################################################
low = posterior_mean - 1.65*sqrt(diag(posterior_cov))
high = posterior_mean + 1.65*sqrt(diag(posterior_cov))
plot_df = as.data.frame(cbind(train,
                              posterior_mean,high,low,yTrain))

ggplot() + geom_line(aes(train,yTrain),plot_df)+ geom_line(aes(train,V2),plot_df,color = "red")+
  geom_line(aes(train,V3),plot_df,color = "blue")+geom_line(aes(train,V4),plot_df,color = "blue")+
  labs(y= "spnbmd", x = "age") 






################################################
# part b for knots smaller than   N

df = 50
H_2 = ns(train,df = df)

df_2=data.frame(yTrain,H_2)
regression = lm(yTrain~.,data = df_2)
yhat = predict(regression,data.frame(cbind(1,H_2)))
sigma = (t(yTrain - yhat)%*%(yTrain - yhat))/N
tao_2 = 1
prior_sigma_2 = 1*diag(df)
posterior_mean_2 = H_2%*%(solve(t(H_2)%*%(H_2) + 
                (sigma[1]/tao_2)*solve(prior_sigma_2)))%*%t(H_2)%*%yTrain
posterior_cov_2 = H_2%*%(solve(t(H_2)%*%(H_2) + 
                    (sigma[1]/tao_2)*solve(prior_sigma_2)))%*%t(H_2)*sigma[1]
################################################
low = posterior_mean_2 - 1.65*sqrt(diag(posterior_cov_2))
high = posterior_mean_2 + 1.65*sqrt(diag(posterior_cov_2))
plot_df = as.data.frame(cbind(train,
                              posterior_mean_2,high,low,yTrain))

ggplot() + geom_line(aes(train,yTrain),plot_df)+ geom_line(aes(train,V2),plot_df,color = "red")+
  geom_line(aes(train,V3),plot_df,color = "blue")+geom_line(aes(train,V4),plot_df,color = "blue")+
  labs(y= "spnbmd", x = "age") 



####################################################
# part c
# Step 1 sample with replacement
x_y_data = cbind(train,yTrain)

ind = c(1:N)
B = c(1:10)
gg = ggplot()
for (i in B){
  sample_ind = sample(ind,replace = TRUE)
  boot_strap_data = x_y_data[sample_ind,]
  x_train = boot_strap_data[,1]
  y_train = boot_strap_data[,2]
  smooth_boot <- smooth.spline(x_train,y_train,cv = FALSE,all.knots = TRUE)
  yhat_boot = predict(smooth_boot,sort(x_train))$y
  plot_df = as.data.frame(cbind(sort(x_train),sort(y_train),yhat_boot))
  gg =  gg + geom_line(aes(sort(x_train),yhat_boot),plot_df,color = "red")
  
}
gg + geom_line(aes(xTrain_sorted,yTrain_sorted),plot_df)

