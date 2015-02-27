
#n<-rbinom(1000, size=10, prob=0.5)
iid<-rnorm(1000, mean = 0, sd = 1)
iid
hist(iid)
plot(iid)
result = t.test(iid,mu = 0)
result$statistic
reps = 1000
result=array(dim = reps)
for(i in 1:reps){
  rdm<-sample(1:1000,1)
  x = rnorm(rdm,0,1)
  result[i]=t.test(x,mu=0)$statistic
}
hist(x, prob = TRUE)
x<-rchisq(1000,1000)
curve(dchisq(x,df = 1000), col = 'red',add = TRUE)
lines(dchisq(result,100))
result.sp[i]=chisq.test()


#Q2)
par(mfrow=c(2,2))

#Used n = 1

n = 1000
x = matrix(0,1,n)

for(i in 1:n){
  #rdm<-sample(1:10,1)
  x[i] = sum(rnorm(1,0,1)^2)
}
hist(x, prob = TRUE,ylim =range(c(0,1)), main="Histogram of Chi-squared and density plot for n = 1")
a = seq(from = 0, to = 20,length.out = 100)
lines(a,dchisq(a,1), type = 'l')


#Used n = 2

n = 1000
x = matrix(0,1,n)

for(i in 1:n){
  #rdm<-sample(1:10,1)
  x[i] = sum(rnorm(2,0,1)^2)
}
hist(x, prob = TRUE,ylim =range(c(0,0.5)), main="Histogram of Chi-squared and density plot for n = 2")
a = seq(from = 0, to = 20,length.out = 100)
lines(a,dchisq(a,2), type = 'l')

#Used n = 5
n = 1000
x = matrix(0,1,n)

for(i in 1:n){
  #rdm<-sample(1:10,1)
  x[i] = sum(rnorm(5,0,1)^2)
}
hist(x, prob = TRUE,ylim =range(c(0,0.25)), main="Histogram of Chi-squared and density plot for n = 5")
a = seq(from = 0, to = 20,length.out = 100)
lines(a,dchisq(a,5), type = 'l')

#used n = 10
n = 1000
x = matrix(0,1,n)

for(i in 1:n){
  #rdm<-sample(1:10,1)
  x[i] = sum(rnorm(10,0,1)^2)
}
hist(x, prob = TRUE,ylim =range(c(0,0.25)), main="Histogram of Chi-squared and density plot for n = 10")
a = seq(from = 0, to = 20,length.out = 100)
lines(a,dchisq(a,10), type = 'l')


#Q3 Part b)
ghat<- function(x){
  a <- mean(x)*var(x)
  a<- sqrt(a)
  return(a)
}

x1 = matrix(0,1,100)
x2 = matrix(0,1,100)
x3 = matrix(0,1,100)

for(i in 1:100){
  y = rpois(100,lambda = 1)
  x1[i] = mean(y)
  x2[i] = var(y)
  x3[i] = ghat(y)
}
par(mfrow=c(1,3))
hist(x1, breaks = 10, main="Mean(x)")
hist(x2, breaks = 10, main="Var(x)")
hist(x3, breaks = 10, main="sqrt(var(x)*mean(x))")

##Q3 Part c)
##bias for x1
mean(x1)-1
##variance for x1
var(as.numeric(x1))
##MSE for x1
(mean(x1)-1)^2+var(as.numeric(x1))

##bias for x2
mean(x2)-1
##variance for x2
var(as.numeric(x2))
##MSE for x2
(mean(x2)-1)^2+var(as.numeric(x2))

##bias for x3
mean(x3)-1
##variance for x3
var(as.numeric(x3))
##MSE for x3
(mean(x3)-1)^2+var(as.numeric(x3))


##Q3 Part d)
par(mfrow=c(1,1))

ghat<- function(x){
  a <- mean(x)*var(x)
  a<- sqrt(a)
  return(a)
}
n = sample.int(1000,50, replace = T)
mse = numeric(50)
for(j in 1:50){
  x = numeric(n[j])
  for(i in 1:n[j]){
    y = rpois(n[j],lambda = 1)
    x[i] = ghat(y)
  }
  ##MSE for x given lambda = 1
  mse[j] = (mean(x)-1)^2+var(as.numeric(x))  
}
hist(mse)
mean(mse)
hist(x,main="Histogram of Function G for large number of samples")

hwk2_data

## Q4 Part b)
x0 = sum(hwk2_data)/length(hwk2_data$V1)
x0

##Q4 Part d)
start<-median(hwk2_data$V1)
newton<-function(start){
  N = 20
  x = hwk2_data$V1
  for(i in 1:N){
    x1<-start + ((sum(x)/(start^2))-N/start)/(2*sum(x)/(start^3)-N/(start^2))
    if(abs(x1-start)<0.0001) break
    start<-x1
  }
  return(x1)
}
result<-newton(start)
result

##Q4 Part e)
N = length(hwk2_data$V1)
f<-function(mu,x){
  N*log(mu) + sum(x)/mu
}
start<-median(hwk2_data$V1)
out<-nlm(f, start, x = hwk2_data)
mu_hat<-out$estimate
mu_hat

##Q4 part g)
f<-function(mu,x){
  20*log(mu) + sum(x)/mu
}
start<-median(hwk2_data$V1)
out<-nlm(f, start, x = hwk2_data, hessian = TRUE)

var<-solve(out$hessian)
#Standard Error of Theta_MLE
std_error<-sqrt(diag(var))
std_error
#Information Matrix
I<- -out$hessian
I

##Q5 Part a)
xvec<-hwk2_data2$V1
l<-length(xvec)

fn<-function(theta){
  sum( theta[1]*log(theta[2]) + log(gamma(theta[1])) + (1-theta[1])*log(xvec) + xvec/theta[2] )
}
alpha<-mean(xvec)^2/var(xvec)
beta<-mean(xvec)/var(xvec)
theta<-c(alpha,beta)
out<-nlm(fn, theta, hessian=TRUE)
# MLE of alpha and beta
out$estimate
var1<-solve(out$hessian)
#Standard Error using the hessian matrix
std_error<-sqrt(diag(var1))
std_error

##Q5 Part b)
xvec<-hwk2_data2$V1

fn<-function(theta){
  sum( theta[1]*log(theta[2]) + log(gamma(theta[1])) + (1-theta[1])*log(xvec) + xvec/theta[2] )
}
alpha<-mean(xvec)^2/var(xvec)
beta<-mean(xvec)/var(xvec)
theta<-c(alpha,beta)
out1<-optim(theta, fn, hessian = TRUE)
#MLE of alpha and beta using optim
out1$par
var2<-solve(out$hessian)
#Standard Error using the hessian matrix
var2
std_error<-sqrt(diag(var2))
std_error

##Q5 Part c)
xvec<-hwk2_data2$V1

fn<-function(theta){
  sum( theta[1]*log(theta[2]) + log(gamma(theta[1])) + (1-theta[1])*log(xvec) + xvec/theta[2] )
}
alpha<-mean(xvec)^2/var(xvec)
beta<-mean(xvec)/var(xvec)
theta<-c(alpha,beta)
out1<-optim(theta, fn, hessian = TRUE)
#MLE of alpha and beta using optim
theta<-out1$par
var2<-solve(out1$hessian)
#Standard Error using the hessian matrix
std_error<-sqrt(diag(var2))

conf_interval<-0.95
theta1<-c(5,1)
critical_value<-qnorm((1+conf_interval)/2)
for(i in 1:2){
  print(theta1[i] + c(-1,1)*critical_value*std_error[i])
}
