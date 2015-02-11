##################################################################################
############# Assignment 1: Due Wednesday 2/11, 5:00 PM via NYU Classes ##########
## Submit typed [or very neatly written and scanned] answers (no R code needed) ##
##################################################################################

##################################################################################
### This assignment uses the iris data set (see lab1 for info on how to obtain) ##
##################################################################################

### Exercise 1: How does the density() function work? Make sure to explain both what the
# implications are of the choice of 'kernel' and 'adjust'. Demonstrate the difference in smoothing
# amounts and kernel choices by making a plot similar to the one for Sepal Length (see below)
# with the following specifications:
# a) half the default amount of smoothing, gaussian kernel
# b) half the default amount of smoothing, cosine kernel
# c) twice the default amount of smoothing, gaussian kernel
## Make sure to label all plots appropriately by adjusting the main = "  " command
## Note - you may find this useful: http://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/AV0405/MISHRA/kde.html

par(mfrow=c(2,2))
par(mfrow=c(1,1))
plot(density(iris$Sepal.Length), main = "Density")

plot(density(iris$Sepal.Length, adjust = 0.5, kern = "gaussian"), main = "Gaussian, 0.5*BW")
plot(density(iris$Sepal.Length, adjust = 0.5, kern = "cosine"), main = "Cosine, 0.5*BW")
plot(density(iris$Sepal.Length, adjust = 2, kern = "gaussian"), main = "Gaussian, 2*BW")
### Exercise 2:
# a) What does a Q-Q plot show? Use the following plots to justify your explanation.
# Make sure to explain what is seen in each plot.
# b) Show an example where a Q-Q plot gives clearer information than a histogram. 

par(mfrow=c(1,2))
qqnorm(scale(rnorm(10)), xlim=c(-3,3), ylim=c(-3,3))
abline(0,1, col="red")
hist(rnorm(10), main = "10 draws from N(0,1)")

par(mfrow=c(1,2))
qqnorm(scale(rnorm(100)), xlim=c(-3,3), ylim=c(-3,3))
abline(0,1, col="red")
hist(rnorm(100), main = "100 draws from N(0,1)")

par(mfrow=c(1,2))
qqnorm(scale(rnorm(1000)), xlim=c(-3,3), ylim=c(-3,3))
abline(0,1, col="red")
hist(rnorm(1000), main = "1000 draws from N(0,1)")

par(mfrow=c(1,2))
qqnorm(scale(rt(100, df=2)), xlim=c(-3,3), ylim=c(-3,3))
abline(0,1, col="red")
hist(rt(100, df=2), main = "100 draws from T_2")

par(mfrow=c(1,2))
qqnorm(scale(rbeta(100, 0.5,2)), xlim=c(-3,3), ylim=c(-3,3))
abline(0,1, col="red")
hist(rbeta(100, 0.5,2), main = "100 draws from Beta(0.5,2)")

par(mfrow=c(1,3))
qqnorm(scale(rnorm(100)), xlim=c(-3,3), ylim=c(-3,3))
abline(0,1, col="red")
hist(rnorm(100), breaks = 5, main = "100 draws from N(0,1), 5 bins")
hist(rnorm(100), breaks = 25, main = "100 draws from N(0,1), 25 bins")


### Exercise 3: 
# a) Explain the major differences between the Pearson's and Spearman's correlation tests. 
# b) Which is the most appropriate to test the correlation between Sepal.Length and Petal.Length. Justify with the appropriate plots.

cor.test(iris[,1], iris[,3], method="pearson")
cor.test(iris[,1], iris[,3], method="spearman")
par(mfrow=c(1,1))
plot(iris[,1], iris[,3], xlab = 'Sepal Length',ylab='Petal Length')
reg<-lm(iris[,3]~iris[,1])
abline(reg, col = 'red')
iris
### Exercise 4: 
## a) Describe in both words and with formula how the naiveBayes classifier works.
## The documentation for the the function states "The standard naive Bayes classifier
## (at least this implementation) assumes independence of the predictor variables,
## and Gaussian distribution (given the target class) of metric predictors."
## Make sure to address how both of these assumptions are used in the naiveBayes classifier.
## b) Would you consider this method to be supervised or unsupervised? Why?

?naiveBayes
install.packages("e1071")
library(e1071)
classifier = naiveBayes(iris[,1:4], iris[,5])
classifier
### Exercise 5: 
## a) Plot the distribution of each of the four variables for the 3 different species.
## Make sure to include a legend that provides the color/species mapping.
## b) Comment about the differences between these plots  how do these variables compare
## in their ability to univariately classify petals? 
 
classifier$tables$Petal.Length[3,1]
par(mfrow=c(2,2))
plot(function(x) dnorm(x, classifier$tables$Petal.Length[1,1], classifier$tables$Petal.Length[1,2]),ylab ='', 0, 8, col="red", main="Petal length")
curve(dnorm(x, classifier$tables$Petal.Length[2,1], classifier$tables$Petal.Length[2,2]), add=TRUE, ylab = '',col = "blue")
curve(dnorm(x, classifier$tables$Petal.Length[3,1], classifier$tables$Petal.Length[3,2]), add=TRUE, ylab = '',col = "green")
legend('topright', legend = c('setosa','versicolor','virginica') ,lty=1, col=c('red', 'blue', 'green'), bty='n', cex=.75)

plot(function(x) dnorm(x, classifier$tables$Petal.Length[1,1], classifier$tables$Petal.Width[1,2]),ylab ='', 0, 8, col="red", main="Petal Width")
curve(dnorm(x, classifier$tables$Petal.Length[2,1], classifier$tables$Petal.Width[2,2]), add=TRUE, ylab = '',col = "blue")
curve(dnorm(x, classifier$tables$Petal.Length[3,1], classifier$tables$Petal.Width[3,2]), add=TRUE, ylab = '',col = "green")
legend('topright', legend = c('setosa','versicolor','virginica') ,lty=1, col=c('red', 'blue', 'green'), bty='n', cex=.75)

plot(function(x) dnorm(x, classifier$tables$Petal.Length[1,1], classifier$tables$Sepal.Length[1,2]),ylab ='', 0, 8, col="red", main="Sepal length")
curve(dnorm(x, classifier$tables$Petal.Length[2,1], classifier$tables$Sepal.Length[2,2]), add=TRUE, ylab = '',col = "blue")
curve(dnorm(x, classifier$tables$Petal.Length[3,1], classifier$tables$Sepal.Length[3,2]), add=TRUE, ylab = '',col = "green")
legend('topright', legend = c('setosa','versicolor','virginica') ,lty=1, col=c('red', 'blue', 'green'), bty='n', cex=.75)

plot(function(x) dnorm(x, classifier$tables$Petal.Length[2,1], classifier$tables$Sepal.Width[2,2]),ylab ='', 0, 8, col="blue", main="Sepal Width")
curve(dnorm(x, classifier$tables$Petal.Length[1,1], classifier$tables$Sepal.Width[1,2]), add=TRUE, ylab = '',col = "red")
curve(dnorm(x, classifier$tables$Petal.Length[3,1], classifier$tables$Sepal.Width[3,2]), add=TRUE, ylab = '',col = "green")
legend('topright', legend = c('setosa','versicolor','virginica') ,lty=1, col=c('red', 'blue', 'green'), bty='n', cex=.75)

