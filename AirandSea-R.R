#########################################################################
#title: "Assignment 1 - SIT743"
#author: "Said Abdullahi"
#Date: '2023-04-22'
#Id:221377847 
#########################################################################

#########################################################################
#Understand the Data
#########################################################################
#setting work directory
setwd("/Users/saidpiccolo/Desktop/assignment1")

#import dataset
the.fulldata <- as.matrix(read.csv("DataMyrmindon2023.csv", 
                                   header = TRUE, sep = ","))

#Generate a sample of 30,000 data
set.seed(1)
my.data <- the.fulldata [sample(1: 52561, 30000), c(1:5)]

#View the some of the the data
head(my.data)

#Save sampled data
write.table(my.data,"Said Abdullahi-221377847-MyrmMyData.txt")

###########################################################################
#Question 1.1: Histogram, Box plot, five number summary
###########################################################################
#Histogram, boxplot, and five summary for the 'Humidity' variable
hist(my.data[,3], xlab = "Humidity", main="HUMIDITY") # Histogram for hummidity
boxplot(my.data[,3], xlab="Humidity", main="HUMIDITY") #Boxplot for humidity
summary(my.data[,3]) #five number summary for humidity

###########################################################################
#Question 1.3: Parallel box plot
###########################################################################
#Parallel boxplot for variables AirTemperature and WaterTemperature
par(mfrow=c(1,1))
boxplot(my.data[,1],my.data[,4],
        main = "Air Temperatur vs. Water Temperature",
        ylab = "degrees Celsuis",
        names = c("Air Temperature", "Water Temperature"))

############################################################################
#Question 1.4: Scatterplot, linear regression model, correlation coefficient
#coefficient of Determination
############################################################################
#Analyse the correlation between Pressure and AirTemperature
plot(my.data[,2], my.data[,1], xlab="Pressure", ylab="Air Temperature", 
     main="Pressure and Air Temperature")

#linear regression model and line
lm(my.data[,1]~my.data[,2])
abline(lm(my.data[,1]~my.data[,2]), col="red")

#Correlation Coefficient and the coefficient of Determination
cor(my.data[,1],my.data[,2])
coeOfDet = cor(my.data[,1],my.data[,2])^2*100 # in percentage
coeOfDet
############################################################################
#Question 1.5: Create three new variables, WaterTB, PreB, and WSB
############################################################################
#write function for bucketing the tb
#high if tb > 28, Moderate if tb is between 24 and 28, low if tb is below 25
compute.tb_bucketing <- function(x){
  a = array(0,length(x))
  for (i in 1:length(x)){
    if(x[i] > 28){
      a[i] <- 'High'
    }
    else if (x[i] >= 25 & x[i] <= 28){
      a[i] <- 'Moderate'
    }
    else {
      a[i] <- 'Low'
    }
  }
  a
}

my.data <- cbind(my.data, WaterTB = compute.tb_bucketing(my.data[,4]))
head(my.data)


#write function for bucketing the cholesterol
#high if if Pressure > 1013 low otherwise
compute.ch_bucketing <- function(x){
  b = array(0,length(x))
  for (i in 1:length(x)){
    if(x[i] > 1013){
      b[i] <- 'High'
    }
    else {
      b[i] <- 'Low'
    }
  }
  b
}

my.data <- cbind(my.data, PreB = compute.ch_bucketing(my.data[,2]))
head(my.data)

#write function for bucketing the pressure
#high if if WindSpeed > 30 low otherwise
compute.bp_bucketing <- function(x){
  c = array(0,length(x))
  for (i in 1:length(x)){
    if(x[i] > 30){
      c[i] <- 'High'
    }
    else {
      c[i] <- 'Low'
    }
  }
  c
}

#combine the to previous dataframe
my.data <- cbind(my.data, WSB = compute.bp_bucketing(my.data[,5]))
head(my.data)

# crosstable
ct <- table(my.data[,6], my.data[,7], my.data[,8])

# length of the dat which is 30000
length(my.data[,1])

ftable(ct) # print 3-way table with ftable() function


#########################################################################
#Question 3.2 c
#########################################################################
library(Bolstad)
#https://cran.r-project.org/web/packages/Bolstad/Bolstad.pdf

## observations
y = c(7,5,8,12,10,8,9,8)
results = poisgamp(y,0.2,0.1)

#plot prior, liklihood and posterior on a single plot
plot(results, overlay = TRUE, which = 1:3, ylim = c(0, 1))

## find the posterior mean and std. deviation for the above
inverse_to_get_exponential_mu <- 1/mean(results)
inverse_to_get_exponential_var <- 1/var(results)
inverse_to_get_exponential_sd <- 1/sd(results)

#########################################################################
#Question 4 c: Bayesian inference for Gaussians 
#(unknown mean and known variance)
#########################################################################
library(Bolstad)
#https://cran.r-project.org/web/packages/Bolstad/Bolstad.pdf

#mu values 
mu = seq(-5, 5, by = 0.001)

#define the trapezoidal prior
mu.prior = rep(0, length(mu))
mu.prior[mu <= -4] = 1 / 2 + mu[mu <= -4] /10
mu.prior[mu>-4 & mu<=0] = 23/80 + 5/4
mu.prior[mu>0 & mu<=4] = -23/80 +5/4
mu.prior[mu>4] =  -1 / 10 + mu[mu > 4] /2

#find posterior
results = normgcp(0.5,1, density = "user", mu = mu, mu.prior = mu.prior)

#plot prior, liklihood and posterior on a single plot
plot(results, overlay = TRUE, which = 1:3)

#plot the above results (prior, liklihood. posterior) in different axes
decomp(results)

#Finding the posterior mean and standard deviation for the above.

## find the posterior mean and std. deviation for the above
mean(results)
var(results)
sd(results)
                                                                                        
#####################################################################
#Question 5 Clustering
#####################################################################

#####################################################################
#Question 5.1 K-Means clustering:
####################################################################

#a)
zz <- read.table("lettersdata.txt")
zz<- as.matrix(zz)

colnames(zz) <- c("x", "y")
plot(zz, main="scatterplot for lettersdata")

#b)
#We can see the scatterplot maybe 4 clusters needed
cl <- kmeans(zz, 4, nstart = 25)
plot(zz, col = cl$cluster, main="scatterplot for lettersdata")

points(cl$centers, col = 1:5, pch = 8)

#c)
totwss = array(,c(20,1)) 

for (i in 2:20)
{
  print(i) 
  totwss[i,1]=(kmeans(zz,centers=i))$tot.withinss
  print(totwss[i]) 
}
plot(totwss, main="Total within sum of squares (totWSS) with different K value", xlab="(k) number of cluster")

####################################################################
#Question 5.2: Spectral Clustering
####################################################################
spectral_clustering <- function(X, # matrix of data points
                                nn = 10, # the k nearest neighbors to consider
                                n_eig = 3) # m number of eignenvectors to keep
{
  mutual_knn_graph <- function(X, nn = 10)
  {
    D <- as.matrix( dist(X) ) # matrix of euclidean distances between data 
    
    # intialize the knn matrix
    knn_mat <- matrix(0,
                      nrow = nrow(X),
                      ncol = nrow(X))
    
    # find the 10 nearest neighbors for each point
    for (i in 1: nrow(X)) {
      neighbor_index <- order(D[i,])[2:(nn + 1)]
      knn_mat[i,][neighbor_index] <- 1 
    }
    
    # Now we note that i,j are neighbors iff K[i,j] = 1 or K[j,i] = 1 
    knn_mat <- knn_mat + t(knn_mat) # find mutual knn
    
    knn_mat[ knn_mat == 2 ] = 1
    
    return(knn_mat)
  }
  
  graph_laplacian <- function(W, normalized = TRUE)
  {
    stopifnot(nrow(W) == ncol(W)) 
    
    g = colSums(W) # degrees of vertices
    n = nrow(W)
    
    if(normalized)
    {
      D_half = diag(1 / sqrt(g) )
      return( diag(n) - D_half %*% W %*% D_half )
    }
    else
    {
      return( diag(g) - W )
    }
  }
  
  W = mutual_knn_graph(X) # 1. matrix of similarities
  L = graph_laplacian(W) # 2. compute graph laplacian
  ei = eigen(L, symmetric = TRUE) # 3. Compute the eigenvectors and values of L
  n = nrow(L)
  return(ei$vectors[,(n - n_eig):(n - 1)]) # return the eigenvectors of the 
                                           # n_eig smallest eigenvalues
  
}

# do spectral clustering procedure

X_sc <- spectral_clustering(zz)

# run kmeans on the 2 eigenvectors
X_sc_kmeans <- kmeans(X_sc, 4)

plot(zz, col = X_sc_kmeans$cluster, main="scatterplot for lettersdata")

# Source: https://rpubs.com/nurakawa/spectral-clustering

####################################################################
#Question 6 a) ii)
###################################################################
#95% credible interval for Canterbury
a=779
b=696790
x<-rbeta(1000000, a, b) #randomly draw 1 million
q1<-quantile(x, c(0.025, 0.50, 0.975))
q1

#95% credible interval for Auckland
a=478
b=416207
x<-rbeta(1000000, a, b) #randomly draw 1 million
q2<-quantile(x, c(0.025, 0.50, 0.975))
q2

####################################################################
#Question 6 a) ii)
###################################################################
#Prior probability and Posterior probability for Canterbury
theta= seq(from=0.000, to=0.005, length=10000)

pTheta1 = dbeta(theta,400,400000)
pTheta2 = dbeta(theta,779,696790)


colors <- c("blue", "green")
labels <- c("Beta(400,400000)", "Beta(779,696790)")

plot(theta,pTheta1, main="Canterbury: Priors and Posterior", axes=TRUE, ylim = c(0,10000))
lines(theta, pTheta1, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors[1])
lines(theta, pTheta2,lwd=2, col=colors[2])
legend("topright", inset=.005,
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
#########################################################
#Prior probability and Posterior probability for Auckland
theta= seq(from=0.000, to=0.005, length=10000)

pTheta1 = dbeta(theta,300,200000)
pTheta2 = dbeta(theta,478,416207)


colors <- c("blue", "green")
labels <- c("Beta(300,200000)", "Beta(478,416207)")

plot(theta,pTheta1, main="Auckland: Priors and Posterior", axes=TRUE, ylim = c(0,10000))
lines(theta, pTheta1, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors[1])
lines(theta, pTheta2,lwd=2, col=colors[2])
legend("topright", inset=.005,
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
###########################################################
