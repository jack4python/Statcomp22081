## -----------------------------------------------------------------------------
s = "This is THE Euler's Formular"
print(s)


## ---- warning=FALSE, message=FALSE--------------------------------------------
par(mar = rep(0,4))
cols <- rainbow(1000) #pick colors from rainbow the function
pie(rep(1,1000),labels = NA,col = cols,border = cols)

## -----------------------------------------------------------------------------
n <- 1000
set.seed(777)  #set a seed
u <- runif(n)
x <- 2/sqrt(1-u) #generate the random numbers
hist(x, 
     freq = F, 
     main =expression(f(x) == 8 / x ^ 3), 
     xlim = c(0 , 40), 
     col = "Light Blue", 
     border = "White",
     xlab = "random numbers",
     las = 1)
check<- seq(0 , 40 , .01)
lines( check , 
       8 / (check ^ 3),
       lwd = 3)

## -----------------------------------------------------------------------------
set.seed(329)  #set a seed
betafun <- function(n, alpha, beta)
{
a <- alpha
b <- beta
lim <-  n     #acquire all the parameters from the function
k <- 0
y <- numeric(lim)
while(k < lim){
    x <- runif(1)
    if( x^(a - 1) * (1 - x)^(b - 1)> runif(1))
    { 
        k <- k + 1
        y[k] <- x
      }
} #generate random numbers via acceptance-rejection method
   return(y)
}#construct a function which makes a given numbers of random sample from Beta(a, b)
brdm = betafun(1000, 3, 2) #use the function to produce 1000 random numbers
hist(brdm,
     freq = F,
     main = expression(f(x) == 12 * x^2 *(1 - x)),
     ylim = c(0 , 2),
     col = "Light Blue",
     border = "White",
     las = 1,
     xlab = "Beta random numbers"
     )
check <- seq(0, 1, .01 )
lines(check, 
      12 * check ^ 2 * (1 - check), 
      lwd = 3)

## -----------------------------------------------------------------------------
set.seed(777) #set a seed
n <- 1000
r <- 4
beta <- 2
lambda <- rgamma(n, r, beta)
rdm <- rexp(lambda)
hist(rdm, 
     freq = F, 
     main = "Exponential-Gamma Mixture",
     ylim = c(0, 1),
     col = "Peachpuff", 
     border = "Black",
     xlab = "mixture random numbers",
     las = 1)


## -----------------------------------------------------------------------------
set.seed(888)  #set a seed
n <- 1000
r <- 4
beta <- 2
lambda <- rgamma(n, r, beta)
rdm <- rexp(lambda)
hist(rdm, 
     freq = F, 
     main = expression(f(x) == 64 / (2 + x) ^ 5),
     ylim = c(0, 1),
     col = "Peachpuff", 
     border = "Black",
     xlab = "mixture random numbers",
     las = 1)
lines(check <-  seq(0, 6, .01),
      64 / (2 + check) ^ 5,
      lwd = 3, 
      col = "Chocolate")

## -----------------------------------------------------------------------------
quick_sort<-function(sample){
  num<-length(sample)                          #acquire the sample for sorting
  if(num==0||num==1){return(sample)
  }else{
    first_one<-sample[1]                       #pick a number  
    rest_ones<-sample[-1]
    lower<-rest_ones[rest_ones < first_one]
    upper<-rest_ones[rest_ones >= first_one]   #sort the sample according to the quicksorting algorithm
    count <<-  count + num - 1                 #jot down the number of times of comparison i.e. time complexity
    return(c(quick_sort(lower),first_one,quick_sort(upper)))} #recursion until the sample is completely sorted
}

## -----------------------------------------------------------------------------
a <- numeric(5)                              #array "a" to contain the five results
total <- c(1e4, 2e4, 4e4, 6e4, 8e4)
for (j in 1:5) {
    total_count <- numeric(100)             
    #the array "total_count" is to contain the time complexity created by the following loop
    for (i in 1:100) {
        count <- 0
        random_sample <- sample(1:total[j])
        quick_sort(random_sample)
        total_count[i] <- count
    }
    a[j]<- sum(total_count) / 100
}
knitr::kable(a, 
             "pipe", 
             col.names = "average time complexity", 
             align = "l") #print array "a"

## -----------------------------------------------------------------------------
par(mar = c(5, 8, 4, 2), 
    yaxt = "s")
plot(total, 
     a,
     main = "Quicksort Computation Time",
     type = "b",
     xlab = "Number of Sample",
     ylab = "Computation Time",
     )                #regress array "a"
axis(side = 4)
line <- seq(1, 80000, 1)
lines(line, line*log(line), 
      lwd = 3, 
      col = "Chocolate") #draw nlog(n)

## -----------------------------------------------------------------------------
MC.simple_n_antithetic <- function(x, R = 1000, antithetic = FALSE){
    front_half <- runif(R/2)
    if(antithetic) back_half <- 1-front_half
    else           back_half <- runif(R/2)
    total_sample <- c(front_half, back_half)
    return(mean(x * exp(x * total_sample)))
}

## -----------------------------------------------------------------------------
MC_simple <-  MC_antithetic <- vector(length = 1000)
for (i in 1:1000) {
    MC_simple[i] <- MC.simple_n_antithetic(x = 1)
    MC_antithetic[i] <- MC.simple_n_antithetic(x = 1, antithetic = TRUE)
}
reduction <- 1 - var(MC_antithetic) / var(MC_simple)
knitr::kable(c(var(MC_simple), var(MC_antithetic), reduction), 
             "pipe", 
             col.names = "variance and reduction",
             align = "l")

## -----------------------------------------------------------------------------
x <- seq(1, 20, .01)
f1 <- sqrt(exp(1)) * x * exp(-x ^ 2 / 2)
f2 <- exp(1 - x)
g <- x ^ 2 * exp(-x ^ 2 / 2) / sqrt(2 * pi)
plot(x, g, type = "l", main = "f1, f2, with g", ylab = "", ylim = c(0, 0.6), lwd = 2)
lines(x, f1, lty = 2, lwd = 2)
lines(x, f2, lty = 3, lwd = 2)
legend("topright", legend = c("g", "f1", "f2"), lty = 1:3, lwd = 2)

## -----------------------------------------------------------------------------
plot(x, g, type = "l", main = "g / f", ylab = "", ylim = c(0, 1), lwd = 2, lty = 2)
lines(x, g / f1, lty = 3, lwd = 2)
lines(x, g / f2, lty = 4, lwd = 2)
legend("topright", legend = c(0:2), lty = 2:4, lwd = 2)


## -----------------------------------------------------------------------------
m <- 1e4
set.seed(123)
theta_hat <- v <- numeric(2)
# theta_hat and v to contain estimated theta and variance of f1 and f2
g <- function(x){
    x ^ 2 * exp(-x ^ 2 / 2)/sqrt(2 * pi) * (x > 1)
}
u <- runif(m)
x <- sqrt(1 - 2 * log(1 - u))
#inverse transformation to produce random number of f1
fg <- g(x) / (sqrt(exp(1)) * x * exp(-x ^ 2 / 2))
theta_hat[1] <- mean(fg)
v[1] <- var(fg)

## -----------------------------------------------------------------------------
n <- 1e4
u <- runif(n)
y <- 1 - log(1 - u) 
#inverse transformation to produce random number of f2
fg <- g(y) / exp(1 - y)
theta_hat[2] <- mean(fg)
v[2] <- var(fg)

## -----------------------------------------------------------------------------
rev <- data.frame(c("f1", "f2"), est <- theta_hat, va <- v)
knitr::kable(rev, "pipe", align = "l", col.names = c("importance function", "theta hat", "variance"))

## -----------------------------------------------------------------------------
M <- 1e3
u <- matrix(0, 200, 5)  
# 200 * 5 matrix to store replicates of random uniform distribution numbers
x <- matrix(0, 200, 5)  
# 200 * 5 matrix to store replicates of x from every strata generated by inverse transform method
fg <- matrix(0, 200, 5) 
#2 200 * 5 matrix to store replicates of gi / fi from every strata
T <- numeric(5)         
# T to store  theta j hat of every strata 
v <- numeric(5)
# v to store sigma j square of every strata
g <- function(x){
    exp(-x - log(1 + x ^ 2)) * (x > 0) *(x < 1)
}
p <- function(j){
    -log((5 - j) / 5 +j / 5 *exp(-1))
}
# function p() to calculate endpoints of every strata
for (j in 0:4) {
    u[ ,j + 1] <- runif(M/5, p(j), p(j + 1))
    x[ ,j + 1] <- -log(exp(-p(j)) - u[ ,j + 1] * (1 - exp(-1)) / 5)
    fg[ ,j + 1] <- g(x[ ,j + 1]) / (5 * exp(-x[ ,j + 1]) / (1 - exp(-1)))
    T[j + 1] <- mean(fg[ ,j + 1])
    v[j + 1] <- var(fg[ ,j + 1])
    j <- j + 1
}
# for loop to calculate theta j hat and sigma j square of each 5 strata respectively
(theta.hat <- sum(T))
(vari <- sum(v) / (M / 5))

## -----------------------------------------------------------------------------
set.seed(234)
m <- 1e4  
# m denotes number of simulation
n <- 10
# n denotes sample size
data_generate <- function(m, n){

lndata <- matrix(0, m, n)
  for (i in 1:m) {
    lndata[i, ] <- rlnorm(n)
  }
 return(lndata)
} 
lndata <- data_generate(m, n)
# for loop to generate random sample
save(lndata,m,n, file = "4data.RData")
# save data
rm(list = ls())
gc()
#clear memory

## -----------------------------------------------------------------------------
alpha <- .05
load("4data.RData")
# load data
level_test <- function(m, n, lndata){
LCL <- UCL <- numeric(m)
for (i in 1:m) {
    LCL[i] <- mean(log(lndata[i, ])) - sd(log(lndata[i, ])) / sqrt(n) * qt(1 - alpha/2, df = n - 1)
    UCL[i]<- mean(log(lndata[i, ])) + sd(log(lndata[i, ])) / sqrt(n) * qt(1 - alpha/2, df = n - 1)
}
# for loop to calculate upper and lower confidence limits
outcome <- mean( (LCL <= 0) & (UCL >= 0) )
return(outcome)
}
outcome <- level_test(m, n, lndata)
# compute the mean to get the confidence level
save(outcome, file = "4outcome.RData")
# save data
rm(list = ls())
gc()
# clear memory

## -----------------------------------------------------------------------------
load("4outcome.RData")
report <- function(outcome){
paste(" Empirical estimate of the confidence level is:", outcome)
}
report(outcome)
write.table(outcome, file = "outcome4.txt")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
set.seed(234)
n_small <- 20
n_medium <- 100
n_large <- 1000
# n_size for sample size of each sample
m <- 1e4
# m for number of simulations
sigma1 <- 1
sigma2 <- 1.5
generate <- function(x, n, m, sigma0){
    for (i in 1:m) {
        x[i, ] <- rnorm(n, 0, sigma0)
    }
    return(x)
}
x_small <- matrix(0, m, n_small)
x_medium <- matrix(0, m, n_medium)
x_large <- matrix(0, m, n_large)
y_small <- matrix(0, m, n_small)
y_medium <- matrix(0, m, n_medium)
y_large <- matrix(0, m, n_large)
x_small <- generate(x_small, n_small, m, sigma1)
x_medium <- generate(x_medium, n_medium, m, sigma1)
x_large <- generate(x_large, n_large, m, sigma1)
y_small <- generate(y_small, n_small, m, sigma2)
y_medium <- generate(y_medium, n_medium, m, sigma2)
y_large <- generate(y_large, n_large, m, sigma2)

#for loop to generate random numbers of small,medium and large sample for x and y
save(x_small, x_medium, x_large, y_small, y_medium, y_large, n_small, n_medium, n_large, m, file = "8xydata.RData")
# save data
rm(list = ls())
gc()
# clear memory

## -----------------------------------------------------------------------------
alpha_hat <- 0.055
#pow_size to store p value from count5 test whereas pow.size stores p value of respective sample of F test
count5test <- function(x, y){
    X <- x - mean(x)
    Y <- y - mean(y)
    outx <- sum(X > max(Y)) + sum(X < min(Y))
    outy <- sum(Y > max(X)) + sum(Y < min(X))
    return(as.integer(max(c(outx, outy)) > 5))
}
# function to perform count5 test
load("8xydata.RData")
pow_small <- pow_medium <- pow_large <- pow.small <- pow.medium <- pow.large<- numeric(m)
for (i in 1:m) {
    pow_small[i] <- count5test(x_small[i, ], y_small[i, ])
    pow_medium[i] <- count5test(x_medium[i, ], y_medium[i, ])
    pow_large[i] <- count5test(x_large[i, ], y_large[i, ])
    pow.small[i] <- var.test(x_small[i, ], y_small[i, ])$p.value
    pow.medium[i] <- var.test(x_medium[i, ], y_medium[i, ])$p.value
    pow.large[i] <- var.test(x_large[i, ], y_large[i, ])$p.value
}
# for loop to compute p values of two tests with different sample sizes
pow_small_d <- mean(pow_small)
pow_medium_d <- mean(pow_medium)
pow_large_d <- mean(pow_large)
pow.small.d <- mean(pow.small <= alpha_hat)
pow.medium.d <- mean(pow.medium <= alpha_hat)
pow.large.d <- mean(pow.large <= alpha_hat)
# compute the mean to get power value of all tests
save(pow.small.d, pow.medium.d, pow.large.d, pow_small_d, pow_medium_d, pow_large_d,  file = "8powdata.RData")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
load("8powdata.RData")
outcome.8 <- data.frame(value.name <- c("count5small", "count5medium", "count5large", "F small", "F medium", "F large"), value <- c(pow_small_d, pow_medium_d, pow_large_d, pow.small.d, pow.medium.d, pow.large.d))
knitr::kable(outcome.8, "pipe", col.names = c("value name", "value"))
write.table(outcome.8, file = "outcome8.txt")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
data(aircondit, package = "boot")
aircondit
save(aircondit, file = "4data.RData")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
load("4data.RData")
set.seed(123)
outcome <- numeric(3)
#vector "outcome" to store estimate bias,standard error and sample standard error
boot <- function(aircondit){
    B <- 200
    lambda_star <- numeric(B)
    outcome <- numeric(3)
    lambda_hat <- 1 / mean(aircondit$hours)
    for (b in 1:B) {
        aircondit_star <- sample(aircondit$hours, replace = TRUE)
        lambda_star[b] <- 1 / mean(aircondit_star)
    }
    outcome[1] <- mean(lambda_star) - lambda_hat
    outcome[2] <- sd(lambda_star)
    outcome[3] <- sd(aircondit$hours)
    return(outcome)
}
# write "boot" function to calculate bias and standard error
outcome <- boot(aircondit)
save(outcome, file = "4outcome.RData")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
load("4outcome.RData")
present <- function(outcome){
    round(c(bias=outcome[1], se.boot=outcome[2], se.sample=outcome[3]), 6)
}
present(outcome)
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
data(aircondit, package = "boot")
aircondit
save(aircondit, file = "5data.RData")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
load("5data.RData")
library(boot)
set.seed(123)
outcome <- matrix(0, 4, 2)
m_time_f <- function(dat, ind){
    mean(dat[ind])
}
boot.obj <- boot(aircondit$hours,
                 R=2000,
                 statistic = m_time_f)
save(boot.obj, outcome, file = "5obj.RData")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
load("5obj.RData")
set.seed(123)
itc <- function(boot.obj, outcome){
    ci <- boot.ci(boot.obj, type = c("basic", "norm", "perc", "bca"))
    outcome[1,]<-ci$norm[2:3]
    outcome[2,]<-ci$basic[4:5] 
    outcome[3,]<-ci$percent[4:5]
    outcome[4,]<-ci$bca[4:5]
    return(outcome)
}
outcome <- itc(boot.obj, outcome)
save(outcome, file = "5outcome.RData")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
load("5outcome.RData")
present <- function(outcome){
    cat("Normal interval:", "\n", "(", outcome[1,1], ",", outcome[1,2], ")", "\n")
    cat("Basic interval:", "\n", "(", outcome[2,1], ",", outcome[2,2], ")", "\n")
    cat("Percentile interval:", "\n", "(", outcome[3,1], ",", outcome[3,2], ")", "\n")
    cat("Bca interval:", "\n", "(", outcome[4,1], ",", outcome[4,2], ")", "\n")
}
present(outcome)
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
set.seed(123)
sample0 <- rnorm(1e5)
#sample0 to store all of the random numbers needed in this project
save(sample0, file = "Adata.RData")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
load("Adata.RData")
library(boot)
boot.mean <- function(dat, ind){
    mean(dat[ind])
}
outcome <- matrix(0, 3, 3)
# 3 * 3 matrix "outcome" stores coverage rate,left and right side of interval in a row for three ways of confidence intervals calculation
set.seed(123)
proces <- function(sample0){
    outcome <- matrix(0, 3, 3)
    m <- 1e4
    mu <- 0
    count <- 0
    ci.norm <- ci.basic <- ci.perc <- matrix(NA,m,2)
    for (i in 1:m) {
        de <- boot(data = sample0[(count + 1):(count + 10)], statistic = boot.mean, R = 999)
        ci <- boot.ci(de, type = c("norm", "basic", "perc"))
        ci.norm[i,]<-ci$norm[2:3]
        ci.basic[i,]<-ci$basic[4:5] 
        ci.perc[i,]<-ci$percent[4:5]
        count <- count + 10
        }
    outcome[1, 1] <- mean(ci.norm[, 1] <= mu & ci.norm[, 2] >= mu)
    outcome[2, 1] <- mean(ci.basic[, 1]<=mu & ci.basic[, 2] >= mu)
    outcome[3, 1] <- mean(ci.perc[, 1] <= mu & ci.perc[, 2] >= mu)
    outcome[1, 2] <- mean(ci.norm[, 1] > mu)
    outcome[1, 3] <- mean(ci.norm[, 2] < mu)
    outcome[2, 2] <- mean(ci.basic[, 1] > mu)
    outcome[2, 3] <- mean(ci.basic[, 2] < mu)
    outcome[3, 2] <- mean(ci.perc[, 1] > mu)
    outcome[3, 3] <- mean(ci.perc[, 2] < mu)
    return(outcome)
}
outcome <- proces(sample0)
outcome
save(outcome, file = "poutcome.RData")
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
load("poutcome.RData")
present <- function(outcome){
    cat('norm =',
    outcome[1, 1], 
    '\n',
    'norm.left=',
    outcome[1, 2],
     '\n',
    'norm.right=',
    outcome[1, 3],
    '\n',
    'basic =',
    outcome[2, 1],
    '\n',
    'basic.left=',
    outcome[2, 2],
    '\n',
    'basic.right=',
    outcome[2, 3],
    '\n',
    'perc =',
    outcome[3, 1],
    '\n',
    'perc.left=',
    outcome[3, 2],
    '\n',
    'perc.right=',
    outcome[3, 3]
    )
}
present(outcome)
rm(list = ls())
gc()

## -----------------------------------------------------------------------------
library(bootstrap)
save(scor, file = "scordata.RData")
rm(list = ls())

## -----------------------------------------------------------------------------
load("scordata.RData")
outcome <- numeric(2)
theta.es <- function(scor){
    return(eigen(cov(scor))$value[1]/sum(eigen(cov(scor))$value))
}
jack_knife <- function(scor){
    n <- nrow(scor)
    theta.jack <- numeric(n)
    theta.hat <- theta.es(scor)
    for (i in 1:n) {
        theta.jack[i] <- theta.es(scor[-i,])
    }
    outcome[1] <- (n-1)*(mean(theta.jack)-theta.hat)
    outcome[2]<- sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))
    return(outcome)
}
outcome <- jack_knife(scor)
save(outcome, file = "1outcome.RData")
rm(list = ls())

## -----------------------------------------------------------------------------
load("1outcome.RData")
report <- function(outcome){
    cat("bias:", outcome[1], "se:", outcome[2])
}
report(outcome)
rm(list = ls())

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
save(ironslag, file = "2data.RData")
rm(list = ls())

## -----------------------------------------------------------------------------
load("2data.RData")
n <- floor(length(magnetic) / 2) 
outcome <- matrix(0, 2 * n, 4)
# for cross validation 
# fit models on leave-two-out samples
cv_2 <- function(outcome, ironslag, n){
    mag <- magnetic[-53]
    che <- chemical[-53]
 for (k in 1:n) { 
    y <-mag[-c(2 * k - 1, 2 * k)]
    x <- che[-c(2 * k - 1, 2 * k)]
    J1 <- lm(y ~ x) 
    yhat1 <- J1$coef[1] + J1$coef[2] * che[c(2 * k - 1, 2 * k)] 
    outcome[c(2 * k - 1, 2 * k), 1] <-mag[c(2 * k - 1, 2 * k)] - yhat1
    J2 <- lm(y ~ x + I(x^2)) 
    yhat2 <- J2$coef[1] + J2$coef[2] * che[c(2 * k - 1, 2 * k)] + J2$coef[3] * che[c(2 * k - 1, 2 * k)]^2
    outcome[c(2 * k - 1, 2 * k), 2] <-mag[c(2 * k - 1, 2 * k)] - yhat2
    J3 <- lm(log(y) ~ x) 
    logyhat3 <- J3$coef[1] + J3$coef[2] * che[c(2 * k - 1, 2 * k)] 
    yhat3 <- exp(logyhat3)
    outcome[c(2 * k - 1, 2 * k), 3] <-mag[c(2 * k - 1, 2 * k)] - yhat3
    J4 <- lm(log(y) ~ log(x)) 
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(che[c(2 * k - 1, 2 * k)]) 
    yhat4 <- exp(logyhat4) 
    outcome[c(2 * k - 1, 2 * k), 4] <-mag[c(2 * k - 1, 2 * k)] - yhat4
 }
    return(outcome)
}
outcome <- cv_2(outcome, ironslag, n)
save(outcome, file = "2outcome.RData")
rm(list = ls())

## -----------------------------------------------------------------------------
load("2outcome.RData")
report <- function(outcome){
cat("model1: ",
    mean(outcome[, 1]^2), "\t",
    "model2: ",
    mean(outcome[, 2]^2), "\t",
    "model3: ",
    mean(outcome[, 3]^2), "\t",
    "model4: ",
    mean(outcome[, 4]^2))
}
report(outcome)
rm(list = ls())

## ----  warning=FALSE, message=FALSE-------------------------------------------
Spearman <- function(x, y) { 
    per_y <- sample(1:length(y))
    return(cor(x, y[per_y], method = "spearman"))
}
aply_sp <- function(x, y, R, outcome){
for (i in 1:R) {
    outcome[i] <- Spearman(x, y)
}
    return(outcome)
}
x<- as.numeric(iris[1:50, 2])
y<- as.numeric(iris[1:50, 4])
R <- 999
set.seed(12345)
outcome <- numeric(R)
outcome <- aply_sp(x, y, R, outcome)
t0<-(cor(x, y))
p_s <- mean(abs(c(t0, outcome)) >= abs(t0))
p_nor <- cor.test(x, y)$p.value
save(p_s, p_nor, file = "3data.RData")
rm(list = ls())

## -----------------------------------------------------------------------------
load("3data.RData")
report <- function(p_s, p_nor){
    cat("significance level of the permutation test: ", p_s, "\n",
        "p value derived from cor.test on the same samples: ", p_nor)
}
report(p_s, p_nor)
rm(list = ls())

## -----------------------------------------------------------------------------
rm(list = ls())
set.seed(123)
rw.Metropolis <- function(sigma, x0, N) { 
    x <- numeric(N) 
    x[1] <- x0 
    u <- runif(N) 
    k<-0 
    for (i in 2:N) { 
        y <- rnorm(1, x[i-1], sigma) 
        if (u[i] <= exp(abs(x[i - 1]) - abs(y))) 
            x[i] <- y 
        else { 
            x[i] <- x[i-1] 
            k<-k+1
        } 
        }
    return(list(x=x, k=k))
}

## -----------------------------------------------------------------------------
cal_R_hat <- function(psi) { 
        # psi[i,j] is the statistic psi(X[i,1:j]) 
        # for chain in i-th row of X
    psi <- as.matrix(psi) 
    n <- ncol(psi) 
    k <- nrow(psi)
    psi.means <- rowMeans(psi)     #row means 
    B <- n * var(psi.means)        #between variance est.
    psi.w <- apply(psi, 1, "var")  #within variances 
    W <- mean(psi.w)               #within est.
    v.hat <- W*(n-1)/n + (B/n) 
    r.hat <- v.hat / W
    return(r.hat)
    }

## -----------------------------------------------------------------------------
list_2_vec<- function(X, N){
    X_v <- numeric(N)
    X_v <- X$x
    return(X_v)
}

## -----------------------------------------------------------------------------
Gelman <- function(n, b, sigma, k){
    x_i <- c(-10, -5, 5, 10)
    X <- matrix(0, nrow=k, ncol=n)
    for (i in 1:k){ 
        rw <- rw.Metropolis(sigma, x_i[i], n) 
        X[i, ] <- list_2_vec(rw, n)
    }
    psi <- t(apply(X, 1, cumsum)) 
    for (i in 1:nrow(psi)) 
        psi[i,] <- psi[i,] / (1:ncol(psi))
    print(cal_R_hat(psi))
    for (i in 1:k) 
        plot(psi[i, (b+1):n], 
            type="l", 
            xlab=i, 
            ylab=bquote(psi))
    rhat <- rep(0, n) 
    for (j in (b+1):n) 
        rhat[j] <- cal_R_hat(psi[,1:j]) 
    plot(rhat[(b+1):n], type="l", main = paste("sigma: ",sigma, ", n: ", n, ", burn: ", b), xlab="n - burn", ylab="R")
    abline(h=1.2, lty=2)
}

## -----------------------------------------------------------------------------
N <- 2000 
sigma <- c(.5, 2, 8, 16)
x0 <- 25 
rw1 <- rw.Metropolis(sigma[1], x0, N) 
rw2 <- rw.Metropolis(sigma[2], x0, N) 
rw3 <- rw.Metropolis(sigma[3], x0, N) 
rw4 <- rw.Metropolis(sigma[4], x0, N)
#number of candidate points rejected
print(c(rw1$k, rw2$k, rw3$k, rw4$k) / N)
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
for (j in 1:4) {
plot((rw)[,j], 
     type="l",
     xlab=bquote(sigma == .(round(sigma[j],3))),
     ylab="X", 
     ylim=range(rw[,j]))
}

## -----------------------------------------------------------------------------
k <- 4
n <- c(20000, 2000, 2000, 1500)
b <- c(5000, 500, 200, 100)
for (i in 1:k) {
    Gelman(n[i], b[i], sigma[i], k)
}
rm(list=ls())
gc()

## -----------------------------------------------------------------------------
rm(list = ls())
set.seed(123)
MC_bnorm <- function(X, N, mu_i1, mu_i2){
    rho <- .9
    #correlation
    mu1 <- mu2 <- 0
    sigma1 <-sigma2 <- 1 
    s1 <- sqrt(1-rho^2)*sigma1
    s2 <- sqrt(1-rho^2)*sigma2
    X[1, ] <- c(mu_i1, mu_i2) #initialize
    for (i in 2:N) { 
        y <- X[i-1, 2] 
        m1 <- mu1 + rho * (y - mu2) * sigma1/sigma2 
        X[i, 1] <- rnorm(1, m1, s1) 
        x <- X[i, 1] 
        m2 <- mu2 + rho * (x - mu1) * sigma2/sigma1 
        X[i, 2] <- rnorm(1, m2, s2)
        }
    return(X)
}

## -----------------------------------------------------------------------------
cal_R_hat <- function(psi) { 
        # psi[i,j] is the statistic psi(X[i,1:j]) 
        # for chain in i-th row of X
    psi <- as.matrix(psi) 
    n <- ncol(psi) 
    k <- nrow(psi)
    psi.means <- rowMeans(psi)     #row means 
    B <- n * var(psi.means)        #between variance est.
    psi.w <- apply(psi, 1, "var")  #within variances 
    W <- mean(psi.w)               #within est.
    v.hat <- W*(n-1)/n + (B/n) 
    r.hat <- v.hat / W
    return(r.hat)
}

## -----------------------------------------------------------------------------
Gelman <- function(n, b, k, margin = T){
    x_i <- c(0, 0, 5, 5, -5, -5, 10, 10)
    X <- matrix(0, nrow = k, ncol=n)
    for (i in 1:k){ 
            tmp <- matrix(0, N, 2)
            tmp <- MC_bnorm(tmp, N, x_i[2 * i - 1], x_i[2 * i])
        if(margin)
            {X[i, ] <- tmp[, 1]}
        else
            {X[i, ] <- tmp[, 2]}
        }
    psi <- t(apply(X, 1, cumsum)) 
    for (i in 1:nrow(psi)) 
        psi[i,] <- psi[i,] / (1:ncol(psi))
    print(cal_R_hat(psi))
    for (i in 1:k) 
        plot(psi[i, (b+1):n], 
             type="l", 
             xlab=i, 
             ylab=bquote(psi))
    rhat <- rep(0, n) 
    for (j in (b+1):n) 
        rhat[j] <- cal_R_hat(psi[,1:j]) 
    plot(rhat[(b+1):n], type="l", xlab="n - burn", ylab="R")
    abline(h=1.2, lty=2)
}

## -----------------------------------------------------------------------------
N <- 5000 #length of chain 
 burn <- 100 #burn-in length
 X <- matrix(0, N, 2) #the chain, a bivariate sample 
 X <- MC_bnorm(X, N, 0, 0)
 b <- burn + 1
 X <- X[b:N, ]
X_f <- as.data.frame(X)
names(X_f)[1] <- "x"
names(X_f)[2] <- "y"
X_l <- lm(y ~ x, X_f)
plot(X, 
     main="Generated sample after burning out 0.1k", 
     cex=.5, 
     xlab=bquote(X), 
     ylab=bquote(Y), 
     ylim=range(X[,2]))
abline(X_l, col = "Chocolate")

## -----------------------------------------------------------------------------
(X_reg <- summary(X_l))

## -----------------------------------------------------------------------------
k <- 4
Gelman(N, burn, k, T)
Gelman(N, burn, k, F)
rm(list=ls())
gc()

## -----------------------------------------------------------------------------
test <- function(alpha, beta, gamma, N, X, em, ey){
    a_m <- 1
    a_y <- 1
    M <- a_m + alpha * X + em
    Y <- a_y + beta * M +gamma * X + ey
    data.one <- data.frame(X, M, Y)
    X.M <- lm(M ~ X, data = data.one)
    X_M.Y <- lm(Y ~ X + M, data = data.one)
    cont <- mediate(X.M, X_M.Y, treat = "X", mediator = "M", sims = 100, boot = T)
    tmp1 <- summary(X.M)
    tmp2 <- summary(X_M.Y)
    tmp3 <- summary(cont)
    print(tmp1) #print the result regression of x to M and X,M to Y as well as result of using mediation package
    print(tmp2)
    print(tmp3)
    plot(cont)
}


## ----  warning=FALSE, message=FALSE-------------------------------------------
library(mediation)
set.seed(123)
gamma <- 1
N <- 1e3
em <- rnorm(N)
ey <- rnorm(N)
X <- rpois(N, 1)
for (alpha in 0:1) {
    for (beta in 0:1) {
        if((alpha ==1) && (beta ==1)) break
        test(alpha, beta, gamma, N, X, em, ey)
    }
}
rm(list=ls())
gc()

## -----------------------------------------------------------------------------
logi.reg <- function(N, b1, b2, b3, f0){
    x1 <- rpois(N, 1)
    x2 <- rexp(N, 1)
    x3 <- rbinom(N, 1, .5)
    g <- function(alpha){
        tmp <- exp(-alpha - b1 * x1 - b2 * x2 - b3 * x3) 
        p <- 1/(1 + tmp)
        mean(p) - f0
    }
    solution <- uniroot(g,c(-20,0))
    return(solution$root)
}

## -----------------------------------------------------------------------------
root<- numeric(4)
f<- numeric(4)
f
for (i in 1:4 ) {
    f[i] <- 1 / 10 ^ i
    root[i] <- logi.reg(1e6, 0, 1, -1, f[i])
    paste("when f0 is: ", f, "root is: ", root[i], "\n")
}

## -----------------------------------------------------------------------------
plot(-log(f, base = 10), root)
rm(list=ls())
gc()

## -----------------------------------------------------------------------------
EM_way <- function(interval, lam_0){
    n <- nrow(interval)
    summ <- 0
    for (i in 1:n) {
        summ <-summ + (interval[i, 1] * exp(-lam_0 * interval[i, 1])-interval[i, 2] * exp(-lam_0 * interval[i, 2]))/(exp(-lam_0 * interval[i, 1])-exp(-lam_0 * interval[i, 2])) 
    }
   lam_1 <- n / (summ + n/lam_0)
   if(abs(lam_0 - lam_1) <= 1e-10) return(lam_1)
   else return(EM_way(interval, lam_1))
}
ob_way <- function(lam_o){
    for (i in 1:10) {
    ob<-ob+(interval[i, 2]*exp(-interval[i, 2]*lam_o)-interval[i, 1]*exp(-interval[i, 1]*lam_o))/(exp(-interval[i, 2]*lam_o)-exp(-interval[i, 1]*lam_o))
    }
  f<-ob

  f
}


## ---- message=FALSE, warning=FALSE--------------------------------------------
library(BB)
ob <- 0
lam_0 <- 1
interval <- matrix(c(11, 8, 27, 13, 16, 0, 23, 10, 24, 2, 12, 9, 28, 14, 17, 1, 24, 11, 25, 3), 10, 2)
paste("lambda hat derived from EM method is:", EM_way(interval, lam_0))
starl<-c(0.7)
result=dfsane(starl,ob_way,control = list(maxit=2500,trace=FALSE))
paste("lambda hat derived from observed data is : ", result$par)
rm(list=ls())

## -----------------------------------------------------------------------------
test.l.1 <-list(1:4, "a", T, list())
test.l.2 <- as.vector(test.l.1)
test.l.3 <- unlist(test.l.1)
str(test.l.1)
str(test.l.2)
str(test.l.3)
rm(list=ls())

## -----------------------------------------------------------------------------
dim(1:10)
dim(list(1, "a", T))
rm(list=ls())

## -----------------------------------------------------------------------------
df <- data.frame(x = 1:4, y = c("a", "b", "c", "d"), stringAsFactors = FALSE)
str(df)
attributes(df)
rm(list=ls())

## -----------------------------------------------------------------------------
df <- data.frame(x = 1:4, y = c("a", "b", "c", "d"), z <- c(T, T, F, T), stringAsFactors = FALSE)
test <- as.matrix(df)
typeof(test)
str(test)
rm(list=ls())

## -----------------------------------------------------------------------------
test=data.frame(col1=character(0),col2=numeric(0),col3=logical(0))
str(test)
rm(list=ls())

## -----------------------------------------------------------------------------
test=matrix(data=NA,nrow = 0,ncol = 3)
test=as.data.frame(test)
colnames(test)=c("col1","col2","col3")
str(test)
rm(list=ls())

## -----------------------------------------------------------------------------
df.empty <- data.frame(row1="",row2="",row3="")[-1,]
str(df.empty)
rm(list=ls())

## -----------------------------------------------------------------------------
library(knitr)
scale01 <- function(x) {       
  rng <- range(x, na.rm = TRUE) 
  (x - rng[1]) / (rng[2] - rng[1])       
}
df <- data.frame(lapply(mtcars,function(x) scale01(x) ))
df
rm(list=ls())

## -----------------------------------------------------------------------------
scale01 <- function(x) {       
  rng <- range(x, na.rm = TRUE) 
  (x - rng[1]) / (rng[2] - rng[1])       
}
df <- data.frame(lapply(iris, function(x) if (is.numeric(x)) scale01(x) else x))
df
rm(list=ls())

## -----------------------------------------------------------------------------
library(knitr)
data <- vapply(mtcars, 
       sd, 
       numeric(1))
kable(data, 
      "pipe",
      align = "l")
rm(list=ls())

## -----------------------------------------------------------------------------
library(knitr)
data <- vapply(iris[vapply(iris, is.numeric, logical(1))],
       sd, 
       numeric(1))
kable(data, 
      "pipe",
      align = "l")
rm(list=ls())

## -----------------------------------------------------------------------------
library(Rcpp)
cppFunction(code='NumericMatrix GibbsC(int N, double mu_i1, double mu_i2){
    NumericMatrix mat(N, 2);
    double rho = .9;
    double mu1 = 0, mu2 = 0;
    double sigma1 = 1, sigma2 = 1; 
    double s1 = sqrt(1 - rho * rho) * sigma1;
    double s2 = sqrt(1 - rho * rho) * sigma2;
    double x =0, y = 0, m1 = 0, m2 = 0;
    NumericVector tmp(1);
    mat(0,0) = mu_i1;
    mat(0,1) = mu_i2;
    for (int i=1; i < N; ++i) { 
        y = mat(i-1, 1); 
        m1 = mu1 + rho * (y - mu2) * sigma1/sigma2; 
        tmp = rnorm(1, m1, s1);
        mat(i, 0) = tmp[0]; 
        x = mat(i, 0); 
        m2 = mu2 + rho * (x - mu1) * sigma2/sigma1; 
        tmp = rnorm(1, m2, s2);
        mat(i, 1) = tmp[0];
    }
    return mat;
}')


## -----------------------------------------------------------------------------
GibbsR <- function(N, mu_i1, mu_i2){
    X <- matrix(0, N, 2)
    rho <- .9
    #correlation
    mu1 <- mu2 <- 0
    sigma1 <-sigma2 <- 1 
    s1 <- sqrt(1-rho^2)*sigma1
    s2 <- sqrt(1-rho^2)*sigma2
    X[1, ] <- c(mu_i1, mu_i2) #initialize
    for (i in 2:N) { 
        y <- X[i-1, 2] 
        m1 <- mu1 + rho * (y - mu2) * sigma1/sigma2 
        X[i, 1] <- rnorm(1, m1, s1) 
        x <- X[i, 1] 
        m2 <- mu2 + rho * (x - mu1) * sigma2/sigma1 
        X[i, 2] <- rnorm(1, m2, s2)
        }
    return(X)
}

## ----warning=F----------------------------------------------------------------
library(Rcpp)
library(microbenchmark)
library(RVAideMemoire)
N <- 5000
burn <- 500
Cnorm <- matrix(0, N, 2)
Rnorm <- matrix(0, N, 2)
Cnorm_b <- matrix(0, N-burn, 2)
Rnorm_b <- matrix(0, N-burn, 2)
Cnorm <- GibbsC(N, 0, 0)
Rnorm <- GibbsR(N, 0, 0)
Cnorm_b <- Cnorm[burn:N ,]
Rnorm_b <- Rnorm[burn:N ,]
mqqnorm(Cnorm_b, main = "Multi-normal Q-Q Plot with C")
mqqnorm(Rnorm_b, main = "Multi-normal Q-Q Plot with R")
qqplot(Cnorm[, 1], 
       Rnorm[, 1], 
       xlab = "Xt with cpp", 
       ylab = "Xt with R")
qqplot(Cnorm[, 2], 
       Rnorm[, 2], 
       xlab = "Yt with cpp", 
       ylab = "Yt with R")

## -----------------------------------------------------------------------------
ts <- microbenchmark(GibbsC(N, 0, 0), GibbsR(N, 0, 0))
ts
rm(list=ls())

