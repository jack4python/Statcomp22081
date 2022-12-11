## ----eval=FALSE---------------------------------------------------------------
#  function(df, method, lam = 0, k = 1, testpoint=0){
#  #this is a self coded function to calculate coefficient of standard regression(method = 0),
#  #locally weighted linear regression(method =2) and ridge regression (method = 1)
#  #the return value of this function is either '-1' for input a singular feature matrix or matrix that
#  #is singular even after it got disposed by ridge or weighted method or return the objective coefficient matrix
#      mat <- matrix(unlist(df), ncol = dim(df)[2], byrow = F)
#      m <- dim(mat)[1]
#      n <- dim(mat)[2]
#      ones <- matrix(1, m, 1)
#      feat1 <- mat[, -n]
#      feat <- cbind(feat1, ones)
#  #adding an all-one column into feature matrix whose coefficient is the intercept of the linear model of the result
#      label <- mat[, n]
#  # feat stands for feature value matrix whereas label contains value of label value of trained sample
#      if(method == 0){
#  #preform standard regression
#          xTx <- t(feat)%*%feat
#          if(det(xTx)==0) {
#              print('feature matrix is singular, cannot do regular regression')
#              return(-1)
#          }
#          coeff <- solve(xTx) %*% (t(feat)%*%label)
#          return(coeff)
#      }
#      if(method == 1){
#  #preform ridge regression
#          xTx <- t(feat)%*%feat
#          eye <- diag(rep(1, n))
#          denom <- xTx + eye * lam
#          if(det(denom) == 0){
#              print('feature matrix is singular, cannot do ridge regerssion')
#              return(-1)
#          }
#          coeff <- solve(denom) %*% (t(feat)%*%label)
#          return(coeff)
#      }
#      if(method == 2){
#  #preform locally weighted linear regression
#          weights <- diag(rep(1, m))
#          for (i in 1:m) {
#              diffmat <- testpoint - feat[i,]
#              weights[i, i] <- exp(t(diffmat)%*%diffmat / -2 * k ^ 2)
#          }
#          xTx <- t(feat)%*%(weights%*%feat)
#          if(det(xTx) == 0){
#              print('feature matrix is singular, cannot do locally weighted linear regerssion')
#              return(-1)
#          }
#          coeff <- solve(xTx) %*% (t(feat)%*%(weights%*%label))
#          return(coeff)
#      }
#  }

## ----eval=FALSE---------------------------------------------------------------
#  summary(standard <- lm(y ~ ., data = longley))

## ----eval=TRUE----------------------------------------------------------------
library(MASS)
library(StatComp22081)
n <- dim(Boston)[1]
m <- dim(Boston)[2]
es_self <- es_r <- er_02 <- er_01 <- er_001 <- er_0001 <- numeric(n)
for (i in 1:n) {
    J1 <- regres(Boston[-i, ], 0)
    yhat1 <- t(J1)%*%as.matrix(c(as.matrix(Boston[i, -m]), 1), 14, 1)
    es_self[i] <- Boston[i, m] - yhat1
    
    J2 <- lm(medv ~ .,data = Boston[-i, ])
    yhat2 <- J2$coef[1]
    for (j in 1:(m-1)) {
       yhat2 <- yhat2 + Boston[i, j] * J2$coef[j+1] 
    }
    es_r[i] <- Boston[i, m] - yhat2
    
    J3 <- regres(Boston[-i, ], 1, lam = 0.2)
    yhat3 <- t(J3)%*%as.matrix(c(as.matrix(Boston[i, -m]), 1), 14, 1)
    er_02[i] <- Boston[i, m] - yhat3
    
    J4 <- regres(Boston[-i, ], 1, lam = 0.1)
    yhat4 <- t(J4)%*%as.matrix(c(as.matrix(Boston[i, -m]), 1), 14, 1)
    er_01[i] <- Boston[i, m] - yhat4
    
    J5 <- regres(Boston[-i, ], 1, lam = 0.01)
    yhat5 <- t(J5)%*%as.matrix(c(as.matrix(Boston[i, -m]), 1), 14, 1)
    er_001[i] <- Boston[i, m] - yhat5
    
    J6 <- regres(Boston[-i, ], 1, lam = 0.001)
    yhat6 <- t(J6)%*%as.matrix(c(as.matrix(Boston[i, -m]), 1), 14, 1)
    er_0001[i] <- Boston[i, m] - yhat6
}
mean(es_r^2)
mean(es_self^2)
mean(er_02^2)
mean(er_01^2)
mean(er_001^2)
mean(er_0001^2)



## ----eval=T-------------------------------------------------------------------
m_out <- dim(Boston)[1]
n_out <- dim(Boston)[2]
e_lw <- numeric(m_out)
for (i in 1:m_out) {
    v <- rbind(as.matrix(t(Boston)[-n_out, i]),1)
    coef <- regres(Boston[-i, ], 2, k=0.01, testpoint =v )
    yhat <- t(coef) %*% v
    e_lw[i] <- Boston[i, n_out] - yhat
}
mean(e_lw^2)

## -----------------------------------------------------------------------------
library(Rcpp)
cppFunction(code='double  targetoutofn(NumericVector x, int target, int n) {
    double prob[n+1][target+1];
    prob[0][0] = 1;
    for(int i = 1; i <= n; i++){
        prob[i][0] = prob[i-1][0]*(1-x[i-1]);
    }
    for(int i = 1; i <= n; i++){
        for(int k = 1; k <= i && k <= target; k++){
            prob[i][k] = prob[i-1][k] * (1-x[i-1]) + prob[i-1][k-1] * x[i-1];
        }
    }
    return prob[n][target];
    }')
prob <- c(0.5, 0.5, 0.5, 0.5, 0.5)
target <- 1
n <- 5
targetoutofn(prob, target, n)

