#' @title self coded regression
#' @description apply standard,ridge or locally weighted regression(using Gaussian kernel) on a given data frame(the predicted variable has to be at the last column 
#' of the data frame like 'Boston' in MASS we used in vignettes )
#' @param df data frame containing the data of predicting variable as well as predicted variable,noted that the predicted variable has to be 
#' at the last column of the given data frame.
#' @param method 0 to preform standard regression,1 for ridge regression and 2 for locally weighted regression
#' @param lam lambda the multiple of identity matrix added to xTx in the process of ridge regression
#' @param k the constant needed in calculating the Gaussian kernel for locally weighted linear regression.
#' @param testpoint the point chosen for the locally weighted regression
#' @return the coefficient of the regression whose last column is the value of intercept
#' @examples
#' \dontrun{
#' coef <- regres(df, 1, lam = 0.2)
#' }
#' @importFrom Rcpp evalCpp
#' @export

regres <- function(df, method, lam = 0, k = 1, testpoint=0){
    #this is a self coded function to calculate coefficient of standard regression(method = 0),
    #locally weighted linear regression(method =2) and ridge regression (method = 1)
    #the return value of this function is either '-1' for input a singular feature matrix or matrix that
    #is singular even after it got disposed by ridge or weighted method or return the objective coefficient matrix
    mat <- matrix(unlist(df), ncol = dim(df)[2], byrow = F)
    m <- dim(mat)[1]
    n <- dim(mat)[2]
    ones <- matrix(1, m, 1)
    feat1 <- mat[, -n]
    feat <- cbind(feat1, ones)
    #adding an all-one column into feature matrix whose coefficient is the intercept of the linear model of the result
    label <- mat[, n]
    # feat stands for feature value matrix whereas label contains value of label value of trained sample   
    if(method == 0){
        #preform standard regression
        xTx <- t(feat)%*%feat
        if(det(xTx)==0) {
            print('feature matrix is singular, cannot do regular regression')
            return(-1)
        }
        coeff <- solve(xTx) %*% (t(feat)%*%label)
        return(coeff)
    }
    if(method == 1){
        #preform ridge regression
        xTx <- t(feat)%*%feat
        eye <- diag(rep(1, n))
        denom <- xTx + eye * lam
        if(det(denom) == 0){
            print('feature matrix is singular, cannot do ridge regerssion')
            return(-1)
        }
        coeff <- solve(denom) %*% (t(feat)%*%label)
        return(coeff)
    }
    if(method == 2){
        #preform locally weighted linear regression
        weights <- diag(rep(1, m))
        for (i in 1:m) {
            diffmat <- testpoint - feat[i,]
            weights[i, i] <- exp(t(diffmat)%*%diffmat / -2 * k ^ 2)
        }
        xTx <- t(feat)%*%(weights%*%feat)
        if(det(xTx) == 0){
            print('feature matrix is singular, cannot do locally weighted linear regerssion')
            return(-1)
        }
        coeff <- solve(xTx) %*% (t(feat)%*%(weights%*%label))
        return(coeff)
    }
}