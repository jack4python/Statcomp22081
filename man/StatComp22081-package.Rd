\name{StatComp22081-package}
\alias{StatComp22081-package}
\alias{StatComp22081}
\docType{package}
\title{
  regression and calculation
}
\description{
  standard,ridge and locally weighted linear regression as well as a calculator of a delivery problem
}
\details{
  R package developed to compare the performance of a self coded function regres,which could preform standard regression,ridge regression and locally weighted linear regression (using Gaussian kernel) for a given data frame(the label variable has to stay at the last columns of the data frame,like 'Boston' in MASS),we compare the predicted value of label variable by the three means mentioned above with that calculated by function lm() using Cross-Validation method.
This package also contains a cpp function targetoutofn  to calculate the  probability of  a certain number 'target' out of n (target <= n) delivery man with respectively different probability to accept an order.
}
\author{
Hao Wu

Maintainer: Hao Wu <xjhfwh@gmail.com>
}
\references{
  Machine Learning in action
}
\keyword{ package }
\examples{
  \dontrun{
     ## regres(df, method, lambda, k, testpoint)
     ## These can be in \dontrun{} and \donttest{} blocks.   
  }
}
