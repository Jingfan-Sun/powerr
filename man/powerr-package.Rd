\name{powerr-package}
\alias{powerr-package}
\docType{package}
\title{
Power System Analysis in R
}
\description{
powerr is used for electric power system analysis and control, 
    including basic power flow analysis and CASCADE analysis.
    Both sparse and dense complex matrix approach can be used.
    Other features will be updated soon.
}
\details{
\tabular{ll}{
Package: \tab powerr\cr
Type: \tab Package\cr
Version: \tab 0.1-3\cr
Date: \tab 2014-09-17\cr
License: \tab GPL (>= 2)\cr
}
First, use \code{powerInit()} to initialize variables.

Then, you can set global settings by using \code{Settings$settingName <- settingValue}.
For example, to use sparse approach, type \code{Settings$sparse <- TRUE}.

Use \code{powerData(dataName, dataPath)} to load data. For example, 
use \code{powerData('d_003.R')} to load a three-bus demo provided.

Use \code{powerPF()} to run power flow analysis.

Use \code{powerCASCADE()} to run CASCADE model.
}
\author{
Jingfan Sun

Maintainer: Jingfan Sun <sunjfsjfan@gmail.com>
}
\references{
Milano, F. (2010). Power system modelling and scripting. London, Dordrecht, Heidelberg, New York: Springer.

Eddelbuettel, D. (2013). Seamless R and C++ Integration with Rcpp. Springer.

Wu, H., & Dobson, I. (2012). Cascading stall of many induction motors in a simple system. Power Systems, IEEE Transactions on, 27(4), 2116-2126.

Milano, F. (2005). An open source power system analysis toolbox. Power Systems, IEEE Transactions on, 20(3), 1199-1206.

Wang, X., Sing, Y., & Irving, M. (2008). Modern Power Systems Analysis (Power Electronics and Power Systems).
}
\keyword{ power system, CASCADE }
\seealso{
\code{\link{powerInit}}, \code{\link{powerData}}, \code{\link{powerPF}}, \code{\link{powerCASCADE}}
}
\examples{
powerInit()
Settings$sparse <- TRUE
powerData('d_003.R')
powerPF()
}
