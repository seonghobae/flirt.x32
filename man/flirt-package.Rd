\name{flirt-package}
\alias{flirt-package}

\docType{package}

\title{
Flexible item response theory modeling with efficient maximum likelihood (ML) estimation
}

\description{
This package provides a flexible framework for uni- and multi- dimensional explanatory item response theory modeling 
for binary and polytomous item responses.  The flexibility stems from specifying IRT models 
as generalized linear and nonlinear mixed models (Rijmen, Tuerlinckx, De Boeck, & Kuppens, 2003).

For estimation, the package \code{flirt} uses an efficient modified EM algorithm based on the graphical model framework. 
The modified EM algorithm is much faster than the traditional EM algorithm. 
For more details on the modified EM algorithm, refer to e.g.,  Rijmen, F., Vansteelandt, K., & De Boeck, P. (2008). 
   
Currently, uni- and multi- dimensional Rasch models, two-parameter logistic (2PL) IRT models, and 
bifactor models are available with extensions to multiple groups, item covariates, person covariates, 
and differential item functioning analyses. 

The package \code{flirt} is based on the Matlab code BNLflirt (Rijmen and Jeon, 2013) for estimation 
that employs sub-functions from the Matlab toolbox BNL (Bayesian Networks with Logistic Regression
Nodes; Rijmen, 2006). 

\bold{flirt} requires  the Matlab Compiler Runtime (MCR) for Matlab 2014a, Windows. 
Having the correct version of the MCR is critical.  If you have a different version of Matlab on your computer, please make sure to 
remove the MCR that you have and  download/install the correct version in the following link:
 
Having the correct version of the MCR is critical.  If you have a different version of Matlab on your computer, please make sure to 
remove the MCR that you have and  download/install the correct version in the following link:

\code{http://www.mathworks.com/products/compiler/mcr/} 

To cite \code{flirt}, 

Jeon, M.,  Rijmen, F. & Rabe-Hesketh, S. (2014). 
Flexible item response theory modeling with \code{flirt}. Applied Psychological Methods, 38, 404-405

}

\details{
\tabular{ll}{
Package: \tab flirt\cr
Type: \tab Package\cr
Version: \tab 1.15\cr
Date: \tab 2015-1-18\cr
License: \tab \acronym{GPL} \cr

}

The R script containing sample analyses using \code{flirt} is available by contacting the first author.
}

\author{
Minjeong Jeon, Frank Rijmen, and 
Sophia Rabe-Hesketh

Maintainer:   Minjeong Jeon<jeon.117@osu.edu>
}

\references{

  Jeon, M. and Rijmen, F. (2014). A modular approach for item response theory modeling with the R package flirt. Under revision. 

  Jeon, M. and De Boeck, P. (2015). A generalized item response tree model for psychological assessments. Under revision. 

  Jeon, M.,  Rijmen, F. & Rabe-Hesketh, S. (2014). 
  Flexible item response theory modeling with \code{flirt}. Applied Psychological Methods, 38, 404-405

  Jeon, M., Rijmen, F., and Rabe-Hesketh, R. (2013). Modeling differential item functioning using 
  a generalization of the multiple-group bifactor model. Journal of Behavioral and Educational Statistics, 38, 32-60.

  Rijmen, F. (2006). BNL: A Matlab toolbox for Bayesian networks with logistic regression. Technical
  Report. Vrije Universiteit Medical Center, Amsterdam.

  Rijmen, F., Tuerlinckx, F., De Boeck, P., & Kuppens, P. (2003). A nonlinear mixed model framework 
  for item response theory. Psychological Methods, 8, 185-205.
  
  Rijmen, F., Vansteelandt, K., & De Boeck, P. (2008). Latent class models for diary method data:
  Parameter estimation by local computations. Psychometrika, 73, 167-182. 

  Rijmen, F. and Jeon, M. (2013). BNLflirt: Flexible item response theory modeling with BNL. Matlab file exchange. 
  
}

\keyword{package}
\keyword{IRT}
