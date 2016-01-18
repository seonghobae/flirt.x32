\name{anova}

\alias{anova.flirt}

\title{Anova method for fitted nested models}

\description{
  A generic function to conduct a likelihood ratio test between two nested models. 
}

\usage{
\method{anova}{flirt}(object, object2, \dots)

}

\arguments{
  \item{object}{an object inheriting from class \code{flirt} for the model under the null hypothesis.}
  \item{object2}{an object inheriting from class \code{flirt} for the model under the alternative hypothesis.}
  \item{\dots}{additional arguments; currently none is used.}
}

\details{
}


\value{
An object of either class \code{anova.flirt} with components,
    \item{name0}{the name of \code{object}.}
    \item{L0}{the log-likelihood under the null hypothesis (\code{object}).}
    \item{aic0}{the AIC value for the model given by \code{object}.}
    \item{bic0}{the BIC value for the model given by \code{object}. }
    \item{name1}{the name of \code{object2}.}
    \item{L1}{the log-likelihood under the alternative hypothesis (\code{object2}).}
    \item{aic1}{the AIC value for the model given by \code{object2}.}
    \item{bic1}{the BIC value for the model given by \code{object2}.}
    \item{LRT}{the value of the Likelihood Ratio Test statistic. }
    \item{df}{the degrees of freedom for the test (i.e., the difference in the number of parameters).}
    \item{p.value}{the \eqn{p}-value of the test.}
}

\section{Warning}{
    The code does not check if the models are nested. The user is responsible to supply nested models in
    order for the LRT to be valid.
}



\author{
  Minjeong Jeon <jeon.117@osu.edu>
}

\note{
}

\seealso{
 \code{\link{flirt}}, \code{\link{logLik}}
}

\examples{

library(flirt)

## verbal aggression data

data(verb2)

# 2-dimensional 2PL model for binary data: a(th+b) parameterization
model1 <- flirt(data=verb2, select=2:25, loading=list(on=TRUE, inside=TRUE),  
            mul=list(on=TRUE, dim_info=list(dim1=1:12, dim2=13:24))  )

#  2-dimensional Rasch model 
model2 <- flirt(data=verb2, select=2:25, 
            mul=list(on=TRUE, dim_info=list(dim1=1:12, dim2=13:24))  )

# LR test
anova(model2, model1) # model1 is nested within model2



}


\keyword{methods} 
