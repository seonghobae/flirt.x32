\name{coef}

\alias{coef.flirt}

\title{Extract estimated coefficients}

\description{
    A generic function to extract parameter estimates from \code{flirt} objects.

}

\usage{
\method{coef}{flirt}(object, \dots)

}

\arguments{
  \item{object}{an object inheriting from class \code{flirt}.}
  \item{\dots}{additional arguments; currently none is used. }

}


\value{a matrix of the estimated parameters for the fitted model.}

\author{
  Minjeong Jeon <jeon.117@osu.edu>
}


\seealso{
 \code{\link{flirt}}
}

\examples{

library(flirt)


## verbal aggression data

data(verb2)

# 2-dimensional 2PL model  for binary data:  a(th+b) parameterization
model1 <- flirt(data=verb2, select=2:25, loading=list(on=TRUE, inside=TRUE),  
            mul=list(on=TRUE, dim_info=list(dim1=1:12, dim2=13:24))  )

coef(model1)

}


\keyword{methods}
