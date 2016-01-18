\name{logLik}

\alias{logLik.flirt}

\title{Extract log-likelihood}

\description{
    A generic function to extract the log-likelihood from \code{flirt} objects.

}

\usage{
\method{logLik}{flirt}(object, \dots)

}

\arguments{
  \item{object}{an object inheriting from class \code{flirt}.}
  \item{\dots}{additional arguments; currently none is used. }

}


\value{Returns an object of class \code{logLik} giving  
         the log-likelihood value and \code{npar} (number of freely estimated parameters) }

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

# 2-dimensional 2PL model  for binary data: a(th+b) parameterization
model1 <- flirt(data=verb2, select=2:25, loading=list(on=TRUE, inside=TRUE),  
            mul=list(on=TRUE, dim_info=list(dim1=1:12, dim2=13:24))  )

logLik(model1)


}


\keyword{methods}
