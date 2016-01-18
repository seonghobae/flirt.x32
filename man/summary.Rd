\name{summary}


\alias{summary.flirt}

\title{Summary method for fitted models }

\description{
   A generic function to produce result summaries from \code{flirt}  objects.
}

\usage{
\method{summary}{flirt}(object, \dots)

}

\arguments{
  \item{object}{an object inheriting from class \code{flirt}. }
  \item{\dots}{additional arguments; currently none is used. }

}

\details{

}
\value{
  An object of class \code{summary.flirt} with components,
  \item{Data}{nobs: number of observations (nobs), nitem: number of items, 
                maxcat: maximum response category,  and ngroup: number of groups }
  \item{Model fit}{npar: number of parameters, AIC, BIC, and loglik}
  \item{Parameterization}{for 2PL models, \eqn{\alpha \theta + \beta}  or \eqn{\alpha(\theta + \beta)}  }
  \item{Type}{between-item or within-item multidimensional models}
  \item{Dimension}{number of dimensions and number of items in each dimension for multidimensional and bifactor models}
  \item{Parameter estimates}{parameter estimates and standard errors. }
 
 }

\references{
}

\author{
  Minjeong Jeon <jeon.117@osu.edu>
}

\note{
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

summary(model1)

}


\keyword{methods} 
