\name{IRF}

\alias{IRF.flirt}

\title{Plot item response curves}

\description{
    A function to plot item response curves as a function of latent ability for unidimensional 1PL, 2PL, and 3PL models. 
}

\usage{
\method{IRF}{flirt}(alpha=NULL, beta, guess=NULL , inside=NULL, ylim=NULL)

}

\arguments{
  \item{alpha}{ \eqn{\alpha_i} parameter estimates. If \code{NULL}, \eqn{\alpha_i=1}  }
  \item{beta}{ \eqn{\beta_i} parameter estimates }
  \item{guess}{ \eqn{c_i} (guessing) parameter estimates. If \code{NULL}, \eqn{c_i=0}  } 
  \item{inside}{ If \code{TRUE}, \eqn{P_i = c_i + \frac{(1-c_i)}{1+ exp(-\alpha_i (\theta_p + \beta_i))}} is estimated } 
  \item{ylim}{ the y limits of the plot.}

}

\details{

    For single and multiple items.

}


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
result2 <- flirt(data=verb2, loading=list(on=T, inside=T), control=list(nq=5) )
alpha <- result2@pars[1:24,2]
beta <- result2@pars[25:48,1]

# item resonse curve for four items              
IRF(beta=beta[1:4], alpha=alpha[1:4], inside=T)

}


\keyword{methods}
