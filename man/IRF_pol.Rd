\name{IRF_pol}

\alias{IRF_pol.flirt}

\title{Plot category response curves for polytomous items}

\description{
    A function to plot category response curves  as a function of a unidimensional ability 
    for unidimensional graded response models (with a cumulative logit link)

}

\usage{
\method{IRF_pol}{flirt}(alpha=NULL, beta, guess=NULL , inside=NULL, ylim=NULL)

}

\arguments{
  \item{alpha}{ \eqn{\alpha_i} parameter estimates. If \code{NULL}, \eqn{\alpha_i=1}  }
  \item{beta}{ \eqn{\beta_{ir}} parameter estimates for category \eqn{r} to item \eqn{i} }
  \item{guess}{ \eqn{c_i} (guessing) parameter estimates. If \code{NULL}, \eqn{c_i=0} } 
  \item{inside}{ If \code{TRUE}, \eqn{P_i = c_i + \frac{(1-c_i)}{1+ exp(-\alpha_i (\theta_p + \beta_{ir}))}} is estimated } 
  \item{ylim}{ the y limits of the plot.}

}

\details{

    For a single item with three to six response categories. 

}

\author{
  Minjeong Jeon <jeon.117@osu.edu>
}


\seealso{
 \code{\link{flirt}}
}

\examples{

library(flirt)

## charity data

result4 <- flirt(data=charity, subset=1:100, control=list(minpercent=0.05, nq=5, link="cumulative"))         

beta <- result4@pars[2:14,1]
       
# item response function        
IRF_pol(beta=beta[1:2])  # item 1 with three categories 
IRF_pol(beta=beta[3:5])  # item 2 with four categories 
        
}


\keyword{methods}
