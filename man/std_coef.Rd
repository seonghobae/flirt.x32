\name{std_coef}

\alias{std_coef}
\alias{std_coef.flirt}
\alias{std_cov}
\alias{std_cov.flirt}

\title{Standardized factor loading parameter estimates and correlation matrix}

\description{
    \code{std_coef} compute standardized factor loading parameter estimates and correlation matrix.

    \code{std_cov} scales a covariance matrix into the corresponding correlation matrix.    

}

\usage{
\method{std_coef}{flirt}(est, dim_info, cov_matrix)
\method{std_cov}{flirt}(dim_info, cov_matrix)
}

\arguments{
  \item{est}{a vector of loading parameter estimates from \code{flirt}}
  \item{dim_info}{a list of items for each dimension  }
  \item{cov_matrix}{a square numeric variance_covariance matrix}

}


\value{

        \code{std_coef} returns a list of standardized loading parameter estimates and correlation matrix.
       \code{std_cov} returns a correlation matrix. 
}


\author{
  Minjeong Jeon <jeon.117@osu.edu>
}


\seealso{
 \code{\link{flirt}}
}

\examples{

library(flirt)


## verbal aggression data: polytmous item responses 

data(verb2)

# 2-dimensional 2PL model for binary data 
# a*th + b parameterization 
model1 <- flirt(data=resp, loading=list(on=TRUE, inside=FALSE),  
    mul=list(on=TRUE, dim_info=list(dim1=1:12, dim2=13:24)) , control=list(nq=5) )

## function for standardized loadings and covariance matrix 
# length 24, unstandardized loading parameter estimates 
est <- coef(model1)[1:24,1] 

# length 3 (sd1, cov12, sd2) unstandardized sd and cov estimates vector   
cov_el <- coef(model1)[51:53,1] 

# covariance matrix 
cov_matrix <- matrix(c(cov_el[1]^2,cov_el[3], cov_el[3],cov_el[2]^2),2,2, byrow=FALSE) 

# list 
dim_info <- list(dim1=1:12, dim2=13:24) 

test0 <- std_coef(est=est, dim_info=dim_info, cov_matrix= cov_matrix) 
test1 <- std_cov(dim_info=dim_info, cov_matrix= cov_matrix)


}


\keyword{methods}
