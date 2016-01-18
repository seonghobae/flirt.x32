\name{item_design_bin}

\alias{item_design_bin}

\title{Verbal Aggression Data: item covariates}

\description{
   Data frame for item design matrix with binary responses for the verbal aggression data. 
   There are five columns for for intercept, four item covariates (dowant, otherself, blame, express). 
   
}

\format{
A data frame in size 24 by 5.
  \describe{
    \item{intercept:}{1}
    \item{dowant:}{ do:1 vs. want:0 }    
    \item{otherself:}{ other-to-blame:0, self-to-blame:1 }  
    \item{blame:}{ curse:0.5, scold:0.5 vs. shout:-1 }
    \item{express:}{ scold:-1 vs. curse:0.5, shout:0.5 }
  
  }

}
\references{
  De Boeck, P. and Wilson, M. (2004), Explanatory Item Response Models, Springer.
}

\examples{

str(item_design_bin)

}


\keyword{datasets} 
