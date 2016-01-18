\name{item_design_pol}

\alias{item_design_pol}

\title{Verbal Aggression Data: item covariates}

\description{
   Data frame for item design matrix for polytomous responses for the verbal aggression data. 
   There are six columns for intercept, four item covariates (dowant, otherself, blame, express), 
   and one variable for category 2 (compared to 1) (same for 24 items). 
   Two adjacent lines represent an item, one for category 1 and next for category 2 (category 0 is reference). 
   This item design matrix assumes that a difference between category 1 to 2 is the same for all items. 
   For unequal category effects, 24 extra columns (one for each item) are needed instead of one column (item_design_pol_full). 
   
}

\format{
A data frame in size 48 by 6.
  \describe{
    \item{intercept:}{1}
    \item{dowant:}{ do:1 vs. want:0 }    
    \item{otherself:}{ other-to-blame:0, self-to-blame:1 }  
    \item{blame:}{ curse:0.5, scold:0.5 vs. shout:-1 }
    \item{express:}{ scold:-1 vs. curse:0.5, shout:0.5 }
    \item{category:}{ category 1 (perhaps):0, category 2 (yes):1 }    
  }

}
\references{
  De Boeck, P. and Wilson, M. (2004), Explanatory Item Response Models, Springer.
}

\examples{

str(item_design_pol)

}


\keyword{datasets} 
