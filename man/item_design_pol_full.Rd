\name{item_design_pol_full}

\alias{item_design_pol_full}

\title{Verbal Aggression Data: item covariates}

\description{
   Data frame for item design matrix for polytomous responses for the verbal aggression data. 
   There are 29 columns for intercept, four item covariates (blame, express, dowant, otherself), 
   and 24 variables for category 2 (compared to 1). 
   Two adjacent lines represent an item, one for category 1 and next for category 2 (category 0 is reference). 
   This item design matrix assumes that a difference between category 1 to 2 is different across items. 
   For equal category effects, only one extra column is needed instead of 24 columns (item_design_pol). 
   
}

\format{
A data frame in size 48 by 29.
  \describe{
    \item{intercept:}{1}
    \item{dowant:}{ do:1 vs. want:0 }    
    \item{otherself:}{ other-to-blame:0, self-to-blame:1 }  
    \item{blame:}{ curse:0.5, scold:0.5 vs. shout:-1 }
    \item{express:}{ scold:-1 vs. curse:0.5, shout:0.5 }
    \item{cat_i1 to cat_i24:}{category 1 (perhaps):0, category 2 (yes):1 for item 1 to 24}    
  }

}
\references{
  De Boeck, P. and Wilson, M. (2004), Explanatory Item Response Models, Springer.
}

\examples{

str(item_design_pol_full)

}


\keyword{datasets} 
