\name{item_design_rating}

\alias{item_design_rating}

\title{Verbal Aggression Data}

\description{
   Data frame for item design matrix for for polytomous responses for the verbal aggression data. 
   There are 25 columns for 24 items and one variable for category 2 (compared to 1) (same for 24 items). 
   Two adjacent lines represent an item, one for category 1 and next for category 2 (category 0 is reference). 
   This item design matrix assumes that a difference between category 1 to 2 is the same for all items 
   (as in a rating scale model). 
   
}

\format{
A data frame in size 48 by 25.
  \describe{
    \item{i1 to i24}{indicator variable for item 1 to 24}
    \item{category:}{ category 1 (perhaps):0, category 2 (yes):1 }    
  }

}
\references{
  De Boeck, P. and Wilson, M. (2004), Explanatory Item Response Models, Springer.
}

\examples{

str(item_design_rating)

}


\keyword{datasets} 
