\name{dendrify2}

\alias{dendrify2.flirt}

\title{Expandsion of item responses based on an item resonse tree}

\description{
    Expands a wide-form matrix of item responses based on an item response tree structure 

}

\usage{
\method{dendrify2}{flirt}(mat, cmx, missing.omit=FALSE, wide=FALSE)

}

\arguments{
  \item{mat}{ an integer matrix of IRT responses (columns represent items, rows represent respondents) }
  \item{cmx}{ a mapping matrix with as many rows as there are response options for the items }
  \item{missing}{ for a long-form data, the data lines with a missing item response are removed} 
  \item{wide}{ If \code{TRUE}, expanded data are provided in a wide form   } 


}

\references{
    De Boeck, P. and Partchev, I. (2012). IRTrees: Tree-Based Item Response Models of the GLMM
    Family, Journal of Statistical Software 48, 1-8. 

    Jeon, M. and De Boeck, P. (2015). A generalized item response tree model for psychological assessments. Under revision. 
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

# expand the data based on a tree structure (mapping matrix)
mapping <- cbind(c(0, 1, 1), c(NA, 0, 1))
wide <- dendrify2(verb2, mapping, wide=TRUE)    

}


\keyword{methods}
