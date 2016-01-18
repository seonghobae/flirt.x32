\name{VerbAgg}

\alias{VerbAgg}

\title{Verbal Aggression item responses}

\description{
   These are the item responses to a questionnaire on verbal aggression. These data are used throughout
De Boeck, P. and Wilson, M. (2004) to illustrate various forms of item response models.
}

\format{
A data frame with 7,584 observations on the following 13 variables.
  \describe{
    \item{Anger}{the subject's Trait Anger score as measured on the State-Trait Anger Expression Inventory
(STAXI)} 
    \item{Gender}{the subject's gender - a factor with levels M and F}
    \item{item}{the item on the questionnaire, as a factor}
    \item{resp}{the subject's response to the item - an ordered factor with levels no < perhaps < yes}
    \item{id}{the subject identifier, as a factor}
    \item{btype}{behavior type - a factor with levels curse, scold and shout}
    \item{situ}{situation type - a factor with levels other and self indicating other-to-blame and self-to blame}
    \item{mode}{behavior mode - a factor with levels want and do}
    \item{r2}{dichotomous version of the response - a factor with levels N and Y}
    

  }

}
\source{
  \url{http://bear.soe.berkeley.edu/EIRM}
}
\references{
  De Boeck, P. and Wilson, M. (2004), Explanatory Item Response Models, Springer.
}

\examples{

str(VerbAgg)

}


\keyword{datasets} 

