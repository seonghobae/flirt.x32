\name{spelling_w}

\alias{spelling_w}

\title{spelling item responses with frequency weights}

\description{
   This data come from Thissen et al. (1993, p.71) that examined performance of under-
   graduates on four spelling items `infidelity', `panoramic', `succumb' and
   `girder'. Each spelling item was scored either as correct or incorrect.
   The sample includes 285 male and 374 female undergraduate students from the University of Kansas. 

}

\format{
A data frame with 30 observations on the four spelling items. 
There is one covariate, \code{male} that takes value 1 for male and 0 for female.
There is a frequency weight, \code{wt2} that indicates the number of people who share the same item response patterns and gender.
 
}

\references{
Thissen, D., L. Steinberg, and H. Wainer. 1993. Detection of differential item functioning
using the parameters of item response models. In Differential Item Functioning, ed.
P. Holland and H. Wainer, 67-114. Hillsdale, NJ: Lawrence Erlbaum.
}

\examples{

str(spelling_w)

}


\keyword{datasets}
