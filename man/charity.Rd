\name{charity}

\alias{charity}

\title{charity item responses}

\description{
This data come from the 38th round of the State Survey conducted by 
Michigan State University's Institute for Public Policy and Social Research (2005). 
The survey was administered to 949 Michigan citizens from May 28 to July 18, 2005, by telephone. 
The focus of the survey included charitable giving and volunteer activities of Michigan households.
Five questions measured the public's faith and trust in charity organizations. 

}

\format{
A data frame with 949 observations on the following five items:

\describe{
    \item{ta1}{Charitable organizations are more effective now in providing services than they
               were 5 years ago.}
    \item{ta2}{I place a low degree of trust in charitable organizations.}
    \item{ta3}{Most charitable organizations are honest and ethical in their use of donated funds.}
    \item{ta4}{Generally, charitable organizations play a major role in making our communities
          better places to live.}
    \item{ta5}{On the whole, charitable organizations do not do a very good job in helping those
               who need help.}
}

    The questions had four response categories corresponding to `strongly agree',
    `somewhat agree`, `somewhat disagree, and `strongly disagree. In this dataset,
    the responses are coded from 0 to 3, with larger scores indicating less favorable views of
    charities.
}

\references{
  Institute for Public Policy and Michigan State University Social Research. 2005. State
  of the state survey-38. Spring 2005. http://www.ippsr.msu.edu/SOSS.

}

\examples{

str(charity)

}


\keyword{datasets}
