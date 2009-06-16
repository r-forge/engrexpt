\name{drought}
\Rdversion{1.1}
\alias{drought}
\docType{data}
\title{
  Water usage in 2001 and 2002
}
\description{
  In 2002, Colorado experienced a drought and many towns on the front
  range (a geographical district close to the Rocky mountains) issued
  mandatory or voluntary water restirctions on outdoor watering.  These
  data are comparative water usage between 2001 and 2002 for five such
  towns.
}
\format{
  A data frame with 5 observations on the following 3 variables.
  \describe{
    \item{\code{town}}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{E}}
    \item{\code{j2001}}{water usage (millions of gallons) in 2001}
    \item{\code{j2002}}{water usage (millions of gallons) in 2001}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(drought)
}
\keyword{datasets}
