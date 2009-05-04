\name{moisture}
\Rdversion{1.1}
\alias{moisture}
\docType{data}
\title{
  Moisture content of a silica product
}
\description{
  Moisture content of a silica product
}
\format{
  A data frame with 8 observations on the following 5 variables.
  \describe{
    \item{\code{temp}}{temperature in degrees Fahrenheit - an ordered
      factor with levels \code{200} < \code{300}}
    \item{\code{speed}}{process speed - an ordered factor with levels
      \code{A} and \code{B}}
    \item{\code{solids}}{percent solids - an ordered factor with levels
      \code{10} < \code{15}}
    \item{\code{pH}}{pH of the process - an ordered factor with levels
      \code{6.5} < \code{7}}
    \item{\code{moisture}}{moisture content of the product}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(moisture)
}
\keyword{datasets}
