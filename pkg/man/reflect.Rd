\name{reflect}
\Rdversion{1.1}
\alias{reflect}
\docType{data}
\title{
  Anti-reflective coating measurements
}
\description{
  Anti-reflective coating measurements
}
\format{
  A data frame with 8 observations on the following 5 variables.
  \describe{
    \item{\code{binder}}{a factor with levels \code{A} and \code{B}}
    \item{\code{base}}{a factor with levels \code{A} and \code{B}}
    \item{\code{time}}{a factor with levels \code{H} and \code{L}}
    \item{\code{ratio}}{a factor with levels \code{A} and \code{B}}
    \item{\code{reflect}}{a numeric vector}
  }
}
\details{
  This experiment was to test components in an anti-reflective coating.
  The goal was to minimize the reflectance (\code{reflect}).
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(reflect)
}
\keyword{datasets}
