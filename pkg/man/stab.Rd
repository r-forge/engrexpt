\name{stab}
\Rdversion{1.1}
\alias{stab}
\docType{data}
\title{
  Stability of a chemical product
}
\description{
  Stability of a chemical product
}
\format{
  A data frame with 12 observations on the following 4 variables.
  \describe{
    \item{\code{temp}}{a numeric factor at two levels}
    \item{\code{humidity}}{a numeric factor at two levels}
    \item{\code{time}}{a numeric factor at three levels}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
  The stability of the active ingredient is critical.  This study looks
  at the impact of temperature and humidity on the stability of the
  active ingredient over time.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(stab)
}
\keyword{datasets}
