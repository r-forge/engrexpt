\name{thinfilm}
\Rdversion{1.1}
\alias{thinfilm}
\docType{data}
\title{
  Strength of thin film coatings
}
\description{
  Strength of thin film coatings
}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{material}}{a factor with levels \code{foil} \code{glass}}
    \item{\code{comp}}{a numeric vector}
    \item{\code{maxload}}{a numeric vector}
  }
}
\details{
  \dQuote{Thin films}, a coating cured on some substrate other than the
  actual product, are used by researchers in development.  Glass and
  foil are two substrates that are often used.  This study was used to
  study the impact of a component of the coating on the strength of the
  coating.  The researcher also wanted to know if the substrate used to
  cure the thin film had an impact on the results.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(thinfilm)
}
\keyword{datasets}
