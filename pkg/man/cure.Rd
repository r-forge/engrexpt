\name{cure}
\Rdversion{1.1}
\alias{cure}
\docType{data}
\title{
  Yield from a chemical curing process
}
\description{
  Process engineers wish to determine a \dQuote{bake window}, the
  combination of time and temperature in a curing process, that will
  maximize the yield of the product.
}
\usage{data(cure)}
\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{\code{time}}{a numeric vector}
    \item{\code{temp.s10.}}{a factor with levels \code{High} \code{Low} \code{Med}}
    \item{\code{yield}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(cure)
}
\keyword{datasets}
