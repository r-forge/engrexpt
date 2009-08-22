\name{ratings2}
\Rdversion{1.1}
\alias{ratings2}
\docType{data}
\title{
  Product ratings and moisture content
}
\description{
  Product ratings and moisture content
}
\format{
  A data frame with 26 observations on the following 2 variables.
  \describe{
    \item{\code{rating}}{a numeric vector}
    \item{\code{moisture}}{a numeric vector}
  }
}
\details{
  A company has a new raw material that they wish to quickly evaluate.
  To do so they rate how well their process is running at key steps when
  using the new material.  The rating is done on a scale of 1 - 10 at
  each key step and then averaged.  In addition the moisture level of
  the raw material was recorded for each batch.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(ratings2)
}
\keyword{datasets}
