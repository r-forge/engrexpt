\name{pigment}
\Rdversion{1.1}
\alias{pigment}
\docType{data}
\title{
  Yellowing of paint for different pigments
}
\description{
  Yellowing of paint for different pigments
}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{batch}}{a factor with levels \code{A}, \code{B} and
      \code{C}}
    \item{\code{pigment}}{a factor with levels \code{A}, \code{B} and
      \code{C}}
    \item{\code{deltab}}{a numeric vector}
  }
}
\details{
  This experiment was to study the impact of pigments on the quality of
  a white paint.  The measure of interest was delta b (smaller is
  better).  Three batches of paint were use to test each of the three
  pigments (so there were three measurements for each pigment, one from
  each batch).
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(pigment)
}
\keyword{datasets}
