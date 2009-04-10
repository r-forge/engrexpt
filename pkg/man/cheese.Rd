\name{cheese}
\Rdversion{1.1}
\alias{cheese}
\docType{data}
\title{
  Percentage fat in cheese
}
\description{
  A manufacturer of cheese supplies a major pizza chain from three
  different manufacturing locations.  Samples from 6 different batches
  at each of three different plants were assayed for the percentage fat
  content.
}
\format{
  A data frame with 90 observations on the following 3 variables.
  \describe{
    \item{\code{plant.s10.}}{a factor with levels \code{A} \code{B} \code{C}}
    \item{\code{batch}}{a numeric vector}
    \item{\code{fat}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(cheese)
}
\keyword{datasets}
