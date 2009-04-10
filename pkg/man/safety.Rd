\name{safety}
\Rdversion{1.1}
\alias{safety}
\docType{data}
\title{
  Safety violations over time
}
\description{
  Safety violations over time
}
\format{
  A data frame with 30 observations on the following 4 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{place}}{a factor with levels \code{CA} \code{KY} \code{MI} \code{NY} \code{TN} \code{TX}}
    \item{\code{employees}}{a numeric vector}
    \item{\code{cases}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(safety)
}
\keyword{datasets}
