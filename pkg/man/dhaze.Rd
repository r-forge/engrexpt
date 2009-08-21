\name{dhaze}
\Rdversion{1.1}
\alias{dhaze}
\docType{data}
\title{
  Haze of lenses after abrasion
}
\description{
  Haze of lenses after 150 test cycles of abrasion
}
\usage{data(dhaze)}
\format{
  A data frame with 28 observations on the following 2 variables.
  \describe{
    \item{\code{treatment}}{a factor with levels \code{A}, \code{B},
      \code{C} and \code{D}}
    \item{\code{dhaze}}{a numeric vector}
  }
}
\details{
  A maker of coatings for eye glass lenses tested three treatments (or
  coating formulations) for the ability to withstand wear (simulated with
  an abrasion test) as measured by haze.  Low haze is desirable.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(dhaze)
}
\keyword{datasets}
