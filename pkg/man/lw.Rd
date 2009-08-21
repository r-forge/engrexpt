\name{lw}
\Rdversion{1.1}
\alias{lw}
\docType{data}
\title{
  Automotive paint formulation
}
\description{
  Automotive paint formulation
}
\format{
  A data frame with 24 observations on the following 3 variables.
  \describe{
    \item{\code{lw}}{a numeric vector}
    \item{\code{comp1}}{a factor with levels \code{A}, \code{B},
      \code{C} and \code{D}}
    \item{\code{comp2}}{a factor with levels \code{A}, \code{B} and
      \code{C}}
  }
}
\details{
  LW (longwave) is a surface appearance measure.  This data was from an
  experiment to choose the best combination of two components in an
  automotive paint formulation.  The goal is to maximize the LW measure.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(lw)
}
\keyword{datasets}
