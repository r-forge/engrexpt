\name{gloss}
\Rdversion{1.1}
\alias{gloss}
\docType{data}
\title{
  Gloss of paint on cars
}
\description{
  Gloss of paint on cars
}
\format{
  A data frame with 40 observations on the following 2 variables.
  \describe{
    \item{\code{color}}{a factor with levels \code{Black}, \code{Green},
      \code{Silver} and \code{White}}
    \item{\code{gloss}}{a numeric vector}
  }
}
\details{
  The final coat of paint on a car is typically a clear coat.  This
  layer is applied on top of the layer with the color and formulations
  must be compatible with many base colors. This data was used to
  investigate the performance of a formulation on the appearance of the
  coating when used with various base colors.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(gloss)
}
\keyword{datasets}
