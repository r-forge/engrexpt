\name{assay}
\Rdversion{1.1}
\alias{assay}
\docType{data}
\title{
  Yield from two processes
}
\description{
  A resin is one component in paint formulations.  A chemist was working
  on a process for producing a resin needed in a paint formulation.  Two
  competing processes were tested for the amount of resin the could
  produce.
}
\format{
  A data frame with 9 observations on the following 2 variables.
  \describe{
    \item{\code{process}}{a factor with levels \code{A} and \code{B}}
    \item{\code{yield}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(assay)
summary(assay)
dotplot(process ~ yield, assay)
}
\keyword{datasets}
