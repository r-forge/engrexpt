\name{labcomp}
\Rdversion{1.1}
\alias{labcomp}
\docType{data}
\title{
  Inter-lab study
}
\description{
  Inter-lab study
}
\format{
  A data frame with 28 observations on the following 3 variables.
  \describe{
    \item{\code{lab}}{a factor with levels \code{A}, \code{B}, \code{C}
      and \code{D}}
    \item{\code{pt1}}{a numeric vector}
    \item{\code{pt5}}{a numeric vector}
  }
}
\details{
  This study was an "inter-lab" study to investigate measurements taken
  on a product at various facilities.  In particular, this company has a
  research lab, a pilot line lab, and multiple labs at plants.  When
  characterizing products and especially in trouble shooting performance
  issues samples may be tested at multiple facilities and it is important
  to know if there is an impact of location on the resulting measurements.
  For this study material was sent to each lab and tested 7 times by two
  testing methods, a 1-point method at all labs and a 5-point method at
  two of the labs.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(labcomp)
}
\keyword{datasets}
