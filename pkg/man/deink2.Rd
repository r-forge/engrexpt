\name{deink2}
\Rdversion{1.1}
\alias{deink2}
\docType{data}
\title{
  De-inking of newsprint
}
\description{
  De-inking of newsprint
}
\format{
  A data frame with 15 observations on the following 3 variables.
  \describe{
    \item{\code{formula}}{a factor with levels \code{A}, \code{B} and
      \code{C}}
    \item{\code{newspaper}}{a factor with levels \code{A}, \code{B},
      \code{C}, \code{D} and \code{E}}
    \item{\code{bright}}{a numeric vector}
  }
}
\details{
  One step of recycling newsprint is to "de-ink" the newsprint, that is
  to remove the ink.  The brightness of the paper pulp after a de-inking
  process is a measure of how well the process to remove the ink worked.
  In this experiment three formulations were tested on three newspapers
  (not all newsprint and ink are identical).
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(deink2)
}
\keyword{datasets}
