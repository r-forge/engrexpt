\name{deink}
\Rdversion{1.1}
\alias{deink}
\docType{data}
\title{
  De-inking of newsprint
}
\description{
  De-inking of newsprint
}
\format{
  A data frame with 27 observations on the following 3 variables.
  \describe{
    \item{\code{alkali}}{amount of alkali in the solution}
    \item{\code{hardness}}{hardness of the water - an ordered factor
      with levels \code{50} < \code{150} < \code{250}}
    \item{\code{bright}}{a measure of brightness}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(deink)
xtabs(~ alkali + hardness, deink)
dotplot(hardness ~ bright, deink, groups = alkali,
        auto.key = list(columns = 3, lines = TRUE),
        type = c("p","a"), aspect = 'xy',
        xlab = "Brightness by amount of alkali",
        ylab = "Hardness of the water")
}
\keyword{datasets}
