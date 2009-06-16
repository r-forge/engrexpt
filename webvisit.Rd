\name{webvisit}
\Rdversion{1.1}
\alias{webvisit}
\docType{data}
\title{
  Web site visits over a 3-week period
}
\description{
  Web site visits over a 3-week period
}
\format{
  A data frame with 21 observations on the following variable.
  \describe{
    \item{\code{visits}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(webvisit)
densityplot(~ visits, webvisit,
            xlab = "Web site visits in a 3-week period")
qqmath(~ visits, webvisit, aspect = 1, type = c("g","p"),
       ylab = "Web site visits in a 3-week period",
       xlab = "Standard normal quantiles")
with(webvisit, summary(visits))
}
\keyword{datasets}
