\name{weight}
\Rdversion{1.1}
\alias{weight}
\docType{data}
\title{
  Weight plastic bags held before breaking
}
\description{
  Weight plastic bags held before breaking
}
\format{
  A data frame with 43 observations on the following variable.
  \describe{
    \item{\code{weight}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(weight)
densityplot(~ weight, weight, xlab = "Weight bag held before breaking")
qqmath(~ weight, weight, ylab = "Weight bag held before breaking",
       aspect = 1, type = c("g","p"), xlab = "Standard normal quantiles")
}
\keyword{datasets}
