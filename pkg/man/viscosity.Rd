\name{viscosity}
\Rdversion{1.1}
\alias{viscosity}
\docType{data}
\title{
  Time to gelling of paint samples
}
\description{
  Time to gelling of paint samples
}
\format{
  A data frame with 17 observations on the following variable.
  \describe{
    \item{\code{time}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(viscosity)
with(viscosity, summary(time))
qqmath(~ time, viscosity, aspect = 1,
       xlab = "Standard normal quantiles",
       ylab = "Time until paint sample gelled")
qqmath(~ sqrt(time), viscosity, aspect = 1,
       xlab = "Standard normal quantiles",
       ylab = "Square root of time until paint sample gelled")
}
\keyword{datasets}
