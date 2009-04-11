\name{water}
\Rdversion{1.1}
\alias{water}
\docType{data}
\title{
  Bacteria concentrations in water samples
}
\description{
  Bacteria concentrations in water samples
}
\format{
  A data frame with 50 observations on the following variable.
  \describe{
    \item{\code{bacteria}}{bacteria concentration (ppm)}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(water)
qqmath(~ bacteria, water, aspect = 1,
       xlab = "Standard normal quantiles",
       ylab = "Bacteria concentration (ppm)")
histogram(~ bacteria, water, breaks = 0:14 - 0.5,
          xlab = "Bacteria concentration (ppm)")
xtabs(~ bacteria, water)
}
\keyword{datasets}
