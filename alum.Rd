\name{alum}
\Rdversion{1.1}
\alias{alum}
\docType{data}
\title{
  Aluminum impurity amounts
}
\description{
  As described in Nelson, Coffin and Copeland (2003),
  \dQuote{
    Over a 2-hour period, twenty-five 200 gm samples were drawn at
    random from a process that recycles plastic, and the amount of
    aluminum impurities in the sample in ppm was determined for each
    sample.
  }
}
\format{
  A data frame with 25 observations on the following variable.
  \describe{
    \item{\code{ppm}}{amount of aluminum impurities (ppm)}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(alum)
qqmath(~ ppm, alum, xlab = "Standard normal quantiles",
       aspect = 1, ylab = "Amount of aluminum impurities (ppm)",
       type = c("g","p"))
qqmath(~ log(ppm), alum, xlab = "Standard normal quantiles",
       aspect = 1, type = c("g","p"),
       ylab = "Natural log of amount of aluminum impurities (log(ppm))")
}
\keyword{datasets}
