\name{bacteria}
\Rdversion{1.1}
\alias{bacteria}
\docType{data}
\title{
  Bacteria count in water samples
}
\description{
  As described in Nelson, Coffin and Copeland (2003), \dQuote{Purified
    water is used in one step in the production of a medical device.
    The water is tested daily for bacteria.  These data are the results
    from 50 days of testing.  They are the counts of a particular strain
    of bacteria in a 100 ml. sample of water.}
}
\format{
  A data frame with 50 observations on the following variable.
  \describe{
    \item{\code{level}}{bacteria count}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(bacteria)
qqmath(~ level, bacteria, aspect = 1,
       xlab = "Standard normal quantiles",
       ylab = "Bacteria count")
}
\keyword{datasets}
