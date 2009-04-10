\name{tensile}
\Rdversion{1.1}
\alias{tensile}
\docType{data}
\title{
  Tensile breaking strength of steel samples
}
\description{
  The breaking strength of samples of steel that were stress tested.
  This data set is called \code{break} in Nelson, Coffin and Copeland
  (2003).  We changed the name because \code{break} is a reserved word
  in R.
}
\format{
  A data frame with 18 observations on the following variable.
  \describe{
    \item{\code{bstrength}}{breaking strength (unknown units)}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}

\examples{
str(tensile)
qqmath(~ bstrength, tensile, aspect = 1,
       xlab = "Standard normal quantiles",
       ylab = "Breaking strength of steel samples (unknown units)")
}
\keyword{datasets}
