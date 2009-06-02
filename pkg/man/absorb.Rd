\name{absorb}
\Rdversion{1.1}
\alias{absorb}
\docType{data}
\title{
  Oil absorption of silica
}
\description{
	  Time ordered measurements of oil absorption from silica samples produced during 
	one manufacturing shift.
}
\format{
  A data frame with 102 observations on the following variable.
  \describe{
    \item{\code{absorb}}{a numeric vector}
  }
}
\details{
  As stated in Nelson, Coffin and Copeland (2003, p. 424), "One key
  quality parameter in the making of silica is the amount of oil that it
  can absorb since silica is often mixed with rubber and oil in various
  applications (battery separators, tires, shoe soles, etc.)" 
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(absorb)
summary(absorb$absorb)
densityplot(~ absorb, absorb,
            xlab = "Average oil absorption of silica samples")
qqmath(~ absorb, absorb,
       ylab = "Average oil absorption of silica samples",
       xlab = "Standard normal quantiles", aspect = 1)
}
\keyword{datasets}
