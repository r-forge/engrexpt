\name{yield}
\Rdversion{1.1}
\alias{yield}
\docType{data}
\title{
  Yield of a chemical process
}
\description{
  Yield of a chemical process
}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{temp}}{temperature of the slurry(degrees C)}
    \item{\code{pH}}{pH of the slurry}
    \item{\code{yield}}{yield of the process (tons)}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(yield)
xyplot(yield ~ temp|ordered(pH), yield, aspect = 'xy',
       type = c("g","p","r"), xlab = "Temperature of the slurry (degrees C)",
       ylab = "Yield (tons)")
}
\keyword{datasets}
