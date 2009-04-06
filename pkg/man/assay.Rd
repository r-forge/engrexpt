\name{assay}
\Rdversion{1.1}
\alias{assay}
\docType{data}
\title{
  Yield from two processes
}
\description{
  As described in Nelson, Coffin and Copeland (2003),
  \dQuote{Two different chemical processes are being considered for
    producing a resin needed in the manufacture of paint}.
}
\format{
  A data frame with 9 observations on the following 2 variables.
  \describe{
    \item{\code{process}}{a factor with levels \code{A} \code{B}}
    \item{\code{yield}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(assay)
summary(assay)
dotplot(process ~ yield, assay)
}
\keyword{datasets}
