\name{cure}
\Rdversion{1.1}
\alias{cure}
\docType{data}
\title{
  Yield from a chemical curing process
}
\description{
  Process engineers wish to determine a \dQuote{bake window}, the
  combination of time and temperature in a curing process, that will
  maximize the yield of the product.
}
\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{\code{time}}{time - an ordered factor with levels
      \code{20} < \code{40}}
    \item{\code{temp}}{temperature - an ordered factor with levels
      \code{Low} < \code{Med} < \code{High}}
    \item{\code{yield}}{yield of parts}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(cure)
dotplot(temp ~ yield, cure, groups = time, type = c("p","a"),
        jitter.y = TRUE, xlab = "Yield of parts")
summary(fm1 <- lm(yield ~ time * temp, cure))
}
\keyword{datasets}
