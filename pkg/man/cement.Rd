\name{cement}
\Rdversion{1.1}
\alias{cement}
\docType{data}
\title{
  Drying times of concrete
}
\description{
  Drying times of different concrete mixes used in a sidewalk.  Mix
  \sQuote{\code{A}} is the conventional mix.  Mixes \sQuote{\code{B}}
  and \sQuote{\code{C}} are experimental, and more expensive, mixes.
}
\format{
  A data frame with 19 observations on the following 2 variables.
  \describe{
    \item{\code{type}}{a factor with levels \code{A} \code{B} \code{C}}
    \item{\code{time}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(cement)
dotplot(reorder(type, time) ~ time, cement, type = c("p","a"), jitter.y = TRUE,
        xlab = "Time until concrete is dry enough to walk on (hours)",
        ylab = "Mix type")

}
\keyword{datasets}
