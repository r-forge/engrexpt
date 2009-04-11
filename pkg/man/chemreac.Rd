\name{chemreac}
\Rdversion{1.1}
\alias{chemreac}
\docType{data}
\title{
  Yield of a chemical reaction
}
\description{
  The yield of a chemical reaction was determined in a replicated \eqn{2^3}
  factorial design.
}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{
    \item{\code{temp}}{a numeric vector}
    \item{\code{cat}}{a factor with levels \code{A} \code{B}}
    \item{\code{time}}{a numeric vector}
    \item{\code{yield}}{a numeric vector}
  }
}
\examples{
str(chemreac)
dotplot(as.factor(temp) ~ yield | as.factor(time), chemreac, groups = cat,
        strip = FALSE, strip.left = TRUE, type = c("p", "a"), layout = c(1,2),
        auto.key = list(space = "right", title = "Catalyst", lines = TRUE))
}
\keyword{datasets}
