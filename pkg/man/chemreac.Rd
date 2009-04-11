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
    \item{\code{temp}}{temperature in degrees C - an ordered factor with levels
      \code{120} < \code{140}}
    \item{\code{cat}}{catalyst - an ordered factor with levels \code{A} < \code{B}}
    \item{\code{time}}{time reaction was run, in minutes - an ordered
      factor with levels \code{10} < \code{30} }
    \item{\code{yield}}{yield of the reaction}
  }
}
\examples{
str(chemreac)
dotplot(temp ~ yield | time, chemreac, groups = cat,
        strip = FALSE, strip.left = TRUE, type = c("p", "a"), layout = c(1,2),
        auto.key = list(space = "right", title = "Catalyst", lines = TRUE))
summary(fm1 <- lm(yield ~ (time + temp + cat)^3, chemreac))
summary(fm2 <- lm(yield ~ time * temp + cat, chemreac))
}
\keyword{datasets}
