\name{bright}
\Rdversion{1.1}
\alias{bright}
\docType{data}
\title{
  Brightness of de-inked newspaper
}
\description{
  Data from a half-fraction of a \eqn{2^5} factorial experiment
  measuring the brightness of de-inked newsprint.
}
\usage{data(bright)}
\format{
  A data frame with 16 observations on the following 6 variables.
  \describe{
    \item{\code{type}}{type of alkali - an ordered factor with levels \code{A} < \code{B}}
    \item{\code{percent}}{percentage of alkali (25\% or 75\%)}
    \item{\code{time}}{time pulp is soaked (30 or 40 minutes)}
    \item{\code{hardness}}{water hardness (150 or 250)}
    \item{\code{speed}}{agitation rate - an ordered factor with levels
      \code{S} < \code{F}}
    \item{\code{bright}}{Brightness of pulp}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(bright)
options(contrasts = c("contr.treatment", "contr.helmert"))
summary(fm1 <- lm(bright ~ (type + percent + time + hardness + speed)^2,
                  bright))
qqmath(coef(fm1)[-1], aspect = 1,
       panel = function(...)
   {
       panel.grid(h = -1, v = -1)
       panel.qqmath(...)
       panel.qqmathline(..., alpha = 0.5, lty = 2)
   },
       xlab = "Standard normal quantiles",
       ylab = "First- and second-order effects")
summary(fm2 <- lm(bright ~ (type + hardness)^2, bright))
dotplot(type ~ bright, bright, groups = hardness,
        type = c("p", "a"), jitter.y = TRUE,
        xlab = "Brightness of pulp (lines and point styles are levels of water hardness)",
        ylab = "Alkali type", aspect = 0.4,
        auto.key = list(lines = TRUE, columns = 2))
}
\keyword{datasets}
