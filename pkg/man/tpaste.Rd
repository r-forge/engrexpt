\name{tpaste}
\Rdversion{1.1}
\alias{tpaste}
\docType{data}
\title{
  Turbidity of a toothpaste formulation
}
\description{
  Results from an experiment to assess the turbidity of a toothpaste
  formulation.  The three factors that are varied at the NaCl level, the
  reaction temperature and the addition rate of a particular component.
}
\format{
  A data frame with 8 observations on the following 4 variables.
  \describe{
    \item{\code{NaCl}}{NaCl level (2 or 20 ml.)}
    \item{\code{temp}}{Reaction temperature (50 or 80 degrees C)}
    \item{\code{rate}}{Addition rate (30 or 60 seconds)}
    \item{\code{turbidity}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(tpaste)
opt <- options(contrasts = c("contr.treatment", "contr.helmert"))
summary(fm6 <- lm(turbidity ~ (NaCl + temp + rate)^3, tpaste))$coefficients
qqmath(coef(fm6)[-1], xlab = "Standard normal quantiles",
       ylab = "Estimated coefficients", aspect = 1, type = c("g", "p"),
       panel = function(...){panel.qqmath(...); panel.qqmathline(...,lty=2,alpha=0.5)})
xyplot(sort(abs(coef(fm6)[-1])) ~ sqrt(qchisq(ppoints(7), df = 1)),
       xlab = "Quantiles of absolute value of the standard normal",
       ylab = "Absolute value of estimated coefficients", type = c("g","p"))
summary(fm7 <- lm(turbidity ~ NaCl * rate, tpaste))
options(opt)
}
\keyword{datasets}
