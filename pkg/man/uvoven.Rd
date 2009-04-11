\name{uvoven}
\Rdversion{1.1}
\alias{uvoven}
\docType{data}
\title{
  UV absorbance for lens cured in different ovens
}
\description{
  UV absorbance for lens cured in different ovens
}
\format{
  A data frame with 60 observations on the following 2 variables.
  \describe{
    \item{\code{oven}}{oven - a factor with levels \code{A} \code{B}}
    \item{\code{uv}}{UV absorbance}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(uvoven)
summary(uvoven)
densityplot(~ uv, uvoven, groups = oven, auto.key = list(columns = 2),
            xlab = "UV absorbance")
qqmath(~ uv, uvoven, groups = oven,
       auto.key = list(space = "right", title = "Oven"),
       xlab = "Standard normal quantiles", type = c("p","g"),
       ylab = "UV absorbance",
       panel = function(...)
    {
       panel.qqmath(...)
       panel.qqmathline(..., alpha = 0.5)
    })
}
\keyword{datasets}
