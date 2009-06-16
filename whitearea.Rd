\name{whitearea}
\Rdversion{1.1}
\alias{whitearea}
\docType{data}
\title{
  Comparison of mixing processes
}
\description{
  The percentage of \dQuote{white area} in a sample of a dark product
  is used to evaluate how well a white raw material has been mixed
  into the product.  Data are obtained from two processes.
}
\format{
  A data frame with 48 observations on the following 2 variables.
  \describe{
    \item{\code{process}}{a factor with levels \code{A} \code{B}}
    \item{\code{whitearea}}{a numeric vector}
  }
}
\examples{
str(whitearea)
qqmath(~ whitearea, whitearea, groups = process, aspect = 1,
       auto.key = list(space = "right", title = "Process"),
       xlab = "Standard normal quantiles",
       ylab = "White area")
t.test(whitearea ~ process, whitearea)
}
\keyword{datasets}
