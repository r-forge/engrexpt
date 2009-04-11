\name{yellow}
\Rdversion{1.1}
\alias{yellow}
\docType{data}
\title{
  Initial and 1 month color measure of coated lens 
}
\description{
  Initial and 1 month color measure of coated lens 
}
\format{
  A data frame with 23 observations on the following 2 variables.
  \describe{
    \item{\code{b1}}{a numeric vector}
    \item{\code{b2}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(yellow)
show(plt1 <- xyplot(b2 ~ b1, yellow, aspect = 'iso', type = c("g","p"),
                    xlab = "Initial color measure of lens",
                    ylab = "Color measure after 1 month"))
show(tmd(plt1))
}
\keyword{datasets}
